######################################################################
##   Figure and Tables code                                         ##
##   RFQAmodel, CE West, SHP de Oliveira & CM Deane                 ##
##   Clare E. West     May 2019                                     ##
######################################################################

library(tidyverse)
library(randomForest)
library(caret)
library(ROCR)
library(cowplot)
library(xtable)

calculate_performance <- function(score,labels,plot_index,plot.colours,add_to_plot=FALSE, do_not_plot=FALSE) {
       		pr <- prediction(score, as.factor(labels))
       		perf <- performance(pr, "tpr", "fpr")
       		auc = performance(pr, measure = "auc")
          if (!do_not_plot){
       		  plot(perf,col=plot.colours[plot_index],lwd=2,lty=plot_index,add=add_to_plot)
          }
          return (c(pr,auc,perf))
}

##### ROC plots
plot_roc <- function(resultsdf, featurelist, topmodel = FALSE, addhighconf = FALSE) {
  roc.results <- list(length(featurelist))
  if (topmodel){
  roc_scores <- resultsdf %>% 
    mutate(SAINT2=-SAINT2) %>% 
    select(Target, Decoy, TMScore, Label, featurelist) %>% 
    group_by(Target) %>% 
    gather(key="Score", value="value", -c("Target","Decoy", "TMScore", "Label")) } 
  else{
  roc_scores <- resultsdf %>% 
    mutate(SAINT2=-SAINT2, EigenTHREADER=EigenTHREADER/ET_Max) %>% 
    select(Target, Decoy, TMScore, Label, featurelist) %>% 
    group_by(Target) %>% 
    gather(key="Score", value="value", -c("Target","Decoy", "TMScore", "Label")) 
  }
  roc_scores.top <- roc_scores %>% 
    group_by(Target, Score) %>% 
    slice(which.max(value)) 
  pdf(NULL)
  dev.control(displaylist="enable")
  par(pty="s", xpd=NA, bg = "transparent", mgp = c(2,1,0))
  plot.colours=gg_color_hue(length(featurelist))
  if (topmodel){
    plottitle = "Highest-ranking model per target"
    plotscores <- roc_scores.top
    legx = 0.5
  } else {
    plottitle = "All models"
    plotscores <- roc_scores
    legx=0.44
  }
  legend.labels <- vector()
  for (i in 1:length(featurelist)) {
    add = !i==1
    plotroc = topmodel*(featurelist[i]=="EigenTHREADER")  ## Remove EigenTHREADER because it's normalised so is not informative on this plot
    roc.results[[i]] <- calculate_performance((subset(plotscores, Score==featurelist[i])$value),subset(plotscores, Score==featurelist[i])$Label,i,plot.colours,add, plotroc)
      legend.labels[i] <- paste(featurelist[i]," (",round(roc.results[[i]][[2]]@y.values[[1]],2),")", sep="")
  }
  if (topmodel){
    legend.index = which(featurelist!="EigenTHREADER")} else {
      legend.index = 1:length(featurelist)
  }
  legend(x = c(legx, 1), y = c(length(featurelist)*0.05, 0), legend.labels[legend.index], lwd = 2, lty = legend.index,col=plot.colours[legend.index], cex=0.8,bty = "n", y.intersp=1.5)
  title(plottitle)
  if (addhighconf){
    ## Add lines to indicate values at RFQAmodel=0.5 cutoff
    perf <- roc.results[[1]][[3]]
    cutoffs <- data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]],tpr=perf@y.values[[1]])
    high <- cutoffs %>% filter(abs(cut-0.5)==min(abs(cut-0.5)))
    abline(h=high$tpr, v=high$fpr, lty="dotted",col="gray85")
  }
  p <- recordPlot()
  invisible(dev.off())
  return(list(p = p, results = roc.results))
}

plot_roclike <- function(rocresultslist, methods, val_max){
### ROC-like plot showing TP vs FPR
  colours=gg_color_hue(length(methods))
tprs <- list()
    for (METHOD in methods){
        pred <- rocresultslist[[which(methods==METHOD)]][[1]]
        perf <- rocresultslist[[which(methods==METHOD)]][[3]]
        tprs[[which(methods==METHOD)]] <- data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]],tpr=perf@y.values[[1]], tp = pred@tp[[1]], n.pos.pred=pred@n.pos.pred[[1]],n.neg.pred=pred@n.neg.pred[[1]]) %>% mutate(Method=METHOD)
}

tprs %>% bind_rows() %>% mutate(recall=tp/val_max) %>% mutate_if(is.numeric, round, 2)-> all
all$Method <- factor(all$Method, levels=methods)

## Legend position
if (length(methods)>7){
  legs = c(.2, .7)
} else {
  legs = c(.8, .84)
}

## Make the plot
roclikep <- ggplot(all, aes(x=fpr,y=tp,linetype=Method, colour=Method)) + 
  theme_bw() + 
  geom_line(size=0.7) + 
  theme_bw() + 
  scale_x_continuous(trans="log10") + 
  annotation_logticks(sides="b") + 
  scale_linetype_manual(values=c(1:length(methods))) + 
  scale_colour_manual(values=colours[1:length(methods)]) + 
  geom_hline(yintercept=val_max, lty="dotted", colour="darkgrey") + 
  theme(panel.border = element_rect(colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position=legs, 
        legend.key.size = unit(0.8, "lines"), 
        legend.background = element_blank(), 
        legend.title=element_blank(), 
        plot.title = element_text(size=14, hjust = 0.5, face="bold", margin = ggplot2::margin(0,0,20,0)), 
        axis.text = element_text(size=12, colour="black"), 
        axis.text.y = element_text(margin = ggplot2::margin(0,9,0,0)), 
        axis.text.x = element_text(margin = ggplot2::margin(9,0,0,0)),  
        axis.title = element_text(size=12), 
        axis.ticks = element_line(colour="black"), 
        axis.ticks.length = unit(0.5,"lines"), aspect.ratio = 1) + 
  labs(x="False positive rate", y="True positives", title="Highest-ranking model per target") + 
  scale_y_continuous(breaks = seq(0,200,ifelse(val_max > 100, 20, 5)), limits=c(0,val_max+ifelse(val_max > 100, 10, 5)))

return(list(p=roclikep, results=all))
}


##### Beautiful colour palettes #####

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

colours=gg_color_hue(6)

## Results from classifying Validation Set models
results <- read.table(file="results/RFQAmodel_validation_results.txt", header=TRUE, stringsAsFactors=FALSE)


### ROC Curves ###

roc.features = c("RFQAmodel", "PCons", "ProQ3D", "EigenTHREADER", "PCombC", "SAINT2") ## ordered by AUC

p1.validationroc <- plot_roc(results, roc.features)
p2.validationroc <- plot_roc(results, roc.features, topmodel = TRUE)

allroc.features = c("RFQAmodel", "SAINT2", "PCons", "PCombC", "ProQ2D","ProQ3D","ProQRosFAD","ProQRosCenD","PPV","MapAlign","EigenTHREADER")
p3.validationroc <- plot_roc(results, allroc.features)
p4.validationroc <- plot_roc(results, allroc.features, topmodel = TRUE)

max_correct <- results %>% group_by(Target) %>% slice(which.max(TMScore)) %>% ungroup %>% summarise(sum(TMScore>=0.5)) %>% pull()
validationroclike.some <- plot_roclike(p2.validationroc$results, roc.features, max_correct)
validationroclike.all <- plot_roclike(p4.validationroc$results, allroc.features, max_correct)

### Main text Figure, top features
ggsave(validationroclike.some$p, file="figures/Figure03.eps", width=4.5, height=4.5)
ggsave(validationroclike.some$p, file="figures/Figure03.pdf", width=4.5, height=4.5)

### SI Figure, all features
pg.validationroc <- plot_grid(p3.validationroc$p, validationroclike.all$p, labels=c("A","B"), ncol=2)
ggsave(pg.validationroc, file="figures/SIFig11.pdf", width=8.5, height=4.5)

### Results from classifying CASP models
CASP_results <- read.table("results/RFQAmodel_CASP_test_results.txt", header=TRUE, stringsAsFactors=FALSE)

## SI Table 4: CASP results 
CASP_results %>% 
  bind_rows(CASP_results %>% mutate(Confidence = "All")) %>% 
  group_by(Type, Confidence, Target) %>% 
  filter(!is.na(TMScore)) %>% 
  mutate(Best=max(TMScore)>=0.5) %>% 
  arrange(-RFQAmodel) %>% 
  slice(1:5) %>%  mutate(Top5=max(TMScore >=0.5)) %>% 
  slice(1) %>% summarise(Top5=Top5, Top1=sum(TMScore>=0.5), Max=sum(Best)) %>% 
  summarise(Top5=sum(Top5), Top1=sum(Top1),Max=sum(Max),Total=length(Confidence)) %>% 
  mutate(Precision.Top5=round(Top5/Total,2), Precision.Top1=round(Top1/Total,2)) %>% 
  xtable() %>% 
  print(file="results/RFQAmodel_CASP_summary_table.tex", include.rownames=FALSE)

### CASP13 ROC Curves compared to other QA scores 

not_evaluated <- c("T0952","T0956","T0972","T0988","T0994","T1007","T1012","T1023s1","T1023s2","T1023s3","T0908","T0916","T0919","T0924","T0925","T0926","T0927","T0935","T0936","T0937","T0938","T0939","T0940","T0999","T1000","T1004","T1011","T0974s2","T0963","T0960","T0980s2")
QA <- read.table("data/CASP_stage2_QA_predictions.txt", stringsAsFactors=FALSE, header=TRUE) %>% filter(! Target %in% not_evaluated)
cQA <- QA %>% reshape2::dcast(Set + Stage + Target + Decoy ~ Group, value.var="QA") 
tQA <- CASP_results %>% merge(cQA, by=c("Set","Target","Decoy"))
tQA <- tQA %>% mutate(Label = ifelse(TMScore >= 0.5, 1,0)) %>% filter(Type=="FM", !is.na(TMScore), Set=="CASP13") 

casp13roc.features = c("RFQAmodel", "QA058", "QA022", "QA275","QA139", "EigenTHREADER")
p1.casp13roc <- plot_roc(tQA %>% filter(Type=="FM"), casp13roc.features)
p2.casp13roc <- plot_roc(tQA %>% filter(Type=="FM"), casp13roc.features, topmodel=TRUE)

CASP13_max = CASP_results %>% filter(Type=="FM", Set=="CASP13") %>% group_by(Target) %>% slice(which.max(TMScore)) %>% ungroup() %>% summarise(sum(TMScore>=0.5)) %>% pull()
casp13roclike <- plot_roclike(p2.casp13roc$results, casp13roc.features, CASP13_max)

pg.casp13proc <- plot_grid(p1.casp13roc$p, casp13roclike$p, labels=c("A","B"))
ggsave(pg.casp13proc, file="figures/Figure05.eps",width=9,height=4.5)
ggsave(pg.casp13proc, file="figures/Figure05.pdf",width=9,height=4.5)

## SI Fig 12
all_results <- results %>% 
  bind_rows(read.table(file="results/RFQAmodel_training_results.txt", header=TRUE, stringsAsFactors=FALSE)) %>%
  bind_rows(CASP_results %>% 
            filter(Type=="FM") %>% 
            select(-TMscore.C,-Category, -Domain, -Type) %>% 
            mutate(Set="CASP12/13")) %>%
  mutate(Set = factor(Set, levels=c('Training','Validation','CASP12/13')))

clare_pal <- c("#00BF7D", "#A3A500", "#F8766D","#00B0F6","#E76BF3","#636363")
axis_label_size = 8
facet_label_size = axis_label_size + 1
title_size = 10
label_size = 2
legend_label_size = axis_label_size
rectangles=data.frame(x1=c(0,0,0,0), x2=c(1,1,1,1), y1=c(0,0.1,0.3,0.5), y2=c(0.1,0.3,0.5,1), category=c('Failed','Low','Medium','High'))
rectangles$category = factor(rectangles$category, levels=c("High","Medium","Low","Failed"))

np <- ggplot(all_results %>% 
             group_by(Set, Target) %>% 
             slice(which.max(RFQAmodel))) + 
  geom_rect(data=rectangles, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=category), alpha=0.1) + 
  geom_point(aes(x=TMScore, y=RFQAmodel), alpha=0.6, size=0.5) +   
  scale_y_continuous(limits=c(0,1), breaks=c(seq(0,1,0.2)), expand=c(0,0)) +   
  scale_x_continuous(limits=c(0,1), breaks=c(seq(0,1,0.2)), expand=c(0,0)) + 
  geom_hline(yintercept=c(0.1,0.3,0.5)) + 
  geom_vline(xintercept=0.5) +   
  theme_minimal() + 
  theme(legend.position="bottom", 
        legend.box="vertical", 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=axis_label_size),
        axis.title=element_text(size=facet_label_size), 
        legend.text=element_text(size=legend_label_size), 
        panel.spacing.x = unit(5,"mm")) + 
  labs(x="TM-Score",y="RFQAmodel") + 
  facet_wrap(~Set) + 
  scale_fill_manual("", values=c(clare_pal[1:3],"darkgray")) + 
  coord_equal() 

ggsave(np, file="figures/SIFig12.pdf", width=10,height=4,dpi=350,device="pdf")

### Extra models
extra_results <- read.table("results/RFQAmodel_extras_results.txt",header=TRUE, stringsAsFactors=FALSE)

p.extras <- extra_results %>% 
  group_by(Set, Target) %>% 
  mutate(Top.TM=max(TMScore)) %>% 
  arrange(Set, Target, -RFQAmodel) %>% 
  slice(1:5) %>% 
  arrange(Set, Target, -TMScore) %>% 
  slice(1)  %>% 
  mutate(Confidence = factor(Confidence, levels=c("High","Medium","Low","Failed"))) %>% 
  ggplot(., aes(x=Set)) + 
         geom_hline(yintercept=0.5, colour="#333333") + 
         geom_boxplot(data=extra_results, aes(x=Set, y=TMScore, group=Set), alpha=0.1, colour="#333333") + 
         geom_point(aes(y=TMScore, colour=Confidence), shape=16) + 
         geom_point(aes(y=Top.TM), shape=1) + 
         geom_line(aes(x=as.numeric(Set), y=TMScore, colour=Confidence)) + 
         facet_wrap(~Target) + 
         theme_bw() +  
         theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
         scale_y_continuous(limits=c(0,0.75)) + 
         scale_colour_manual("", values=c(clare_pal[1:4])) + 
         labs(x="Number of Models") +  
         theme(legend.position="bottom", strip.background = element_blank(),panel.border = element_rect(colour = "black"), panel.grid.minor = element_blank())

ggsave(p.extras, file="figures/Figure06.eps", width=7, height=5)
ggsave(p.extras, file="figures/Figure06.pdf", width=7, height=5)


load("RFQAmodel_classifier.Rda")
### Importance plot
imp <- varImpPlot(RFQAmodel) %>% as.data.frame() 
imp <- imp %>% mutate(Feature = rownames(imp))
imp$Feature <- recode(imp$Feature, "MapAlign"="map_align", "MapLength" = "map_length", "ProQRosCenD" = "Rosetta_centroid", "ProQRosFAD" = "Rosetta_full_atom", "Contact" = "SAINT2_Contact", "Correct" = "RFQAmodel", "Best" = "Total_Successes" )

plot_importance <- function(df, measure){
  ggplot(df %>% arrange(-get(measure)) %>% slice(1:20), aes(x=get(measure), y=reorder(Feature, get(measure)))) +
   geom_point() + 
   labs(x=measure, y="") + 
   theme_minimal() + 
   theme(panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank())
}

p.importance <- plot_grid(plot_importance(imp,"MeanDecreaseAccuracy"), plot_importance(imp,"MeanDecreaseGini"), rel_widths=c(1,1))
ggsave(p.importance, file="figures/importance.pdf")
