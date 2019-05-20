######################################################################
##   Code for RFQAmodel, CE West, SHP de Oliveira & CM Deane        ##
##   Clare E. West     May 2019                                     ##
######################################################################

library(tidyverse)
library(randomForest)
library(caret)
library(ROCR)

### get ensemble features, PCombC, and remove extras
get_features <- function(df){
df2 <- df %>% 
  select(-SCOP_Class) %>%  
  group_by(Target) %>% 
  mutate(S2_Max=min(SAINT2), S2_Med=median(SAINT2), S2_Min=max(SAINT2), S2_Spr=S2_Max-S2_Med,
        Con_Max=min(Contact), Con_Med=median(Contact), Con_Min=max(Contact),Con_Spr=Con_Max-Con_Med,
        PC_Max=max(PCons), PC_Med=median(PCons), PC_Min=min(PCons), PC_Spr=PC_Max-PC_Med,
        ProQ2D_Max=max(ProQ2D), ProQ2D_Med=median(ProQ2D), ProQ2D_Min=min(ProQ2D), ProQ2D_Spr=ProQ2D_Max-ProQ2D_Med,
        RosCen_Max=max(ProQRosCenD), RosCen_Med=median(ProQRosCenD),RosCen_Min=min(ProQRosCenD), RosCen_Spr=RosCen_Max-RosCen_Med,
        RosFA_Max=max(ProQRosFAD), RosFA_Med=median(ProQRosFAD), RosFA_Min=min(ProQRosFAD), RosFA_Spr=RosFA_Max-RosFA_Med,
        ProQ3D_Max=max(ProQ3D), ProQ3D_Med=median(ProQ3D), ProQ3D_Min=min(ProQ3D), ProQ3D_Spr=ProQ3D_Max-ProQ3D_Med,
        PCombC_Max=max(PCombC), PCombC_Med=median(PCombC), PCombC_Min=min(PCombC), PCombC_Spr=PCombC_Max-PCombC_Med,
        PPV_Max=max(PPV), PPV_Num=(PPV*NumCon),
        MA_Max=max(MapAlign), MA_Med=median(MapAlign), MA_Min=min(MapAlign), MA_Spr=MA_Max-MA_Med, MA_LEN=max(MapLength),
        ET_Max=max(EigenTHREADER), ET_Med=median(EigenTHREADER), ET_Min=min(EigenTHREADER), ET_Spr=ET_Max-ET_Med)
return(df2)
}

## Get features
trainingset = read.table("RFQAmodel_training.txt", header=TRUE, stringsAsFactors=FALSE)
validationset = read.table("RFQAmodel_validation.txt", header=TRUE, stringsAsFactors=FALSE)

## Label models as correct or incorrect 
trainingset <- trainingset %>% mutate(Label = ifelse(TMScore >= 0.5, 1,0))
validationset <- validationset %>% mutate(Label = ifelse(TMScore >= 0.5, 1,0))

## get features for RFQAmodel
train_tab <- get_features(trainingset)
val_tab <- get_features(validationset)

# Train Random Forest Classifier to classify models into Correct/Incorrect (or load a pre-trained version)
if(0){
  set.seed(1011)
  RFQAmodel <- randomForest(as.factor(Label) ~ . -Decoy -Target -TMScore -Set, data=train_tab, importance=TRUE, ntree=500,mtry = 7)
  save(RFQAmodel, file="RFQAmodel_classifier.Rda")
} else {
  load("RFQAmodel_classifier.Rda")
}

## Classify the Validation set and categorise predictions into confidence levels
results <- val_tab %>% ungroup() %>% 
  mutate(RFQAmodel = as.numeric(predict(RFQAmodel,val_tab,type="prob")[,2])) %>% 
  group_by(Target) %>% 
  mutate(Confidence = ifelse(max(RFQAmodel) > 0.5 , "High", ifelse(max(RFQAmodel) > 0.3, "Medium", ifelse(max(RFQAmodel) > 0.1, "Low","Failed"))))

#write.table(results, file="RFQAmodel_validation_results.txt", row.names=FALSE, quote=FALSE)

## Add "All", "High and Medium", and "Predicted Modelling"
## For each confidence level, get:
## - the number modelling successes (Max) 
## - number of correct highest-ranking (top1)
## - best of five highest ranking (top5)
## - the total number of targets (total)
## - precision

stats <- results %>% 
  bind_rows((results %>% mutate(Confidence = "All")), 
            (results %>% filter(Confidence %in% c("High","Medium")) %>% mutate(Confidence = "High.and.Medium")), 
            results %>% filter(Confidence != "Failed") %>% mutate(Confidence = "Predicted.Modelling.Success")) %>% 
  group_by(Confidence, Target) %>% 
  mutate(Best=max(TMScore)>=0.5) %>% 
  arrange(-RFQAmodel) %>% 
  slice(1:5) %>%
  mutate(Top5=max(TMScore >=0.5)) %>% 
  slice(1) %>% 
  summarise(Top5=Top5, Top1=sum(TMScore>=0.5), Max=sum(Best)) %>% 
  summarise(Top5=sum(Top5), Top1=sum(Top1),Max=sum(Max),Total=length(Confidence)) %>% 
  mutate(Precision.Top5=Top5/Total, Precision.Top1=Top1/Total)

print(stats)

## Classify models in the Training Set
train_results <- train_tab %>% ungroup() %>% mutate(RFQAmodel=as.numeric(predict(RFQAmodel, train_tab, type="prob")[,2]))
write.table(train_results, file="RFQAmodel_training_results.txt", row.names=FALSE, quote=FALSE)


## CASP Test set
not_evaluated <- c("T0952","T0956","T0972","T0988","T0994","T1007","T1012","T1023s1","T1023s2","T1023s3","T0908","T0916","T0919","T0924","T0925","T0926","T0927","T0935","T0936","T0937","T0938","T0939","T0940","T0999","T1000","T1004","T1011","T0974s2","T0963","T0960","T0980s2")
testset <- read.table("RFQAmodel_CASP_test.txt", header=TRUE, stringsAsFactors=FALSE) %>% filter(! Target %in% not_evaluated)
test_tab <- get_features(testset)

res <- read.table("CASPs_results_table.txt", header=TRUE, stringsAsFactors=FALSE) %>% rename(Set=CASP)             # scores from CASP
cat <- read.table("classifications.txt", col.names=c("Set","Target","Category","Domain"), stringsAsFactors=FALSE)  # classification of targets from CASP

## Classify models
## Add QA predictions, CASP results and target categories
CASP_results <- test_tab %>% 
  ungroup() %>% 
  mutate(RFQAmodel = as.numeric(predict(RFQAmodel, test_tab, type="prob")[,2]))  %>% 
  left_join(res, by=c("Set","Decoy")) %>% 
  left_join(cat, by=c("Set","Target")) %>% 
  mutate(TMScore = TMscore.C) %>% 
  group_by(Target) %>% 
  mutate(Label = ifelse(TMScore >= 0.5, 1,0)) %>% 
  mutate(Confidence = ifelse(max(RFQAmodel) > 0.5 , "High", ifelse(max(RFQAmodel) > 0.3, "Medium", ifelse(max(RFQAmodel) > 0.1, "Low","Failed")))) %>% 
  filter(Category!="not") %>% 
  mutate(Type=ifelse(Category %in% c("FM","FM/TBM"), "FM", "TBM"))

#write.table(CASP_results, file="RFQAmodel_CASP_test_results.txt", quote=FALSE, row.names=FALSE)

## Calculate precision
CASP_stats <- CASP_results %>% 
  bind_rows((CASP_results %>% mutate(Confidence = "All")), 
            (CASP_results %>% filter(Confidence %in% c("High","Medium")) %>% mutate(Confidence = "High.and.Medium")), 
            CASP_results %>% filter(Confidence != "Failed") %>% mutate(Confidence = "Predicted.Modelling.Success")) %>% 
  group_by(Set, Type, Confidence, Target) %>% 
  filter(!is.na(TMScore)) %>% 
  mutate(Best=max(TMScore)>=0.5) %>% 
  arrange(-RFQAmodel) %>% 
  slice(1:5) %>% mutate(Top5=max(TMScore >=0.5)) %>% 
  slice(1) %>% summarise(Top5=Top5, Top1=sum(TMScore>=0.5), Max=sum(Best)) %>% 
  summarise(Top5=sum(Top5), Top1=sum(Top1),Max=sum(Max),Total=length(Confidence)) %>% 
  mutate(Precision.Top5=Top5/Total, Precision.Top1=Top1/Total)

print(CASP_stats)

if(0){
#train_prediction = predict(RFQAmodel,train_tab,type="prob")
#train_results <- cbind(as.data.frame(train_tab), train_prediction)

all_results <- bind_rows(results, train_tab %>% ungroup() %>% mutate(RFQAmodel=as.numeric(predict(RFQAmodel, train_tab, type="prob")[,2]))) 
}
if(0){

clare_pal <- c("#00BF7D", "#A3A500", "#F8766D","#00B0F6","#E76BF3","#636363")
axis_label_size = 8
facet_label_size = axis_label_size + 1
title_size = 10
label_size = 2
legend_label_size = axis_label_size 

rectangles=data.frame(x1=c(0,0,0,0), x2=c(1,1,1,1), y1=c(0,0.1,0.3,0.5), y2=c(0.1,0.3,0.5,1), category=c('Failed','Low','Medium','High'))
rectangles$category = factor(rectangles$category, levels=c("High","Medium","Low","Failed"))

np <- ggplot(all_results %>% group_by(Set, Target) %>% slice(which.max(RFQAmodel))) + geom_rect(data=rectangles, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=category), alpha=0.1) + geom_point(aes(x=TMScore, y=RFQAmodel), alpha=0.6, size=0.5) +   scale_y_continuous(limits=c(0,1), breaks=c(seq(0,1,0.2)), expand=c(0,0)) +   scale_x_continuous(limits=c(0,1), breaks=c(seq(0,1,0.2)), expand=c(0,0)) + geom_hline(yintercept=c(0.1,0.3,0.5)) + geom_vline(xintercept=0.5) +   theme_minimal() + theme(legend.position="bottom", legend.box="vertical", panel.grid.minor = element_blank(),axis.text=element_text(size=axis_label_size),axis.title=element_text(size=facet_label_size), legend.text=element_text(size=legend_label_size), panel.spacing.x = unit(5,"mm")) + labs(x="TM-Score",y="RFQAmodel") + facet_wrap(~Set) + scale_fill_manual("", values=c(clare_pal[1:3],"darkgray")) + facet_wrap(~Set)

ggsave(np, file="~/pub_html/Clare/Apr2019/new_plot.pdf", width=8.1,height=4,dpi=350,device="pdf")

all_all_results <- CASP_results2 %>% filter(CASP=="CASP13", Type=="FM") %>% mutate(SCOP_Class = "n") %>% bind_rows(all_results)

all_all_results$Set <- factor(all_all_results$Set, levels = c("Training","Validation","CASP13"))

np2 <- ggplot(all_all_results) + geom_rect(data=rectangles, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=category), alpha=0.2) + geom_point(aes(x=TMScore, y=Correct),alpha=0.6, size=0.5) +   scale_y_continuous(limits=c(0,1), breaks=c(seq(0,1,0.2)), expand=c(0,0)) +   scale_x_continuous(limits=c(0,1), breaks=c(seq(0,1,0.2)), expand=c(0,0)) + geom_hline(yintercept=c(0.1,0.3,0.5)) + geom_vline(xintercept=0.5) +   theme_minimal() + theme(legend.position="bottom", legend.box="vertical", legend.margin=margin(0,0,0,0), panel.grid.minor = element_blank(),axis.text=element_text(size=axis_label_size),axis.title=element_text(size=facet_label_size), legend.text=element_text(size=legend_label_size)) + labs(x="TM-Score",y="RFQAmodel") + facet_wrap(~Set) + scale_fill_manual("", values=c(clare_pal[1:3],"darkgray"))
ggsave(np2, file="~/pub_html/Clare/Feb2019/new_plot_trio.pdf", width=12.1,height=4,dpi=350,device="pdf")

all_all_results %>% group_by(Set, Target) %>% arrange(Target, -Correct) %>% slice(1) %>% ungroup() %>% group_by(Set, Correct.bin = cut(Correct, breaks=seq(-0.1,1.1,by=0.1) )) %>% select(Set, Correct.bin, Correct, TMScore) %>% mutate(Precision = sum(TMScore>=0.5)/length(TMScore)) %>% ggplot(., aes(x=Correct.bin, y=Precision, colour=Set)) + geom_point() + theme_bw() + facet_wrap(~Set)


#"In SI figure 15, The RFQAmodel with and without EigenThreader doesn't seem to be that much different. A scatter plot of the RFQAmodel scores (with and without eigenthreader) and R^2 value, might make the difference more apparent. Just to make sure the difference isn't simply due to some points at the borderline of hard/medium/low, with a minor shift."

load("RFQAmodel_classifier_noET_noMA.Rda")
val_tab_results <- val_tab
val_tab_results$RFQAmodel <- predict(RFQAmodel,val_tab,type="prob")[,1]
#val_tab_results$RFQAmodel_noET <- predict(RFQAmodel_noET,val_tab,type="prob")[1,]
val_tab_results$RFQAmodel_noET_noMA <- predict(RFQAmodel_noET_noMA,val_tab, type="prob")[,1]

rsq <- function(x,y) summary(lm(y~x))$r.squared
RFQA.rsq <- rsq(val_tab_results$RFQAmodel_noET_noMA,val_tab_results$RFQAmodel)
plot.confidence.order=c("Failed","Low","Medium","High")
ggplot(val_tab_results, aes(x=RFQAmodel, y=RFQAmodel_noET_noMA)) + geom_point(alpha=0.5) + geom_abline(intercept=c(-0.1,0,0.1), colour="red") + labs(x="RFQAmodel Score", y="RFQAmodel no ET") + geom_smooth(method="lm") + annotate(geom="text",x=0.15,y=0.75, label=paste("R^2 =",RFQA.rsq))
ggsave("~/pub_html/Clare/Feb2019/rsq_noETMA.pdf")

png("~/pub_html/Clare/Feb2019/roc_noET_noMA.png", width=2250, height=2250, res=450)
result2 = calculate_performance_rf(val_tab_results,val_tab_results$Label,RFQAmodel,1,)
result3 = calculate_performance_rf(val_tab_results,val_tab_results$Label,RFQAmodel_noET,2,"TRUE")
result4 = calculate_performance_rf(val_tab_results,val_tab_results$Label,RFQAmodel_noET_noMA,3,"TRUE")
result5 = calculate_performance_rf(val_tab_results,val_tab_results$Label,RFQAmodel_noET_noMA_noPPV,4,"TRUE")
#result4 = calculate_performance(-val_tab$ProQ3D,val_tab$Label,3,"TRUE")
#result5 = calculate_performance(-val_tab$PCombC,val_tab$Label,4,"TRUE")
#result6 = calculate_performance(-val_tab$PCons,val_tab$Label,5,"TRUE")
#result7 = calculate_performance(-val_tab$ProQRosFAD,val_tab$Label,6,"TRUE")
legend("bottomright",	c(paste("RFQAmodel - AUC=",round(result2[[2]]@y.values[[1]],2)),paste("No ET - AUC=",round(result3[[2]]@y.values[[1]],2)),paste("No ET or MA - AUC=",round(result4[[2]]@y.values[[1]],2)),paste("No ET MA PPV - AUC=",round(result5[[2]]@y.values[[1]],2))), lwd = 2, lty = seq(1,6) ,col=colours[1:6])
dev.off()

### Importance plot
imp <- varImpPlot(RFQAmodel) %>% as.data.frame() %>% mutate(Feature = rownames(imp))
imp$Feature <- recode(imp$Feature, "MapAlign"="map_align", "MapLength" = "map_length", "ProQRosCenD" = "Rosetta_centroid", "ProQRosFAD" = "Rosetta_full_atom", "Contact" = "SAINT2_Contact", "Correct" = "RFQAmodel", "Best" = "Total_Successes" )

#}

## Correlation

ddply(val_tab_results %>% filter(TMScore>=0.2), .(Target), summarise, "corr" = cor.test(TMScore, RFQAmodel, method = "spearman", exact=FALSE)$estimate, "pval" = cor.test(TMScore, RFQAmodel, method = "spearman", exact=FALSE)$p.value)

## no P-values
ddply(val_tab_results, .(Target), summarise, "corr" = cor(TMScore, RFQAmodel, method = "spearman"))

#tQA %>% select(-NumCon, -c(18:64), -aux)

#tQA %>% select(-NumCon, -c(18:64), -aux, -Label) %>% reshape2::melt(., id.vars=c("CASP","Target","Decoy","TMScore","Category","Domain","Confidence", "Type", "TMScore"), na.rm=TRUE) %>%  arrange(value) %>% ddply(., .(Target, variable), summarise, "corr" = cor(TMScore, value, method = "spearman"))
library(plyr)
tQA %>% mutate(EigenTHREADER=EigenTHREADER/ET_Max, SAINT2=-SAINT2, Contact=-Contact) %>% filter(TMScore>=0.2) %>% select(-c(20:62), -Stage, -Label, -Beff, -Length, -NumCon) %>% reshape2::melt(., id.vars=c("Set","Target","Decoy","Category","Domain","Confidence", "Type", "TMScore"), na.rm=TRUE) %>%  arrange(value) %>% plyr::ddply(., .(Target, variable), summarise, "rho" = cor.test(TMScore, value, method = "spearman", exact=FALSE)$estimate, pval = cor.test(TMScore, value, method = "spearman", exact=FALSE)$p.val) -> corr

corr %>% group_by(variable) %>% dplyr::summarise(mean=mean(rho, na.rm=TRUE)) %>% arrange(-abs(mean)) %>% as.data.frame()
corr %>% group_by(variable) %>% dplyr::summarise(mean=mean(rho, na.rm=TRUE)) %>% arrange(-abs(mean)) %>% as.data.frame() %>% ggplot(., aes(x=reorder(variable,mean), y=mean)) + geom_bar(stat="identity") + coord_flip()
ggsave("~/pub_html/Clare/Mar2019/corr.pdf", width=8, height=8)

detach("package:plyr")

loss <- tQA %>% mutate(EigenTHREADER=EigenTHREADER/ET_Max, SAINT2=-SAINT2, Contact=-Contact) %>% select(-c(20:62), -Stage, -Label, -Beff, -Length, -NumCon) %>% reshape2::melt(., id.vars=c("Set","Target","Decoy","Category","Domain","Confidence", "Type", "TMScore", "GDT.TS"), na.rm=TRUE) %>% group_by(variable, Target) %>% mutate(Top.TM=max(TMScore), Top.GDT.TS = max(GDT.TS)) %>% slice(which.max(value)) %>% mutate(TM.loss = Top.TM-TMScore, GDT.TS.loss = Top.GDT.TS-GDT.TS) %>% ungroup() %>% group_by(variable) %>% summarise(mean.GDT.TS.loss = mean(GDT.TS.loss), mean.TM.loss = mean(TM.loss)) %>% arrange(mean.TM.loss)

loss %>% mutate(mean.GDT.TS.loss = mean.GDT.TS.loss/100) %>% filter(variable!="TMscore.C")  %>% gather(key="Measure",value="Loss", -variable) %>% ggplot(., aes(x=reorder(variable, -Loss), y=Loss, fill=Measure)) + geom_bar(stat="identity", position="dodge") + labs(x="", y="Mean Loss") + coord_flip()
ggsave("~/pub_html/Clare/Mar2019/loss.pdf", width=8, height=8)

#}


}
