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
  group_by(Target, Set) %>% 
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
trainingset = read.table("data/RFQAmodel_training.txt", header=TRUE, stringsAsFactors=FALSE)
validationset = read.table("data/RFQAmodel_validation.txt", header=TRUE, stringsAsFactors=FALSE)

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

write.table(results, file="results/RFQAmodel_validation_results.txt", row.names=FALSE, quote=FALSE)

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
write.table(train_results, file="results/RFQAmodel_training_results.txt", row.names=FALSE, quote=FALSE)


## CASP Test set
not_evaluated <- c("T0952","T0956","T0972","T0988","T0994","T1007","T1012","T1023s1","T1023s2","T1023s3","T0908","T0916","T0919","T0924","T0925","T0926","T0927","T0935","T0936","T0937","T0938","T0939","T0940","T0999","T1000","T1004","T1011","T0974s2","T0963","T0960","T0980s2")
testset <- read.table("data/RFQAmodel_CASP_test.txt", header=TRUE, stringsAsFactors=FALSE) %>% filter(! Target %in% not_evaluated)
test_tab <- get_features(testset)

res <- read.table("data/CASP_model_scores.txt", header=TRUE, stringsAsFactors=FALSE) %>% rename(Set=CASP) 
cat <- read.table("data/CASP_target_classifications.txt", header=TRUE, stringsAsFactors=FALSE)  

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

write.table(CASP_results, file="results/RFQAmodel_CASP_test_results.txt", quote=FALSE, row.names=FALSE)

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


## Extra models
extras <- read.table("data/RFQAmodel_extras.txt", header=TRUE, stringsAsFactors=FALSE)
extra_tab <- get_features(extras)
extra_results <- extra_tab %>% 
  ungroup() %>% 
  mutate(RFQAmodel = as.numeric(predict(RFQAmodel,extra_tab,type="prob")[,2])) %>% 
  group_by(Set, Target) %>% 
  mutate(Confidence = ifelse(max(RFQAmodel) > 0.5 , "High", ifelse(max(RFQAmodel) > 0.3, "Medium", ifelse(max(RFQAmodel) > 0.1, "Low","Failed"))))

write.table(extra_results, "results/RFQAmodel_extras_results.txt",quote=FALSE, row.names=FALSE)


