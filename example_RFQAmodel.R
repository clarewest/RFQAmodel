### Example of how to use the RFQAmodel classifier 

library(dplyr)
library(randomForest)
library(caret)

### A function to get the ensemble features
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

## Load the classifier
load("RFQAmodel_classifier.Rda")

## Read in the raw scores 
validationset = read.table("data/RFQAmodel_validation.txt", header=TRUE, stringsAsFactors=FALSE)

## Calculate the ensemble features
val_tab <- get_features(validationset)

## Classify the models set and categorise predictions into confidence levels
results <- val_tab %>% ungroup() %>%
  mutate(RFQAmodel = as.numeric(predict(RFQAmodel,val_tab,type="prob")[,2])) %>%
  group_by(Target) %>%
  mutate(Confidence = ifelse(max(RFQAmodel) > 0.5 , "High", ifelse(max(RFQAmodel) > 0.3, "Medium", ifelse(max(RFQAmodel) > 0.1, "Low","Failed"))))


