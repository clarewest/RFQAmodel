## Install or load library
devtools::install_github("clarewest/RFQAmodelr")
library(RFQAmodelr)

library(randomForest)

## Read in the input scores 
input <- read.table("data/RFQAmodel_validation.txt", header=TRUE)

## Caluculate ensemble features
features <- RFQAmodelr::get_features(input)

## Load classifier
load("RFQAmodel_classifier.Rda")

## Classify models
classifications <- RFQAmodelr::classify_models(features, classifier=RFQAmodel)
## By default the new column will be named RFQAmodel
## For a different name, use optional argument name = string 

## Add confidence categories 
confidence <- RFQAmodelr::get_confidence(classifications)
# Optional arguments:
# predictior = "RFQAmodel"
# - column to use as the predictior
# confidence_cutoffs = c(0.5, 0.3, 0.1)
# - custom confidence cutoff levels for High, Medium and Low confidence

## All in one 
library(dplyr)
results <- 
  read.table("data/RFQAmodel_validation.txt", header=TRUE) %>%
  RFQAmodelr::get_features(.) %>%
  RFQAmodelr::classify_models(., classifier=RFQAmodel) %>%
  RFQAmodelr::get_confidence(.)
