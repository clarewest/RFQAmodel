# RFQAmodel: code and supporting data

This repository contains scripts and data to accompany:

Clare E. West, Saulo H.P. de Oliveira & Charlotte M. Deane. *RFQAmodel: Random Forest Quality Assessment to identify a predicted protein structure in the correct fold.*

## Dependencies

These scripts use R and the following packages:
- tidyverse 
  - ggplot
  - dplyr
- randomForest
- caret
- ROCR
- cowplot
- xtable (for producing latex tables)

## Description of Files

- RFQAmodel.R - Script for model training and classification
- RFQAmodel_classifier.Rda - Trained RFQAmodel classifier
- datasets_details.txt - Details of targets in Training, Validation and CASP sets

### data files:
- RFQAmodel features:
  - RFQAmodel_training.txt
  - RFQAmodel_validation.txt
  - RFQAmodel_CASP_test.txt
  - RFQAmodel_extras.txt
- CASP data:
  - CASP_model_scores.txt
  - CASP_stage2_QA_predictions.txt
  - CASP_target_classifications.txt
 
### results files:
Note that the lines writing these tables are included in RFQAmodel.R, but are commented out. 
- RFQAmodel_validation_results.txt
- RFQAmodel_CASP_test_results.txt
- RFQAmodel_training_results.txt
- RFQAmodel_CASP_summary_table.tex
- RFQAmodel_extras_results.txt

### A script for figure and table generation is also included:
- figures.R




