# RFQAmodel: code and supporting data
### CE West

This repository contains scripts and data to accompany:

Clare E. West, Saulo H.P. de Oliveira & Charlotte M. Deane. RFQAmodel: Random Forest Quality Assessment to identify a predicted protein structure in the correct fold.

These scripts use R and the following packages:
- tidyverse 
  - ggplot
  - dplyr
- randomForest
- caret
- ROCR
- cowplot
- xtable (for producing latex tables)

### Script for model training and classification:
- RFQAmodel.R

### Trained RFQAmodel classifier:
- RFQAmodel_classifier.Rda

#### Details of targets in Training, Validation and CASP sets
- datasets_details.txt

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
 
### Results files:
Note that the lines writing these tables are included, but commented out. 
- RFQAmodel_validation_results.txt
- RFQAmodel_CASP_test_results.txt
- RFQAmodel_training_results.txt
- RFQAmodel_CASP_summary_table.tex
- RFQAmodel_extras_results.txt

### A script for figure and table generation is also included:
- figures.R




