# RFQAmodel: code and supporting data
### CE West

This repository contains scripts and data for:

Clare E. West, Saulo H.P. de Oliveira & Charlotte M. Deane.
RFQAmodel: Random Forest Quality Assessment to identify a predicted protein structure in the correct
fold.

These scripts use R and the following packages:
- tidyverse 
  - ggplot: plotting
  - dplyr: data handling
- randomForest
- caret: random forest
- ROCR: comparing performance
- cowplot: plotting
- xtable: producing latex tables

### Script for model training and classification:
- RFQAmodel.R

### Trained RFQAmodel classifier:
- RFQAmodel_classifier_ALL.Rda

### Results files:
Note that the lines writing these tables are included, but commented out. 
- RFQAmodel_validation_results.txt
- RFQAmodel_CASP_test_results.txt

### Data files:
#### Details of targets in Training, Validation and CASP sets
- datasets_details.txt

#### Data for RFQAmodel features:
- RFQAmodel_training.txt
- RFQAmodel_validation.txt
- RFQAmodel_CASP
- RFQAmodel_extras.txt

####CASP data:
- classifications.txt
- CASPs_results_table.txt
- stage2_QA_predictions.txt

### Scripts for figure and table generation are also included:
- figures.R

