# RFQAmodel: code and supporting data

This repository contains scripts and data to accompany:

Clare E. West, Saulo H.P. de Oliveira & Charlotte M. Deane. [*RFQAmodel: 
Random Forest Quality Assessment to identify a predicted protein structure 
in the correct fold.*](https://www.biorxiv.org/content/10.1101/654293v1)

## Dependencies

These scripts use R and the following packages:
- tidyverse 
  - ggplot
  - dplyr
- randomForest
- caret
- ROCR
- cowplot (for plots only)
- xtable (for producing latex tables only)

## Using RFQAmodel
- example.R - an example script for how to apply RFQAmodel on a dataset

## Description of Files

- RFQAmodel.R - Script for model training and classification
- RFQAmodel_classifier.Rda - Trained RFQAmodel classifier
- datasets_details.txt - Details of targets in Training, Validation and CASP sets

### data files:
- RFQAmodel features:
  - RFQAmodel_training.txt
  - RFQAmodel_validation.txt
  - RFQAmodel_CASP_test.txt
  - RFQAmodel_extras.txt - for 6 extra targets
  - RFQAmodel_mediumextras.Rda - for 50 medium confidence targets
  - TMscores.txt - scores for all models using TM-score and TM-align
- CASP data:
  - CASP_model_scores.txt
  - CASP_stage2_QA_predictions.txt
  - CASP_target_classifications.txt
 
### results files:
Note that the lines writing these tables are included in RFQAmodel.R, but are 
commented out. 
- RFQAmodel_validation_results.txt
- RFQAmodel_CASP_test_results.txt
- RFQAmodel_training_results.txt
- RFQAmodel_CASP_summary_table.tex
- RFQAmodel_extras_results.txt

## example of using RFQAmodel
- example_RFQAmodel.R
  Example script for using the trained RFQAmodel classifier to classify models
  and categorise into confidence categories

### A script for figure and table generation is also included:
- figures.R

### extras
This directory contains the minimal requirements to score models using the 
SAINT2 scoring function
- bin/saint2 - SAINT2 binary
- data - contains data required for scoring
- saint2_score.sh - an example of script for how to score a single model
- 1ASH_A_00839 - an example model of the target 1ASH_A
- 1ASH_A.con - predicted contact file required for the contact component of the
  SAINT2 scoring function
- 1ASH_A_00839_scores - example score output file containing the values for 
  each component of the SAINT2 scoring function
- 1ASH_A.fasta.txt - fasta sequence for the target, required for scoring
- config_1ASH_A_c_n_1ASH_A.flib_10000_1000_t2.5_linear - example configuration
  file for SAINT2 score containing the paths to data files and the weightings 
  for each component of the scoring function


