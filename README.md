# Targeting Audiences Moral Values via Moral Framing Increases Spread of Misinformation

This is the repository for the paper Targeting Audiences Moral Values via Moral Framing Increases Spread of Misinformation. The repository contains all necessary codes, data, and result files, to replicate the project.

# Code Overview
## Study 1a
- `scripts/prepare_data.R`: formats and cleans data. Afterwards, manual selection of best stimuli based on response pattern.

## Study 1b
- `scripts/prepare_data.R`: formats and cleans data. 
- `scripts/run_models.R`: fits statistical models with cleaned data
- `scripts/run_model_mediation.R`: fits mediation models
- `scripts/results_summary.R`: summarizes model outputs. Creates easy to read tables.
- `scripts/power_analysis.Rmd`: power analysis to whether sample size is sufficient
- `scripts/public_sharing_analyses.Rmd`: repeats main analyses for public vs private sharing intentions
- `scripts/veracity_analyses.Rmd`: analyzes the effects of moral alignment for true vs misinformation

## Study 2
- `scripts/prepare_data.R`: formats and cleans data. 
- `scripts/run_models.R`: fits statistical models with cleaned data
- `scripts/run_mediation.R`: fits mediation models
- `scripts/run_models_order.R`: fits statistical model for analyses of order effects
- `scripts/results_summary.R`: summarizes model outputs. Creates easy to read tables.
- `scripts/prepare_data_order.R` and `run_models_order.R` prepare and run the additional analyses of stimuli presentation order effects respectively.
- `scripts/public_sharing_analyses.Rmd`: repeats main analyses for public vs private sharing intentions
- `scripts/veracity_analyses.Rmd`: analyzes the effects of moral alignment for true vs misinformation

## Study 3
- `analysis/check_classifiers.R`: robustness checks; overview of moral framing distributions, political ideology, and stance on vaccinations. Also calculate inter-correlations of moral language classifications.
- `analysis/example_messages.R`: produces example messages for each type of moral language, and stance (pro/antivax)
- `analysis/results_summary.R`: summarizes results; produces easy to read tables
- `analysis/get_misinfo_data.Rmd`: gathers political ideology of users in the dataset; also determines exposure to misinformation of the users in the dataset as a proxy for the prevalence of misinformation
- `analysis/modelling_scripts/run_models_rt.R`: fits models to predict tweets' retweet count
- `analysis/modelling_scripts/run_models_fv.R`: fits models to predict tweets' favourite count
- `dataset_moral.py`: creates train/test data (preprocessing+embeddings) from annotated training data set (MFTC)
- `moral_classifier.py`: trains and evaluates moral classifier
- `predict_foundations.py`: classifies moral language for all tweet files
- `merge_results.py`: merges annotated tweet files into a single file
- `tokenization.py`: auxiliary file for tokenizing inputs (used in training file creation)

# Data Overview
## Study 1a
- `materials/qualtrics_export.csv`: raw survey data as exported from qualtrics

## Study 1b
- `materials/qualtrics_export.csv`: raw survey data as exported from qualtrics

## Study 2
- `materials/qualtrics_export_R.csv`: raw survey data as exported from qualtrics

## Study 3
- `data/tweets/`: hydrated tweets (one file per month)
- `data/stance/`: output of stance detection. hydrated tweets with stance (pro/antivax) annotations
- `data/political_ideology_misinfo.csv`: Political ideology and exposure to misinformation for users in the dataset
- `data/mftc_cleaned.csv`: annotated training data (MFTC)