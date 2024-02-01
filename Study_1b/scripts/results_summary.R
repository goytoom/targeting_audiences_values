rm(list = ls())
library(tidyverse)

#### Load Model comparisons
results_comparison <- readRDS("../results/comparison.rds") #compares models out-of-sample prediction performance

#### Main Model Summaries
results_m4 <- readRDS("../results/m4_effects.rds") #summarizes results for the main model
results_m6 <- readRDS("../results/m6_effects.rds") #summarizes results for the main model (controlled for veracity)
results_mediation <- readRDS("../results/mediation_effects.rds") #shows that the effect of matching moral values and post framing is mediated by perceived alignment and agreement with a post

# main model
print(results_m4)

# model with veracity as control variable
print(results_m6)

# mediation model
print(results_mediation)

### Alternative models (M1-M3)
results_m1 <- readRDS("../results/m1_fit.rds")
results_m2 <- readRDS("../results/m2_fit.rds")
results_m3 <- readRDS("../results/m3_fit.rds")

# shows most predictive feature for each type of predictor
print(results_m1) #headline level
print(results_m2) #post level
print(results_m3) #agreement/alignment with post content




