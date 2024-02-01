rm(list = ls())
library(tidyverse)

#### Load Model comparisons
results_comparison <- readRDS("../results/results_comparison.rds") #compares models out-of-sample prediction performance

#### Main Model Summaries
results_m4 <- readRDS("../results/m4_effects.rds") #summarizes results for the main model
results_m6 <- readRDS("../results/m6_effects.rds") #summarizes results for the main model (controlled for veracity)
results_mediation <- readRDS("../results/results_mediation.rds") #shows that the effect of matching moral values and post framing is mediated by perceived alignment and agreement with a post

# main model
print(results_m4)

# model with veracity as control variable
print(results_m6)

# mediation model
print(results_mediation)




