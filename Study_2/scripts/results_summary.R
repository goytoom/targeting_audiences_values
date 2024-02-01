rm(list = ls())

# Notes -------------------------------------------------------------------
# This scripts loads and outputs summaries for the main models and the mediations (Study 2)

# Library -----------------------------------------------------------------
library(tidyverse)


# Results -----------------------------------------------------------------
#### Load Model comparisons
results_comparison <- readRDS("../results/comparison.rds")

# Show main model summaries (moral alignment & mediation)
results_m4 <- readRDS("../results/m4_effects.rds")
results_m6 <- readRDS("../results/m6_effects.rds")
results_mediation_replication <- readRDS("../mediations/mediation_replication_effects.rds")
results_mediation <- readRDS("../mediations/mediation_del_effects.rds")
results_mediation_alt <- readRDS("../mediations/mediation_RT_effects.rds") # mediation with alternative mediator (response time)
results_m11 <- readRDS("../results/m11_effects.rds") # CRT moderation of moral alignment

# show main model (moral alignment)
print(results_m4)

# main model controlled for veracity
print(results_m6)

# Interaction of CRT and moral alignment: Shows no effect
print(results_m11)

# main mediation (deliberation; see "ind_" for the mediation path)
print(results_mediation)

# main mediation (response time as an alternative to deliberation; see "ind_" for the mediation path)
print(results_mediation_alt)

# replication of study 1 mediation (see "ind_" for the mediation path)
print(results_mediation_replication)


### Alternative models (M1-M3)
results_m1 <- readRDS("../results/m1_fit.rds")
results_m2 <- readRDS("../results/m2_fit.rds")
results_m3 <- readRDS("../results/m3_fit.rds")

# shows most predictive feature for each type of predictor
print(results_m1) #headline level
print(results_m2) #post level
print(results_m3) #agreement/alignment with post content

########### Additional analyses (non-moral stimuli & analytical thinking)
#### Summary: M10 shows that, for non-moral stimuli, analytical thinking increases truth discernment (reduced sharing of fake-news, compared to true-news)
results_m10 <- readRDS("../results/m10_effects.rds") #positive crt & veracity interaction
print(results_m10)


######## Evaluate indices
library(ltm)
# Load data
dl <- read_rds("../data/dl_R.rds")

# Get Post Share Index
dl <- dl %>% mutate(
  post_share_index = (post_share_public + post_like_public + post_share_private + post_share_offline)/4,
  post_deliberation_index = (post_consideration + post_knowledge + post_thought + post_self)/4
) %>% drop_na(-strata, -source_text)

# calculate alpha
cronbach.alpha(dl[, c("post_share_public", "post_like_public", "post_share_private", "post_share_offline")], 1, 1)
cronbach.alpha(dl[, c("post_thought", "post_consideration", "post_knowledge", "post_self")], 1, 1)