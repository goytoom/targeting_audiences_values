rm(list = ls())
library(tidyverse)

#################### Main Analyses (Binding/Individualizing values)
## Model Comparison
#### Retweets
comparison_rt <- readRDS("results/comparison_rt.rds") #compares models out-of-sample prediction performance
print(comparison_rt)

#### Favourite count
comparison_fv <- readRDS("results/comparison_fv.rds") #compares models out-of-sample prediction performance
print(comparison_fv)


## Main Model Effects Summary
#### Retweets
results_m2_rt <- readRDS("results/m2_rt_effects.rds") #summarizes results for the main model
print(results_m2_rt)

#### Favourite count
results_m2_fv <- readRDS("results/m2_fv_effects.rds") #summarizes results for the main model
print(results_m2_fv)


## Comparison with stance models
#### Retweets
results_comparison_stance_rt <- readRDS("results/comparison_stance_rt.rds")
results_comparison_id_stance_rt <- readRDS("results/comparison_id_stance_rt.rds")
print(results_comparison_stance_rt)
print(results_comparison_id_stance_rt)
# positive value means that the first model (with pol. ideol.) is more predictive 
#than the model with stance instead of ideology

#### Favourite count
results_comparison_stance_fv <- readRDS("results/comparison_stance_fv.rds")
results_comparison_id_stance_fv <- readRDS("results/comparison_id_stance_fv.rds")
print(results_comparison_stance_fv)
print(results_comparison_id_stance_fv) 
# positive value means that the first model (with pol. ideol.) is more predictive 
#than the model with both stance and ideology

## ==> shows that stance does not contribute beyond ideology (is indeed a proxy)


# ############## Check inter-correlation of tweet classifier (Are the same tweets classified as binding/individualizing?)
# # Import data
# df_anti_binding <- read_csv("../results/stance/antivax_binding.csv") %>% select(-c(full_text)) 
# df_pro_binding <- read_csv("../results/stance/provax_binding.csv") %>% select(-c(full_text))
# 
# # Prepare data (filter, merge, convert data type)
# df_total_binding <- rbind(df_anti_binding, df_pro_binding) %>% mutate(
#   binding = factor(binding),
#   individual = factor(individual),
#   stance = factor(stance_label, labels = c("anti-vax", "pro-vax"))
# )
# 
# individual = df_total_binding$individual
# individual = as.numeric(levels(individual))[individual]
# binding = df_total_binding$binding
# binding = as.numeric(levels(binding))[binding]
# 
# # calculate inter-correlation
# cor.test(individual, binding) #low inter-correlation (meaning that the classifier did not simply classify the same posts as binding and individualizing)