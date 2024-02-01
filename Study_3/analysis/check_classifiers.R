rm(list = ls())
library(tidyverse)

########### Analyse data independent of stance information
df_binding <- read_csv("../results/stance/antivax_binding.csv") %>% 
  dplyr::select(-c(full_text, stance_label )) %>% mutate(binding = factor(binding), 
                                                 individual = factor(individual))%>% drop_na()
df_moral <- read_csv("../results/stance/antivax_moral.csv") %>% 
  dplyr::select(-c(full_text, stance_label)) %>% mutate(moral = factor(moral, 
                                                                labels = c("non-moral", "moral"))) %>% drop_na()

df_pol <- read_csv("../data/political_ideology_misinfo.csv")

##### Distribution of moral values:

## moral model
means_moral <- mean(as.numeric(df_moral$moral))-1
print(means_moral)
# 28% moral tweets vs 72% non moral tweets

## binding model
means_binding <- colMeans(sapply(df_binding[, (ncol(df_binding)-1):ncol(df_binding)], as.numeric))-1
print(means_binding)
#7% of all tweets are binding
#20% of all tweets are individualizing

#proportion of conservatives>
df_pol %>% mutate(partisanship = ifelse(partisan_score >= 0, "conservative", "liberal")) %>% 
  summarise(Proportion_cons = mean(partisanship=="conservative", na.rm = T)) %>% pull

# average misinformation exposure score
df_pol %>% summarise(Proportion_misinfo = mean(misinfo_score, na.rm = T)) %>% pull

df_vax <- read_csv("../results/stance/antivax_binding.csv") %>% 
  rbind(read_csv("../results/stance/provax_binding.csv")) %>%
  dplyr::select(stance_label) 
df_vax %>% summarise(Proportion = mean(stance_label==1, na.rm = T)) %>% pull 
# 36% provax vs 64%antivax










