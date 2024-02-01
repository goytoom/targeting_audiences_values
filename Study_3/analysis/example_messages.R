rm(list = ls())
library(tidyverse)
library(brms)

df_anti_binding <- read_csv("../results/stance/antivax_binding.csv")
df_pro_binding <- read_csv("../results/stance/provax_binding.csv")

df_total_binding <- rbind(df_anti_binding, df_pro_binding) %>%
  mutate(stance = factor(stance_label, labels = c("anti-vax", "pro-vax")))

#select random example of 10 for each foundation
examples <- df_total_binding %>% arrange(desc(favorite_count)) %>% mutate(moral_count = rowSums(select(., c(binding, individual)))) %>% 
  filter(moral_count == 1 | moral_count == 0) %>% mutate(non_moral = ifelse(moral_count==0, 1, 0)) %>% 
  pivot_longer(c(binding, individual, non_moral), names_to = "concern", values_to = "value")  %>% filter(value==1) %>% 
  group_by(concern) %>% do(head(., n=10)) %>% select(-stance, -value, -moral_count, -stance_label)
