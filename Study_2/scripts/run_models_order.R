rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

# Load packages
library(tidyverse)
library(brms)
library(future)

# Options
options(
  mc.cores = parallel::detectCores(),
  brms.backend = "cmdstanr",
  future.globals.maxSize = 2147483648
)


# Prepare -----------------------------------------------------------------

# Load data
dl <- read_rds("../data/dl_order.rds")

# Calculate Post: Share Index
dl <- dl %>% mutate(
  post_share_index = (post_share_public + post_like_public + post_share_private + post_share_offline)/4,
  post_deliberation_index = (post_consideration + post_knowledge + post_thought + post_self)/4
)

# Calculate Individualizing/Binding scores
dl <- dl %>% mutate(
  mfq_indi = (mfq_care + mfq_equa)/2,
  mfq_bind = (mfq_loya + mfq_auth + mfq_puri)/3
)

# Drop missing data
dl <- dl %>% drop_na(-strata, -source_text)

# Assign new indices
dl <- dl %>% mutate(across(c(ii, kk, kk), ~as.integer(factor(.))))


# Standardize -------------------------------------------------------------

# Create data set for analyses
df <- dl %>% select(ii, jj, kk)

# Standardize person-level variables
df <- dl %>% 
  select(jj, starts_with("mfq_"), conservatism, crt) %>% 
  distinct() %>% 
  mutate(
    across(-jj, list(z_jj = ~(. - mean(.))/sd(.)), .names = "{.fn}_{.col}")
  ) %>% 
  rename_with(~paste0("x_jj_", .), c(-jj, -starts_with("z_"))) %>% 
  left_join(df, ., by = "jj")

# Standardize headline-level variables
df <- dl %>% 
  select(kk, starts_with("headline_"), -headline_text) %>% 
  group_by(kk) %>% 
  dplyr::summarize(
    across(everything(), mean)
  ) %>% 
  mutate(
    across(-kk, list(z_kk = ~(. - mean(.))/sd(.)), .names = "{.fn}_{.col}")
  ) %>% 
  rename_with(~paste0("x_kk_", .), c(-kk, -starts_with("z_"))) %>% 
  left_join(df, ., by = "kk")

# Standardize post-level variables
df <- dl %>% 
  dplyr::select(
    ii, starts_with("post_"), 
    -post_sentiment, -post_framing, -post_text,
    -matches("share|like")
  ) %>% 
  group_by(ii) %>% 
  dplyr::summarize(
    across(everything(), mean)
  ) %>% 
  mutate(
    across(-ii, list(z_ii = ~(. - mean(.))/sd(.)), .names = "{.fn}_{.col}")
  ) %>% 
  rename_with(~paste0("x_ii_", .), c(-ii, -starts_with("z_"))) %>% 
  left_join(df, ., by = "ii")

# Standardize headline-/person-level variables
df <- dl %>% 
  select(ii, jj, kk, starts_with("headline_"), -headline_true, -headline_text) %>% 
  left_join(
    df %>% select(ii, jj, kk, starts_with("x_kk_")),
    by = c("ii", "jj", "kk")
  ) %>%
  mutate(
    z_headline_believable = headline_believable - x_kk_headline_believable,   
    z_headline_controversial = headline_controversial - x_kk_headline_controversial,   
    z_headline_surprising = headline_surprising - x_kk_headline_surprising,
    z_headline_interesting = headline_interesting - x_kk_headline_interesting, 
    z_headline_positive = headline_positive - x_kk_headline_positive,
    z_headline_familiar = headline_familiar - x_kk_headline_familiar
  ) %>% 
  mutate(
    across(starts_with("z_"), ~./sd(.))
  ) %>% 
  select(-starts_with("x_kk_")) %>% 
  rename_with(~paste0("x_", .), c(-ii, -jj, -kk, -starts_with("z_"))) %>% 
  left_join(df, ., by = c("ii", "jj", "kk"))

# Standardize post-/person-level variables
df <- dl %>% 
  select(
    ii, jj, kk,
    starts_with("post_"), 
    -post_sentiment, -post_framing, -post_text,
    -matches("share|like")
  ) %>% 
  left_join(
    df %>% select(ii, jj, kk, starts_with("x_ii_")),
    by = c("ii", "jj", "kk")
  ) %>%
  mutate(
    z_post_controversial = post_controversial - x_ii_post_controversial,
    z_post_surprising = post_surprising - x_ii_post_surprising,
    z_post_interesting = post_interesting - x_ii_post_interesting,
    z_post_positive = post_positive - x_ii_post_positive,
    z_post_agree = post_agree - x_ii_post_agree,
    z_post_align = post_align - x_ii_post_align,
    z_post_RT = post_response_time - x_ii_post_response_time,
    z_post_click_count = post_click_count - x_ii_post_click_count,
    z_post_consideration = post_consideration - x_ii_post_consideration,
    z_post_thought = post_thought - x_ii_post_thought,
    z_post_knowledge = post_knowledge - x_ii_post_knowledge,
    z_post_self = post_self - x_ii_post_self,
    z_post_deliberation = post_deliberation_index - x_ii_post_deliberation_index,
  ) %>% 
  mutate(
    across(starts_with("z_"), ~./sd(.))
  ) %>% 
  select(-starts_with("x_ii_")) %>% 
  rename_with(~paste0("x_", .), c(-ii, -jj, -kk, -starts_with("z_"))) %>% 
  left_join(df, ., by = c("ii", "jj", "kk"))

# Contrast code categorical predictor variables
dl <- dl %>% 
  mutate(
    post_framing = factor(
      post_framing, 
      levels = c("individualizing", "binding", "nonmoral")
    )
  )
contrasts(dl$post_framing) <- contr.sum(3)
df <- left_join(
  df, 
  dl %>% select(ii, x_post_framing = post_framing) %>% distinct(), 
  by = "ii"
)

# Standardize outcome variable
df <- dl %>% 
  transmute(
    ii, jj, kk, order,
    y_post_share_index = post_share_index,
    z_post_share_index = (post_share_index - mean(post_share_index))/sd(post_share_index)
  ) %>% 
  left_join(df, by = c("ii", "jj", "kk"))

df2 = df[df$x_post_framing == "nonmoral",]

# Estimate ----------------------------------------------------------------

#### Order effects
# participants learn about procedures over the course of the experiments
# This might influence the effect of other variables
# E.g., participants might decide to share a stimulus when seeing it, then adjust the post/headline ratings to justify it
# -> control for order of stimuli presentation

# Model 1 with controls for stimuli order
m12_fit <- brm(
z_post_share_index ~ 1 + z_kk_headline_familiar + z_kk_headline_believable + z_kk_headline_controversial + z_kk_headline_surprising + z_kk_headline_interesting + z_kk_headline_positive + z_kk_headline_true + z_headline_familiar*order + z_headline_believable + z_headline_controversial + z_headline_surprising + z_headline_interesting + z_headline_positive + (1|ii) + (1|jj) + (1 + z_headline_familiar + z_headline_believable + z_headline_controversial + z_headline_surprising + z_headline_interesting + z_headline_positive|kk),
data = df,
chains = 8,
iter = 1000,
warmup = 750,
seed = 9103465
)

# Model 3 with only first stimulus
m13_fit <- brm(
z_post_share_index ~ 1 + z_post_agree*z_post_align + z_ii_post_agree*z_ii_post_align + (1 + z_post_agree*z_post_align|ii) + (1|jj) + (1|kk),
data = df3,
chains = 8,
iter = 1500,
warmup = 750,
seed = 3076878,
adapt_delta = 0.999
)

# Control model 6 with controls for order effects
m14_fit <- brm(
z_post_share_index ~ 1 + z_kk_headline_familiar + z_headline_familiar*order + z_kk_headline_true + x_post_framing*z_jj_mfq_indi*order + x_post_framing*z_jj_mfq_bind*order + x_post_framing*z_jj_mfq_prop*order + (1|jj) + (1 + x_post_framing*z_jj_mfq_indi*order + x_post_framing*z_jj_mfq_bind*order + x_post_framing*z_jj_mfq_prop*order + z_headline_familiar*order|kk),
data = df,
chains = 8,
iter = 1500,
warmup = 750,
seed = 4962216,
adapt_delta = 0.999
)

# order effect analyses
write_rds(m12_fit, "../results/m12_fit.rds")
write_rds(m13_fit, "../results/m13_fit.rds")
write_rds(m14_fit, "../results/m14_fit.rds")

# Compare -----------------------------------------------------------------

# Assign folds for 10-fold cross-validation
set.seed(9735121)
df <- df %>%
distinct(jj) %>%
mutate(fold = sample.int(10, n(), replace = TRUE)) %>%
left_join(df, ., by = c("jj"))

# Run 10-fold cross-valitation
plan(multisession)

#order effect analyses
m12_fit <- add_criterion(
m12_fit,
criterion = "kfold",
chains = 1,
folds = df$fold
)
m13_fit <- add_criterion(
m13_fit,
criterion = "kfold",
chains = 1,
folds = df3$fold
)
m14_fit <- add_criterion(
m14_fit,
criterion = "kfold",
chains = 1,
folds = df$fold
)


plan("default")

#order effect analyses
m12_fit <- add_criterion(
m12_fit,
criterion = "bayes_R2",
re_formula = NA
)
m13_fit <- add_criterion(
m13_fit,
criterion = "bayes_R2",
re_formula = NA
)
m14_fit <- add_criterion(
m14_fit,
criterion = "bayes_R2",
re_formula = NA
)


# Export ------------------------------------------------------------------

  # order effect analyses
  write_rds(m12_fit, "../results/m12_fit.rds")
  write_rds(m13_fit, "../results/m13_fit.rds")
  write_rds(m14_fit, "../results/m14_fit.rds")

# Process -----------------------------------------------------------------

  #Load results
  m0 <- read_rds("../results/m0_fit.rds")
  m12 <- read_rds("../results/m12_fit.rds")
  m13 <- read_rds("../results/m13_fit.rds")
  m14 <- read_rds("../results/m14_fit.rds")
  
  # Compile models
  results <- crossing(
    model0 = paste0("M", c(0,12:14)),
    model1 = paste0("M", c(0,12:14))
  ) %>%
    mutate(
      fit0 = map(model0, ~read_rds(paste0("../results/", str_to_lower(.), "_fit.rds"))),
      fit1 = map(model1, ~read_rds(paste0("../results/", str_to_lower(.), "_fit.rds")))
    )
  
  # Compare models
  results <- results %>%
    mutate(
      ELPD = map2(
        fit0,
        fit1,
        ~loo_compare(.x, .y, criterion = "kfold") %>%
          as.data.frame() %>%
          rownames_to_column("model")
      )
    ) %>%
    unnest(ELPD) %>%
    group_by(model0, model1) %>%
    dplyr::summarize(
      elpd_diff = elpd_diff[model == ".x"] - elpd_diff[model == ".y"],
      elpd_se_diff = max(se_diff)
    ) %>%
    ungroup() %>%
    left_join(
      results %>%
        mutate(
          R2 = map(
            fit0,
            ~bayes_R2(., robust = T) %>% as_tibble()
          )
        ) %>%
        unnest(R2) %>%
        transmute(
          model0,
          r2 = Estimate,
          r2_l95 = Q2.5,
          r2_u95 = Q97.5
        ) %>%
        distinct(),
      by = "model0"
    )
  
  #combine comparisons
  results_comparison <- results %>% mutate(z = elpd_diff / elpd_se_diff)
  
  # Save results as .rds
  write_rds(results_comparison, "../results/comparison_order.rds")
