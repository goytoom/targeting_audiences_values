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
dl <- read_rds("../data/dl_R.rds")

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
    ii, jj, kk, 
    y_post_share_index = post_share_index,
    z_post_share_index = (post_share_index - mean(post_share_index))/sd(post_share_index)
  ) %>% 
  left_join(df, by = c("ii", "jj", "kk"))

df2 = df[df$x_post_framing == "nonmoral",]

# Estimate ----------------------------------------------------------------

# Model 0 (Random Intercepts)
m0_fit <- brm(
  z_post_share_index ~ 1 + (1|ii) + (1|jj) + (1|kk),
  data = df,
  chains = 8,
  iter = 1000,
  warmup = 750,
  seed = 6943546
  )

# Model 1 (Headline-Level Predictors)
m1_fit <- brm(
  z_post_share_index ~ 1 + z_kk_headline_familiar + z_kk_headline_believable + z_kk_headline_controversial + z_kk_headline_surprising + z_kk_headline_interesting + z_kk_headline_positive + z_kk_headline_true + z_headline_familiar + z_headline_believable + z_headline_controversial + z_headline_surprising + z_headline_interesting + z_headline_positive + (1|ii) + (1|jj) + (1 + z_headline_familiar + z_headline_believable + z_headline_controversial + z_headline_surprising + z_headline_interesting + z_headline_positive|kk),
  data = df,
  chains = 8,
  iter = 1000,
  warmup = 750,
  seed = 9103465
  )

# Model 2 (Post-Level Predictors)
m2_fit <- brm(
  z_post_share_index ~ 1 + z_ii_post_controversial + z_ii_post_surprising + z_ii_post_interesting + z_ii_post_positive + z_post_controversial + z_post_surprising + z_post_interesting + z_post_positive + (1 + z_post_controversial + z_post_surprising + z_post_interesting + z_post_positive|ii) + (1|jj) + (1|kk),
  data = df,
  chains = 8,
  iter = 1000,
  warmup = 750,
  seed = 3000256
  )

# Model 3 (Person-/Post-Level Agreement/Alignment Predictors)
m3_fit <- brm(
  z_post_share_index ~ 1 + z_post_agree*z_post_align + z_ii_post_agree*z_ii_post_align + (1 + z_post_agree*z_post_align|ii) + (1|jj) + (1|kk),
  data = df,
  chains = 8,
  iter = 1500,
  warmup = 750,
  seed = 3076878,
  adapt_delta = 0.999
  )

# Model 4 (Person-Level Moral Predictors)
m4_fit <- brm(
  z_post_share_index ~ 1 + x_post_framing*z_jj_mfq_indi + x_post_framing*z_jj_mfq_bind + x_post_framing*z_jj_mfq_prop + (1|jj) + (1 + x_post_framing*z_jj_mfq_indi + x_post_framing*z_jj_mfq_bind + x_post_framing*z_jj_mfq_prop|kk),
  data = df,
  chains = 8,
  iter = 1500,
  warmup = 750,
  seed = 4962216,
  adapt_delta = 0.999
  )

# Model 5 (Person-Level Ideological Predictor)
m5_fit <- brm(
  z_post_share_index ~ 1 + x_post_framing*z_jj_conservatism + (1|jj) + (1 + x_post_framing*z_jj_conservatism|kk),
  data = df,
  chains = 8,
  iter = 1000,
  warmup = 750,
  seed = 6943546
  )

# Model 6 (Model 4 with controls for inferences: veracity & familiarity)
m6_fit <- brm(
  z_post_share_index ~ 1 + z_kk_headline_familiar + z_headline_familiar + z_kk_headline_true + x_post_framing*z_jj_mfq_indi + x_post_framing*z_jj_mfq_bind + x_post_framing*z_jj_mfq_prop + (1|jj) + (1 + x_post_framing*z_jj_mfq_indi + x_post_framing*z_jj_mfq_bind + x_post_framing*z_jj_mfq_prop + z_headline_familiar|kk),
  data = df,
  chains = 8,
  iter = 1500,
  warmup = 750,
  seed = 4962216,
  adapt_delta = 0.999
  )

### Models for deliberation:
# Model 7 (deliberation as a trait x veracity: Does analytical thinking reduce sharing intentions for false vs true news?)
m7_fit <- brm(
  z_post_share_index ~ 1 + x_kk_headline_true*z_jj_crt + (1|jj) + (1 + z_jj_crt|kk),
  data = df,
  chains = 8,
  iter = 1500,
  warmup = 750,
  seed = 4962216,
  adapt_delta = 0.999
  )

# Model 8 (deliberation as a response x veracity: Does deliberating over a post reduce sharing intentions for false vs true news?)
m8_fit <- brm(
  z_post_share_index ~ 1 + z_ii_post_deliberation_index + z_post_deliberation*x_kk_headline_true + (1|jj) + (1 + z_post_deliberation|kk),
  data = df,
  chains = 8,
  iter = 1500,
  warmup = 750,
  seed = 4962216,
  adapt_delta = 0.999
  )

# Model 9: Does analytical thinking increase plausibility concerns (reduce sharing of unbelievable posts)?
m9_fit <- brm(
  z_post_share_index ~ 1 + z_kk_headline_believable + z_jj_crt*z_headline_believable*x_kk_headline_true + (1|jj) + (1 + z_jj_crt*z_headline_believable|kk),
  data = df,
  chains = 8,
  iter = 1500,
  warmup = 750,
  seed = 4962216,
  adapt_delta = 0.999
)

#Only nonmoral stimuli: control for familiarity and moral values (Does analytical thinking reduce truth discernment for nonmoral messages?)
m10_fit <- brm(
  z_post_share_index ~ 1 + z_kk_headline_familiar + z_headline_familiar + z_jj_crt*z_kk_headline_true + (1|jj) + (1 + z_jj_crt + z_headline_familiar|kk),
  data = df2,
  chains = 8,
  iter = 1500,
  warmup = 750,
  seed = 4962216,
  adapt_delta = 0.999
)

# Model 4 with analytical thinking as moderator of moral alignment (Does CRT moderate the effect of moral alignment on sharing?)
m11_fit <- brm(
  z_post_share_index ~ 1 + z_kk_headline_true + z_jj_crt*x_post_framing*z_jj_mfq_bind + z_jj_crt*x_post_framing*z_jj_mfq_indi + (1|jj) + (1 + z_jj_crt*x_post_framing*z_jj_mfq_bind + z_jj_crt*x_post_framing*z_jj_mfq_indi|kk),
  data = df,
  chains = 8,
  iter = 1500,
  warmup = 750,
  seed = 4962216,
  adapt_delta = 0.999
)

  # Save results as .rds
  write_rds(m0_fit, "../results/m0_fit.rds")
  write_rds(m1_fit, "../results/m1_fit.rds")
  write_rds(m2_fit, "../results/m2_fit.rds")
  write_rds(m3_fit, "../results/m3_fit.rds")
  write_rds(m4_fit, "../results/m4_fit.rds")
  write_rds(m5_fit, "../results/m5_fit.rds")
  write_rds(m6_fit, "../results/m6_fit.rds")
  
  # additional analyses
  write_rds(m7_fit, "../results/m7_fit.rds")
  write_rds(m8_fit, "../results/m8_fit.rds")
  write_rds(m9_fit, "../results/m9_fit.rds")
  write_rds(m10_fit, "../results/m10_fit.rds")
  write_rds(m11_fit, "../results/m11_fit.rds")


# Compare -----------------------------------------------------------------

# Assign folds for 10-fold cross-validation
set.seed(9735121)
df <- df %>% 
  distinct(jj) %>% 
  mutate(fold = sample.int(10, n(), replace = TRUE)) %>% 
  left_join(df, ., by = c("jj"))

df2 <- df2 %>% 
  distinct(jj) %>% 
  mutate(fold = sample.int(10, n(), replace = TRUE)) %>% 
  left_join(df2, ., by = c("jj"))

# Run 10-fold cross-valitation
plan(multisession)

m0_fit <- add_criterion(
  m0_fit,
  criterion = "kfold",
  chains = 1,
  folds = df$fold
  )
m1_fit <- add_criterion(
  m1_fit,
  criterion = "kfold",
  chains = 1,
  folds = df$fold
  )
m2_fit <- add_criterion(
  m2_fit,
  criterion = "kfold",
  chains = 1,
  folds = df$fold
  )
m3_fit <- add_criterion(
  m3_fit,
  criterion = "kfold",
  chains = 1,
  folds = df$fold
  )
m4_fit <- add_criterion(
  m4_fit,
  criterion = "kfold",
  chains = 1,
  folds = df$fold
  )
m5_fit <- add_criterion(
  m5_fit,
  criterion = "kfold",
  chains = 1,
  folds = df$fold
  )
m6_fit <- add_criterion(
  m6_fit,
  criterion = "kfold",
  chains = 1,
  folds = df$fold
  )

# additional analyses
m7_fit <- add_criterion(
  m7_fit,
  criterion = "kfold",
  chains = 1,
  folds = df$fold
  )
m8_fit <- add_criterion(
  m8_fit,
  criterion = "kfold",
  chains = 1,
  folds = df$fold
  )
m9_fit <- add_criterion(
  m9_fit,
  criterion = "kfold",
  chains = 1,
  folds = df$fold
  )
m10_fit <- add_criterion(
  m10_fit,
  criterion = "kfold",
  chains = 1,
  folds = df2$fold
)
m11_fit <- add_criterion(
  m11_fit,
  criterion = "kfold",
  chains = 1,
  folds = df$fold
)


plan("default")

# Compare models
#  loo_compare(
#    m0_fit,
#    m1_fit,
#    m2_fit,
#    m3_fit,
#    m4_fit,
#    m5_fit,
#    criterion = "kfold"
#  )

# Calculate Bayesian R2
m0_fit <- add_criterion(
  m0_fit,
  criterion = "bayes_R2",
  re_formula = NA
  )
m1_fit <- add_criterion(
  m1_fit,
  criterion = "bayes_R2",
  re_formula = NA
  )
m2_fit <- add_criterion(
  m2_fit,
  criterion = "bayes_R2",
  re_formula = NA
  )
m3_fit <- add_criterion(
  m3_fit,
  criterion = "bayes_R2",
  re_formula = NA
  )
m4_fit <- add_criterion(
  m4_fit,
  criterion = "bayes_R2",
  re_formula = NA
  )
m5_fit <- add_criterion(
  m5_fit,
  criterion = "bayes_R2",
  re_formula = NA
  )
m6_fit <- add_criterion(
  m6_fit,
  criterion = "bayes_R2",
  re_formula = NA
  )

# Additional analyses
m7_fit <- add_criterion(
  m7_fit,
  criterion = "bayes_R2",
  re_formula = NA
  )
m8_fit <- add_criterion(
  m8_fit,
  criterion = "bayes_R2",
  re_formula = NA
  )
m9_fit <- add_criterion(
  m9_fit,
  criterion = "bayes_R2",
  re_formula = NA
  )
m10_fit <- add_criterion(
  m10_fit,
  criterion = "bayes_R2",
  re_formula = NA
  )
m11_fit <- add_criterion(
  m11_fit,
  criterion = "bayes_R2",
  re_formula = NA
)


# Export ------------------------------------------------------------------

  # Save results as .rds
  write_rds(m0_fit, "../results/m0_fit.rds")
  write_rds(m1_fit, "../results/m1_fit.rds")
  write_rds(m2_fit, "../results/m2_fit.rds")
  write_rds(m3_fit, "../results/m3_fit.rds")
  write_rds(m4_fit, "../results/m4_fit.rds")
  write_rds(m5_fit, "../results/m5_fit.rds")
  write_rds(m6_fit, "../results/m6_fit.rds")
  
  # additional analyses
  write_rds(m7_fit, "../results/m7_fit.rds")
  write_rds(m8_fit, "../results/m8_fit.rds")
  write_rds(m9_fit, "../results/m9_fit.rds")
  write_rds(m10_fit, "../results/m10_fit.rds")
  write_rds(m11_fit, "../results/m11_fit.rds")

# Process -----------------------------------------------------------------

  ######### Model Comparisons
# Load results
m0 <- read_rds("../results/m0_fit.rds")
m1 <- read_rds("../results/m1_fit.rds")
m2 <- read_rds("../results/m2_fit.rds")
m3 <- read_rds("../results/m3_fit.rds")
m4 <- read_rds("../results/m4_fit.rds")
m5 <- read_rds("../results/m5_fit.rds")
m6 <- read_rds("../results/m6_fit.rds")

# Compile models
results <- crossing(
    model0 = paste0("M", 0:5),
    model1 = paste0("M", 0:5)
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

#get R2 (robust) comparison
models <- paste0("m", 0:5)
R2 <- as.data.frame(sapply(models, function(x)round(100*bayes_R2(get(x), robust = T),2)[1]))
colnames(R2) = c("R2")

#combine comparisons
results_comparison <- results %>% mutate(z = elpd_diff / elpd_se_diff)

# Save results as .rds
write_rds(results_comparison, "../results/comparison.rds")

############## Model comparison additional analyses (M0, M7, M8)
#Load results
m0 <- read_rds("../results/m0_fit.rds")
m7 <- read_rds("../results/m7_fit.rds")
m8 <- read_rds("../results/m8_fit.rds")
m9 <- read_rds("../results/m9_fit.rds")
m10 <- read_rds("../results/m10_fit.rds")
m11 <- read_rds("../results/m11_fit.rds")

# Compile models
results <- crossing(
  model0 = paste0("M", c(0,7,8)),
  model1 = paste0("M", c(0,7,8))
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
write_rds(results, "../results/comparison_deliberation.rds")

################ Summarize main results
#### Load main models (M4, M6)

# Extract coefficients: Model 4 (main model)
m4_results <- m4 %>% 
  spread_draws(
    b_z_jj_mfq_indi,
    b_z_jj_mfq_bind,
    b_z_jj_mfq_prop,
    `b_x_post_framing1:z_jj_mfq_indi`,
    `b_x_post_framing2:z_jj_mfq_indi`,
    `b_x_post_framing1:z_jj_mfq_bind`,
    `b_x_post_framing2:z_jj_mfq_bind`,
    `b_x_post_framing1:z_jj_mfq_prop`,
    `b_x_post_framing2:z_jj_mfq_prop`
  ) %>% 
  transmute(
    .chain, .iteration, .draw,
    b_indi_indi = b_z_jj_mfq_indi + 1 * `b_x_post_framing1:z_jj_mfq_indi` + 0 * `b_x_post_framing2:z_jj_mfq_indi`,
    b_indi_bind = b_z_jj_mfq_indi + 0 * `b_x_post_framing1:z_jj_mfq_indi` + 1 * `b_x_post_framing2:z_jj_mfq_indi`,
    b_indi_nonm = b_z_jj_mfq_indi + -1 * `b_x_post_framing1:z_jj_mfq_indi` + -1 * `b_x_post_framing2:z_jj_mfq_indi`,
    b_bind_indi = b_z_jj_mfq_bind + 1 * `b_x_post_framing1:z_jj_mfq_bind` + 0 * `b_x_post_framing2:z_jj_mfq_bind`,
    b_bind_bind = b_z_jj_mfq_bind + 0 * `b_x_post_framing1:z_jj_mfq_bind` + 1 * `b_x_post_framing2:z_jj_mfq_bind`,
    b_bind_nonm = b_z_jj_mfq_bind + -1 * `b_x_post_framing1:z_jj_mfq_bind` + -1 * `b_x_post_framing2:z_jj_mfq_bind`,
    b_prop_indi = b_z_jj_mfq_prop + 1 * `b_x_post_framing1:z_jj_mfq_prop` + 0 * `b_x_post_framing2:z_jj_mfq_prop`,
    b_prop_bind = b_z_jj_mfq_prop + 0 * `b_x_post_framing1:z_jj_mfq_prop` + 1 * `b_x_post_framing2:z_jj_mfq_prop`,
    b_prop_nonm = b_z_jj_mfq_prop + -1 * `b_x_post_framing1:z_jj_mfq_prop` + -1 * `b_x_post_framing2:z_jj_mfq_prop`,
    d_indi_indi_bind = b_indi_indi - b_indi_bind,
    d_indi_indi_nonm = b_indi_indi - b_indi_nonm,
    d_indi_bind_nonm = b_indi_bind - b_indi_nonm,
    d_bind_bind_indi = b_bind_bind - b_bind_indi,
    d_bind_bind_nonm = b_bind_bind - b_bind_nonm,
    d_bind_ind_nonm = b_bind_indi - b_bind_nonm
  ) %>% 
  pivot_longer(c(-.chain, -.iteration, -.draw)) %>% 
  group_by(name) %>% 
  median_qi(value) %>% 
  mutate(across(where(is.numeric), numform::f_num, digits = 2, retain.leading.zero = T))
write_rds(m4_results, "../results/m4_effects.rds")


# Extract coefficients: Model 6 (main model + control)
m6_results <- m6 %>% 
  spread_draws(
    b_z_jj_mfq_indi,
    b_z_jj_mfq_bind,
    b_z_jj_mfq_prop,
    `b_x_post_framing1:z_jj_mfq_indi`,
    `b_x_post_framing2:z_jj_mfq_indi`,
    `b_x_post_framing1:z_jj_mfq_bind`,
    `b_x_post_framing2:z_jj_mfq_bind`,
    `b_x_post_framing1:z_jj_mfq_prop`,
    `b_x_post_framing2:z_jj_mfq_prop`
  ) %>% 
  transmute(
    .chain, .iteration, .draw,
    b_indi_indi = b_z_jj_mfq_indi + 1 * `b_x_post_framing1:z_jj_mfq_indi` + 0 * `b_x_post_framing2:z_jj_mfq_indi`,
    b_indi_bind = b_z_jj_mfq_indi + 0 * `b_x_post_framing1:z_jj_mfq_indi` + 1 * `b_x_post_framing2:z_jj_mfq_indi`,
    b_indi_nonm = b_z_jj_mfq_indi + -1 * `b_x_post_framing1:z_jj_mfq_indi` + -1 * `b_x_post_framing2:z_jj_mfq_indi`,
    b_bind_indi = b_z_jj_mfq_bind + 1 * `b_x_post_framing1:z_jj_mfq_bind` + 0 * `b_x_post_framing2:z_jj_mfq_bind`,
    b_bind_bind = b_z_jj_mfq_bind + 0 * `b_x_post_framing1:z_jj_mfq_bind` + 1 * `b_x_post_framing2:z_jj_mfq_bind`,
    b_bind_nonm = b_z_jj_mfq_bind + -1 * `b_x_post_framing1:z_jj_mfq_bind` + -1 * `b_x_post_framing2:z_jj_mfq_bind`,
    b_prop_indi = b_z_jj_mfq_prop + 1 * `b_x_post_framing1:z_jj_mfq_prop` + 0 * `b_x_post_framing2:z_jj_mfq_prop`,
    b_prop_bind = b_z_jj_mfq_prop + 0 * `b_x_post_framing1:z_jj_mfq_prop` + 1 * `b_x_post_framing2:z_jj_mfq_prop`,
    b_prop_nonm = b_z_jj_mfq_prop + -1 * `b_x_post_framing1:z_jj_mfq_prop` + -1 * `b_x_post_framing2:z_jj_mfq_prop`,
    d_indi_indi_bind = b_indi_indi - b_indi_bind,
    d_indi_indi_nonm = b_indi_indi - b_indi_nonm,
    d_indi_bind_nonm = b_indi_bind - b_indi_nonm,
    d_bind_bind_indi = b_bind_bind - b_bind_indi,
    d_bind_bind_nonm = b_bind_bind - b_bind_nonm,
    d_bind_ind_nonm = b_bind_indi - b_bind_nonm
  ) %>% 
  pivot_longer(c(-.chain, -.iteration, -.draw)) %>% 
  group_by(name) %>% 
  median_qi(value) %>% 
  mutate(across(where(is.numeric), numform::f_num, digits = 2, retain.leading.zero = T))
write_rds(m6_results, "../results/m6_effects.rds")

########## Results summary additional analyses
### Load main model (M10)
m10_results <- m10 %>% 
  spread_draws(
    b_z_jj_crt,
    b_z_kk_headline_true,
    `b_z_jj_crt:z_kk_headline_true`,
  ) %>% 
  transmute(
    .chain, .iteration, .draw,
    b_z_jj_crt_true = b_z_jj_crt + 1 * `b_z_jj_crt:z_kk_headline_true`,
    b_z_jj_crt_false = b_z_jj_crt + 0 * `b_z_jj_crt:z_kk_headline_true`,
  ) %>% 
  pivot_longer(c(-.chain, -.iteration, -.draw)) %>% 
  group_by(name) %>% 
  median_qi(value) %>% 
  mutate(across(where(is.numeric), numform::f_num, digits = 2, retain.leading.zero = T))
write_rds(m10_results, "../results/m10_effects.rds")

### Load additional model (M7)
m7_results <- m7 %>% 
  spread_draws(
    b_z_jj_crt,
    b_x_kk_headline_true,
    `b_x_kk_headline_true:z_jj_crt`
  ) %>% 
  transmute(
    .chain, .iteration, .draw,
    b_crt_true = b_z_jj_crt + 1 * `b_x_kk_headline_true:z_jj_crt`,
    b_crt_false = b_z_jj_crt + 0 * `b_x_kk_headline_true:z_jj_crt`,
    d_crt_false_true = b_crt_false - b_crt_true 
    #truth discernment: CRT effect difference on false vs true news
  ) %>% 
  pivot_longer(c(-.chain, -.iteration, -.draw)) %>% 
  group_by(name) %>% 
  median_qi(value) %>% 
  mutate(across(where(is.numeric), numform::f_num, digits = 2, retain.leading.zero = T))
write_rds(m7_results, "../results/m7_effects.rds")

### Load additional model (M8)
m8_results <- m8 %>% 
  spread_draws(
    b_z_post_deliberation,
    b_x_kk_headline_true,
    `b_z_post_deliberation:x_kk_headline_true`
  ) %>% 
  transmute(
    .chain, .iteration, .draw,
    b_delib_true = b_z_post_deliberation + 1 * `b_z_post_deliberation:x_kk_headline_true`,
    b_delib_false = b_z_post_deliberation + 0 * `b_z_post_deliberation:x_kk_headline_true`,
    d_delib_false_true = b_delib_false - b_delib_true 
    #truth discernment: deliberation effect difference on false vs true news 
    #(does deliberation lead to fake news being shared less than true news?)
  ) %>% 
  pivot_longer(c(-.chain, -.iteration, -.draw)) %>% 
  group_by(name) %>% 
  median_qi(value) %>% 
  mutate(across(where(is.numeric), numform::f_num, digits = 2, retain.leading.zero = T))
write_rds(m8_results, "../results/m8_effects.rds")

### Load additional model (M9)
m9_results <- m9 %>% 
  spread_draws(
    b_z_jj_crt,
    b_x_kk_headline_true,
    `b_z_jj_crt:x_kk_headline_true`,
    b_z_headline_believable,
    `b_z_jj_crt:z_headline_believable`
  ) %>% 
  transmute(
    .chain, .iteration, .draw,
    d_plausib = `b_z_jj_crt:z_headline_believable` #difference in plausibility concerns (for analytical thinkers)
  ) %>% 
  pivot_longer(c(-.chain, -.iteration, -.draw)) %>% 
  group_by(name) %>% 
  median_qi(value) %>% 
  mutate(across(where(is.numeric), numform::f_num, digits = 2, retain.leading.zero = T))
write_rds(m9_results, "../results/m9_effects.rds")

### Load additional model (M11)
m11_results <- m11 %>% 
  spread_draws(
    `b_z_jj_crt:x_post_framing2:z_jj_mfq_bind`,
    `b_z_jj_crt:x_post_framing1:z_jj_mfq_indi`
  ) %>% 
  transmute(
    .chain, .iteration, .draw,
    d_crt_bind_match = `b_z_jj_crt:x_post_framing2:z_jj_mfq_bind`, #moderation of matching binding framing and values by analytical thinking
    d_crt_indi_match = `b_z_jj_crt:x_post_framing1:z_jj_mfq_indi` #moderation of matching individualizing framing and values by analytical thinking
    #Does analytical thinking reduce susceptibility to moral framing?
    ) %>% 
  pivot_longer(c(-.chain, -.iteration, -.draw)) %>% 
  group_by(name) %>% 
  median_qi(value) %>% 
  mutate(across(where(is.numeric), numform::f_num, digits = 2, retain.leading.zero = T))
write_rds(m11_results, "../results/m11_effects.rds")
