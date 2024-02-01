rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse)
  library(brms)
  library(future)
  library(numform)

  # Options
  options(
    mc.cores = parallel::detectCores(),
    brms.backend = "cmdstanr",
    future.globals.maxSize = 2147483648
  )


# Prepare -----------------------------------------------------------------

  # Load data
  dl <- read_rds("../data/dl.rds")
  
  # Calculate Post: Share Index
  dl <- dl %>% mutate(
    post_share_index = (post_share_public + post_like_public + post_share_private + post_share_offline)/4
  )
  
  # Calculate Individualizing/Binding scores
  dl <- dl %>% mutate(
    mfq_indi = (mfq_care + mfq_equa)/2,
    mfq_bind = (mfq_loya + mfq_auth + mfq_puri)/3
  )
  
  # Drop missing data
  dl <- dl %>% drop_na(-headline_rating_true, -source_text)
  
  # Assign new indices
  dl <- dl %>% mutate(across(c(ii, kk, kk), ~as.integer(factor(.))))
  

# Standardize -------------------------------------------------------------

  # Create data set for analyses
  df <- dl %>% select(ii, jj, kk)
  
  # Standardize person-level variables
  df <- dl %>% 
    select(jj, starts_with("mfq_"), conservatism) %>% 
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
      z_headline_positive = headline_positive - x_kk_headline_positive
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
      z_post_align = post_align - x_ii_post_align
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
      z_post_share_index = (post_share_index - mean(post_share_index))/sd(post_share_index),
    ) %>% 
    left_join(df, by = c("ii", "jj", "kk"))


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
   z_post_share_index ~ 1 + z_kk_headline_believable + z_kk_headline_controversial + z_kk_headline_surprising + z_kk_headline_interesting + z_kk_headline_positive + z_kk_headline_true + z_headline_believable + z_headline_controversial + z_headline_surprising + z_headline_interesting + z_headline_positive + (1|ii) + (1|jj) + (1 + z_headline_believable + z_headline_controversial + z_headline_surprising + z_headline_interesting + z_headline_positive|kk),
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
   iter = 1000,
   warmup = 750,
   seed = 3076878
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

  # Model 6 (Model 4 with controls for inferences)
  m6_fit <- brm(
    z_post_share_index ~ 1 + z_kk_headline_true + x_post_framing*z_jj_mfq_indi + x_post_framing*z_jj_mfq_bind + x_post_framing*z_jj_mfq_prop + (1|jj) + (1 + x_post_framing*z_jj_mfq_indi + x_post_framing*z_jj_mfq_bind + x_post_framing*z_jj_mfq_prop|kk),
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
  

# Compare -----------------------------------------------------------------

  # Assign folds for 10-fold cross-validation
  set.seed(9735121)
  df <- df %>% 
    distinct(jj) %>% 
    mutate(fold = sample.int(10, n(), replace = TRUE)) %>% 
    left_join(df, ., by = c("jj"))
  
  # Run 10-fold cross-validation
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
  
  plan("default")
  
  # Compare models
 loo_compare(
   m0_fit,
   m1_fit,
   m2_fit,
   m3_fit,
   m4_fit,
   m5_fit,
   criterion = "kfold"
 )
  
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
  

# Export ------------------------------------------------------------------
  
  # Save results as .rds
 write_rds(m0_fit, "../results/m0_fit.rds")
 write_rds(m1_fit, "../results/m1_fit.rds")
 write_rds(m2_fit, "../results/m2_fit.rds")
 write_rds(m3_fit, "../results/m3_fit.rds")
 write_rds(m4_fit, "../results/m4_fit.rds")
 write_rds(m5_fit, "../results/m5_fit.rds")
 write_rds(m6_fit, "../results/m6_fit.rds")


# Process -----------------------------------------------------------------

##### Model Comparison
  # Load results
  m0 <- read_rds("../results/m0_fit.rds")
  m1 <- read_rds("../results/m1_fit.rds")
  m2 <- read_rds("../results/m2_fit.rds")
  m3 <- read_rds("../results/m3_fit.rds")
  m4 <- read_rds("../results/m4_fit.rds")
  m5 <- read_rds("../results/m5_fit.rds")
  m6 <- read_rds("../results/m6_fit.rds")
  
  # Initialize models
  results <- crossing(
      model0 = paste0("M", 0:5),
      model1 = paste0("M", 0:5)
    ) %>% 
      mutate(
        fit0 = map(model0, ~read_rds(paste0("../results/", str_to_lower(.), "_fit.rds"))),
        fit1 = map(model1, ~read_rds(paste0("../results/", str_to_lower(.), "_fit.rds")))
      )
  
  # Calculate ELPD differences
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
  write_rds(results_comparison, "../results/comparison.rds")
  
############ Summarize main model results
  
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
