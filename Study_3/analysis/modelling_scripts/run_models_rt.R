rm(list = ls())

# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse)
  library(brms)
  library(future)
  library(numform)
  library(tidybayes)

  # Options
  options(
    mc.cores = 10,
    brms.backend = "cmdstanr",
    future.globals.maxSize = 2147483648
  )


# Prepare -----------------------------------------------------------------

  # Import data
  df_anti_binding <- read_csv("antivax_binding.csv") %>% 
    select(-c(full_text)) 
  df_pro_binding <- read_csv("provax_binding.csv") %>% 
    select(-c(full_text))
  
  df_ideology <- read_csv("../../data/political_ideology_misinfo.csv")
  
  # Prepare data
  df_total_binding <- rbind(df_anti_binding, df_pro_binding) %>%
    mutate(
      binding = factor(binding),
      individual = factor(individual),
      stance = factor(stance_label, labels = c("anti-vax", "pro-vax"))
    )
  
  df_total_binding <- merge(df_total_binding, df_ideology, by.x = "user_id", by.y = "id") #add political ideology to the data
  


# Estimate ----------------------------------------------------------------

  # Model 0: Only random intercepts on the user level
  m0_rt <- brm(
    retweet_count ~ partisan_score + (1|user_id),
    family = "negbinomial", 
    data = df_total_binding,
    chains = 8,
    iter = 1500,
    warmup = 750,
    seed = 4962216
  )
  
  #M1: add moral values
  m1_rt <- brm(
    retweet_count ~ partisan_score + (binding + individual) + (1|user_id),
    family = "negbinomial", 
    data = df_total_binding,
    chains = 8,
    iter = 1500,
    warmup = 750,
    seed = 4962216
  )
  
  #M2: add political ideology and values interaction
  m2_rt <- brm(
    retweet_count ~ partisan_score*(binding + individual) + (1|user_id),
    family = "negbinomial", 
    data = df_total_binding,
    chains = 8,
    iter = 1500,
    warmup = 750,
    seed = 4962216
  )
  
  # add stance to the main model (for comparisons)
  m2_ideo_stance_rt <- brm(
    retweet_count ~ partisan_score*stance*(binding + individual) + (1|user_id),
    family = "negbinomial", 
    data = df_total_binding,
    chains = 8,
    iter = 1500,
    warmup = 750,
    seed = 4962216
  )
  
  # fit analogue model with stance instead of ideology (for comparison)
  m2_stance_rt <- brm(
    retweet_count ~ stance*(binding + individual) + (1|user_id), 
    family = "negbinomial", 
    data = df_total_binding[!is.na(df_total_binding$partisan_score),],
    chains = 8,
    iter = 1500,
    warmup = 750,
    seed = 4962216
  )
  
  # Stance x ideology (supplementary, test whether stance and ideology is aligned)
  m3_rt <- brm(
    retweet_count ~ partisan_score*stance + (1|user_id), 
    family = "negbinomial", 
    data = df_total_binding,
    chains = 8,
    iter = 1500,
    warmup = 750,
    seed = 4962216
  )
  
  
  ### Save models
  write_rds(m0_rt, "../results/m0_rt.rds")
  write_rds(m1_rt, "../results/m1_rt.rds")
  write_rds(m2_rt, "../results/m2_rt.rds")
  write_rds(m2_stance_rt, "../results/m2_stance_rt.rds")
  write_rds(m2_ideo_stance_rt, "../results/m2_ideo_stance_rt.rds")
  write_rds(m3_rt, "../results/m3_rt.rds")
  

# Compare -----------------------------------------------------------------

  # Run 10-fold cross-validation
  m0_rt <- readRDS("../results/m0_rt.rds")
  m1_rt <- readRDS("../results/m1_rt.rds")
  m2_rt <- readRDS("../results/m2_rt.rds")
  m2_stance_rt <- readRDS("../results/m2_stance_rt.rds")
  m2_ideo_stance_rt <- readRDS("../results/m2_ideo_stance_rt.rds")
  m3_rt <- readRDS("../results/m3_rt.rds")
  
  
  plan(multisession)
  m0_rt <- add_criterion(
    m0_rt,
    criterion = "kfold",
    chains = 1,
    group = "user_id",
    folds = "grouped"
  )
  m1_rt <- add_criterion(
    m1_rt,
    criterion = "kfold",
    chains = 1,
    group = "user_id",
    folds = "grouped"
  )
  m2_rt <- add_criterion(
    m2_rt,
    criterion = "kfold",
    chains = 1,
    group = "user_id",
    folds = "grouped"
  )
  m2_stance_rt <- add_criterion(
    m2_stance_rt,
    criterion = "kfold",
    chains = 1,
    group = "user_id",
    folds = "grouped"
  )
  m2_ideo_stance_rt <- add_criterion(
    m2_ideo_stance_rt,
    criterion = "kfold",
    chains = 1,
    group = "user_id",
    folds = "grouped"
  )
  m3_rt <- add_criterion(
    m3_rt,
    criterion = "kfold",
    chains = 1,
    group = "user_id",
    folds = "grouped"
  )
  
  plan("default")
  
  # Calculate Bayesian R2
  m0_rt <- add_criterion(
    m0_rt, 
    criterion = "bayes_R2"
  )
  m1_rt <- add_criterion(
    m1_rt, 
    criterion = "bayes_R2"
  )
  m2_rt <- add_criterion(
    m2_rt, 
    criterion = "bayes_R2"
  )
  m2_stance_rt <- add_criterion(
    m2_stance_rt, 
    criterion = "bayes_R2"
  )
  m2_ideo_stance_rt <- add_criterion(
    m2_ideo_stance_rt, 
    criterion = "bayes_R2"
  )
  m3_rt <- add_criterion(
    m3_rt, 
    criterion = "bayes_R2"
  )
  
  # Export ------------------------------------------------------------------
  
  # Save results as .rds
  write_rds(m0_rt, "../results/m0_rt.rds")
  write_rds(m1_rt, "../results/m1_rt.rds")
  write_rds(m2_rt, "../results/m2_rt.rds")
  write_rds(m2_stance_rt, "../results/m2_stance_rt.rds")
  write_rds(m2_ideo_stance_rt, "../results/m2_ideo_stance_rt.rds")
  write_rds(m3_rt, "../results/m3_rt.rds")
  
  # Main Results ------------------------------------------------------------
  ######## Extract coefficients
  m2_rt_results <- m2_rt %>% 
    spread_draws(
      `b_binding1`,
      `b_individual1`,
      `b_partisan_score:individual1`,
      `b_partisan_score:binding1`,
    ) %>% 
    transmute(
      .chain, .iteration, .draw,
      b_indi_cons = `b_individual1` + 1* `b_partisan_score:individual1` + 0 * `b_partisan_score:binding1`,
      b_indi_lib = `b_individual1` - 1 * `b_partisan_score:individual1` + 0 * `b_partisan_score:binding1`,
      b_bind_cons = `b_binding1` + 0 * `b_partisan_score:individual1` + 1 * `b_partisan_score:binding1`,
      b_bind_lib = `b_binding1` + 0 * `b_partisan_score:individual1` - 1 * `b_partisan_score:binding1`,
      d_lib_indi_bind = b_indi_lib - b_bind_lib,
      d_con_bind_indi = b_bind_cons - b_indi_cons,
      d_bind_con_lib = b_bind_cons - b_bind_lib,
      d_indi_lib_con = b_indi_lib - b_indi_cons
    ) %>% 
    pivot_longer(c(-.chain, -.iteration, -.draw)) %>% 
    group_by(name) %>% 
    median_qi(value) %>% 
    mutate(across(where(is.numeric), numform::f_num, digits = 2, retain.leading.zero = T))
  
  write_rds(m2_rt_results, "../results/m2_rt_effects.rds")
  
# Model comparison ---------------------------------------------------------

  # Load results
  m0 <- read_rds("../results/m0_rt.rds")
  m1 <- read_rds("../results/m1_rt.rds")
  m2 <- read_rds("../results/m2_rt.rds")
  
# Initialize models
  results <- crossing(
    model0 = paste0("M", 0:2),
    model1 = paste0("M", 0:2)
  ) %>% 
    mutate(
      fit0 = map(model0, ~read_rds(paste0("../results/", str_to_lower(.), "_rt.rds"))),
      fit1 = map(model1, ~read_rds(paste0("../results/", str_to_lower(.), "_rt.rds")))
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
  write_rds(results_comparison, "../results/comparison_rt.rds")
  
  
# Comparison with old models (that use stance) ---------------------------------------------------------
  # Initialize models
  results <- crossing(
    model0 = paste0("M", 2),
    model1 = paste0("M", 2)
  ) %>% 
    mutate(
      fit0 = map(model0, ~read_rds(paste0("", str_to_lower(.), "_rt.rds"))),
      fit1 = map(model1, ~read_rds(paste0("", str_to_lower(.), "_stance_rt.rds")))
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
  results_comparison_stance <- results %>% mutate(z = elpd_diff / elpd_se_diff)
  
  # Save results as .rds
  write_rds(results_comparison_stance, "../results/comparison_stance_rt.rds")
  
  
  # compare the ideology model with a model containing both ideology and stance
  results <- crossing(
    model0 = paste0("M", 2),
    model1 = paste0("M", 2)
  ) %>% 
    mutate(
      fit0 = map(model0, ~read_rds(paste0("", str_to_lower(.), "_rt.rds"))),
      fit1 = map(model1, ~read_rds(paste0("", str_to_lower(.), "_ideo_stance_rt.rds")))
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
  results_comparison_id_stance <- results %>% mutate(z = elpd_diff / elpd_se_diff)
  
  # Save results as .rds
  write_rds(results_comparison_id_stance, "../results/comparison_id_stance_rt.rds")
  