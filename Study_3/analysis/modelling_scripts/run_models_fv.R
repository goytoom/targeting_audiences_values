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
  m0_fv <- brm(
    favorite_count ~ partisan_score + (1|user_id),
    family = "negbinomial", 
    data = df_total_binding,
    chains = 8,
    iter = 1500,
    warmup = 750,
    seed = 4962216
  )
  
  m1_fv <- brm(
    favorite_count ~ partisan_score + (binding + individual) + (1|user_id),
    family = "negbinomial", 
    data = df_total_binding,
    chains = 8,
    iter = 1500,
    warmup = 750,
    seed = 4962216
  )
  
  m2_fv <- brm(
    favorite_count ~ partisan_score*(binding + individual) + (1|user_id),
    family = "negbinomial", 
    data = df_total_binding,
    chains = 8,
    iter = 1500,
    warmup = 750,
    seed = 4962216
  )
  
  # add stance to the main model (for comparisons)
  m2_ideo_stance_fv <- brm(
    favorite_count ~ partisan_score*stance*(binding + individual) + (1|user_id),
    family = "negbinomial", 
    data = df_total_binding,
    chains = 8,
    iter = 1500,
    warmup = 750,
    seed = 4962216
  )
  
  # fit analogue model with stance instead of ideology (for comparison)
  m2_stance_fv <- brm(
    favorite_count ~ stance*(binding + individual) + (1|user_id), 
    family = "negbinomial", 
    data = df_total_binding[!is.na(df_total_binding$partisan_score),], #make sure same data points are used for stance and ideology models
    chains = 8,
    iter = 1500,
    warmup = 750,
    seed = 4962216
  )
  
  
  ### Save models
  write_rds(m0_fv, "../results/m0_fv.rds")
  write_rds(m1_fv, "../results/m1_fv.rds")
  write_rds(m2_fv, "../results/m2_fv.rds")
  write_rds(m2_stance_fv, "../results/m2_stance_fv.rds")
  write_rds(m2_ideo_stance_fv, "../results/m2_ideo_stance_fv.rds")
  

# Compare -----------------------------------------------------------------

  # Run 10-fold cross-validation
  m0_fv <- readRDS("../results/m0_fv.rds")
  m1_fv <- readRDS("../results/m1_fv.rds")
  m2_fv <- readRDS("../results/m2_fv.rds")
  m2_stance_fv <- readRDS("../results/m2_stance_fv.rds")
  m2_ideo_stance_fv <- readRDS("../results/m2_ideo_stance_fv.rds")
  
  plan(multisession)
  m0_fv <- add_criterion(
    m0_fv,
    criterion = "kfold",
    chains = 1,
    group = "user_id",
    folds = "grouped"
  )
  m1_fv <- add_criterion(
    m1_fv,
    criterion = "kfold",
    chains = 1,
    group = "user_id",
    folds = "grouped"
  )
  m2_fv <- add_criterion(
    m2_fv,
    criterion = "kfold",
    chains = 1,
    group = "user_id",
    folds = "grouped"
  )
  m2_ideo_stance_fv <- add_criterion(
    m2_ideo_stance_fv,
    criterion = "kfold",
    chains = 1,
    group = "user_id",
    folds = "grouped"
  )
  m2_stance_fv <- add_criterion(
    m2_stance_fv,
    criterion = "kfold",
    chains = 1,
    group = "user_id",
    folds = "grouped"
  )
  
  plan("default")
  
  # Calculate Bayesian R2
  m0_fv <- add_criterion(
    m0_fv, 
    criterion = "bayes_R2"
  )
  m1_fv <- add_criterion(
    m1_fv, 
    criterion = "bayes_R2"
  )
  m2_fv <- add_criterion(
    m2_fv, 
    criterion = "bayes_R2"
  )
  m2_ideo_stance_fv <- add_criterion(
    m2_ideo_stance_fv, 
    criterion = "bayes_R2"
  )
  m2_stance_fv <- add_criterion(
    m2_stance_fv, 
    criterion = "bayes_R2"
  )
  
  
  # Export ------------------------------------------------------------------
  
  # Save results as .rds
  write_rds(m0_fv, "../results/m0_fv.rds")
  write_rds(m1_fv, "../results/m1_fv.rds")
  write_rds(m2_fv, "../results/m2_fv.rds")
  write_rds(m2_ideo_stance_fv, "../results/m2_ideo_stance_fv.rds")
  write_rds(m2_stance_fv, "../results/m2_stance_fv.rds")
  
# Main Results ------------------------------------------------------------
  
  ######## Extract coefficients
  m2_fv_results <- m2_fv %>% 
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
  
  write_rds(m2_fv_results, "../results/m2_fv_effects.rds")
  
# Model comparison ----------------------------------------------------------

  # Load results
  m0 <- read_rds("../results/m0_fv.rds")
  m1 <- read_rds("../results/m1_fv.rds")
  m2 <- read_rds("../results/m2_fv.rds")
  
# Initialize models
  results <- crossing(
    model0 = paste0("M", 0:2),
    model1 = paste0("M", 0:2)
  ) %>% 
    mutate(
      fit0 = map(model0, ~read_rds(paste0("../results/", str_to_lower(.), "_fv.rds"))),
      fit1 = map(model1, ~read_rds(paste0("../results/", str_to_lower(.), "_fv.rds")))
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
  write_rds(results_comparison, "../results/comparison_fv.rds")
  
# Comparison with alternative models that use stance --------------------------------
  # Initialize models
  results <- crossing(
    model0 = paste0("M", 2),
    model1 = paste0("M", 2)
  ) %>% 
    mutate(
      fit0 = map(model0, ~read_rds(paste0("../results/", str_to_lower(.), "_fv.rds"))),
      fit1 = map(model1, ~read_rds(paste0("../results/", str_to_lower(.), "_stance_fv.rds")))
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
  write_rds(results_comparison_stance, "../results/comparison_stance_fv.rds")
  

  ## Main model vs stance, ideology interaction (does adding stance to the model increase predictive power?)
  # Initialize models
  results <- crossing(
    model0 = paste0("M", 2),
    model1 = paste0("M", 2)
  ) %>% 
    mutate(
      fit0 = map(model0, ~read_rds(paste0("../results/", str_to_lower(.), "_fv.rds"))),
      fit1 = map(model1, ~read_rds(paste0("../results/", str_to_lower(.), "_ideo_stance_fv.rds")))
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
  results_comparison_id_stance <- results %>% mutate(z = elpd_diff / elpd_se_diff) #the original model is significantly better compared to when stance is added
  
  # Save results as .rds
  write_rds(results_comparison_id_stance, "../results/comparison_id_stance_fv.rds")
  