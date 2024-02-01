rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse)
  library(brms)

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

  # Create dataset for analyses
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
  
  # Standardize headline-/post-level variables
  df <- dl %>% 
    select(
      ii, jj, kk, 
      starts_with("headline_"), 
      -headline_true,
      -headline_text,
      starts_with("post_"), 
      -post_sentiment, -post_framing, -post_text,
      -matches("share|like")
    ) %>% 
    transmute(
      ii, jj, kk,
      across(
        starts_with("post_"), 
        list(x = ~.), 
        .names = "{.fn}_{.col}"
      ),
      across(
        starts_with("post_"), 
        list(z = ~(. - mean(.))/sd(.)), 
        .names = "{.fn}_{.col}"
      ),
      across(
        starts_with("headline_"), 
        list(x = ~.), 
        .names = "{.fn}_{.col}"
      ),
      across(
        starts_with("headline_"), 
        list(z = ~(. - mean(.))/sd(.)), 
        .names = "{.fn}_{.col}"
      )
    ) %>% 
    left_join(df, by = c("ii", "jj", "kk"))
  
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
  
  df <- dl %>% select(ii, jj, kk, headline_true) %>% 
    mutate(z_headline_true = (headline_true - mean(headline_true))/sd(headline_true)) %>% 
    left_join(df, by = c("ii", "jj", "kk"))


# Estimate ----------------------------------------------------------------

  # Mediation (Alignment/Agreement)
  fit <- brm(
    bf(z_post_agree ~ 1 + z_headline_true + x_post_framing*z_jj_mfq_indi + x_post_framing*z_jj_mfq_bind + (1|jj) + (1 + x_post_framing*z_jj_mfq_indi + x_post_framing*z_jj_mfq_bind|kk)) +
    bf(z_post_align ~ 1 + z_headline_true + x_post_framing*z_jj_mfq_indi + x_post_framing*z_jj_mfq_bind + (1|jj) + (1 + x_post_framing*z_jj_mfq_indi + x_post_framing*z_jj_mfq_bind|kk)) +
    bf(z_post_share_index ~ 1 + z_headline_true + x_post_framing*z_jj_mfq_indi + x_post_framing*z_jj_mfq_bind + x_post_framing*z_post_agree*z_post_align + (1|jj) + (1 + x_post_framing*z_jj_mfq_indi + x_post_framing*z_jj_mfq_bind + x_post_framing*z_post_agree*z_post_align|kk)) +
    set_rescor(FALSE),
    data = df,
    chains = 8,
    iter = 1250,
    warmup = 750,
    seed = 2352701,
    adapt_delta = 0.99
  )

  
# Export ------------------------------------------------------------------

  # Save results as .rds
  write_rds(fit, "../results/fit_mediation.rds")
  

# Extract -----------------------------------------------------------------

  # Load packages
  library(tidybayes)

  # Extract estimates
  draws <- fit %>% 
    gather_draws(`b_.*`, regex = TRUE) %>% 
    crossing(
      contrasts(df$x_post_framing) %>% 
        as.data.frame() %>% 
        rownames_to_column() %>% 
        rename(.condition = rowname, x_post_framing1 = V1, x_post_framing2 = V2)
    )
  
  # Transform estimates
  draws <- draws %>% 
    mutate(
      .outcome = case_when(
        str_detect(.variable, "b_zpostagree") ~ "agree",
        str_detect(.variable, "b_zpostalign") ~ "align",
        str_detect(.variable, "b_zpostshareindex") ~ "share"
      ),
      .predictor = case_when(
        str_detect(.variable, "z_jj_mfq_bind") ~ "mfq_bind",
        str_detect(.variable, "z_jj_mfq_indi") ~ "mfq_indi",
        str_detect(.variable, "z_post_agree:z_post_align") ~ "agree_align",
        str_detect(.variable, "z_post_agree") ~ "agree",
        str_detect(.variable, "z_post_align") ~ "align",
      ),
      .term = case_when(
        str_detect(.variable, "x_post_framing1") ~ "b_post_framing1",
        str_detect(.variable, "x_post_framing2") ~ "b_post_framing2",
        TRUE ~ "b_post_framing0"
      )
    ) %>% 
    filter(!is.na(.predictor)) %>% 
    select(-.variable) %>% 
    pivot_wider(
      names_from = .term,
      values_from = .value
    ) %>% 
    mutate(
      .value = b_post_framing0 + b_post_framing1 * x_post_framing1 + b_post_framing2 * x_post_framing2
    ) %>% 
    select(-starts_with("b_"), -starts_with("x_")) %>% 
    pivot_wider(
      names_from = c(.predictor, .outcome),
      values_from = .value
    )
  
  # Calculate indirect effects
  draws <- draws %>% 
    mutate(
      ind_mfq_bind = mfq_bind_agree * agree_share + mfq_bind_align * align_share + (mfq_bind_agree + mfq_bind_align) * agree_align_share,
      ind_mfq_indi = mfq_indi_agree * agree_share + mfq_indi_align * align_share + (mfq_indi_agree + mfq_indi_align) * agree_align_share,
      dir_mfq_bind = mfq_bind_share,
      dir_mfq_indi = mfq_indi_share,
      tot_mfq_bind = ind_mfq_bind + dir_mfq_bind,
      tot_mfq_indi = ind_mfq_indi + dir_mfq_indi
    ) %>% 
    # select(.chain:.draw, .condition, starts_with("ind_"), starts_with("dir_"), starts_with("tot_")) %>%
    select(.chain:.draw, .condition, ends_with("_align")) %>%
    pivot_longer(
      c(-.chain:-.draw, -.condition),
      names_to = ".path",
      values_to = ".value"
    ) %>% 
    group_by(.condition, .path) %>% 
    median_qi(.value) %>% 
    mutate(across(where(is.double), round, 2))
    write_rds(draws, "../results/results_mediation.rds")
