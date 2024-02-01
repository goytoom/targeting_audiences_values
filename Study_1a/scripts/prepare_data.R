rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse)


# Import ------------------------------------------------------------------

  # Import survey data (Qualtrics)
  dr <- read_csv("Study 1a/data/raw/qualtrics_export.csv")[-1:-2,]
  
  # Import survey stimuli
  stimuli <- read_csv(
    "Study 1a/materials/stimuli_for_qualtrics.csv", 
    col_types = "iilccccc"
  )

  # Clean variable names
  dr <- dr %>% janitor::clean_names()


# Prepare -----------------------------------------------------------------
  
  # Select variables
  dw <- dr %>% 
    select(id, exclude, strata, contains("q"))
  
  # Transform variables
  dw <- dw %>% 
    mutate(
      exclude = as.logical(exclude),
      across(
        c(contains("q"), -contains("text"), -q58_1),
        as.integer
      )
    )

  # Exclude participants
  dw <- dw %>% filter(!exclude)

  # Transform to long data
  dl <- dw %>% 
    select(-matches("^x[1-3]")) %>% 
    left_join(
      dw %>% 
        select(id, matches("^x[1-3]")) %>% 
        mutate(across(matches("^x[1-3]"), as.character)) %>% 
        pivot_longer(
          matches("^x[1-3]"),
          names_to = c("ii", "kk", "item"),
          names_pattern = "x([1-3])_q([0-9]+)_(.*)",
          names_transform = list(ii = as.integer, kk = as.integer),
          values_to = "response",
          values_transform = list(response = as.integer),
          values_drop_na = TRUE
        ) %>% 
        mutate(kk = kk - 3L),
      by = "id"
    )
  
  # Create variables
  dl <- dl %>% 
    transmute(
      id,
      strata = recode_factor(
        strata,
        "L" = "Liberal",
        "M" = "Moderate",
        "C" = "Conservative"
      ),
      ii, kk,
      item = recode_factor(
        item,
        "3_1" = "headline_believable",
        "3_2" = "headline_controversial",
        "3_3" = "headline_surprising",
        "3_4" = "headline_interesting",
        "3_5" = "headline_positive",
        "5_1" = "post_controversial",
        "5_2" = "post_surprising",
        "5_3" = "post_interesting",
        "5_4" = "post_positive",
        "6"   = "post_agree",
        "7"   = "post_align",
        "9_1" = "post_share_public",
        "9_2" = "post_like_public",
        "9_3" = "post_share_private",
        "9_4" = "post_share_offline"
      ),
      response,
      mfq_care = (q56_35 + q56_36 + q56_37 + q56_38 + q56_39 + q56_40)/6,
      mfq_equa = (q56_29 + q56_30 + q56_31 + q56_32 + q56_33 + q56_34)/6,
      mfq_prop = (q56_23 + q56_24 + q56_25 + q56_26 + q56_27 + q56_28)/6,
      mfq_loya = (q56_17 + q56_18 + q56_19 + q56_20 + q56_21 + q56_22)/6,
      mfq_auth = (q56_11 + q56_12 + q56_13 + q56_14 + q56_15 + q56_16)/6,
      mfq_puri = (q56_5 + q56_6 + q56_7 + q56_8 + q56_9 + q56_10)/6,
      age = as.integer(q57_1),
      gender = recode_factor(
        q57_2,
        "1" = "Woman",
        "2" = "Man",
        "3" = "Other"
      ),
      education = q57_3,
      conservatism = q57_5_1
    ) %>% 
    pivot_wider(names_from = item, values_from = response)
  
  # Add stimuli information
  dl <- dl %>% left_join(stimuli, by = c("ii", "kk"))
  
  # Reassign indeces
  dl <- dl %>% 
    distinct(ii, kk) %>% 
    arrange(kk, ii) %>% 
    rowid_to_column("new_ii") %>% 
    left_join(dl, ., by = c("ii", "kk")) %>% 
    mutate(ii = new_ii, jj = as.integer(factor(id))) %>% 
    select(id, strata, ii, kk, jj, starts_with("headline"), starts_with("post"), everything(), -new_ii) %>% 
    arrange(jj, ii)
  

# Export ------------------------------------------------------------------
  
  # Export as .rds
  write_rds(dl, "Study 1a/data/dl.rds")
  
  # Export as .csv
  write_excel_csv(dl, "Study 1a/data/csv/dl.csv")
