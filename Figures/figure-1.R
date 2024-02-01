rm(list = ls())

# Notes -------------------------------------------------------------------


# Library -----------------------------------------------------------------

  # Load packages
  library(tidyverse)


# Prepare -----------------------------------------------------------------
  
  # Import data
  dw <- bind_rows(
    read_rds("../Study_2a/data/dl.rds") %>% 
      distinct(id, strata, conservatism) %>% 
      mutate(study = "Study 2a"),
    read_rds("../Study_2b/data/dl.rds") %>% 
      distinct(id, strata, conservatism) %>% 
      mutate(study = "Study 2b")
  )
  

# Make figure -------------------------------------------------------------

  # Figure 1
  ggplot(dw, aes(x = conservatism)) +
    geom_bar(
      aes(y = ..prop.., colour = study, fill = study),
      position = position_identity(),
      width = 0.8,
      linewidth = 0.455/2
    ) +
    geom_hline(
      yintercept = 1/7,
      linetype = "dashed",
      linewidth = 0.455/2
    ) +
    scale_x_continuous(
      breaks = 1:7
    ) +
    scale_y_continuous(
      labels = ~scales::percent(., accuracy = 1)
    ) +
    scale_colour_manual(
      values = c(
        "Study 2a" = alpha("grey70", 1),
        "Study 2b" = alpha("black", 1)
      )
    ) +
    scale_fill_manual(
      values = c(
        "Study 2a" = alpha("grey70", 1),
        "Study 2b" = alpha("grey70", 0)
      )
    ) +
    theme_bw(base_size = 10) +
    theme(
      axis.text = element_text(colour = "black"),
      legend.key.size = unit(12, "pt"),
      legend.title = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    ) +
    labs(
      x = "Conservatism",
      y = NULL
    )


# Export ------------------------------------------------------------------

  # Export as .png
  ggsave(
    "figure-1.png",
    width = 6.5/2, height = 6.5/3, units = "in", dpi = 600,
    type = "cairo"
  )
  
  # Export as .pdf
  ggsave(
    "figure-1.pdf",
    width = 6.5/2, height = 6.5/3, units = "in",
    device = cairo_pdf
  )  
  
