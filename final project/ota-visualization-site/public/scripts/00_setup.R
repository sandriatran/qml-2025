# -----------------------------------------------------------------------------
# STEP 0: Setup, Libraries, and Aesthetics
# -----------------------------------------------------------------------------
library(tidyverse)
library(brms)
library(tidybayes)
library(bayesplot)
library(here)
library(colorspace)
library(patchwork)

# COLOR PALETTE
palette <- list(
    indigo      = "#6c65fc", # Primary accent / LR contrast
    hot_pink    = "#e657c7", # H (homophones)
    purple      = "#c674ff", # PB contrast
    lavender    = "#a7a0e6", # F baseline / secondary elements
    bg          = "#ffffff", # Plot background
    grid        = "#e7e0f0", # Grid lines
    light_blue  = "#d8e6ff", # Confidence bands
    black       = "#000000" # Text
)

# Custom ggplot theme
theme_ota <- function(base_size = 14) {
    theme_minimal(base_size = base_size) %+replace%
        theme(
            plot.background    = element_rect(fill = palette$bg, color = NA),
            panel.background   = element_rect(fill = palette$bg, color = NA),
            panel.grid.major   = element_line(color = palette$grid, linewidth = 0.3),
            panel.grid.minor   = element_blank(),
            plot.title         = element_text(color = palette$black, face = "bold"),
            axis.title         = element_text(color = palette$black),
            axis.text          = element_text(color = palette$black)
        )
}

contrast_fills <- c(
    "F (Spelling Control)" = palette$lavender,
    "LR (/l/-/r/)"         = palette$indigo,
    "H (Homophones)"       = palette$hot_pink,
    "PB (/p/-/b/)"         = palette$purple
)
