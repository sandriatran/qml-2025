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

# COLOR PALETTE (Light mode — default)
palette <- list(
    indigo      = "#6c65fc", # Primary accent / LR contrast
    hot_pink    = "#e657c7", # H (homophones)
    purple      = "#c674ff", # PB contrast
    lavender    = "#a7a0e6", # F baseline / secondary elements
    bg          = "#ffffff", # Plot background
    grid        = "#e7e0f0", # Grid lines
    light_blue  = "#d8e6ff", # Confidence bands
    text        = "#000000", # Text
    text_secondary = "#555555",
    text_muted  = "#888888"
)

# COLOR PALETTE (Dark mode — matches website [data-theme="dark"])
palette_dark <- list(
    indigo      = "#8480ff", # Lightened for dark bg
    hot_pink    = "#f06dd8",
    purple      = "#d18aff",
    lavender    = "#b8b2f0",
    bg          = "#0a0b14", # Dark background
    bg_surface  = "#13141f", # Panel/strip background
    grid        = "#1e1f35", # Subtle grid
    light_blue  = "#1a2040", # Confidence bands (muted)
    text        = "#f2f2ff", # Primary text
    text_secondary = "#b0b0c8",
    text_muted  = "#606078"
)

# Custom ggplot theme (Light)
theme_ota <- function(base_size = 14) {
    theme_minimal(base_size = base_size) %+replace%
        theme(
            plot.background    = element_rect(fill = palette$bg, color = NA),
            panel.background   = element_rect(fill = palette$bg, color = NA),
            panel.grid.major   = element_line(color = palette$grid, linewidth = 0.3),
            panel.grid.minor   = element_blank(),
            plot.title         = element_text(color = palette$text, face = "bold"),
            plot.subtitle      = element_text(color = palette$text_secondary),
            plot.caption       = element_text(color = palette$text_muted),
            axis.title         = element_text(color = palette$text),
            axis.text          = element_text(color = palette$text),
            legend.text        = element_text(color = palette$text),
            legend.title       = element_text(color = palette$text),
            strip.text         = element_text(color = palette$text)
        )
}

# Custom ggplot theme (Dark)
theme_ota_dark <- function(base_size = 14) {
    theme_minimal(base_size = base_size) %+replace%
        theme(
            plot.background    = element_rect(fill = palette_dark$bg, color = NA),
            panel.background   = element_rect(fill = palette_dark$bg, color = NA),
            panel.grid.major   = element_line(color = palette_dark$grid, linewidth = 0.3),
            panel.grid.minor   = element_blank(),
            plot.title         = element_text(color = palette_dark$text, face = "bold"),
            plot.subtitle      = element_text(color = palette_dark$text_secondary),
            plot.caption       = element_text(color = palette_dark$text_muted),
            axis.title         = element_text(color = palette_dark$text),
            axis.text          = element_text(color = palette_dark$text),
            legend.text        = element_text(color = palette_dark$text),
            legend.title       = element_text(color = palette_dark$text),
            legend.background  = element_rect(fill = palette_dark$bg, color = NA),
            legend.key         = element_rect(fill = palette_dark$bg, color = NA),
            strip.text         = element_text(color = palette_dark$text),
            strip.background   = element_rect(fill = palette_dark$bg_surface, color = NA)
        )
}

# Contrast fills (Light)
contrast_fills <- c(
    "F (Spelling Control)" = palette$lavender,
    "LR (/l/-/r/)"         = palette$indigo,
    "H (Homophones)"       = palette$hot_pink,
    "PB (/p/-/b/)"         = palette$purple
)

# Contrast fills (Dark — brighter for dark backgrounds)
contrast_fills_dark <- c(
    "F (Spelling Control)" = palette_dark$lavender,
    "LR (/l/-/r/)"         = palette_dark$indigo,
    "H (Homophones)"       = palette_dark$hot_pink,
    "PB (/p/-/b/)"         = palette_dark$purple
)

# Helper: save a plot in both light and dark mode
# Usage: save_dual(p, "filename.png", width = 9, height = 6)
# - p must be a ggplot object (not bayesplot/pp_check)
# - saves light version to output_dir, dark version to output_dir/dark_mode/
save_dual <- function(p, filename, output_dir, width = 9, height = 6, dpi = 300) {
    # Light mode (as-is)
    ggsave(file.path(output_dir, filename), p, width = width, height = height, dpi = dpi)

    # Dark mode
    dark_dir <- file.path(output_dir, "dark_mode")
    if (!dir.exists(dark_dir)) dir.create(dark_dir, recursive = TRUE)

    p_dark <- p + theme_ota_dark()
    ggsave(file.path(dark_dir, filename), p_dark, width = width, height = height, dpi = dpi)

    cat(sprintf("Saved: %s (light + dark)\n", filename))
}
