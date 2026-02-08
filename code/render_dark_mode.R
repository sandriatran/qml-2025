# ==============================================================================
# DARK MODE RE-RENDERER
# ==============================================================================
# Standalone script to re-render existing ggplot outputs in dark mode.
#
# USAGE:
#   1. Source this script from an R session that already has all plot objects
#      in the environment (e.g., after running the full analysis pipeline):
#
#        source("code/render_dark_mode.R")
#
#   2. Or, if you have saved plot objects as .rds files, point this script
#      at a directory of .rds plot files:
#
#        Rscript code/render_dark_mode.R "final project/outputs/plot_objects"
#
# OUTPUT:
#   Saves dark-mode PNGs (and GIFs if gganimate is available) to:
#     <input_dir>/dark_mode/
# ==============================================================================

library(ggplot2)

# -----------------------------------------------------------------------------
# DARK THEME (matches website [data-theme="dark"] in variables.css)
# -----------------------------------------------------------------------------
palette_dark <- list(
  # Accent colors (brightened for dark backgrounds)
  indigo      = "#8480ff",
  hot_pink    = "#f06dd8",
  purple      = "#d18aff",
  lavender    = "#b8b2f0",

  # Backgrounds
  bg          = "#0a0b14",
  bg_surface  = "#13141f",
  bg_elevated = "#1a1b2e",

  # Grid & borders
  grid        = "#1e1f35",
  border      = "rgba(255, 255, 255, 0.08)",

  # Text hierarchy
  text        = "#f2f2ff",
  text_secondary = "#b0b0c8",
  text_muted  = "#606078",
  text_caption = "#484860"
)

# Contrast fills for dark mode
contrast_fills_dark <- c(
  "F (Spelling Control)" = palette_dark$lavender,
  "LR (/l/-/r/)"         = palette_dark$indigo,
  "H (Homophones)"       = palette_dark$hot_pink,
  "PB (/p/-/b/)"         = palette_dark$purple
)

theme_ota_dark <- function(base_size = 14) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      # Backgrounds
      plot.background    = element_rect(fill = palette_dark$bg, color = NA),
      panel.background   = element_rect(fill = palette_dark$bg, color = NA),

      # Grid
      panel.grid.major   = element_line(color = palette_dark$grid, linewidth = 0.3),
      panel.grid.minor   = element_blank(),

      # Text
      plot.title         = element_text(color = palette_dark$text, face = "bold"),
      plot.subtitle      = element_text(color = palette_dark$text_secondary),
      plot.caption       = element_text(color = palette_dark$text_muted),
      axis.title         = element_text(color = palette_dark$text),
      axis.text          = element_text(color = palette_dark$text),

      # Legend
      legend.text        = element_text(color = palette_dark$text),
      legend.title       = element_text(color = palette_dark$text),
      legend.background  = element_rect(fill = palette_dark$bg, color = NA),
      legend.key         = element_rect(fill = palette_dark$bg, color = NA),

      # Strips (facets)
      strip.text         = element_text(color = palette_dark$text),
      strip.background   = element_rect(fill = palette_dark$bg_surface, color = NA)
    )
}

# -----------------------------------------------------------------------------
# CORE FUNCTION: Convert a ggplot to dark mode
# -----------------------------------------------------------------------------
#' Apply dark theme to a ggplot object
#'
#' Uses theme() merge (not %+replace%) so that per-plot overrides like
#' axis.text.x angle, face, and size are preserved. Only colors change.
#'
#' @param p A ggplot object
#' @return A ggplot with dark theme applied
to_dark <- function(p) {
  p + theme(
    plot.background    = element_rect(fill = palette_dark$bg, color = NA),
    panel.background   = element_rect(fill = palette_dark$bg, color = NA),
    panel.grid.major   = element_line(color = palette_dark$grid, linewidth = 0.3),
    panel.grid.minor   = element_blank(),
    plot.title         = element_text(color = palette_dark$text),
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

# -----------------------------------------------------------------------------
# BATCH RE-RENDER: Process .rds plot files from a directory
# -----------------------------------------------------------------------------
#' Re-render all .rds ggplot objects in a directory as dark-mode PNGs
#'
#' @param input_dir Directory containing .rds files of ggplot objects
#' @param width Plot width in inches (default 9)
#' @param height Plot height in inches (default 6)
#' @param dpi Resolution (default 300)
render_dark_from_rds <- function(input_dir, width = 9, height = 6, dpi = 300) {
  dark_dir <- file.path(input_dir, "dark_mode")
  if (!dir.exists(dark_dir)) dir.create(dark_dir, recursive = TRUE)

  rds_files <- list.files(input_dir, pattern = "\\.rds$", full.names = TRUE)

  if (length(rds_files) == 0) {
    cat("No .rds files found in:", input_dir, "\n")
    cat("To use this script, save your ggplot objects with:\n")
    cat('  saveRDS(my_plot, "outputs/plot_objects/my_plot.rds")\n')
    return(invisible(NULL))
  }

  cat(sprintf("Found %d .rds files in %s\n", length(rds_files), input_dir))

  for (f in rds_files) {
    name <- tools::file_path_sans_ext(basename(f))
    out_file <- file.path(dark_dir, paste0(name, ".png"))

    tryCatch({
      p <- readRDS(f)
      p_dark <- to_dark(p)
      ggsave(out_file, p_dark, width = width, height = height, dpi = dpi)
      cat(sprintf("  [OK] %s.png\n", name))
    }, error = function(e) {
      cat(sprintf("  [SKIP] %s — %s\n", name, e$message))
    })
  }

  cat(sprintf("\nDark mode plots saved to: %s\n", dark_dir))
}

# -----------------------------------------------------------------------------
# RENDER FROM ENVIRONMENT: Re-save all ggplot objects in the current session
# -----------------------------------------------------------------------------
#' Re-render all ggplot objects currently in the global environment
#'
#' @param output_dir Directory to save dark-mode PNGs
#' @param width Plot width in inches (default 9)
#' @param height Plot height in inches (default 6)
#' @param dpi Resolution (default 300)
render_dark_from_env <- function(output_dir, width = 9, height = 6, dpi = 300) {
  dark_dir <- file.path(output_dir, "dark_mode")
  if (!dir.exists(dark_dir)) dir.create(dark_dir, recursive = TRUE)

  # Find all ggplot objects in global env
  env_objects <- ls(envir = .GlobalEnv)
  plot_names <- c()

  for (name in env_objects) {
    obj <- get(name, envir = .GlobalEnv)
    if (inherits(obj, "ggplot") || inherits(obj, "gg")) {
      plot_names <- c(plot_names, name)
    }
  }

  if (length(plot_names) == 0) {
    cat("No ggplot objects found in the global environment.\n")
    return(invisible(NULL))
  }

  cat(sprintf("Found %d ggplot objects: %s\n",
              length(plot_names), paste(plot_names, collapse = ", ")))

  for (name in plot_names) {
    p <- get(name, envir = .GlobalEnv)
    out_file <- file.path(dark_dir, paste0(name, ".png"))

    tryCatch({
      p_dark <- to_dark(p)
      ggsave(out_file, p_dark, width = width, height = height, dpi = dpi)
      cat(sprintf("  [OK] %s.png\n", name))
    }, error = function(e) {
      cat(sprintf("  [SKIP] %s — %s\n", name, e$message))
    })
  }

  cat(sprintf("\nDark mode plots saved to: %s\n", dark_dir))
}

# -----------------------------------------------------------------------------
# AUTO-RUN: If called from the command line with a directory argument
# -----------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) {
  render_dark_from_rds(args[1])
} else {
  cat("render_dark_mode.R loaded.\n")
  cat("Available functions:\n")
  cat("  to_dark(p)                     — convert a single ggplot to dark mode\n")
  cat("  render_dark_from_env(out_dir)  — re-render all ggplots in global env\n")
  cat("  render_dark_from_rds(rds_dir)  — re-render all .rds ggplots in a dir\n")
  cat("  theme_ota_dark()               — dark theme for manual use\n")
  cat("  contrast_fills_dark            — dark-mode fill scale for contrasts\n")
  cat("\nQuick start after running your analysis:\n")
  cat('  source("code/render_dark_mode.R")\n')
  cat('  render_dark_from_env("final project/outputs")\n')
}
