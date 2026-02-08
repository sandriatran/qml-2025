# -----------------------------------------------------------------------------
# FINAL PROJECT: Bayesian Re-Analysis of Ota, Hartsuiker & Haywood (2009)
# Title: The KEY to the ROCK: Near-homophony in nonnative visual word recognition
# Authors: Violet Mansion and Sandria Tran
# Date: December 2nd 2025
# -----------------------------------------------------------------------------
# Group Project Title:
#   Project Proposal - Re-Analyzing Ota, Hartsuiker and Haywood (2009)
#   Translate Frequentist Model into Bayesian Model
# -----------------------------------------------------------------------------
# Model: Bayesian hierarchical logistic regression with random effects for subjects and items
# -----------------------------------------------------------------------------
# Model also called:
#       (a) Bayesian Multilevel
#       (b) Hierarchical
#       (c) Generalized Mixed-Effects Model (GLMM)
# -----------------------------------------------------------------------------
# Alignment with Ota et al. (2009) Findings
# -----------------------------------------------------------------------------
# This establishes a clear directional hypothesis:
#       Error Rate (LR) > Error Rate (Control).
#       Dependent Variables: Error Rates(false positives)
# -----------------------------------------------------------------------------
# Multiple theory-driven models
# Formulae:
#    (a) Comprehensive:
#       accuracy ~ contrast_type + (1|subject_id) + (1|item_id)

#    (b) Linguistic:
#        accuracy ~ phonological_status + (1\|subject_id) + (1\|item_id)

#    (c) Distinctness
#       accuracy ~ Quantify core mechanism phonological_distinctness_scaled + (1\|subject_id) + (1\|item_id)

# -----------------------------------------------------------------------------
# Model's Feature:
# Bayesian hierarchical logistic regression with
#     (a) random effects for subjects and items, model participant differences.
#     (b) Random intercepts for both subjects and items (captures individual differences)
# -----------------------------------------------------------------------------
# Bayesian approach with priors (better than frequentist for small-medium samples)
#   VALIDATION MODEL ASSUMPTION
#   Multiple models (comprehensive, linguistic, distinctness) to test different theories Posterior predictive checks
#    (validates model assumptions)
# -----------------------------------------------------------------------------
#   Bernoulli/logit for binary accuracy
#      -  (appropriate for yes/no data)
#    Sensitivity analysis (weak priors, different codings)
# -----------------------------------------------------------------------------
#  (a)  Sensitivity analyses (weak priors testing robustness)
#  (b) Posterior predictive checks (validates assumptions)
#  (c) Effect sizes by contrast (shows individual subject effects implicitly)

# -----------------------------------------------------------------------------
# STEP 0: Load Packages ----
# -----------------------------------------------------------------------------
library(tidyverse)
library(brms)
library(tidybayes)
library(bayesplot)
library(here)
library(colorspace)
library(patchwork)

# -----------------------------------------------------------------------------
# COLOR PALETTE - Custom aesthetic theme
# -----------------------------------------------------------------------------
palette <- list(
  indigo      = "#6c65fc", # Primary accent / LR contrast
  hot_pink    = "#e657c7", # H (homophones)
  purple      = "#c674ff", # PB contrast
  lavender    = "#a7a0e6", # F baseline / secondary elements
  bg          = "#ffffff", # Plot background (clean white)
  grid        = "#e7e0f0", # Grid lines (subtle lavender-gray)
  light_blue  = "#d8e6ff", # Confidence bands / smooth fills
  light_pink  = "#ffd6ff", # Accent fills
  pink        = "#f79cee", # Reference lines / highlights
  black       = "#000000" # Text / borders
)

# Custom ggplot theme using the palette
theme_ota <- function(base_size = 14) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      plot.background    = element_rect(fill = palette$bg, color = NA),
      panel.background   = element_rect(fill = palette$bg, color = NA),
      panel.grid.major   = element_line(color = palette$grid, linewidth = 0.3),
      panel.grid.minor   = element_blank(),
      plot.title         = element_text(color = palette$black, face = "bold", size = base_size + 2),
      plot.subtitle      = element_text(color = palette$purple, face = "italic", size = base_size - 1),
      plot.caption       = element_text(color = palette$lavender, size = base_size - 3),
      axis.title         = element_text(color = palette$black),
      axis.text          = element_text(color = palette$black),
      legend.background  = element_rect(fill = palette$bg, color = NA),
      legend.text        = element_text(color = palette$black),
      legend.title       = element_text(color = palette$black, face = "bold")
    )
}

# Shared color scales for contrast types
contrast_fills <- c(
  "F (Spelling Control)" = palette$lavender,
  "LR (/l/-/r/)"         = palette$indigo,
  "H (Homophones)"       = palette$hot_pink,
  "PB (/p/-/b/)"         = palette$purple
)

contrast_fills_short <- c(
  "F"  = palette$lavender,
  "LR" = palette$indigo,
  "H"  = palette$hot_pink,
  "PB" = palette$purple
)

cat("\n---PROJECT SETUP---\n")
cat("Working directory:", here(), "\n")
cat("Date:", Sys.Date(), "\n\n")

# -----------------------------------------------------------------------------
# STEP 1: Load and Inspect Data
# -----------------------------------------------------------------------------
cat("=== STEP 1: LOADING DATA ===\n")

data_path <- here("data", "ota2009", "key-rock.csv")
data_raw <- read_csv(data_path, show_col_types = FALSE)

cat("Total rows:", nrow(data_raw), "\n")
cat("Unique subjects:", n_distinct(data_raw$Subject), "\n")
cat("Contrast types:", paste(unique(data_raw$Contrast), collapse = ", "), "\n\n")

#-----------------------------------------------------------------------------
# STEP 2: Preprocess Data
#- ----------------------------------------------------------------------------
cat("=== STEP 2: DATA PREPROCESSING ===\n")

data_clean <- data_raw %>%
  filter(Procedure == "TrialProc") %>%
  filter(Condition == "Unrelated") %>%
  filter(Contrast %in% c("F", "LR", "H", "PB")) %>%
  mutate(
    subject_id = factor(Subject),
    item_id = factor(Item),
    accuracy = Words.ACC,
    contrast_type = factor(Contrast, levels = c("F", "LR", "H", "PB")),

    # # -----------------------------------------------------------------------------
    # LINGUISTIC PROPERTIES - Based on Ota et al. (2009) theory
    # # -----------------------------------------------------------------------------
    # Property 1: Is this a true homophone (identical pronunciation)?
    is_homophone = (Contrast == "H"),

    # Property 2: Is this a phonological contrast ABSENT in Japanese L1?
    # Japanese lacks /l/-/r/ contrast but has /p/-/b/ contrast
    contrast_absent_in_L1 = (Contrast == "LR"),

    # Property 3: Phonologically distinct scale (theoretical mechanism)
    # Lower values = more phonological indeterminacy in Japanese mental lexicon
    # This reflects the core mechanism: L1 phonology constrains L2 representations
    phonologically_distinct = case_when(
      Contrast == "F" ~ 1.0, # Spelling control: fully distinct (baseline)
      Contrast == "PB" ~ 0.8, # /p/-/b/ present in Japanese: fairly distinct
      Contrast == "LR" ~ 0.3, # /l/-/r/ absent in Japanese: indeterminate
      Contrast == "H" ~ 0.0 # True homophones: phonologically fused
    ),

    # Property 4: Theoretical factor (meaningful levels for interpretation)
    phonological_status = factor(
      case_when(
        Contrast == "F" ~ "Unrelated", # Reference: phonologically unrelated
        Contrast == "H" ~ "Homophone", # Universally confusable (all speakers)
        Contrast == "LR" ~ "L1_absent_contrast", # Representationally indeterminate
        Contrast == "PB" ~ "L1_present_contrast" # Phonologically separate
      ),
      levels = c(
        "Unrelated",
        "L1_present_contrast",
        "L1_absent_contrast",
        "Homophone"
      )
    )
  )

cat("\nFull contrast distribution in UNRELATED trials:\n")
contrast_summary_all <- data_clean %>%
  group_by(contrast_type) %>%
  summarise(
    n_trials = n(),
    n_subjects = n_distinct(subject_id),
    n_items = n_distinct(item_id),
    n_errors = sum(accuracy == 0),
    error_rate = mean(accuracy == 0),
    .groups = "drop"
  )
print(contrast_summary_all)

# Create output directory
output_dir <- here("final project", "outputs")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

#-----------------------------------------------------------------------------
# STEP 3: Prior Specification | Weak Priors | normal(0, 1.5)
# -----------------------------------------------------------------------------
cat("\n=== STEP 3: PRIOR SPECIFICATION ===\n")

priors_main <- c(
  prior(normal(0, 1.5), class = Intercept),
  prior(normal(0, 1.5), class = b),
  prior(exponential(1), class = sd)
)
# -----------------------------------------------------------------------------
# STEP 4: PRIMARY ANALYSIS - ALL CONTRASTS MODEL
# NOTE: SET.SEED(2025) | Reproducibilty
#-----------------------------------------------------------------------------
cat("---STEP 4: FITTING COMPREHENSIVE MODEL (ALL CONTRASTS) --- \n")

set.seed(2025) # Important for reproducibility

# Make F (Spelling Control) the reference level
data_all <- data_clean %>%
  mutate(contrast_type = factor(contrast_type, levels = c("F", "LR", "H", "PB")))

# Fit comprehensive model
model_all <- brm(
  formula = accuracy ~ contrast_type + (1 | subject_id) + (1 | item_id),
  data = data_all,
  family = bernoulli(link = "logit"),
  prior = priors_main,
  iter = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  seed = 2025,
  refresh = 0,
  file = file.path(output_dir, "model_comprehensive")
)

cat("✓ Comprehensive model fitted successfully\n\n")

# -----------------------------------------------------------------------------
# STEP 4b:  Phonological Status (Linguistics Model)
# -----------------------------------------------------------------------------
cat("--- STEP 4b: LINGUISTIC MODEL (Phonological Status Coding) --- \n")

# Use the theoretically ordered phonological_status factor
data_linguistic <- data_clean %>%
  mutate(phonological_status = factor(phonological_status,
    levels = c("Unrelated", "L1_present_contrast", "L1_absent_contrast", "Homophone")
  ))

model_linguistic <- brm(
  formula = accuracy ~ phonological_status + (1 | subject_id) + (1 | item_id),
  data = data_linguistic,
  family = bernoulli(link = "logit"),
  prior = priors_main,
  iter = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  seed = 2025,
  refresh = 0,
  file = file.path(output_dir, "model_linguistic")
)

cat("✓ Linguistic model fitted successfully\n\n")

# -----------------------------------------------------------------------------
# STEP 4c: CONTINUOUS DISTINCTNESS MODEL - Direct Test of Mechanism
# -----------------------------------------------------------------------------
cat("---STEP 4c: REPRESENTATIONAL INDETERMINACY MODEL---\n")
cat("QUESTION: ")
cat("Direct test: Does phonological distinctness predict accuracy?\n")
cat("Mechanism: L1 phonology constrains L2 representations\n\n")

# Scale the distinctness predictor for interpretability
data_distinctness <- data_clean %>%
  mutate(phonological_distinctness_scaled = scale(phonologically_distinct)[, 1])

model_distinctness <- brm(
  formula = accuracy ~ phonological_distinctness_scaled + (1 | subject_id) + (1 | item_id),
  data = data_distinctness,
  family = bernoulli(link = "logit"),
  prior = priors_main,
  iter = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  seed = 2025,
  refresh = 0,
  file = file.path(output_dir, "model_distinctness")
)

cat("✓ Representational indeterminacy model fitted successfully\n\n")

#-----------------------------------------------------------------------------
# STEP 5: Contrast Effects Analysis
#-----------------------------------------------------------------------------
cat("---STEP 5: CONTRAST EFFECTS (vs SPELLING CONTROL (F))---\n\n")

posterior_all <- as_draws_df(model_all)

# Extract effects for each contrast (relative to F/Spelling Control baseline)
lr_effect <- posterior_all$b_contrast_typeLR
h_effect <- posterior_all$b_contrast_typeH
pb_effect <- posterior_all$b_contrast_typePB
intercept_all <- posterior_all$b_Intercept

# Summary statistics for each effect
effects_summary <- tribble(
  ~contrast, ~median_effect, ~lower_95, ~upper_95, ~prob_positive,
  "LR (/l/-/r/)",
  median(lr_effect),
  quantile(lr_effect, 0.025),
  quantile(lr_effect, 0.975),
  mean(lr_effect > 0),
  "H (Homophones)",
  median(h_effect),
  quantile(h_effect, 0.025),
  quantile(h_effect, 0.975),
  mean(h_effect > 0),
  "PB (/p/-/b/)",
  median(pb_effect),
  quantile(pb_effect, 0.025),
  quantile(pb_effect, 0.975),
  mean(pb_effect > 0)
)

cat("Effects on log-odds of CORRECT response (relative to Spelling Control/Filler baseline):\n")
cat("(Negative values = MORE errors; Positive values = FEWER errors)\n\n")
print(effects_summary)

# -----------------------------------------------------------------------------
# STEP 6: Predicted Error Rates for All Contrasts
# -----------------------------------------------------------------------------
cat("---STEP 6: PREDICTED ERROR RATES BY CONTRAST ---\n\n")

# Convert to probability scale (Spelling Control/F is baseline)
f_prob_correct <- plogis(intercept_all)
lr_prob_correct <- plogis(intercept_all + lr_effect)
h_prob_correct <- plogis(intercept_all + h_effect)
pb_prob_correct <- plogis(intercept_all + pb_effect)

# Error rates
f_error <- 1 - f_prob_correct
lr_error <- 1 - lr_prob_correct
h_error <- 1 - h_prob_correct
pb_error <- 1 - pb_prob_correct

error_rates <- tribble(
  ~contrast, ~median_error_pct, ~lower_95_pct, ~upper_95_pct,
  "F (Spelling Control)",
  median(f_error) * 100,
  quantile(f_error, 0.025) * 100,
  quantile(f_error, 0.975) * 100,
  "LR (/l/-/r/)",
  median(lr_error) * 100,
  quantile(lr_error, 0.025) * 100,
  quantile(lr_error, 0.975) * 100,
  "H (Homophones)",
  median(h_error) * 100,
  quantile(h_error, 0.025) * 100,
  quantile(h_error, 0.975) * 100,
  "PB (/p/-/b/)",
  median(pb_error) * 100,
  quantile(pb_error, 0.025) * 100,
  quantile(pb_error, 0.975) * 100
)

cat("Predicted Error Rates by Contrast Type:\n")
print(error_rates)

# -----------------------------------------------------------------------------
# STEP 6b: LINGUISTIC MODEL ANALYSIS
# -----------------------------------------------------------------------------
cat("\n=== STEP 6b: LINGUISTIC MODEL INTERPRETATION ===\n")
cat("Model: accuracy ~ phonological_status (Unrelated < L1-present < L1-absent < Homophone)\n\n")

posterior_linguistic <- as_draws_df(model_linguistic)

# Extract effects (relative to Unrelated baseline)
l1_present_effect <- posterior_linguistic$b_phonological_statusL1_present_contrast
l1_absent_effect <- posterior_linguistic$b_phonological_statusL1_absent_contrast
homophone_effect <- posterior_linguistic$b_phonological_statusHomophone
intercept_linguistic <- posterior_linguistic$b_Intercept

cat("Effects on log-odds (relative to Unrelated baseline):\n\n")
cat("1. L1 PRESENT CONTRAST (/p/-/b/):\n")
cat("   Median effect: ", round(median(l1_present_effect), 3), "\n")
cat(
  "   95% CrI: [", round(quantile(l1_present_effect, 0.025), 3), ", ",
  round(quantile(l1_present_effect, 0.975), 3), "]\n"
)
cat("   Interpretation: Contrast present in Japanese L1\n")
cat("                   → Should show minimal interference\n\n")

cat("2. L1 ABSENT CONTRAST (/l/-/r/):\n")
cat("   Median effect: ", round(median(l1_absent_effect), 3), "\n")
cat(
  "   95% CrI: [", round(quantile(l1_absent_effect, 0.025), 3), ", ",
  round(quantile(l1_absent_effect, 0.975), 3), "]\n"
)
cat("   Interpretation: Contrast ABSENT in Japanese L1\n")
cat("                   → Should show HIGH interference\n")
cat("                   → REPRESENTATIONAL INDETERMINACY\n\n")

cat("3. HOMOPHONES (universally confusable):\n")
cat("   Median effect: ", round(median(homophone_effect), 3), "\n")
cat(
  "   95% CrI: [", round(quantile(homophone_effect, 0.025), 3), ", ",
  round(quantile(homophone_effect, 0.975), 3), "]\n"
)

# -----------------------------------------------------------------------------
# STEP 6c: REPRESENTATIONAL INDETERMINACY MODEL ANALYSIS
# -----------------------------------------------------------------------------
cat("=== STEP 6c: REPRESENTATIONAL INDETERMINACY MECHANISM ===\n")
cat("Model: accuracy ~ phonological_distinctness_scaled\n")
cat("Question:")
cat("Does L1-based distinctness scale predict errors?\n\n")

posterior_distinctness <- as_draws_df(model_distinctness)
distinctness_effect <- posterior_distinctness$b_phonological_distinctness_scaled

cat("Phonological Distinctness Effect:\n")
cat("  Coefficient: ", round(median(distinctness_effect), 3), "\n")
cat(
  "  95% CrI: [", round(quantile(distinctness_effect, 0.025), 3), ", ",
  round(quantile(distinctness_effect, 0.975), 3), "]\n\n"
)

cat("INTERPRETATION:\n")
cat("Each 1-SD increase in phonological distinctness\n")
cat("(on the L1-constrained scale) increases log-odds of correct response by\n")
cat(round(median(distinctness_effect), 3), "\n\n")

# -----------------------------------------------------------------------------
# MODEL USED IN QUARTO PDF
# STEP 7: Forest Plot Visualization
# **Figure 2** : Contrast Effects Relative to Spelling  Control Baseline
# -----------------------------------------------------------------------------
cat("\n=== STEP 7: CREATING FOREST PLOT ===\n")

# Prepare data for forest plot
forest_data <- effects_summary %>%
  mutate(
    contrast = factor(contrast, levels = c("PB (/p/-/b/)", "H (Homophones)", "LR (/l/-/r/)")),
    significant = abs(lower_95) > 0 | abs(upper_95) < 0
  ) %>%
  arrange(median_effect)

# Create forest plot
p_forest <- forest_data %>%
  ggplot(aes(x = median_effect, y = contrast)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = palette$pink, linewidth = 1.0) +
  geom_errorbar(aes(xmin = lower_95, xmax = upper_95, color = contrast),
    width = 0.15, linewidth = 0.9, orientation = "y"
  ) +
  geom_point(aes(fill = contrast), size = 4, shape = 21, stroke = 0.8, color = palette$black) +
  scale_fill_manual(
    values = c(
      "PB (/p/-/b/)" = "#7c3aed",
      "H (Homophones)" = palette$hot_pink,
      "LR (/l/-/r/)" = palette$indigo
    ),
    guide = "none"
  ) +
  scale_color_manual(
    values = c(
      "PB (/p/-/b/)" = "#7c3aed",
      "H (Homophones)" = palette$hot_pink,
      "LR (/l/-/r/)" = palette$indigo
    ),
    guide = "none"
  ) +
  labs(
    title = "Contrast Effects on Word Recognition Accuracy",
    subtitle = "Negative values = more errors relative to spelling controls (F baseline)",
    x = "Effect on log-odds of correct response",
    y = "Contrast Type",
    caption = "Each contrast compared to spelling controls (F) | /l/-/r/ and homophones show the largest effects"
  ) +
  theme_ota(base_size = 14)

print(p_forest)
ggsave(file.path(output_dir, "09_forest_plot_all_contrasts.png"),
  p_forest,
  width = 11, height = 6, dpi = 300
)
cat("✓ Saved: 09_forest_plot_all_contrasts.png\n")

# -----------------------------------------------------------------------------
#  MODEL USED IN QUARTO PDF
# STEP 8: Error Rate Comparison Plot
# Figure 1: False Positive Error Rates by Contrast Type
#-----------------------------------------------------------------------------
cat("\n---STEP 8: ERROR RATE COMPARISON PLOT---\n")

error_rate_summary <- tribble(
  ~contrast, ~median_error, ~lower_error, ~upper_error,
  "F (Spelling Control)", median(f_error), quantile(f_error, 0.025), quantile(f_error, 0.975),
  "LR (/l/-/r/)", median(lr_error), quantile(lr_error, 0.025), quantile(lr_error, 0.975),
  "H (Homophones)", median(h_error), quantile(h_error, 0.025), quantile(h_error, 0.975),
  "PB (/p/-/b/)", median(pb_error), quantile(pb_error, 0.025), quantile(pb_error, 0.975)
) %>%
  mutate(contrast = factor(contrast, levels = c("F (Spelling Control)", "PB (/p/-/b/)", "H (Homophones)", "LR (/l/-/r/)")))

p_error_rates <- error_rate_summary %>%
  ggplot(aes(x = reorder(contrast, median_error), y = median_error, fill = contrast)) +
  geom_col(alpha = 0.8, color = palette$black, linewidth = 0.5, width = 0.6) +
  geom_errorbar(aes(ymin = lower_error, ymax = upper_error),
    width = 0.15, linewidth = 0.7, color = palette$black
  ) +
  geom_text(
    aes(label = sprintf(
      "%.1f%%\n[%.1f, %.1f]",
      median_error * 100,
      lower_error * 100,
      upper_error * 100
    )),
    vjust = -0.5, size = 3.8, fontface = "bold", color = palette$black
  ) +
  scale_fill_manual(
    values = contrast_fills,
    guide = "none"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.35)) +
  labs(
    title = "Predicted Error Rates by Contrast Type",
    subtitle = "Model-estimated false positive rates with 95% credible intervals",
    x = "Contrast Type",
    y = "Error Rate",
    caption = "Japanese speakers most often confuse words differing by /l/-/r/ — a contrast absent from their native language"
  ) +
  theme_ota(base_size = 14) +
  theme(axis.text.x = element_text(angle = 15, hjust = 0.5))

print(p_error_rates)
ggsave(file.path(output_dir, "10_error_rates_all_contrasts.png"),
  p_error_rates,
  width = 10, height = 6, dpi = 300
)
cat("✓ Saved: 10_error_rates_all_contrasts.png\n")

# -----------------------------------------------------------------------------
# STEP 9: Complete Model Summary
# -----------------------------------------------------------------------------
cat("\n=== STEP 9: MODEL SUMMARY ===\n\n")
print(summary(model_all))

sink(file.path(output_dir, "model_comprehensive_summary.txt"))
cat("=== COMPREHENSIVE BAYESIAN MODEL SUMMARY ===\n")
cat("LR (/l/-/r/) , H (Homophones), and PB (/p/-/b/)  Contrasts vs Spelling Control Baseline (F)\n\n")
print(summary(model_all))
sink()

# -----------------------------------------------------------------------------
#  MODEL USED IN QUARTO PDF
# STEP 10: Posterior Predictive Check | Our Model Illustrates Excellent Fit
# **Figure 4:** Posterior Predictive Check: Model Validation
# -----------------------------------------------------------------------------
cat("\n=== STEP 10: MODEL VALIDATION ===\n")
cat("Posterior Predictive Check: Does the model generate realistic data?\n\n")

bayesplot::color_scheme_set(c(
  palette$light_blue, palette$lavender, palette$indigo,
  palette$purple, palette$hot_pink, palette$pink
))
pp_check_plot <- pp_check(model_all, ndraws = 100) +
  labs(
    title = "Posterior Predictive Check",
    subtitle = "Dark line = observed data | Light lines = 100 simulated datasets from the fitted model"
  ) +
  theme_ota(base_size = 12)

print(pp_check_plot)
ggsave(file.path(output_dir, "11_posterior_predictive_check.png"),
  pp_check_plot,
  width = 10, height = 6, dpi = 300
)
cat("✓ Saved: 11_posterior_predictive_check.png\n")

# -----------------------------------------------------------------------------
# STEP 11: Alternative Priors (Sensitive Analysis)
# -----------------------------------------------------------------------------
cat("\n---STEP 11: SENSITIVITY ANALYSIS---\n")
cat("QUESTION: ")
cat("Testing robustness: Do conclusions hold with weaker priors?\n\n")

# Fitting Model | Weaker (Wider) Priors
priors_weak <- c(
  prior(normal(0, 3.0), class = Intercept), # Wider prior
  prior(normal(0, 3.0), class = b), # Wider prior
  prior(exponential(1), class = sd)
)

cat("Fitting sensitivity model with weaker priors...\n")
model_sensitive <- brm(
  formula = accuracy ~ contrast_type + (1 | subject_id) + (1 | item_id),
  data = data_all,
  family = bernoulli(link = "logit"),
  prior = priors_weak,
  iter = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  seed = 2025,
  refresh = 0,
  file = file.path(output_dir, "model_sensitivity_weak")
)

cat("✓ Sensitivity model fitted\n\n")

# Extract posteriors for comparison
posterior_sensitive <- as_draws_df(model_sensitive)
lr_effect_sens <- posterior_sensitive$b_contrast_typeLR
h_effect_sens <- posterior_sensitive$b_contrast_typeH
pb_effect_sens <- posterior_sensitive$b_contrast_typePB

# Compare results
sensitivity_comparison <- tribble(
  ~Effect, ~Original_Priors_Median, ~Weak_Priors_Median, ~Difference,
  "LR effect", median(lr_effect), median(lr_effect_sens),
  median(lr_effect) - median(lr_effect_sens),
  "H effect", median(h_effect), median(h_effect_sens),
  median(h_effect) - median(h_effect_sens),
  "PB effect", median(pb_effect), median(pb_effect_sens),
  median(pb_effect) - median(pb_effect_sens)
)

cat("SENSITIVITY ANALYSIS: Effect Size Comparison\n")
cat("QUESTION: ")
cat("(Do results change with weaker priors?)\n\n")
print(sensitivity_comparison)

# -----------------------------------------------------------------------------
# STEP 12: LEXICON ITEM-LEVEL ANALYSIS
#       - Checking Robustness Across Items
# -----------------------------------------------------------------------------
cat("=== STEP 12: ITEM-LEVEL ROBUSTNESS ===\n")
cat("QUESTION:")
cat("Are effects driven by a few outlier items, or robust across items?\n\n")

item_summary <- data_all %>%
  group_by(item_id, contrast_type) %>%
  summarise(
    n_trials = n(),
    n_errors = sum(accuracy == 0),
    error_rate = mean(accuracy == 0),
    .groups = "drop"
  )

p_items <- item_summary %>%
  ggplot(aes(x = contrast_type, y = error_rate, fill = contrast_type)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, linewidth = 0.5, color = palette$black) +
  geom_jitter(aes(color = contrast_type), width = 0.15, alpha = 0.7, size = 2.5, shape = 16) +
  scale_color_manual(values = contrast_fills_short, guide = "none") +
  scale_fill_manual(
    values = contrast_fills_short,
    guide = "none"
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Item-Level Error Rates by Contrast",
    subtitle = "Each dot = one word pair | Boxplots summarize the distribution across items",
    x = "Contrast Type",
    y = "Error Rate per Item"
  ) +
  theme_ota(base_size = 12)

print(p_items)
ggsave(file.path(output_dir, "12_item_level_robustness.png"),
  p_items,
  width = 10, height = 6, dpi = 300
)
cat("✓ Saved: 12_item_level_robustness.png\n\n")

# -----------------------------------------------------------------------------
# STEP 12b: LINGUISTIC MODEL
# -----------------------------------------------------------------------------
cat("-- Creating Linguistic Model ---\n")

# Prepare linguistic model effects for visualization
linguistic_effects <- tribble(
  ~phonological_status, ~median_effect, ~lower_95, ~upper_95,
  "L1-present (/p/-/b/)", median(l1_present_effect),
  quantile(l1_present_effect, 0.025), quantile(l1_present_effect, 0.975),
  "L1-absent (/l/-/r/)", median(l1_absent_effect),
  quantile(l1_absent_effect, 0.025), quantile(l1_absent_effect, 0.975),
  "Homophone", median(homophone_effect),
  quantile(homophone_effect, 0.025), quantile(homophone_effect, 0.975)
)

p_linguistic <- linguistic_effects %>%
  mutate(
    phonological_status = factor(phonological_status,
      levels = c("L1-present (/p/-/b/)", "L1-absent (/l/-/r/)", "Homophone")
    ),
    significant = abs(lower_95) > 0 | abs(upper_95) < 0
  ) %>%
  ggplot(aes(x = median_effect, y = phonological_status)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = palette$pink, linewidth = 1.0) +
  geom_errorbar(aes(xmin = lower_95, xmax = upper_95, color = phonological_status),
    width = 0.15, linewidth = 0.9, orientation = "y"
  ) +
  geom_point(aes(fill = phonological_status), size = 4, shape = 21, stroke = 0.8, color = palette$black) +
  scale_fill_manual(
    values = c(
      "L1-present (/p/-/b/)" = "#7c3aed",
      "L1-absent (/l/-/r/)" = palette$indigo,
      "Homophone" = palette$hot_pink
    ),
    guide = "none"
  ) +
  scale_color_manual(
    values = c(
      "L1-present (/p/-/b/)" = "#7c3aed",
      "L1-absent (/l/-/r/)" = palette$indigo,
      "Homophone" = palette$hot_pink
    ),
    guide = "none"
  ) +
  labs(
    title = "Linguistic Model: Effects by Phonological Status",
    subtitle = "Native phonological status predicts error rates relative to unrelated baseline",
    x = "Effect on log-odds of correct response",
    y = "Phonological Status",
    caption = "L1-absent contrasts (/l/-/r/) cause the most errors | L1-present contrasts (/p/-/b/) cause the fewest"
  ) +
  theme_ota(base_size = 14)

print(p_linguistic)
ggsave(file.path(output_dir, "13_linguistic_model_effects.png"),
  p_linguistic,
  width = 11, height = 6, dpi = 300
)
cat("✓ Saved: 13_linguistic_model_effects.png\n\n")

# -----------------------------------------------------------------------------
# QUARTO PDF
# STEP 12c: REPRESENTATIONAL DISTINCTNESS CONTINUOUS SCALE
# Figure 3: Representational Indeterminacy: Continuous Mechanism
# -----------------------------------------------------------------------------
cat("=== Representational Distinctness Visualization ===\n")

# Create visualization of the distinctness model
# Per-subject aggregation: 20 subjects × 4 contrasts = 80 data points
# (not 4 collapsed means — this shows individual variation at each level)
data_plot <- data_clean %>%
  group_by(subject_id, phonologically_distinct, Contrast) %>%
  summarise(
    error_rate = mean(accuracy == 0),
    n_trials = n(),
    .groups = "drop"
  )

# Grand means for large annotated points
data_means <- data_plot %>%
  group_by(phonologically_distinct, Contrast) %>%
  summarise(
    error_rate = mean(error_rate),
    n_trials = sum(n_trials),
    .groups = "drop"
  )

p_distinctness <- data_plot %>%
  ggplot(aes(x = phonologically_distinct, y = error_rate)) +
  # Individual subject points (smaller, semi-transparent)
  geom_jitter(
    aes(fill = Contrast),
    shape = 21, alpha = 0.45,
    color = palette$black, stroke = 0.4, size = 3,
    width = 0.03, height = 0
  ) +
  # Grand mean points (large, bold)
  geom_point(
    data = data_means,
    aes(fill = Contrast), shape = 21, alpha = 1,
    color = palette$black, stroke = 1.2, size = 7
  ) +
  # Trend line through individual data
  geom_smooth(
    method = "lm", se = TRUE, color = palette$indigo,
    linewidth = 1, alpha = 0.15, fill = palette$light_blue
  ) +
  # Grand mean labels
  geom_text(
    data = data_means,
    aes(label = sprintf("%.0f%%", error_rate * 100)),
    vjust = -1.5, size = 4, fontface = "bold", color = palette$black
  ) +
  scale_fill_manual(
    values = contrast_fills_short,
    name = "Contrast Type"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(-0.05, 0.55)) +
  scale_x_continuous(
    breaks = c(0, 0.3, 0.8, 1.0),
    labels = c("Homophone\n(0.0)", "L1-absent\n(0.3)", "L1-present\n(0.8)", "Unrelated\n(1.0)")
  ) +
  labs(
    title = "L1 Sound Distinctness Predicts L2 Word Confusion",
    subtitle = "Small dots = individual subjects | Large dots = group mean | Line = linear trend",
    x = "Phonological Distinctness\n(determined by the speaker's native sound inventory)",
    y = "Error Rate (False Positive)",
    caption = "Downward trend: less distinct L1 categories → more L2 word confusion (Ota et al., 2009)"
  ) +
  theme_ota(base_size = 13) +
  theme(legend.position = "right")

print(p_distinctness)
ggsave(file.path(output_dir, "14_representational_distinctness_mechanism.png"),
  p_distinctness,
  width = 12, height = 6.5, dpi = 300
)
cat("✓ Saved: 14_representational_distinctness_mechanism.png\n\n")

# Summary statistics
item_stats <- item_summary %>%
  group_by(contrast_type) %>%
  summarise(
    median_error = median(error_rate),
    min_error = min(error_rate),
    max_error = max(error_rate),
    sd_error = sd(error_rate),
    .groups = "drop"
  )

cat("Item-Level Summary Statistics:\n")
print(item_stats)

cat("--- FINAL ANALYSIS --- \n")
cat("Our model successfully reproduces the original study's\n")
cat("findings for the Japanese participant group.\n\n")
cat("The study found that Japanese speakers, whose language lacks an /l/-/r/ distinction ")
cat("have high error rates when identifying English words with this contrast")

# =============================================================================
# STEP 13: ENHANCED VISUALIZATIONS
#   (a) ggdist  - Gradient-shaded posterior intervals
#   (b) plotly  - Interactive hover/zoom plots
#   (c) gganimate - Animated posterior reveal
# =============================================================================

# Install if needed (uncomment as necessary):
# install.packages(c("ggdist", "plotly", "gganimate", "gifski", "transformr"))

library(ggdist)
library(plotly)
library(gganimate)
library(gifski)

cat("\n=== STEP 13a: GRADIENT-SHADED POSTERIOR DISTRIBUTIONS (ggdist) ===\n")

# --- 13a: ggdist gradient forest plot ---
# Combine posterior draws into a long-format data frame for ggdist
posterior_long <- tibble(
  contrast = rep(c("LR (/l/-/r/)", "H (Homophones)", "PB (/p/-/b/)"), each = length(lr_effect)),
  effect = c(lr_effect, h_effect, pb_effect)
) %>%
  mutate(contrast = factor(contrast, levels = c("PB (/p/-/b/)", "H (Homophones)", "LR (/l/-/r/)")))

p_gradient_forest <- posterior_long %>%
  ggplot(aes(x = effect, y = contrast, fill = contrast)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = palette$pink, linewidth = 1.2) +
  stat_halfeye(
    aes(fill_ramp = after_stat(cut_cdf_qi(cdf))),
    .width = c(0.5, 0.8, 0.95),
    point_size = 4,
    interval_color = palette$black,
    point_color = palette$black,
    slab_color = palette$black,
    slab_linewidth = 0.5,
    slab_alpha = 0.85
  ) +
  scale_fill_ramp_discrete(
    range = c(1, 0.45),
    guide = "none"
  ) +
  scale_fill_manual(
    values = c(
      "PB (/p/-/b/)" = "#7c3aed",
      "H (Homophones)" = palette$hot_pink,
      "LR (/l/-/r/)" = palette$indigo
    ),
    guide = "none"
  ) +
  labs(
    title = "Gradient Posterior Distributions by Contrast",
    subtitle = "Darker shading = higher probability | Bars show 50%, 80%, and 95% credible intervals",
    x = "Effect on log-odds of correct response",
    y = "Contrast Type",
    caption = "/l/-/r/ shows the strongest negative effect — its posterior is entirely below zero"
  ) +
  theme_ota(base_size = 14)

print(p_gradient_forest)
ggsave(file.path(output_dir, "15_gradient_posterior_forest.png"),
  p_gradient_forest,
  width = 11, height = 6, dpi = 300
)
cat("Saved: 15_gradient_posterior_forest.png\n")

# --- 13a-ii: ggdist gradient halfeye per contrast with individual fills ---
p_gradient_halfeye <- posterior_long %>%
  ggplot(aes(x = effect, y = contrast)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = palette$pink, linewidth = 1.2) +
  stat_slab(
    aes(fill = contrast, fill_ramp = after_stat(cut_cdf_qi(cdf, .width = c(0.5, 0.8, 0.95, 1)))),
    color = palette$black,
    linewidth = 0.5,
    alpha = 0.85
  ) +
  stat_pointinterval(
    .width = c(0.66, 0.95),
    point_size = 3,
    interval_color = palette$black,
    point_color = palette$black
  ) +
  scale_fill_manual(
    values = c(
      "PB (/p/-/b/)" = "#7c3aed",
      "H (Homophones)" = palette$hot_pink,
      "LR (/l/-/r/)" = palette$indigo
    ),
    guide = "none"
  ) +
  scale_fill_ramp_discrete(range = c(1, 0.4), guide = "none") +
  labs(
    title = "Posterior Density with Credible Intervals",
    subtitle = "Full distribution of plausible effect sizes for each contrast",
    x = "Effect on log-odds of correct response",
    y = "Contrast Type",
    caption = "Black dot = posterior median | Inner bar = 66% interval | Outer bar = 95% interval"
  ) +
  theme_ota(base_size = 14)

print(p_gradient_halfeye)
ggsave(file.path(output_dir, "16_gradient_halfeye_posteriors.png"),
  p_gradient_halfeye,
  width = 11, height = 6, dpi = 300
)
cat("Saved: 16_gradient_halfeye_posteriors.png\n")

# =============================================================================
cat("\n=== STEP 13b: INTERACTIVE PLOTS (plotly) ===\n")
# =============================================================================

# Note: ggplot2 4.0.0+ changed internal guide representations which can cause
# "Unknown input: <function>" errors in plotly. We use guides() layer instead
# of guide = "none" inside scale_*, and wrap in tryCatch for robustness.

tryCatch(
  {
    # --- 13b-i: Interactive error rate plot ---
    p_error_interactive <- error_rate_summary %>%
      ggplot(aes(
        x = reorder(contrast, median_error),
        y = median_error,
        fill = contrast,
        text = sprintf(
          "Contrast: %s\nError Rate: %.1f%%\n95%% CrI: [%.1f%%, %.1f%%]",
          contrast, median_error * 100, lower_error * 100, upper_error * 100
        )
      )) +
      geom_col(alpha = 0.8, color = palette$black, linewidth = 0.8, width = 0.6) +
      geom_errorbar(aes(ymin = lower_error, ymax = upper_error),
        width = 0.2, linewidth = 1, color = palette$black
      ) +
      scale_fill_manual(values = contrast_fills) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 0.35)) +
      guides(fill = "none") +
      labs(
        title = "False Positive Error Rates by Contrast Type (Interactive)",
        x = "Contrast Type",
        y = "Error Rate"
      ) +
      theme_ota(base_size = 12) +
      theme(axis.text.x = element_text(angle = 15, hjust = 0.5))

    interactive_error <- ggplotly(p_error_interactive, tooltip = "text") %>%
      layout(
        paper_bgcolor = palette$bg,
        plot_bgcolor = palette$bg
      )

    htmlwidgets::saveWidget(interactive_error,
      file.path(output_dir, "17_interactive_error_rates.html"),
      selfcontained = TRUE
    )
    cat("Saved: 17_interactive_error_rates.html\n")

    # --- 13b-ii: Interactive forest plot with posterior info ---
    p_forest_interactive <- forest_data %>%
      ggplot(aes(
        x = median_effect,
        y = contrast,
        text = sprintf(
          "Contrast: %s\nMedian Effect: %.3f\n95%% CrI: [%.3f, %.3f]",
          contrast, median_effect, lower_95, upper_95
        )
      )) +
      geom_vline(xintercept = 0, linetype = "dashed", color = palette$pink, linewidth = 1) +
      geom_point(aes(color = significant), size = 4, shape = 21, stroke = 1.5, fill = palette$light_pink) +
      geom_errorbar(aes(xmin = lower_95, xmax = upper_95, color = significant),
        width = 0.15, linewidth = 1, orientation = "y"
      ) +
      scale_color_manual(
        values = c("TRUE" = palette$indigo, "FALSE" = palette$lavender)
      ) +
      guides(color = "none") +
      labs(
        title = "Contrast Effects (Interactive)",
        x = "Effect on log-odds of correct response",
        y = "Contrast Type"
      ) +
      theme_ota(base_size = 12)

    interactive_forest <- ggplotly(p_forest_interactive, tooltip = "text") %>%
      layout(
        paper_bgcolor = palette$bg,
        plot_bgcolor = palette$bg
      )

    htmlwidgets::saveWidget(interactive_forest,
      file.path(output_dir, "18_interactive_forest_plot.html"),
      selfcontained = TRUE
    )
    cat("Saved: 18_interactive_forest_plot.html\n")

    # --- 13b-iii: Interactive distinctness mechanism plot ---
    p_dist_interactive <- data_plot %>%
      ggplot(aes(
        x = phonologically_distinct,
        y = error_rate,
        fill = Contrast,
        text = sprintf(
          "Contrast: %s\nDistinctness: %.1f\nError Rate: %.1f%%\nN trials: %d",
          Contrast, phonologically_distinct, error_rate * 100, n_trials
        )
      )) +
      geom_point(aes(size = n_trials), shape = 21, alpha = 0.8, color = palette$black, stroke = 1.5) +
      geom_smooth(
        method = "loess", se = TRUE, color = palette$indigo,
        linewidth = 1.2, alpha = 0.2, fill = palette$light_blue
      ) +
      scale_fill_manual(values = contrast_fills_short, name = "Contrast Type") +
      scale_y_continuous(labels = scales::percent) +
      scale_size_continuous() +
      guides(size = "none") +
      labs(
        title = "Representational Indeterminacy (Interactive)",
        x = "Phonological Distinctness",
        y = "Error Rate"
      ) +
      theme_ota(base_size = 12)

    interactive_dist <- ggplotly(p_dist_interactive, tooltip = "text") %>%
      layout(
        paper_bgcolor = palette$bg,
        plot_bgcolor = palette$bg
      )

    htmlwidgets::saveWidget(interactive_dist,
      file.path(output_dir, "19_interactive_distinctness.html"),
      selfcontained = TRUE
    )
    cat("Saved: 19_interactive_distinctness.html\n")
  },
  error = function(e) {
    cat("\nWarning: Interactive plotly plots could not be generated.\n")
    cat("Error:", conditionMessage(e), "\n")
    cat("This is likely a ggplot2/plotly version compatibility issue.\n")
    cat("Try: install.packages('plotly') to update plotly.\n")
    cat("Continuing with remaining outputs...\n\n")
  }
)

# =============================================================================
cat("\n=== STEP 13c: ANIMATED PLOTS (gganimate) ===\n")
# =============================================================================

# --- 13c-i: Animated credible interval reveal ---
# Build frames that progressively reveal the credible interval widths
ci_frames <- bind_rows(
  effects_summary %>% mutate(
    frame = "1: Point Estimates",
    show_lower = median_effect, show_upper = median_effect
  ),
  effects_summary %>% mutate(
    frame = "2: 50% Credible Interval",
    show_lower = median_effect + (lower_95 - median_effect) * 0.5,
    show_upper = median_effect + (upper_95 - median_effect) * 0.5
  ),
  effects_summary %>% mutate(
    frame = "3: 80% Credible Interval",
    show_lower = median_effect + (lower_95 - median_effect) * 0.8,
    show_upper = median_effect + (upper_95 - median_effect) * 0.8
  ),
  effects_summary %>% mutate(
    frame = "4: 95% Credible Interval",
    show_lower = lower_95, show_upper = upper_95
  )
) %>%
  mutate(contrast = factor(contrast, levels = c("PB (/p/-/b/)", "H (Homophones)", "LR (/l/-/r/)")))

p_animated_ci <- ci_frames %>%
  ggplot(aes(x = median_effect, y = contrast)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = palette$pink, linewidth = 1.2) +
  geom_errorbar(aes(xmin = show_lower, xmax = show_upper, color = contrast),
    width = 0.2, linewidth = 1.5, orientation = "y"
  ) +
  geom_point(aes(fill = contrast), size = 5, shape = 21, stroke = 2, color = palette$black) +
  scale_color_manual(
    values = c(
      "PB (/p/-/b/)" = palette$purple,
      "H (Homophones)" = palette$hot_pink,
      "LR (/l/-/r/)" = palette$indigo
    ),
    guide = "none"
  ) +
  scale_fill_manual(
    values = c(
      "PB (/p/-/b/)" = palette$purple,
      "H (Homophones)" = palette$hot_pink,
      "LR (/l/-/r/)" = palette$indigo
    ),
    guide = "none"
  ) +
  labs(
    title = "Credible Interval Progression: {closest_state}",
    subtitle = "Intervals widen to capture 50%, 80%, then 95% of the posterior distribution",
    x = "Effect on log-odds of correct response",
    y = "Contrast Type",
    caption = "Relative to Spelling Control (F) | At 95%, /l/-/r/ interval remains entirely below zero"
  ) +
  theme_ota(base_size = 14) +
  transition_states(frame, transition_length = 2, state_length = 1.5) +
  ease_aes("cubic-in-out")

anim_ci <- animate(p_animated_ci,
  nframes = 80, fps = 10,
  width = 800, height = 500, renderer = gifski_renderer()
)
anim_save(file.path(output_dir, "20_credible_interval_buildup.gif"), animation = anim_ci)
cat("Saved: 20_credible_interval_buildup.gif\n")

# --- 13c-ii: Animated error rate bars growing (cumulative) ---
error_anim_data <- error_rate_summary %>%
  mutate(
    contrast_order = as.numeric(factor(contrast,
      levels = c("F (Spelling Control)", "PB (/p/-/b/)", "H (Homophones)", "LR (/l/-/r/)")
    )),
    label = sprintf("%.1f%%", median_error * 100)
  )

# Build cumulative frames so bars accumulate without shadow_mark
error_anim_cumulative <- map_dfr(seq_len(nrow(error_anim_data)), function(i) {
  error_anim_data %>%
    filter(contrast_order <= i) %>%
    mutate(frame = i)
})

p_animated_bars <- error_anim_cumulative %>%
  ggplot(aes(x = reorder(contrast, median_error), y = median_error)) +
  geom_col(aes(fill = contrast), alpha = 0.8, color = "#000000", linewidth = 1, width = 0.6) +
  geom_errorbar(aes(ymin = lower_error, ymax = upper_error),
    width = 0.2, linewidth = 1, color = "#000000"
  ) +
  geom_text(
    aes(x = reorder(contrast, median_error), y = median_error, label = label),
    inherit.aes = FALSE,
    vjust = -1.5, size = 5, fontface = "bold", color = "#000000"
  ) +
  scale_fill_manual(values = contrast_fills, guide = "none") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.38)) +
  labs(
    title = "Error Rates by Contrast Type (Cumulative)",
    subtitle = "Bars accumulate to reveal the pattern: /l/-/r/ produces the highest false positive rate",
    x = "Contrast Type",
    y = "Error Rate",
    caption = "/l/-/r/ (absent from Japanese) produces the highest error rate (~30%)"
  ) +
  theme_ota(base_size = 14) +
  theme(axis.text.x = element_text(angle = 15, hjust = 0.5)) +
  transition_manual(frame)

anim_bars <- animate(p_animated_bars,
  nframes = 40, fps = 4,
  width = 800, height = 500, renderer = gifski_renderer()
)
anim_save(file.path(output_dir, "21_error_rate_accumulation.gif"), animation = anim_bars)
cat("Saved: 21_error_rate_accumulation.gif\n")

# --- 13c-iii: Animated MCMC chain convergence (trace plot) ---
# Show chains converging for the LR effect parameter
mcmc_draws <- as_draws_df(model_all) %>%
  select(.chain, .iteration, b_contrast_typeLR) %>%
  rename(chain = .chain, iteration = .iteration, LR_effect = b_contrast_typeLR) %>%
  mutate(chain = factor(chain))

chain_colors <- c(
  "1" = palette$indigo, "2" = palette$hot_pink,
  "3" = palette$purple, "4" = palette$lavender
)

p_animated_chains <- mcmc_draws %>%
  ggplot(aes(x = iteration, y = LR_effect, color = chain)) +
  geom_line(alpha = 0.7, linewidth = 0.5) +
  geom_hline(
    yintercept = median(lr_effect), linetype = "dashed",
    color = palette$pink, linewidth = 1
  ) +
  scale_color_manual(values = chain_colors, name = "Chain") +
  labs(
    title = "MCMC Chain Convergence for /l/-/r/",
    subtitle = "Four independent chains arrive at the same estimate, confirming reliability",
    x = "Iteration (post-warmup)",
    y = "LR Effect (log-odds)",
    caption = "Well-mixed chains = the sampler has thoroughly explored the posterior distribution"
  ) +
  theme_ota(base_size = 13) +
  transition_reveal(iteration)

anim_chains <- animate(p_animated_chains,
  nframes = 120, fps = 15,
  width = 900, height = 500, renderer = gifski_renderer()
)
anim_save(file.path(output_dir, "22_mcmc_convergence_lr.gif"), animation = anim_chains)
cat("Saved: 22_mcmc_convergence_lr.gif\n")

# =============================================================================
# STEP 14: ADDITIONAL ELEVATED VISUALIZATIONS
# =============================================================================

cat("\n=== STEP 14a: PRIOR vs POSTERIOR COMPARISON ===\n")
cat("Showing how the data updated our beliefs\n\n")

# --- 14a: Prior vs Posterior comparison plot ---
# Generate draws from the prior (normal(0, 1.5))
set.seed(2025)
prior_draws <- rnorm(4000, mean = 0, sd = 1.5)

prior_vs_posterior <- bind_rows(
  tibble(distribution = "Prior: Normal(0, 1.5)", value = prior_draws),
  tibble(distribution = "Posterior: LR Effect", value = lr_effect),
  tibble(distribution = "Posterior: H Effect", value = h_effect),
  tibble(distribution = "Posterior: PB Effect", value = pb_effect)
) %>%
  mutate(distribution = factor(distribution,
    levels = c(
      "Prior: Normal(0, 1.5)", "Posterior: PB Effect",
      "Posterior: H Effect", "Posterior: LR Effect"
    )
  ))

p_prior_posterior <- prior_vs_posterior %>%
  ggplot(aes(x = value, fill = distribution, linetype = distribution)) +
  geom_density(alpha = 0.45, color = palette$black, linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = palette$pink, linewidth = 1) +
  scale_fill_manual(
    values = c(
      "Prior: Normal(0, 1.5)" = palette$light_blue,
      "Posterior: PB Effect" = "#7c3aed",
      "Posterior: H Effect" = palette$hot_pink,
      "Posterior: LR Effect" = palette$indigo
    ),
    name = "Distribution"
  ) +
  scale_linetype_manual(
    values = c(
      "Prior: Normal(0, 1.5)" = "dashed",
      "Posterior: PB Effect" = "solid",
      "Posterior: H Effect" = "solid",
      "Posterior: LR Effect" = "solid"
    ),
    guide = "none"
  ) +
  scale_x_continuous(limits = c(-6, 6)) +
  labs(
    title = "Prior vs. Posterior Distributions",
    subtitle = "Wide prior (before data) narrows into tight posteriors after observing 20 participants",
    x = "Effect Size (log-odds)",
    y = "Density",
    caption = "Prior = Normal(0, 1.5) centered on zero | Each posterior reflects what the data reveals about that contrast"
  ) +
  theme_ota(base_size = 14) +
  theme(legend.position = "bottom")

print(p_prior_posterior)
ggsave(file.path(output_dir, "23_prior_vs_posterior.png"),
  p_prior_posterior,
  width = 11, height = 6, dpi = 300
)
cat("Saved: 23_prior_vs_posterior.png\n")

# --- 14b: Subject-level random effects caterpillar plot ---
cat("\n=== STEP 14b: SUBJECT RANDOM EFFECTS (CATERPILLAR PLOT) ===\n")

subject_re <- ranef(model_all)$subject_id[, , "Intercept"]
subject_df <- tibble(
  subject = rownames(subject_re),
  estimate = subject_re[, "Estimate"],
  lower = subject_re[, "Q2.5"],
  upper = subject_re[, "Q97.5"]
) %>%
  mutate(subject = fct_reorder(subject, estimate))

p_caterpillar <- subject_df %>%
  ggplot(aes(x = estimate, y = subject)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = palette$pink, linewidth = 1) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
    width = 0.3, linewidth = 0.6, color = palette$indigo, alpha = 0.5, orientation = "y"
  ) +
  geom_point(size = 3, shape = 21, fill = palette$indigo, color = palette$black, stroke = 0.6) +
  labs(
    title = "Participant Random Effects",
    subtitle = "Each point = one subject's deviation from the group-level intercept",
    x = "Random Intercept (log-odds)",
    y = "Subject",
    caption = "Left of dashed line = worse than average accuracy | Wide bars = more estimation uncertainty for that participant"
  ) +
  theme_ota(base_size = 13)

print(p_caterpillar)
ggsave(file.path(output_dir, "24_subject_caterpillar.png"),
  p_caterpillar,
  width = 10, height = 7, dpi = 300
)
cat("Saved: 24_subject_caterpillar.png\n")

# --- 14c: Sensitivity analysis visual comparison ---
cat("\n=== STEP 14c: SENSITIVITY ANALYSIS VISUAL ===\n")

sensitivity_visual <- bind_rows(
  tibble(prior = "Original (SD=1.5)", contrast = "LR", effect = lr_effect),
  tibble(prior = "Original (SD=1.5)", contrast = "H", effect = h_effect),
  tibble(prior = "Original (SD=1.5)", contrast = "PB", effect = pb_effect),
  tibble(prior = "Weak (SD=3.0)", contrast = "LR", effect = lr_effect_sens),
  tibble(prior = "Weak (SD=3.0)", contrast = "H", effect = h_effect_sens),
  tibble(prior = "Weak (SD=3.0)", contrast = "PB", effect = pb_effect_sens)
) %>%
  mutate(
    contrast = factor(contrast, levels = c("PB", "H", "LR")),
    prior = factor(prior, levels = c("Original (SD=1.5)", "Weak (SD=3.0)"))
  )

p_sensitivity <- sensitivity_visual %>%
  ggplot(aes(x = effect, y = contrast, fill = prior)) +
  stat_halfeye(
    aes(fill_ramp = after_stat(cut_cdf_qi(cdf))),
    .width = c(0.5, 0.95),
    position = position_dodge(width = 0.6),
    point_size = 3,
    interval_color = palette$black,
    point_color = palette$black,
    slab_color = NA,
    slab_alpha = 0.7,
    height = 0.5
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = palette$pink, linewidth = 1) +
  scale_fill_manual(
    values = c("Original (SD=1.5)" = palette$indigo, "Weak (SD=3.0)" = palette$hot_pink),
    name = "Prior Specification"
  ) +
  scale_fill_ramp_discrete(range = c(1, 0.35), guide = "none") +
  labs(
    title = "Sensitivity Analysis: Standard vs. Weak Priors",
    subtitle = "Near-identical posteriors confirm results are data-driven, not prior-driven",
    x = "Effect on log-odds of correct response",
    y = "Contrast Type",
    caption = "Overlapping distributions = robust to prior specification | Both prior choices yield the same conclusions"
  ) +
  theme_ota(base_size = 14) +
  theme(legend.position = "bottom")

print(p_sensitivity)
ggsave(file.path(output_dir, "25_sensitivity_visual.png"),
  p_sensitivity,
  width = 11, height = 6, dpi = 300
)
cat("Saved: 25_sensitivity_visual.png\n")

# --- 14d: Raincloud plot (density + boxplot + raw data) ---
cat("\n=== STEP 14d: RAINCLOUD PLOT ===\n")

p_raincloud <- data_all %>%
  group_by(subject_id, contrast_type) %>%
  summarise(error_rate = mean(accuracy == 0), .groups = "drop") %>%
  ggplot(aes(x = contrast_type, y = error_rate, fill = contrast_type)) +
  stat_halfeye(
    adjust = 0.5,
    width = 0.5,
    justification = -0.2,
    .width = 0,
    point_colour = NA,
    slab_color = palette$black,
    slab_linewidth = 0.3,
    slab_alpha = 0.7
  ) +
  geom_boxplot(
    width = 0.12,
    outlier.shape = NA,
    alpha = 0.5,
    linewidth = 0.4,
    color = palette$black
  ) +
  geom_jitter(
    width = 0.05,
    alpha = 0.7,
    size = 2.5,
    shape = 21,
    color = palette$black,
    stroke = 0.4
  ) +
  scale_fill_manual(values = contrast_fills_short, guide = "none") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  labs(
    title = "Per-Subject Error Rates by Contrast (Raincloud)",
    subtitle = "Density + boxplot + individual data points for each contrast type",
    x = "Contrast Type",
    y = "Error Rate per Subject",
    caption = "Each dot = one participant's error rate | /l/-/r/ shows higher errors with greater individual variability"
  ) +
  theme_ota(base_size = 14)

print(p_raincloud)
ggsave(file.path(output_dir, "26_raincloud_subject_errors.png"),
  p_raincloud,
  width = 11, height = 7, dpi = 300
)
cat("Saved: 26_raincloud_subject_errors.png\n")

# --- 14e: LOO Model Comparison ---
cat("\n=== STEP 14e: MODEL COMPARISON (LOO-CV) ===\n")
cat("Comparing predictive performance of all three models\n\n")

loo_comprehensive <- loo(model_all)
loo_linguistic <- loo(model_linguistic)
loo_distinctness <- loo(model_distinctness)

loo_comparison <- loo_compare(loo_comprehensive, loo_linguistic, loo_distinctness)
cat("LOO-CV Model Comparison:\n")
print(loo_comparison)

# Save comparison to file
sink(file.path(output_dir, "model_comparison_loo.txt"))
cat("=== LOO-CV MODEL COMPARISON ===\n\n")
print(loo_comparison)
cat("\n\nComprehensive model LOO:\n")
print(loo_comprehensive)
cat("\nLinguistic model LOO:\n")
print(loo_linguistic)
cat("\nDistinctness model LOO:\n")
print(loo_distinctness)
sink()
cat("Saved: model_comparison_loo.txt\n")

# =============================================================================
# STEP 14f: DEEP ANALYTICAL VISUALIZATIONS
#   Two plots that add dimensions not captured above:
#   (1) Subject × Contrast heatmap   — individual differences
#   (2) Pairwise contrast posteriors  — full inferential hierarchy
# =============================================================================

cat("\n=== STEP 14f: DEEP ANALYTICAL VISUALIZATIONS ===\n")

# --- 14f-1: Subject × Contrast Heatmap ---
# Who struggles with what? Reveals whether /l/-/r/ difficulty is universal
# or driven by a subset of the 20 Japanese speakers.

cat("  [1/2] Subject × Contrast Heatmap ...\n")

subject_contrast <- data_all %>%
  group_by(Subject, contrast_type) %>%
  summarise(
    error_rate = mean(accuracy == 0),
    n_trials = n(),
    .groups = "drop"
  )

# Sort subjects by overall error rate (worst at top)
subject_order_hm <- data_all %>%
  group_by(Subject) %>%
  summarise(overall_error = mean(accuracy == 0), .groups = "drop") %>%
  arrange(desc(overall_error))

subject_contrast <- subject_contrast %>%
  mutate(
    Subject = factor(Subject, levels = subject_order_hm$Subject),
    contrast_type = factor(contrast_type, levels = c("LR", "H", "PB", "F")),
    error_pct = round(error_rate * 100)
  )

p_subj_heatmap <- subject_contrast %>%
  ggplot(aes(x = contrast_type, y = Subject, fill = error_rate)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(
    aes(
      label = paste0(error_pct, "%"),
      color = ifelse(error_rate > 0.35, "light", "dark")
    ),
    size = 3.2, fontface = "bold", show.legend = FALSE
  ) +
  scale_color_manual(values = c(light = "white", dark = "gray20")) +
  scale_fill_gradientn(
    colors = c("#f0f0f8", palette$lavender, palette$purple, palette$hot_pink, "#8b0000"),
    values = scales::rescale(c(0, 0.1, 0.25, 0.45, 0.80)),
    limits = c(0, 0.80),
    name = "Error Rate",
    labels = scales::percent,
    oob = scales::squish
  ) +
  labs(
    title = "Individual Variation: Who Struggles With What?",
    subtitle = "Subjects sorted by overall error rate (worst at top) | Each cell = one subject's error rate for that contrast",
    x = "Contrast Type",
    y = "Subject",
    caption = "Data: Ota et al. (2009) | 20 Japanese\u2013English bilinguals"
  ) +
  theme_ota(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(face = "bold", size = 11),
    axis.text.y = element_text(size = 9),
    legend.position = "right",
    legend.key.height = unit(2.5, "cm"),
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave(file.path(output_dir, "38_subject_contrast_heatmap.png"),
  p_subj_heatmap,
  width = 9, height = 10, dpi = 300
)
cat("  Saved: 38_subject_contrast_heatmap.png\n")


# --- 14f-2: Pairwise Contrast Posteriors with ROPE ---
# The forest plot (09) compares each contrast to F baseline.
# But the key question is: Is LR worse than H? Is H worse than PB?
# This shows all 6 pairwise difference posteriors with a ROPE.

cat("  [2/2] Pairwise Contrast Posteriors with ROPE ...\n")

# Compute all 6 pairwise differences on log-odds scale
pairwise_diffs <- tibble(
  comparison = c(
    "LR vs F", "H vs F", "PB vs F",
    "LR vs H", "LR vs PB", "H vs PB"
  ),
  diff = list(
    lr_effect, # LR - F (already relative to F)
    h_effect, # H - F
    pb_effect, # PB - F
    lr_effect - h_effect, # LR - H
    lr_effect - pb_effect, # LR - PB
    h_effect - pb_effect # H - PB
  )
) %>%
  unnest(diff) %>%
  mutate(comparison = factor(comparison,
    levels = c("H vs PB", "LR vs PB", "LR vs H", "PB vs F", "H vs F", "LR vs F")
  ))

# ROPE: log-odds difference of [-0.18, 0.18] ≈ negligible (OR ~ 0.84-1.20)
rope_lo <- -0.18
rope_hi <- 0.18

# Compute summary statistics for annotations
pairwise_summary <- pairwise_diffs %>%
  group_by(comparison) %>%
  summarise(
    median_diff = median(diff),
    lower_95 = quantile(diff, 0.025),
    upper_95 = quantile(diff, 0.975),
    prob_below_rope = mean(diff < rope_lo),
    prob_in_rope = mean(diff >= rope_lo & diff <= rope_hi),
    prob_above_rope = mean(diff > rope_hi),
    .groups = "drop"
  ) %>%
  mutate(
    label = sprintf(
      "Md = %.2f [%.2f, %.2f]\nP(< ROPE) = %.0f%%",
      median_diff, lower_95, upper_95, prob_below_rope * 100
    )
  )

p_pairwise <- pairwise_diffs %>%
  ggplot(aes(x = diff, y = comparison)) +
  # ROPE shaded region
  annotate("rect",
    xmin = rope_lo, xmax = rope_hi,
    ymin = -Inf, ymax = Inf,
    fill = palette$light_blue, alpha = 0.4
  ) +
  annotate("text",
    x = 0, y = Inf, label = "ROPE",
    size = 3, color = palette$indigo, fontface = "italic",
    vjust = -0.5
  ) +
  # Zero reference
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.4) +
  # Posterior densities
  stat_halfeye(
    aes(fill = after_stat(x < rope_lo)),
    .width = c(0.50, 0.95),
    point_interval = "median_qi",
    slab_alpha = 0.7,
    interval_color = "gray30",
    point_color = "gray10",
    point_size = 2.5,
    interval_linewidth = 1.5
  ) +
  scale_fill_manual(
    values = c(`TRUE` = palette$indigo, `FALSE` = palette$lavender),
    guide = "none"
  ) +
  # Annotations
  geom_text(
    data = pairwise_summary,
    aes(x = lower_95 - 0.3, y = comparison, label = label),
    size = 2.6, color = "gray30", hjust = 1, lineheight = 0.9
  ) +
  scale_x_continuous(breaks = seq(-4, 2, 0.5)) +
  labs(
    title = "Pairwise Contrast Differences: Full Inferential Hierarchy",
    subtitle = "Posterior distributions of log-odds differences | Blue band = Region of Practical Equivalence [\u00b10.18]",
    x = "Difference in Log-Odds (negative = first contrast has more errors)",
    y = NULL,
    caption = "Data: Ota et al. (2009) | Comprehensive model posteriors | ROPE corresponds to OR \u2248 0.84\u20131.20"
  ) +
  theme_ota(base_size = 12) +
  theme(
    axis.text.y = element_text(face = "bold", size = 11),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(10, 60, 10, 10)
  )

ggsave(file.path(output_dir, "39_pairwise_contrast_rope.png"),
  p_pairwise,
  width = 14, height = 8, dpi = 300
)
cat("  Saved: 39_pairwise_contrast_rope.png\n")


cat("=== STEP 14f COMPLETE: 2 deep analytical plots saved ===\n")

# =============================================================================
# STEP 15: ANIMATION STYLES SHOWCASE
#   Nine distinct gganimate transition / entrance effects
# =============================================================================

cat("\n=== STEP 15: ANIMATION STYLES SHOWCASE ===\n")

# --- 15-1: FADE IN ---
# Each contrast's posterior density fades in sequentially
cat("  [1/9] Fade In ...\n")

fade_data <- bind_rows(
  tibble(contrast = "PB (/p/-/b/)", effect = pb_effect, frame = 1),
  tibble(contrast = "H (Homophones)", effect = h_effect, frame = 2),
  tibble(contrast = "LR (/l/-/r/)", effect = lr_effect, frame = 3)
) %>%
  mutate(contrast = factor(contrast,
    levels = c("PB (/p/-/b/)", "H (Homophones)", "LR (/l/-/r/)")
  ))

p_fade <- fade_data %>%
  ggplot(aes(x = effect, fill = contrast)) +
  geom_density(alpha = 0.6, color = palette$black, linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = palette$pink, linewidth = 1) +
  scale_fill_manual(values = c(
    "PB (/p/-/b/)" = palette$purple,
    "H (Homophones)" = palette$hot_pink,
    "LR (/l/-/r/)" = palette$indigo
  ), name = "Contrast") +
  labs(
    title = "Posterior Densities Layered by Contrast",
    subtitle = "PB (small) → Homophones (moderate) → /l/-/r/ (largest effect)",
    x = "Effect on log-odds of correct response", y = "Density",
    caption = "Contrasts absent from Japanese (LR) shift furthest from zero"
  ) +
  theme_ota(base_size = 14) +
  theme(legend.position = "bottom") +
  transition_states(frame, transition_length = 2, state_length = 1) +
  enter_fade() +
  shadow_mark(alpha = 0.4)

anim_fade <- animate(p_fade,
  nframes = 60, fps = 10,
  width = 800, height = 500, renderer = gifski_renderer()
)
anim_save(file.path(output_dir, "27_posterior_densities_by_contrast.gif"), animation = anim_fade)
cat("  Saved: 27_posterior_densities_by_contrast.gif\n")

# --- 15-2: SLIDE UP ---
# Error rate bars fly up from below the frame
cat("  [2/9] Slide Up ...\n")

slide_data <- error_rate_summary %>%
  mutate(frame = as.numeric(factor(contrast,
    levels = c("F (Spelling Control)", "PB (/p/-/b/)", "H (Homophones)", "LR (/l/-/r/)")
  )))

p_slide <- slide_data %>%
  ggplot(aes(x = reorder(contrast, median_error), y = median_error)) +
  geom_col(aes(fill = contrast),
    alpha = 0.85, color = "#000000",
    linewidth = 0.8, width = 0.6
  ) +
  geom_errorbar(aes(ymin = lower_error, ymax = upper_error),
    width = 0.2, linewidth = 1, color = "#000000"
  ) +
  scale_fill_manual(values = contrast_fills, guide = "none") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.38)) +
  labs(
    title = "L1 Sound Inventory Predicts L2 Error Rates",
    subtitle = "Contrasts absent from Japanese produce the most word recognition errors",
    x = "Contrast Type", y = "False Positive Error Rate",
    caption = "Data: Ota et al. (2009) | 40 Japanese speakers judging English word pairs"
  ) +
  theme_ota(base_size = 14) +
  theme(axis.text.x = element_text(angle = 15, hjust = 0.5)) +
  transition_states(frame, transition_length = 1, state_length = 1.5) +
  enter_fly(y_loc = -0.4) +
  enter_fade() +
  shadow_mark(alpha = 0.7, fill = NA, color = NA, past = TRUE)

anim_slide <- animate(p_slide,
  nframes = 60, fps = 8,
  width = 800, height = 500, renderer = gifski_renderer()
)
anim_save(file.path(output_dir, "28_error_rates_by_l1_status.gif"), animation = anim_slide)
cat("  Saved: 28_error_rates_by_l1_status.gif\n")

# --- 15-3: HORIZONTAL CLIP ---
# Forest plot intervals extend horizontally from centre using transition_reveal
cat("  [3/9] Horizontal Clip ...\n")

hclip_data <- effects_summary %>%
  mutate(contrast = factor(contrast,
    levels = c("PB (/p/-/b/)", "H (Homophones)", "LR (/l/-/r/)")
  )) %>%
  crossing(reveal_step = seq(0, 1, length.out = 20)) %>%
  mutate(
    show_lower = median_effect + (lower_95 - median_effect) * reveal_step,
    show_upper = median_effect + (upper_95 - median_effect) * reveal_step
  )

p_hclip <- hclip_data %>%
  ggplot(aes(x = median_effect, y = contrast)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = palette$pink, linewidth = 1.2) +
  geom_errorbar(aes(xmin = show_lower, xmax = show_upper, color = contrast),
    width = 0.25, linewidth = 1.8, orientation = "y"
  ) +
  geom_point(aes(fill = contrast), size = 6, shape = 21, stroke = 2, color = "#000000") +
  scale_color_manual(values = c(
    "PB (/p/-/b/)" = palette$purple,
    "H (Homophones)" = palette$hot_pink,
    "LR (/l/-/r/)" = palette$indigo
  ), guide = "none") +
  scale_fill_manual(values = c(
    "PB (/p/-/b/)" = palette$purple,
    "H (Homophones)" = palette$hot_pink,
    "LR (/l/-/r/)" = palette$indigo
  ), guide = "none") +
  labs(
    title = "95% Credible Intervals by Contrast",
    subtitle = "/l/-/r/ and homophones exclude zero, confirming genuine effects",
    x = "Effect on log-odds of correct response", y = "Contrast Type",
    caption = "Intervals excluding zero = strong evidence that the contrast genuinely affects word recognition"
  ) +
  theme_ota(base_size = 14) +
  transition_manual(reveal_step)

anim_hclip <- animate(p_hclip,
  nframes = 40, fps = 8,
  width = 800, height = 500, renderer = gifski_renderer()
)
anim_save(file.path(output_dir, "29_contrast_effect_intervals.gif"), animation = anim_hclip)
cat("  Saved: 29_contrast_effect_intervals.gif\n")

# --- 15-4: VERTICAL CLIP ---
# Bars grow from zero height to full height
cat("  [4/9] Vertical Clip ...\n")

vclip_data <- error_rate_summary %>%
  crossing(grow = seq(0.05, 1, length.out = 20)) %>%
  mutate(
    bar_height  = median_error * grow,
    err_lower   = lower_error * grow,
    err_upper   = upper_error * grow,
    label       = ifelse(grow == 1, sprintf("%.1f%%", median_error * 100), "")
  )

p_vclip <- vclip_data %>%
  ggplot(aes(x = reorder(contrast, median_error), y = bar_height)) +
  geom_col(aes(fill = contrast),
    alpha = 0.85, color = "#000000",
    linewidth = 0.8, width = 0.6
  ) +
  geom_errorbar(aes(ymin = err_lower, ymax = err_upper),
    width = 0.2, linewidth = 1, color = "#000000"
  ) +
  scale_fill_manual(values = contrast_fills, guide = "none") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.38)) +
  labs(
    title = "Error Rates Grow with L1-L2 Phonological Distance",
    subtitle = "/l/-/r/ (~30%) vs. spelling controls (~5%): a six-fold increase in word confusion",
    x = "Contrast Type", y = "False Positive Error Rate",
    caption = "Higher bars = greater misidentification of English near-homophones by Japanese speakers"
  ) +
  theme_ota(base_size = 14) +
  theme(axis.text.x = element_text(angle = 15, hjust = 0.5)) +
  transition_manual(grow)

anim_vclip <- animate(p_vclip,
  nframes = 40, fps = 8,
  width = 800, height = 500, renderer = gifski_renderer()
)
anim_save(file.path(output_dir, "30_error_growth_by_contrast.gif"), animation = anim_vclip)
cat("  Saved: 30_error_growth_by_contrast.gif\n")

# --- 15-5: TILT UP ---
# Camera view pans upward over the distinctness scatter plot
cat("  [5/9] Tilt Up ...\n")

p_tilt_up <- data_plot %>%
  ggplot(aes(x = phonologically_distinct, y = error_rate)) +
  geom_point(aes(fill = Contrast, size = n_trials),
    shape = 21, alpha = 0.8,
    color = palette$black, stroke = 1.5
  ) +
  geom_smooth(
    method = "loess", se = TRUE, color = palette$indigo,
    linewidth = 1.2, alpha = 0.2, fill = palette$light_blue
  ) +
  scale_fill_manual(values = contrast_fills_short, name = "Contrast") +
  scale_size_continuous(guide = "none") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Phonological Distinctness Predicts Error Rates",
    subtitle = "Lower L1 distinctness → higher false positive rates in L2 English",
    x = "Phonological Distinctness\n(0 = identical in L1, 1 = fully distinct)",
    y = "False Positive Error Rate",
    caption = "The continuous scale captures the gradient nature of phonological transfer (Ota et al., 2009)"
  ) +
  theme_ota(base_size = 14) +
  view_zoom_manual(
    xmin = -0.1, xmax = 1.1,
    ymin = seq(-0.15, -0.05, length.out = 20),
    ymax = seq(0.25, 0.45, length.out = 20),
    pause_length = 4, wrap = FALSE
  )

anim_tilt_up <- animate(p_tilt_up,
  nframes = 40, fps = 6,
  width = 800, height = 500, renderer = gifski_renderer()
)
anim_save(file.path(output_dir, "31_distinctness_predicts_errors.gif"), animation = anim_tilt_up)
cat("  Saved: 31_distinctness_predicts_errors.gif\n")

# --- 15-6: TILT DOWN ---
# Camera view pans downward over the posterior densities
cat("  [6/9] Tilt Down ...\n")

p_tilt_down <- posterior_long %>%
  ggplot(aes(x = effect, fill = contrast)) +
  geom_density(alpha = 0.55, color = palette$black, linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = palette$pink, linewidth = 1) +
  scale_fill_manual(values = c(
    "PB (/p/-/b/)" = palette$purple,
    "H (Homophones)" = palette$hot_pink,
    "LR (/l/-/r/)" = palette$indigo
  ), name = "Contrast") +
  labs(
    title = "Posterior Density Comparison Across Contrasts",
    subtitle = "/l/-/r/ shifts furthest from zero — strongest evidence of L1 interference",
    x = "Effect on log-odds of correct response", y = "Density",
    caption = "/l/-/r/ is most negative — consistent with its complete absence from the Japanese sound system"
  ) +
  theme_ota(base_size = 14) +
  theme(legend.position = "bottom") +
  view_zoom_manual(
    xmin = -4, xmax = 2,
    ymin = seq(0.6, -0.05, length.out = 20),
    ymax = seq(1.8, 1.15, length.out = 20),
    pause_length = 4, wrap = FALSE
  )

anim_tilt_down <- animate(p_tilt_down,
  nframes = 40, fps = 6,
  width = 800, height = 500, renderer = gifski_renderer()
)
anim_save(file.path(output_dir, "32_posterior_interference_strength.gif"), animation = anim_tilt_down)
cat("  Saved: 32_posterior_interference_strength.gif\n")

# --- 15-7: FOCUS IN ---
# Zoom from wide view into the LR effect region
cat("  [7/9] Focus In ...\n")

p_focus <- posterior_long %>%
  filter(contrast == "LR (/l/-/r/)") %>%
  ggplot(aes(x = effect)) +
  geom_density(fill = palette$indigo, alpha = 0.6, color = palette$black, linewidth = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", color = palette$pink, linewidth = 1) +
  geom_vline(xintercept = median(lr_effect), color = palette$hot_pink, linewidth = 1.2) +
  labs(
    title = "The /l/-/r/ Posterior Distribution (Zoomed)",
    subtitle = "Entire posterior lies below zero — /l/-/r/ reliably impairs word recognition",
    x = "/l/-/r/ Effect on log-odds of correct response", y = "Density",
    caption = "Pink line = posterior median | Dashed line = null effect (zero) | Posterior entirely below zero"
  ) +
  theme_ota(base_size = 14) +
  view_zoom_manual(
    xmin = seq(-6, median(lr_effect) - 0.6, length.out = 25),
    xmax = seq(4, median(lr_effect) + 0.6, length.out = 25),
    ymin = 0,
    ymax = seq(0.5, 2.5, length.out = 25),
    pause_length = 5, wrap = FALSE
  )

anim_focus <- animate(p_focus,
  nframes = 50, fps = 8,
  width = 800, height = 500, renderer = gifski_renderer()
)
anim_save(file.path(output_dir, "33_lr_indeterminacy_zoom.gif"), animation = anim_focus)
cat("  Saved: 33_lr_indeterminacy_zoom.gif\n")

# --- 15-8: COLLIDE ---
# Prior and posterior densities slide in from opposite sides and meet
cat("  [8/9] Collide ...\n")

collide_data <- bind_rows(
  tibble(
    source = "Prior", value = prior_draws,
    offset = seq(4, 0, length.out = length(prior_draws))
  ),
  tibble(
    source = "Posterior", value = lr_effect,
    offset = seq(-4, 0, length.out = length(lr_effect))
  )
) %>%
  mutate(source = factor(source, levels = c("Prior", "Posterior")))

n_collide_frames <- 20
collide_frames <- map_dfr(seq_len(n_collide_frames), function(i) {
  frac <- (i - 1) / (n_collide_frames - 1)
  collide_data %>%
    mutate(
      shifted = value + offset * (1 - frac),
      frame = i
    )
})

p_collide <- collide_frames %>%
  ggplot(aes(x = shifted, fill = source)) +
  geom_density(alpha = 0.55, color = palette$black, linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = palette$pink, linewidth = 1) +
  scale_fill_manual(
    values = c("Prior" = palette$light_pink, "Posterior" = palette$indigo),
    name = "Distribution"
  ) +
  scale_x_continuous(limits = c(-8, 8)) +
  labs(
    title = "Prior vs. Posterior for the /l/-/r/ Contrast",
    subtitle = "Experimental data transforms broad prior uncertainty into a precise estimate",
    x = "LR Effect on log-odds of correct response", y = "Density",
    caption = "Prior = beliefs before seeing data | Posterior = beliefs after incorporating 40 participants' responses"
  ) +
  theme_ota(base_size = 14) +
  theme(legend.position = "bottom") +
  transition_manual(frame)

anim_collide <- animate(p_collide,
  nframes = 40, fps = 6,
  width = 800, height = 500, renderer = gifski_renderer()
)
anim_save(file.path(output_dir, "34_prior_to_posterior_updating.gif"), animation = anim_collide)
cat("  Saved: 34_prior_to_posterior_updating.gif\n")

# --- 15-9: REVEAL ---
# MCMC iterations progressively revealed left to right
cat("  [9/9] Reveal ...\n")

p_reveal <- mcmc_draws %>%
  ggplot(aes(x = iteration, y = LR_effect, color = chain)) +
  geom_line(alpha = 0.7, linewidth = 0.5) +
  geom_hline(
    yintercept = median(lr_effect), linetype = "dashed",
    color = palette$pink, linewidth = 1
  ) +
  scale_color_manual(values = chain_colors, name = "Chain") +
  labs(
    title = "MCMC Trace Plot for /l/-/r/",
    subtitle = "Four chains sample the posterior independently and converge on the same value",
    x = "Iteration (post-warmup)", y = "/l/-/r/ Effect (log-odds)",
    caption = "Dashed line = posterior median | All four chains stabilize around the same value"
  ) +
  theme_ota(base_size = 13) +
  transition_reveal(iteration)

anim_reveal <- animate(p_reveal,
  nframes = 100, fps = 15,
  width = 900, height = 500, renderer = gifski_renderer()
)
anim_save(file.path(output_dir, "35_mcmc_posterior_sampling.gif"), animation = anim_reveal)
cat("  Saved: 35_mcmc_posterior_sampling.gif\n")

# =============================================================================
# STEP 15b: RAW DATA SHOWCASE — Visualizing the Experiment Itself
#   The most unique way to present this data: show the actual English
#   word pairs that Japanese speakers judged, and reveal which ones
#   they confused. No paper visualizes data at the stimulus level.
# =============================================================================

cat("\n=== STEP 15b: RAW DATA SHOWCASE ===\n")
cat("Visualizing which English word pairs Japanese speakers confuse\n\n")

# --- 15b-1: Word Pair Confusion Spectrum ---
# A horizontal strip plot: every word pair positioned by its false positive
# rate, grouped by contrast type. Flagship pairs that drive the paper's
# conclusions are highlighted and labeled. The visual narrative builds from
# left to right: F and PB cluster near zero (no/known contrast), while H
# and LR spread rightward — LR most of all, because Japanese speakers lack
# the /l/-/r/ phonemic distinction.
#
# Color gradient: white (no error) → dark red (high error)
# — sequential ColorBrewer Reds palette, colorblind- and print-safe.
#
# Flagship pairs (from Ota et al., 2009 key findings):
#   KEY-ROCK  (LR, 60%)  — the paper's title example
#   LAG-CLOTH (LR, 100%) — maximum /l/-/r/ indeterminacy
#   BEAM-LAY  (LR, 80%)  — strong near-homophone confusion
#   MAJOR-MINER (H, 100%) — true homophone, universal effect
#   DOG-TALE    (H, 80%)  — TAIL/TALE phonological mediation
#   BALL-PAT  (PB, 60%)  — highest PB error, but Japanese HAS /p/-/b/
#   READ-FIND (F, 25%)   — highest spelling control error, still trivial

cat("  [1/2] Word Pair Confusion Spectrum ...\n")

# Calculate false positive rate per word pair
pair_stats <- data_clean %>%
  group_by(Contrast, WordL, WordR) %>%
  summarise(
    error_rate = mean(accuracy == 0),
    n_responses = n(),
    .groups = "drop"
  ) %>%
  mutate(
    pair_label = paste0(WordL, " \u2013 ", WordR),
    contrast_label = factor(
      case_when(
        Contrast == "F" ~ "F \u2014 Spelling Control",
        Contrast == "PB" ~ "PB \u2014 /p/-/b/ (in L1)",
        Contrast == "H" ~ "H \u2014 Homophones",
        Contrast == "LR" ~ "LR \u2014 /l/-/r/ (absent in L1)"
      ),
      levels = c(
        "F \u2014 Spelling Control",
        "PB \u2014 /p/-/b/ (in L1)",
        "H \u2014 Homophones",
        "LR \u2014 /l/-/r/ (absent in L1)"
      )
    )
  )

# Tag the flagship pairs that best illustrate the paper's thesis
flagship_keys <- tribble(
  ~WordL,   ~WordR,   ~Contrast,
  "KEY",    "ROCK",   "LR", # Paper's title example (60% error)
  "LAG",    "CLOTH",  "LR", # 100% error — maximum /l/-/r/ confusion
  "BEAM",   "LAY",    "LR", # 80% error — strong near-homophone effect
  "MAJOR",  "MINER",  "H", # 100% error — perfect true-homophone confusion
  "DOG",    "TALE",   "H", # 80% error — TAIL/TALE phonological mediation
  "BALL",   "PAT",    "PB", # Highest PB error — but Japanese HAS /p/-/b/
  "READ",   "FIND",   "F" # Highest spelling-control error — still only 25%
)

pair_stats <- pair_stats %>%
  mutate(is_flagship = paste(Contrast, WordL, WordR) %in%
    paste(flagship_keys$Contrast, flagship_keys$WordL, flagship_keys$WordR))

# Deterministic vertical jitter for strip readability
set.seed(2009)
pair_stats <- pair_stats %>%
  mutate(jitter_y = runif(n(), -0.28, 0.28))

# Flagship label positions: alternate above/below to avoid overlap
flagship_data <- pair_stats %>%
  filter(is_flagship) %>%
  group_by(Contrast) %>%
  arrange(desc(error_rate)) %>%
  mutate(
    label_idx = row_number(),
    label_y = ifelse(label_idx %% 2 == 1, 0.50, -0.50)
  ) %>%
  ungroup()

# Per-contrast mean error rates for reference lines
contrast_means <- pair_stats %>%
  group_by(contrast_label) %>%
  summarise(mean_error = mean(error_rate), .groups = "drop")

# --- Build cumulative animation frames: F → F+PB → F+PB+H → all ---
contrast_order <- c("F", "PB", "H", "LR")

frame_data <- map_dfr(seq_along(contrast_order), function(f) {
  pair_stats %>%
    filter(Contrast %in% contrast_order[1:f]) %>%
    mutate(frame = f)
})

frame_means <- map_dfr(seq_along(contrast_order), function(f) {
  contrast_means %>%
    semi_join(
      pair_stats %>% filter(Contrast %in% contrast_order[1:f]),
      by = "contrast_label"
    ) %>%
    mutate(frame = f)
})

frame_flagships <- map_dfr(seq_along(contrast_order), function(f) {
  flagship_data %>%
    filter(Contrast %in% contrast_order[1:f]) %>%
    mutate(frame = f)
})

# Placeholder rows ensure all 4 facet panels appear in every frame
panel_holders <- expand_grid(
  contrast_label = levels(pair_stats$contrast_label),
  frame = 1:4
) %>% mutate(error_rate = NA_real_, jitter_y = 0, is_flagship = FALSE, pair_label = "")

frame_data <- bind_rows(frame_data, panel_holders)

# Color gradient: sequential white → red (ColorBrewer Reds)
# Unipolar measure (0% = no error, 100% = total confusion) needs a sequential
# palette, not diverging. White = nothing, dark red = bad — self-documenting.
# Breakpoints aligned to data: 8% just above PB mean (6%), 25% at H mean (~24%),
# so H and LR (~31%) map to similar intensities, reinforcing the paper's thesis.
spectrum_colors <- c("#FFFFFF", "#FEE0D2", "#FCBBA1", "#FB6A4A", "#CB181D")
spectrum_values <- scales::rescale(c(0, 0.08, 0.25, 0.50, 1.00))

# --- Animated GIF: panels reveal one contrast at a time ---
p_spectrum <- frame_data %>%
  ggplot(aes(x = error_rate, y = jitter_y)) +
  # Subtle reference lines at 25% and 50%
  geom_vline(
    xintercept = c(0.25, 0.50), linetype = "dotted",
    color = "gray70", linewidth = 0.3
  ) +
  # All word pairs as small dots
  geom_point(
    aes(fill = error_rate),
    shape = 21, size = 3, stroke = 0.3,
    color = "gray40", alpha = 0.6, na.rm = TRUE
  ) +
  # Flagship pairs: larger, bold-bordered
  geom_point(
    data = frame_flagships,
    aes(x = error_rate, y = jitter_y, fill = error_rate),
    shape = 21, size = 6, stroke = 1.4, color = palette$black
  ) +
  # Dotted connector from flagship dot to its label
  geom_segment(
    data = frame_flagships,
    aes(x = error_rate, xend = error_rate, y = jitter_y, yend = label_y),
    linewidth = 0.4, color = "gray50", linetype = "dotted"
  ) +
  # Flagship labels (white background for readability)
  geom_label(
    data = frame_flagships,
    aes(x = error_rate, y = label_y, label = pair_label),
    size = 2.8, fontface = "bold", color = palette$black,
    fill = "white", alpha = 0.92, label.size = 0.3,
    label.padding = unit(0.18, "lines")
  ) +
  # Per-contrast mean error rate (dashed vertical line)
  geom_vline(
    data = frame_means,
    aes(xintercept = mean_error),
    linetype = "dashed", color = palette$indigo, linewidth = 0.7, alpha = 0.5
  ) +
  facet_wrap(~contrast_label, ncol = 1, strip.position = "left") +
  scale_fill_gradientn(
    colors = spectrum_colors, values = spectrum_values,
    limits = c(0, 1), name = "Error Rate",
    labels = scales::percent, na.value = "transparent"
  ) +
  scale_x_continuous(
    labels = scales::percent, limits = c(-0.05, 1.15),
    breaks = seq(0, 1, 0.25)
  ) +
  scale_y_continuous(limits = c(-0.70, 0.70)) +
  labs(
    title = "How L1 Phonology Shapes L2 Word Confusion",
    subtitle = "Each dot = one word pair | Large labeled dots = key findings | White = accurate, Red = confused",
    x = "False Positive Rate",
    caption = "Data: Ota et al. (2009) | 20 Japanese\u2013English bilinguals | Semantic relatedness judgment"
  ) +
  theme_ota(base_size = 13) +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 9.5, hjust = 1),
    strip.placement = "outside",
    legend.position = "bottom",
    legend.key.width = unit(2.5, "cm"),
    legend.key = element_rect(colour = "gray80"),
    plot.margin = margin(10, 20, 10, 10)
  ) +
  transition_manual(frame)

anim_spectrum <- animate(p_spectrum,
  nframes = 20, fps = 2,
  width = 1200, height = 800, renderer = gifski_renderer()
)
anim_save(file.path(output_dir, "36_word_pair_confusion_spectrum.gif"),
  animation = anim_spectrum
)
cat("  Saved: 36_word_pair_confusion_spectrum.gif\n")

# --- Static high-res PNG: Expanded strip plot (refined Proto B) ---
# ~15 labeled pairs: top 2 + bottom 1 per contrast, plus all flagships.
# Flagship labels bold, others plain. 4-position alternating labels.
static_label_candidates <- pair_stats %>%
  group_by(contrast_label) %>%
  arrange(desc(error_rate)) %>%
  mutate(rank_desc = row_number()) %>%
  arrange(error_rate) %>%
  mutate(rank_asc = row_number()) %>%
  ungroup() %>%
  filter(is_flagship | rank_desc <= 2 | rank_asc <= 1)

# Assign alternating label positions within each contrast
static_labels <- static_label_candidates %>%
  group_by(contrast_label) %>%
  arrange(desc(error_rate)) %>%
  mutate(
    label_idx = row_number(),
    label_y = case_when(
      label_idx %% 4 == 1 ~ 0.52,
      label_idx %% 4 == 2 ~ -0.52,
      label_idx %% 4 == 3 ~ 0.68,
      label_idx %% 4 == 0 ~ -0.68
    )
  ) %>%
  ungroup()

p_spectrum_static <- pair_stats %>%
  ggplot(aes(x = error_rate, y = jitter_y)) +
  geom_vline(
    xintercept = c(0.25, 0.50), linetype = "dotted",
    color = "gray70", linewidth = 0.3
  ) +
  # All word pairs as small dots
  geom_point(
    aes(fill = error_rate),
    shape = 21, size = 3, stroke = 0.3,
    color = "gray40", alpha = 0.5
  ) +
  # Highlighted labeled pairs: larger dots
  geom_point(
    data = static_labels,
    aes(fill = error_rate),
    shape = 21, size = ifelse(static_labels$is_flagship, 6.5, 5),
    stroke = ifelse(static_labels$is_flagship, 1.4, 0.8),
    color = palette$black
  ) +
  # Connector segments
  geom_segment(
    data = static_labels,
    aes(x = error_rate, xend = error_rate, y = jitter_y, yend = label_y),
    linewidth = 0.3, color = "gray50", linetype = "dotted"
  ) +
  # Labels: bold for flagships, plain for others
  geom_label(
    data = static_labels,
    aes(y = label_y, label = pair_label),
    size = 2.8,
    fontface = ifelse(static_labels$is_flagship, "bold", "plain"),
    color = palette$black,
    fill = "white", alpha = 0.90, label.size = 0.25,
    label.padding = unit(0.16, "lines")
  ) +
  # Mean lines
  geom_vline(
    data = contrast_means,
    aes(xintercept = mean_error),
    linetype = "dashed", color = palette$indigo, linewidth = 0.7, alpha = 0.5
  ) +
  geom_text(
    data = contrast_means,
    aes(
      x = mean_error, y = 0.82,
      label = sprintf("mean = %.0f%%", mean_error * 100)
    ),
    hjust = -0.12, size = 3, color = palette$indigo, fontface = "italic"
  ) +
  facet_wrap(~contrast_label, ncol = 1, strip.position = "left") +
  scale_fill_gradientn(
    colors = spectrum_colors, values = spectrum_values,
    limits = c(0, 1), name = "Error Rate",
    labels = scales::percent, na.value = "transparent"
  ) +
  scale_x_continuous(
    labels = scales::percent, limits = c(-0.05, 1.18),
    breaks = seq(0, 1, 0.25)
  ) +
  scale_y_continuous(limits = c(-0.85, 0.92)) +
  labs(
    title = "How L1 Phonology Shapes L2 Word Confusion",
    subtitle = "Each dot = one word pair | Bold labels = flagship pairs | Dashed lines = contrast means",
    x = "False Positive Rate (% of trials where unrelated pairs were judged 'related')",
    caption = "Data: Ota et al. (2009) | 20 Japanese\u2013English bilinguals | Semantic relatedness judgment"
  ) +
  theme_ota(base_size = 13) +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 11, hjust = 1),
    strip.placement = "outside",
    legend.position = "bottom",
    legend.key.width = unit(2.5, "cm"),
    legend.key = element_rect(colour = "gray80"),
    plot.margin = margin(10, 20, 10, 10)
  )

ggsave(file.path(output_dir, "36_word_pair_confusion_heatmap.png"),
  p_spectrum_static,
  width = 14, height = 10, dpi = 300
)
cat("  Saved: 36_word_pair_confusion_heatmap.png (static high-res)\n")

# --- 15b-2: All-Pairs Ranked Dot Chart (no cutoff) ---
# Every word pair across all four contrasts, ranked by error rate.
# Flagship pairs get bold labels and larger dots.

cat("  [All-Pairs] Ranked Dot Chart (complete) ...\n")

all_pairs_ranked <- pair_stats %>%
  group_by(contrast_label) %>%
  arrange(desc(error_rate)) %>%
  mutate(display_rank = row_number()) %>%
  ungroup()

# Count total pairs per contrast for subtitle
n_per_contrast <- all_pairs_ranked %>%
  count(contrast_label, name = "n_pairs")

p_all_pairs <- all_pairs_ranked %>%
  ggplot(aes(
    x = error_rate,
    y = reorder(pair_label, -display_rank)
  )) +
  geom_point(aes(fill = error_rate, size = is_flagship),
    shape = 21, stroke = 0.5, color = "gray30"
  ) +
  scale_size_manual(values = c(`FALSE` = 2.8, `TRUE` = 5.5), guide = "none") +
  # Vertical reference lines at 25%, 50%, 75%
  geom_vline(
    xintercept = c(0.25, 0.50, 0.75),
    linetype = "dotted", color = "gray70", linewidth = 0.25
  ) +
  # Mean error line per contrast
  geom_vline(
    data = contrast_means,
    aes(xintercept = mean_error),
    linetype = "dashed", color = palette$indigo, linewidth = 0.6, alpha = 0.5
  ) +
  facet_wrap(~contrast_label, ncol = 2, scales = "free_y") +
  scale_fill_gradientn(
    colors = spectrum_colors, values = spectrum_values,
    limits = c(0, 1), name = "Error Rate",
    labels = scales::percent, na.value = "transparent"
  ) +
  scale_x_continuous(
    labels = scales::percent, limits = c(-0.05, 1.08),
    breaks = seq(0, 1, 0.25)
  ) +
  labs(
    title = "Complete Word-Pair Confusion Ranking Across All Contrasts",
    subtitle = "Every pair shown | Bold dots = flagship pairs | Dashed line = contrast mean",
    x = "False Positive Rate",
    y = NULL,
    caption = "Data: Ota et al. (2009) | 20 Japanese\u2013English bilinguals | Semantic relatedness judgment"
  ) +
  theme_ota(base_size = 11) +
  theme(
    axis.text.y = element_text(
      size = 7,
      face = ifelse(
        all_pairs_ranked %>%
          arrange(contrast_label, display_rank) %>%
          pull(is_flagship),
        "bold", "plain"
      )
    ),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "bottom",
    legend.key.width = unit(2.5, "cm"),
    legend.key = element_rect(colour = "gray80"),
    panel.grid.major.x = element_line(color = palette$grid, linewidth = 0.3),
    panel.grid.major.y = element_line(color = palette$grid, linewidth = 0.1),
    plot.margin = margin(10, 15, 10, 10)
  )

ggsave(file.path(output_dir, "proto_A_ranked_dot_chart.png"),
  p_all_pairs,
  width = 16, height = 18, dpi = 300
)
cat("  Saved: proto_A_ranked_dot_chart.png (all pairs, no cutoff)\n")

# =============================================================================
# STEP 15c: ANIMATED VERSIONS OF KEY ANALYTICAL PLOTS
#   5 animations using gganimate + gifski (no magick text functions)
#   (1) Subject × Contrast Heatmap — columns reveal left to right
#   (2) Pairwise ROPE — comparisons reveal bottom to top
#   (3) All-Pairs Ranked Dot Chart — one facet at a time
#   (4) All-Pairs Strip Plot (Fig 8 source) — dots accumulate by contrast
#   (5) Caterpillar Plot — subjects reveal one by one
# =============================================================================

cat("\n=== STEP 15c: ANIMATING ANALYTICAL PLOTS ===\n")

# --- 15c-1: Animated Subject × Contrast Heatmap ---
# Columns appear one at a time: F → PB → H → LR (easiest → hardest)
cat("  [1/5] Animating Subject × Contrast Heatmap ...\n")

# Order columns from easiest to hardest for dramatic reveal
contrast_reveal_order <- c("F", "PB", "H", "LR")

heatmap_anim_data <- map_dfr(seq_along(contrast_reveal_order), function(i) {
  subject_contrast %>%
    filter(contrast_type %in% contrast_reveal_order[1:i]) %>%
    mutate(frame = i,
           frame_label = paste0("Revealing: ", contrast_reveal_order[i],
                                " (", i, "/4)"))
})

p_heatmap_anim <- heatmap_anim_data %>%
  ggplot(aes(x = contrast_type, y = Subject, fill = error_rate)) +
  geom_tile(color = "white", linewidth = 0.6) +
  # Two separate text layers avoid a color legend with hex codes
  geom_text(
    data = ~filter(.x, error_rate <= 0.35),
    aes(label = paste0(error_pct, "%")),
    color = "gray20", size = 3.2, fontface = "bold"
  ) +
  geom_text(
    data = ~filter(.x, error_rate > 0.35),
    aes(label = paste0(error_pct, "%")),
    color = "white", size = 3.2, fontface = "bold"
  ) +
  scale_fill_gradientn(
    colors = c("#f0f0f8", palette$lavender, palette$purple, palette$hot_pink, "#8b0000"),
    values = scales::rescale(c(0, 0.1, 0.25, 0.45, 0.80)),
    limits = c(0, 0.80),
    name = "Error Rate",
    labels = scales::percent,
    oob = scales::squish
  ) +
  scale_x_discrete(drop = FALSE) +
  labs(
    title = "Who Struggles With What? — {current_frame}",
    subtitle = "Columns reveal from easiest (F) to hardest (LR) contrast",
    x = "Contrast Type", y = "Subject",
    caption = "Data: Ota et al. (2009) | 20 Japanese\u2013English bilinguals"
  ) +
  theme_ota(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(face = "bold", size = 11),
    axis.text.y = element_text(size = 9),
    legend.position = "right",
    legend.key.height = unit(2, "cm")
  ) +
  transition_manual(frame_label)

anim_heatmap <- animate(p_heatmap_anim, nframes = 60, fps = 8,
                        width = 700, height = 800, renderer = gifski_renderer())
anim_save(file.path(output_dir, "42_subject_heatmap_reveal.gif"),
          animation = anim_heatmap)
cat("  Saved: 42_subject_heatmap_reveal.gif\n")

# --- 15c-2: Animated Pairwise ROPE ---
# Each comparison appears one at a time, bottom to top
cat("  [2/5] Animating Pairwise ROPE ...\n")

# Build cumulative frames: add one comparison per frame
rope_reveal_order <- c("H vs PB", "LR vs PB", "LR vs H", "PB vs F", "H vs F", "LR vs F")

rope_anim_data <- map_dfr(seq_along(rope_reveal_order), function(i) {
  pairwise_diffs %>%
    filter(comparison %in% rope_reveal_order[1:i]) %>%
    mutate(frame = i)
})

p_rope_anim <- rope_anim_data %>%
  ggplot(aes(x = diff, y = comparison)) +
  annotate("rect",
    xmin = rope_lo, xmax = rope_hi,
    ymin = -Inf, ymax = Inf,
    fill = palette$light_blue, alpha = 0.4
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.4) +
  stat_halfeye(
    aes(fill = after_stat(x < rope_lo)),
    .width = c(0.50, 0.95),
    point_interval = "median_qi",
    slab_alpha = 0.7,
    interval_color = "gray30",
    point_color = "gray10",
    point_size = 2.5,
    interval_linewidth = 1.5
  ) +
  scale_fill_manual(
    values = c(`TRUE` = palette$indigo, `FALSE` = palette$lavender),
    guide = "none"
  ) +
  scale_x_continuous(breaks = seq(-4, 2, 0.5)) +
  scale_y_discrete(drop = FALSE) +
  labs(
    title = "Building the Inferential Hierarchy",
    subtitle = "Pairwise posterior differences | Blue band = ROPE [\u00b10.18]",
    x = "Difference in Log-Odds",
    y = NULL,
    caption = "Data: Ota et al. (2009) | Comprehensive model posteriors"
  ) +
  theme_ota(base_size = 12) +
  theme(
    axis.text.y = element_text(face = "bold", size = 11),
    panel.grid.major.y = element_blank()
  ) +
  transition_states(frame, transition_length = 1, state_length = 2) +
  enter_fade()

anim_rope <- animate(p_rope_anim, nframes = 80, fps = 8,
                     width = 900, height = 550, renderer = gifski_renderer())
anim_save(file.path(output_dir, "43_pairwise_rope_reveal.gif"),
          animation = anim_rope)
cat("  Saved: 43_pairwise_rope_reveal.gif\n")

# --- 15c-3: Animated All-Pairs Ranked Dot Chart ---
# One facet (contrast) at a time: F → PB → H → LR
cat("  [3/5] Animating All-Pairs Ranked Dot Chart ...\n")

contrast_facet_order <- c(
  "F \u2014 Spelling Control",
  "PB \u2014 /p/-/b/ (in L1)",
  "H \u2014 Homophones",
  "LR \u2014 /l/-/r/ (absent in L1)"
)

dotchart_anim_data <- map_dfr(seq_along(contrast_facet_order), function(i) {
  all_pairs_ranked %>%
    filter(contrast_label %in% contrast_facet_order[1:i]) %>%
    mutate(frame = i)
})

p_dotchart_anim <- dotchart_anim_data %>%
  ggplot(aes(
    x = error_rate,
    y = reorder(pair_label, -display_rank)
  )) +
  geom_point(aes(fill = error_rate, size = is_flagship),
    shape = 21, stroke = 0.5, color = "gray30"
  ) +
  scale_size_manual(values = c(`FALSE` = 2.8, `TRUE` = 5.5), guide = "none") +
  geom_vline(
    xintercept = c(0.25, 0.50, 0.75),
    linetype = "dotted", color = "gray70", linewidth = 0.25
  ) +
  geom_vline(
    data = contrast_means,
    aes(xintercept = mean_error),
    linetype = "dashed", color = palette$indigo, linewidth = 0.6, alpha = 0.5
  ) +
  facet_wrap(~contrast_label, ncol = 2, scales = "free_y") +
  scale_fill_gradientn(
    colors = spectrum_colors, values = spectrum_values,
    limits = c(0, 1), name = "Error Rate",
    labels = scales::percent, na.value = "transparent"
  ) +
  scale_x_continuous(
    labels = scales::percent, limits = c(-0.05, 1.08),
    breaks = seq(0, 1, 0.25)
  ) +
  labs(
    title = "Word-Pair Confusion Ranking — Contrast {frame} of 4",
    subtitle = "Each facet reveals from easiest (F) to hardest (LR)",
    x = "False Positive Rate", y = NULL,
    caption = "Data: Ota et al. (2009) | Bold dots = flagship pairs | Dashed line = contrast mean"
  ) +
  theme_ota(base_size = 11) +
  theme(
    axis.text.y = element_text(size = 6),
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    panel.grid.major.x = element_line(color = palette$grid, linewidth = 0.3),
    panel.grid.major.y = element_line(color = palette$grid, linewidth = 0.1)
  ) +
  transition_manual(frame)

anim_dotchart <- animate(p_dotchart_anim, nframes = 40, fps = 4,
                         width = 1000, height = 900, renderer = gifski_renderer())
anim_save(file.path(output_dir, "44_ranked_dotchart_reveal.gif"),
          animation = anim_dotchart)
cat("  Saved: 44_ranked_dotchart_reveal.gif\n")

# --- 15c-4: Animated Strip Plot (Word-Pair Confusion Spectrum) ---
# Dots accumulate by contrast: F → PB → H → LR
cat("  [4/5] Animating Word-Pair Strip Plot ...\n")

strip_anim_data <- map_dfr(seq_along(contrast_facet_order), function(i) {
  pair_stats %>%
    filter(contrast_label %in% contrast_facet_order[1:i]) %>%
    mutate(frame = i,
           frame_label = paste0("Adding: ",
                                gsub(" \u2014 .*", "", contrast_facet_order[i]),
                                " (", i, "/4)"))
})

p_strip_anim <- strip_anim_data %>%
  ggplot(aes(x = error_rate, y = contrast_label)) +
  geom_vline(xintercept = c(0.25, 0.50, 0.75),
    linetype = "dotted", color = "gray70", linewidth = 0.3
  ) +
  geom_point(
    aes(fill = error_rate, size = is_flagship),
    position = position_jitter(height = 0.25, width = 0, seed = 2009),
    shape = 21, stroke = 0.5, color = "gray30", alpha = 0.85
  ) +
  # Label flagship pairs so the plot is self-explanatory
  geom_text(
    data = ~filter(.x, is_flagship),
    aes(label = pair_label),
    position = position_nudge(y = -0.35),
    size = 2.3, fontface = "bold", hjust = 0,
    color = "gray30", check_overlap = TRUE
  ) +
  scale_size_manual(values = c(`FALSE` = 3.5, `TRUE` = 7), guide = "none") +
  scale_fill_gradientn(
    colors = spectrum_colors, values = spectrum_values,
    limits = c(0, 1), name = "Error Rate",
    labels = scales::percent, na.value = "transparent"
  ) +
  scale_x_continuous(labels = scales::percent, limits = c(-0.05, 1.08),
                     breaks = seq(0, 1, 0.25)) +
  scale_y_discrete(drop = FALSE) +
  labs(
    title = "Word-Pair Confusion Spectrum — {closest_state}",
    subtitle = "Each dot = one word pair | Labeled dots = flagship pairs (e.g., KEY\u2013ROCK)",
    x = "False Positive Rate", y = NULL,
    caption = "Data: Ota et al. (2009) | 258 word pairs across 4 contrast types"
  ) +
  theme_ota(base_size = 13) +
  theme(
    axis.text.y = element_text(face = "bold", size = 11),
    legend.position = "bottom",
    legend.key.width = unit(2.5, "cm")
  ) +
  transition_states(frame_label, transition_length = 1, state_length = 2) +
  enter_fade()

anim_strip <- animate(p_strip_anim, nframes = 60, fps = 8,
                      width = 900, height = 500, renderer = gifski_renderer())
anim_save(file.path(output_dir, "45_strip_plot_accumulate.gif"),
          animation = anim_strip)
cat("  Saved: 45_strip_plot_accumulate.gif\n")

# --- 15c-5: Animated Caterpillar Plot ---
# Subjects reveal one at a time, sorted from worst to best accuracy
cat("  [5/5] Animating Caterpillar Plot ...\n")

# subject_df is already sorted by estimate via fct_reorder
# Reveal from most negative (worst) to most positive (best)
caterpillar_order <- subject_df %>%
  arrange(estimate) %>%
  pull(subject) %>%
  as.character()

caterpillar_anim_data <- map_dfr(seq_along(caterpillar_order), function(i) {
  subject_df %>%
    filter(subject %in% caterpillar_order[1:i]) %>%
    mutate(frame = i)
})

p_caterpillar_anim <- caterpillar_anim_data %>%
  ggplot(aes(x = estimate, y = subject)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = palette$pink, linewidth = 1) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
    width = 0.3, linewidth = 0.6, color = palette$indigo, alpha = 0.5, orientation = "y"
  ) +
  geom_point(size = 3, shape = 21, fill = palette$indigo, color = palette$black, stroke = 0.6) +
  scale_y_discrete(drop = FALSE) +
  labs(
    title = "Participant Random Effects — Subject {frame} of 20",
    subtitle = "Revealing subjects from worst to best accuracy",
    x = "Random Intercept (log-odds)",
    y = "Subject",
    caption = "Left of dashed line = worse than average | Each point added reveals another participant"
  ) +
  theme_ota(base_size = 13) +
  transition_manual(frame)

anim_caterpillar <- animate(p_caterpillar_anim, nframes = 80, fps = 6,
                            width = 800, height = 600, renderer = gifski_renderer())
anim_save(file.path(output_dir, "46_caterpillar_reveal.gif"),
          animation = anim_caterpillar)
cat("  Saved: 46_caterpillar_reveal.gif\n")

cat("=== STEP 15c COMPLETE: 5 animated analytical plots saved ===\n")

# ═══════════════════════════════════════════════════════════════════════════════
# END STEP 15b
# ═══════════════════════════════════════════════════════════════════════════════

# --- 15b-2: Evidence Accumulation — Watching Accuracy Diverge ---
# A cumulative accuracy tracker: as each subject's data enters, the four
# contrast lines diverge. You watch LR accuracy drop while F stays high.
# Like a "horse race" where the outcome reveals the experimental finding.

cat("  [2/2] Evidence Accumulation ...\n")

# Assign subjects a sequential index
subject_order <- data_clean %>%
  distinct(Subject) %>%
  arrange(Subject) %>%
  mutate(subject_num = row_number())

# Compute running accuracy as each subject's data is added
cumulative_accuracy <- map_dfr(1:nrow(subject_order), function(s) {
  included_subjects <- subject_order$Subject[1:s]
  data_clean %>%
    filter(Subject %in% included_subjects) %>%
    group_by(contrast_type) %>%
    summarise(
      accuracy = mean(accuracy),
      n_trials = n(),
      .groups = "drop"
    ) %>%
    mutate(
      n_subjects = s,
      contrast_label = case_when(
        contrast_type == "F" ~ "F (Spelling)",
        contrast_type == "PB" ~ "PB (/p/-/b/)",
        contrast_type == "H" ~ "H (Homophones)",
        contrast_type == "LR" ~ "LR (/l/-/r/)"
      )
    )
})

# Final accuracy values for endpoint labels
final_vals <- cumulative_accuracy %>%
  filter(n_subjects == max(n_subjects))

p_accumulate <- cumulative_accuracy %>%
  ggplot(aes(x = n_subjects, y = accuracy, color = contrast_label, group = contrast_label)) +
  geom_line(linewidth = 1.3, alpha = 0.85) +
  geom_point(size = 2.5, alpha = 0.7) +
  geom_text(
    data = final_vals,
    aes(label = sprintf("%s\n%.0f%%", contrast_label, accuracy * 100)),
    hjust = -0.1, size = 3.2, fontface = "bold", show.legend = FALSE
  ) +
  geom_hline(yintercept = 0.5, linetype = "dotted", color = palette$grid, linewidth = 0.5) +
  scale_color_manual(values = c(
    "F (Spelling)" = palette$lavender,
    "PB (/p/-/b/)" = "#7c3aed",
    "H (Homophones)" = palette$hot_pink,
    "LR (/l/-/r/)" = palette$indigo
  ), guide = "none") +
  scale_y_continuous(labels = scales::percent, limits = c(0.55, 1.02)) +
  scale_x_continuous(breaks = seq(2, 20, 2), limits = c(1, 24)) +
  labs(
    title = "Cumulative Accuracy as Participants Are Added",
    subtitle = "Lines diverge by subject 10 — the /l/-/r/ disadvantage is robust and early",
    x = "Number of Participants Included",
    y = "Cumulative Accuracy",
    caption = "Data: Ota et al. (2009) | 20 Japanese speakers | Unrelated condition"
  ) +
  theme_ota(base_size = 13) +
  transition_reveal(n_subjects)

anim_accumulate <- animate(p_accumulate,
  nframes = 60, fps = 6,
  width = 900, height = 550, renderer = gifski_renderer()
)
anim_save(file.path(output_dir, "37_evidence_accumulation.gif"),
  animation = anim_accumulate
)
cat("  Saved: 37_evidence_accumulation.gif\n")

# =============================================================================
cat("\n=== STEP 16: PORTFOLIO CUMULATIVE GIF ===\n")
# =============================================================================
# Combines the best static PNGs and animated GIFs into one summary reel.
# Uses magick for read/resize/animate only (no image_annotate — ARM64 safe).

if (!requireNamespace("magick", quietly = TRUE)) {
  install.packages("magick")
}
library(magick)

portfolio_w <- 1200
portfolio_h <- 750

# Helper: fit an image to the canvas while preserving aspect ratio.
# Resizes to fit within bounds (no stretching), then pads with white
# to the exact canvas size so all frames are uniform.
fit_to_canvas <- function(img) {
  # Resize to fit inside portfolio_w x portfolio_h, preserving aspect ratio (no "!")
  img <- image_resize(img, paste0(portfolio_w, "x", portfolio_h))
  # Pad to exact canvas size with white background, centered
  image_extent(img, paste0(portfolio_w, "x", portfolio_h),
    gravity = "center", color = "white"
  )
}

# Helper: read a static PNG and hold it for N seconds as repeated frames
hold_png <- function(png_file, hold_sec = 3, fps = 5) {
  path <- file.path(output_dir, png_file)
  if (!file.exists(path)) {
    cat("  [skip] not found:", png_file, "\n")
    return(NULL)
  }
  img <- image_read(path)
  img <- fit_to_canvas(img)
  n <- round(hold_sec * fps)
  cat("  [PNG] ", png_file, " held ", hold_sec, "s (", n, " frames)\n", sep = "")
  rep(img, n)
}

# Helper: read an existing GIF and resize each frame
load_gif <- function(gif_file) {
  path <- file.path(output_dir, gif_file)
  if (!file.exists(path)) {
    cat("  [skip] not found:", gif_file, "\n")
    return(NULL)
  }
  frames <- image_read(path)
  # Apply fit_to_canvas to each frame individually
  frames <- do.call(c, lapply(seq_along(frames), function(i) fit_to_canvas(frames[i])))
  cat("  [GIF] ", gif_file, " (", length(frames), " frames)\n", sep = "")
  frames
}

portfolio_fps <- 5

cat("Assembling portfolio sequence ...\n")
sections <- list(
  # 1. Raw data overview
  hold_png("10_error_rates_all_contrasts.png", 3.5, portfolio_fps),

  # 2. Word-pair confusion spectrum (animated strip)
  load_gif("36_word_pair_confusion_spectrum.gif"),

  # 3. Static heatmap (the paper's Figure 7)
  hold_png("36_word_pair_confusion_heatmap.png", 4, portfolio_fps),

  # 4. Subject × Contrast heatmap (animated column reveal)
  load_gif("42_subject_heatmap_reveal.gif"),

  # 5. All-pairs ranked dot chart (animated facet reveal)
  load_gif("44_ranked_dotchart_reveal.gif"),

  # 6. Evidence accumulation horse race
  load_gif("37_evidence_accumulation.gif"),

  # 7. Bayesian forest plot
  hold_png("09_forest_plot_all_contrasts.png", 3.5, portfolio_fps),

  # 8. Pairwise contrast posteriors with ROPE (animated reveal)
  load_gif("43_pairwise_rope_reveal.gif"),

  # 9. Credible interval buildup
  load_gif("20_credible_interval_buildup.gif"),

  # 10. Error rate accumulation
  load_gif("21_error_rate_accumulation.gif"),

  # 11. Representational distinctness mechanism
  hold_png("14_representational_distinctness_mechanism.png", 4, portfolio_fps),

  # 12. Contrast effect intervals
  load_gif("29_contrast_effect_intervals.gif"),

  # 15. Distinctness predicts errors
  load_gif("31_distinctness_predicts_errors.gif"),

  # 16. Gradient posterior forest
  hold_png("15_gradient_posterior_forest.png", 3.5, portfolio_fps),

  # 17. Posterior densities by contrast
  load_gif("27_posterior_densities_by_contrast.gif"),

  # 18. LR indeterminacy zoom
  load_gif("33_lr_indeterminacy_zoom.gif"),

  # 19. Prior to posterior updating
  load_gif("34_prior_to_posterior_updating.gif"),

  # 20. MCMC convergence
  load_gif("22_mcmc_convergence_lr.gif"),

  # 21. Strip plot accumulation (dots appear by contrast)
  load_gif("45_strip_plot_accumulate.gif"),

  # 22. Caterpillar plot reveal (subjects one by one)
  load_gif("46_caterpillar_reveal.gif")
)

# Drop any NULLs from missing files
sections <- sections[!sapply(sections, is.null)]
all_frames <- do.call(c, sections)

cat("Total portfolio frames: ", length(all_frames), "\n")
cat("Writing portfolio GIF ...\n")

# delay in hundredths of a second: 1/fps * 100
portfolio_delay <- round(100 / portfolio_fps)
portfolio_gif <- image_animate(all_frames, delay = portfolio_delay, loop = 0)
portfolio_path <- file.path(output_dir, "00_portfolio_cumulative.gif")
image_write(portfolio_gif, path = portfolio_path)

portfolio_mb <- round(file.info(portfolio_path)$size / 1024^2, 1)
cat("  Saved: 00_portfolio_cumulative.gif (", portfolio_mb, " MB)\n", sep = "")

cat("\n=== ALL VISUALIZATIONS COMPLETE ===\n")
cat("Outputs saved to:", output_dir, "\n")
cat("  Portfolio:     00_portfolio_cumulative.gif\n")
cat("  Static:        09-16, 23-26, 36, 38-39, proto_A (.png)\n")
cat("  Interactive:   17-19 (.html) - open in browser\n")
cat("  Animated:      20-22, 27-37, 42-46 (.gif)\n")
cat("  Deep analysis: 38 (subject heatmap), 39 (ROPE)\n")
cat("  Model files:   model_comprehensive_summary.txt, model_comparison_loo.txt\n")

# =============================================================================
# DARK MODE: Re-render all visualizations with dark theme
# =============================================================================
# Generates dark counterparts of all PNGs and GIFs into outputs/dark_mode/
# Matches website [data-theme="dark"] CSS variables
# =============================================================================

cat("\n\n")
cat("=============================================================\n")
cat("  DARK MODE RENDERING\n")
cat("=============================================================\n\n")

# -----------------------------------------------------------------------------
# Dark palette (matches website [data-theme="dark"] in variables.css)
# -----------------------------------------------------------------------------
palette_dark <- list(
  indigo      = "#8480ff",
  hot_pink    = "#f06dd8",
  purple      = "#d18aff",
  lavender    = "#b8b2f0",
  bg          = "#0a0b14",
  bg_surface  = "#13141f",
  bg_elevated = "#1a1b2e",
  grid        = "#1e1f35",
  text        = "#f2f2ff",
  text_secondary = "#b0b0c8",
  text_muted  = "#606078",
  pink        = "#f79cee",
  light_blue  = "#2a3555",
  light_pink  = "#4a2545"
)

# Dark ggplot theme
theme_ota_dark <- function(base_size = 14) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      plot.background    = element_rect(fill = palette_dark$bg, color = NA),
      panel.background   = element_rect(fill = palette_dark$bg, color = NA),
      panel.grid.major   = element_line(color = palette_dark$grid, linewidth = 0.3),
      panel.grid.minor   = element_blank(),
      plot.title         = element_text(color = palette_dark$text, face = "bold", size = base_size + 2),
      plot.subtitle      = element_text(color = palette_dark$text_secondary, face = "italic", size = base_size - 1),
      plot.caption       = element_text(color = palette_dark$text_muted, size = base_size - 3),
      axis.title         = element_text(color = palette_dark$text),
      axis.text          = element_text(color = palette_dark$text),
      legend.background  = element_rect(fill = palette_dark$bg, color = NA),
      legend.key         = element_rect(fill = palette_dark$bg, color = NA),
      legend.text        = element_text(color = palette_dark$text),
      legend.title       = element_text(color = palette_dark$text, face = "bold"),
      strip.text         = element_text(color = palette_dark$text),
      strip.background   = element_rect(fill = palette_dark$bg_surface, color = NA)
    )
}

# Dark fill scales
contrast_fills_dark <- c(
  "F (Spelling Control)" = palette_dark$lavender,
  "LR (/l/-/r/)"         = palette_dark$indigo,
  "H (Homophones)"       = palette_dark$hot_pink,
  "PB (/p/-/b/)"         = palette_dark$purple
)
contrast_fills_short_dark <- c(
  "F"  = palette_dark$lavender,
  "LR" = palette_dark$indigo,
  "H"  = palette_dark$hot_pink,
  "PB" = palette_dark$purple
)
chain_colors_dark <- c(
  "1" = palette_dark$indigo, "2" = palette_dark$hot_pink,
  "3" = palette_dark$purple, "4" = palette_dark$lavender
)

# Dark gradient for heatmap / spectrum plots
spectrum_colors_dark <- c(palette_dark$bg_elevated, "#4a2030", "#7a2040", "#c43050", "#ff4040")
spectrum_values_dark <- scales::rescale(c(0, 0.08, 0.25, 0.50, 1.00))
heatmap_colors_dark  <- c(palette_dark$bg_elevated, palette_dark$lavender, palette_dark$purple, palette_dark$hot_pink, "#ff3030")

# Smart to_dark: applies dark colors via partial theme() so that
# per-plot overrides (custom text sizes, grid styles, etc.) are preserved.
# A complete theme (%+replace%) would wipe those — theme() merges instead.
# NOTE: We intentionally do NOT set axis.text.x or axis.text.y here.
# ggplot2's theme() REPLACES the entire element_text() — not just the color.
# Setting axis.text.x = element_text(color = ...) would wipe per-plot
# properties like angle, hjust, face, and size. The parent axis.text
# already sets the color, which child elements (axis.text.x/y) inherit.
to_dark <- function(p) {
  p <- p + theme(
    plot.background    = element_rect(fill = palette_dark$bg, color = NA),
    panel.background   = element_rect(fill = palette_dark$bg, color = NA),
    panel.grid.major   = element_line(color = palette_dark$grid),
    panel.grid.minor   = element_blank(),
    plot.title         = element_text(color = palette_dark$text),
    plot.subtitle      = element_text(color = palette_dark$text_secondary),
    plot.caption       = element_text(color = palette_dark$text_muted),
    axis.title         = element_text(color = palette_dark$text),
    axis.text          = element_text(color = palette_dark$text),
    legend.background  = element_rect(fill = palette_dark$bg, color = NA),
    legend.key         = element_rect(fill = palette_dark$bg, color = NA),
    legend.text        = element_text(color = palette_dark$text),
    legend.title       = element_text(color = palette_dark$text),
    strip.text         = element_text(color = palette_dark$text),
    strip.background   = element_rect(fill = palette_dark$bg_surface, color = NA)
  )
  # Patch common hardcoded light-mode colors in geom layers
  for (i in seq_along(p$layers)) {
    params <- tryCatch(p$layers[[i]]$aes_params, error = function(e) NULL)
    if (is.null(params) || length(params) == 0) next
    col <- params$colour
    if (!is.null(col)) {
      if (col %in% c("#000000", "black", "gray10", "gray20"))
        p$layers[[i]]$aes_params$colour <- palette_dark$text
      else if (col %in% c("gray30", "gray40"))
        p$layers[[i]]$aes_params$colour <- palette_dark$text_secondary
      else if (col == "gray50")
        p$layers[[i]]$aes_params$colour <- palette_dark$text_muted
      else if (col == "gray70")
        p$layers[[i]]$aes_params$colour <- "#2a2a3a"
    }
    fl <- params$fill
    if (!is.null(fl)) {
      if (fl == "white")
        p$layers[[i]]$aes_params$fill <- palette_dark$bg_elevated
      else if (fl == "#d8e6ff")
        p$layers[[i]]$aes_params$fill <- palette_dark$light_blue
      else if (fl == "#ffd6ff")
        p$layers[[i]]$aes_params$fill <- palette_dark$light_pink
    }
  }
  p
}

dark_dir <- file.path(output_dir, "dark_mode")
dir.create(dark_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------------
# DARK MODE: Static PNGs (16 plots)
# -----------------------------------------------------------------------------
cat("--- Dark Mode: Static PNGs ---\n")

cat("  09_forest_plot_all_contrasts.png\n")
ggsave(file.path(dark_dir, "09_forest_plot_all_contrasts.png"),
  to_dark(p_forest), width = 11, height = 6, dpi = 300)

cat("  10_error_rates_all_contrasts.png\n")
ggsave(file.path(dark_dir, "10_error_rates_all_contrasts.png"),
  to_dark(p_error_rates), width = 10, height = 6, dpi = 300)

cat("  11_posterior_predictive_check.png\n")
# pp_check requires re-generation with dark bayesplot color scheme
bayesplot::color_scheme_set(c(
  palette_dark$light_blue, palette_dark$lavender, palette_dark$indigo,
  palette_dark$purple, palette_dark$hot_pink, palette_dark$pink
))
pp_dark <- pp_check(model_all, ndraws = 100) +
  labs(
    title = "Posterior Predictive Check",
    subtitle = "Dark line = observed data | Light lines = 100 simulated datasets from the fitted model"
  ) +
  theme_ota_dark(base_size = 12)
ggsave(file.path(dark_dir, "11_posterior_predictive_check.png"),
  pp_dark, width = 10, height = 6, dpi = 300)
# Restore light bayesplot color scheme
bayesplot::color_scheme_set(c(
  palette$light_blue, palette$lavender, palette$indigo,
  palette$purple, palette$hot_pink, palette$pink
))

cat("  12_item_level_robustness.png\n")
ggsave(file.path(dark_dir, "12_item_level_robustness.png"),
  to_dark(p_items), width = 10, height = 6, dpi = 300)

cat("  13_linguistic_model_effects.png\n")
ggsave(file.path(dark_dir, "13_linguistic_model_effects.png"),
  to_dark(p_linguistic), width = 11, height = 6, dpi = 300)

cat("  14_representational_distinctness_mechanism.png\n")
ggsave(file.path(dark_dir, "14_representational_distinctness_mechanism.png"),
  to_dark(p_distinctness), width = 12, height = 6.5, dpi = 300)

cat("  15_gradient_posterior_forest.png\n")
ggsave(file.path(dark_dir, "15_gradient_posterior_forest.png"),
  to_dark(p_gradient_forest), width = 11, height = 6, dpi = 300)

cat("  16_gradient_halfeye_posteriors.png\n")
ggsave(file.path(dark_dir, "16_gradient_halfeye_posteriors.png"),
  to_dark(p_gradient_halfeye), width = 11, height = 6, dpi = 300)

cat("  23_prior_vs_posterior.png\n")
ggsave(file.path(dark_dir, "23_prior_vs_posterior.png"),
  to_dark(p_prior_posterior), width = 11, height = 6, dpi = 300)

cat("  24_subject_caterpillar.png\n")
ggsave(file.path(dark_dir, "24_subject_caterpillar.png"),
  to_dark(p_caterpillar), width = 10, height = 7, dpi = 300)

cat("  25_sensitivity_visual.png\n")
ggsave(file.path(dark_dir, "25_sensitivity_visual.png"),
  to_dark(p_sensitivity), width = 11, height = 6, dpi = 300)

cat("  26_raincloud_subject_errors.png\n")
ggsave(file.path(dark_dir, "26_raincloud_subject_errors.png"),
  to_dark(p_raincloud), width = 11, height = 7, dpi = 300)

# Heatmap: override gradient scale for dark bg
cat("  36_word_pair_confusion_heatmap.png\n")
p_spectrum_static_dark <- to_dark(p_spectrum_static) +
  scale_fill_gradientn(
    colors = spectrum_colors_dark, values = spectrum_values_dark,
    limits = c(0, 1), name = "Error Rate",
    labels = scales::percent, na.value = "transparent"
  )
ggsave(file.path(dark_dir, "36_word_pair_confusion_heatmap.png"),
  p_spectrum_static_dark, width = 14, height = 10, dpi = 300)

# Subject heatmap: override gradient + text color scale
cat("  38_subject_contrast_heatmap.png\n")
p_subj_heatmap_dark <- to_dark(p_subj_heatmap) +
  scale_fill_gradientn(
    colors = heatmap_colors_dark,
    values = scales::rescale(c(0, 0.1, 0.25, 0.45, 0.80)),
    limits = c(0, 0.80), name = "Error Rate",
    labels = scales::percent, oob = scales::squish
  ) +
  scale_color_manual(values = c(light = "white", dark = palette_dark$text))
ggsave(file.path(dark_dir, "38_subject_contrast_heatmap.png"),
  p_subj_heatmap_dark, width = 9, height = 10, dpi = 300)

cat("  39_pairwise_contrast_rope.png\n")
ggsave(file.path(dark_dir, "39_pairwise_contrast_rope.png"),
  to_dark(p_pairwise), width = 14, height = 8, dpi = 300)

# Ranked dot chart: override gradient scale
cat("  proto_A_ranked_dot_chart.png\n")
p_all_pairs_dark <- to_dark(p_all_pairs) +
  scale_fill_gradientn(
    colors = spectrum_colors_dark, values = spectrum_values_dark,
    limits = c(0, 1), name = "Error Rate",
    labels = scales::percent, na.value = "transparent"
  )
ggsave(file.path(dark_dir, "proto_A_ranked_dot_chart.png"),
  p_all_pairs_dark, width = 16, height = 18, dpi = 300)

cat("  Static PNGs complete\n\n")

# -----------------------------------------------------------------------------
# DARK MODE: Animated GIFs (19 animations)
# -----------------------------------------------------------------------------
cat("--- Dark Mode: Animated GIFs ---\n")

cat("  20_credible_interval_buildup.gif\n")
anim_dark <- animate(to_dark(p_animated_ci),
  nframes = 80, fps = 10, width = 800, height = 500, renderer = gifski_renderer())
anim_save(file.path(dark_dir, "20_credible_interval_buildup.gif"), animation = anim_dark)

cat("  21_error_rate_accumulation.gif\n")
anim_dark <- animate(to_dark(p_animated_bars),
  nframes = 40, fps = 4, width = 800, height = 500, renderer = gifski_renderer())
anim_save(file.path(dark_dir, "21_error_rate_accumulation.gif"), animation = anim_dark)

cat("  22_mcmc_convergence_lr.gif\n")
anim_dark <- animate(to_dark(p_animated_chains),
  nframes = 120, fps = 15, width = 900, height = 500, renderer = gifski_renderer())
anim_save(file.path(dark_dir, "22_mcmc_convergence_lr.gif"), animation = anim_dark)

cat("  27_posterior_densities_by_contrast.gif\n")
anim_dark <- animate(to_dark(p_fade),
  nframes = 60, fps = 10, width = 800, height = 500, renderer = gifski_renderer())
anim_save(file.path(dark_dir, "27_posterior_densities_by_contrast.gif"), animation = anim_dark)

cat("  28_error_rates_by_l1_status.gif\n")
anim_dark <- animate(to_dark(p_slide),
  nframes = 60, fps = 8, width = 800, height = 500, renderer = gifski_renderer())
anim_save(file.path(dark_dir, "28_error_rates_by_l1_status.gif"), animation = anim_dark)

cat("  29_contrast_effect_intervals.gif\n")
anim_dark <- animate(to_dark(p_hclip),
  nframes = 40, fps = 8, width = 800, height = 500, renderer = gifski_renderer())
anim_save(file.path(dark_dir, "29_contrast_effect_intervals.gif"), animation = anim_dark)

cat("  30_error_growth_by_contrast.gif\n")
anim_dark <- animate(to_dark(p_vclip),
  nframes = 40, fps = 8, width = 800, height = 500, renderer = gifski_renderer())
anim_save(file.path(dark_dir, "30_error_growth_by_contrast.gif"), animation = anim_dark)

cat("  31_distinctness_predicts_errors.gif\n")
anim_dark <- animate(to_dark(p_tilt_up),
  nframes = 40, fps = 6, width = 800, height = 500, renderer = gifski_renderer())
anim_save(file.path(dark_dir, "31_distinctness_predicts_errors.gif"), animation = anim_dark)

cat("  32_posterior_interference_strength.gif\n")
anim_dark <- animate(to_dark(p_tilt_down),
  nframes = 40, fps = 6, width = 800, height = 500, renderer = gifski_renderer())
anim_save(file.path(dark_dir, "32_posterior_interference_strength.gif"), animation = anim_dark)

cat("  33_lr_indeterminacy_zoom.gif\n")
anim_dark <- animate(to_dark(p_focus),
  nframes = 50, fps = 8, width = 800, height = 500, renderer = gifski_renderer())
anim_save(file.path(dark_dir, "33_lr_indeterminacy_zoom.gif"), animation = anim_dark)

cat("  34_prior_to_posterior_updating.gif\n")
anim_dark <- animate(to_dark(p_collide),
  nframes = 40, fps = 6, width = 800, height = 500, renderer = gifski_renderer())
anim_save(file.path(dark_dir, "34_prior_to_posterior_updating.gif"), animation = anim_dark)

cat("  35_mcmc_posterior_sampling.gif\n")
anim_dark <- animate(to_dark(p_reveal),
  nframes = 100, fps = 15, width = 900, height = 500, renderer = gifski_renderer())
anim_save(file.path(dark_dir, "35_mcmc_posterior_sampling.gif"), animation = anim_dark)

# Spectrum GIF: override gradient scale
cat("  36_word_pair_confusion_spectrum.gif\n")
p_spectrum_dark <- to_dark(p_spectrum) +
  scale_fill_gradientn(
    colors = spectrum_colors_dark, values = spectrum_values_dark,
    limits = c(0, 1), name = "Error Rate",
    labels = scales::percent, na.value = "transparent"
  )
anim_dark <- animate(p_spectrum_dark,
  nframes = 20, fps = 2, width = 1200, height = 800, renderer = gifski_renderer())
anim_save(file.path(dark_dir, "36_word_pair_confusion_spectrum.gif"), animation = anim_dark)

cat("  37_evidence_accumulation.gif\n")
anim_dark <- animate(to_dark(p_accumulate),
  nframes = 60, fps = 6, width = 900, height = 550, renderer = gifski_renderer())
anim_save(file.path(dark_dir, "37_evidence_accumulation.gif"), animation = anim_dark)

# Heatmap GIF: override gradient scale
cat("  42_subject_heatmap_reveal.gif\n")
p_heatmap_anim_dark <- to_dark(p_heatmap_anim) +
  scale_fill_gradientn(
    colors = heatmap_colors_dark,
    values = scales::rescale(c(0, 0.1, 0.25, 0.45, 0.80)),
    limits = c(0, 0.80), name = "Error Rate",
    labels = scales::percent, oob = scales::squish
  )
anim_dark <- animate(p_heatmap_anim_dark,
  nframes = 60, fps = 8, width = 700, height = 800, renderer = gifski_renderer())
anim_save(file.path(dark_dir, "42_subject_heatmap_reveal.gif"), animation = anim_dark)

cat("  43_pairwise_rope_reveal.gif\n")
anim_dark <- animate(to_dark(p_rope_anim),
  nframes = 80, fps = 8, width = 900, height = 550, renderer = gifski_renderer())
anim_save(file.path(dark_dir, "43_pairwise_rope_reveal.gif"), animation = anim_dark)

# Dot chart GIF: override gradient scale
cat("  44_ranked_dotchart_reveal.gif\n")
p_dotchart_anim_dark <- to_dark(p_dotchart_anim) +
  scale_fill_gradientn(
    colors = spectrum_colors_dark, values = spectrum_values_dark,
    limits = c(0, 1), name = "Error Rate",
    labels = scales::percent, na.value = "transparent"
  )
anim_dark <- animate(p_dotchart_anim_dark,
  nframes = 40, fps = 4, width = 1000, height = 900, renderer = gifski_renderer())
anim_save(file.path(dark_dir, "44_ranked_dotchart_reveal.gif"), animation = anim_dark)

# Strip plot GIF: override gradient scale
cat("  45_strip_plot_accumulate.gif\n")
p_strip_anim_dark <- to_dark(p_strip_anim) +
  scale_fill_gradientn(
    colors = spectrum_colors_dark, values = spectrum_values_dark,
    limits = c(0, 1), name = "Error Rate",
    labels = scales::percent, na.value = "transparent"
  )
anim_dark <- animate(p_strip_anim_dark,
  nframes = 60, fps = 8, width = 900, height = 500, renderer = gifski_renderer())
anim_save(file.path(dark_dir, "45_strip_plot_accumulate.gif"), animation = anim_dark)

cat("  46_caterpillar_reveal.gif\n")
anim_dark <- animate(to_dark(p_caterpillar_anim),
  nframes = 80, fps = 6, width = 800, height = 600, renderer = gifski_renderer())
anim_save(file.path(dark_dir, "46_caterpillar_reveal.gif"), animation = anim_dark)

cat("  Animated GIFs complete\n\n")

# -----------------------------------------------------------------------------
# DARK MODE: Portfolio Cumulative GIF
# -----------------------------------------------------------------------------
cat("--- Dark Mode: Portfolio GIF ---\n")

fit_to_canvas_dark <- function(img) {
  img <- image_resize(img, paste0(portfolio_w, "x", portfolio_h))
  image_extent(img, paste0(portfolio_w, "x", portfolio_h),
    gravity = "center", color = palette_dark$bg
  )
}

hold_png_dark <- function(png_file, hold_sec = 3, fps = 5) {
  path <- file.path(dark_dir, png_file)
  if (!file.exists(path)) { cat("  [skip]", png_file, "\n"); return(NULL) }
  img <- image_read(path)
  img <- fit_to_canvas_dark(img)
  n <- round(hold_sec * fps)
  cat("  [PNG] ", png_file, " (", n, " frames)\n", sep = "")
  rep(img, n)
}

load_gif_dark <- function(gif_file) {
  path <- file.path(dark_dir, gif_file)
  if (!file.exists(path)) { cat("  [skip]", gif_file, "\n"); return(NULL) }
  frames <- image_read(path)
  frames <- do.call(c, lapply(seq_along(frames), function(i) fit_to_canvas_dark(frames[i])))
  cat("  [GIF] ", gif_file, " (", length(frames), " frames)\n", sep = "")
  frames
}

cat("Assembling dark portfolio ...\n")
dark_sections <- list(
  hold_png_dark("10_error_rates_all_contrasts.png", 3.5, portfolio_fps),
  load_gif_dark("36_word_pair_confusion_spectrum.gif"),
  hold_png_dark("36_word_pair_confusion_heatmap.png", 4, portfolio_fps),
  load_gif_dark("42_subject_heatmap_reveal.gif"),
  load_gif_dark("44_ranked_dotchart_reveal.gif"),
  load_gif_dark("37_evidence_accumulation.gif"),
  hold_png_dark("09_forest_plot_all_contrasts.png", 3.5, portfolio_fps),
  load_gif_dark("43_pairwise_rope_reveal.gif"),
  load_gif_dark("20_credible_interval_buildup.gif"),
  load_gif_dark("21_error_rate_accumulation.gif"),
  hold_png_dark("14_representational_distinctness_mechanism.png", 4, portfolio_fps),
  load_gif_dark("29_contrast_effect_intervals.gif"),
  load_gif_dark("31_distinctness_predicts_errors.gif"),
  hold_png_dark("15_gradient_posterior_forest.png", 3.5, portfolio_fps),
  load_gif_dark("27_posterior_densities_by_contrast.gif"),
  load_gif_dark("33_lr_indeterminacy_zoom.gif"),
  load_gif_dark("34_prior_to_posterior_updating.gif"),
  load_gif_dark("22_mcmc_convergence_lr.gif"),
  load_gif_dark("45_strip_plot_accumulate.gif"),
  load_gif_dark("46_caterpillar_reveal.gif")
)

dark_sections <- dark_sections[!sapply(dark_sections, is.null)]
all_dark_frames <- do.call(c, dark_sections)

cat("Total dark portfolio frames: ", length(all_dark_frames), "\n")
cat("Writing dark portfolio GIF ...\n")

dark_portfolio <- image_animate(all_dark_frames, delay = round(100 / portfolio_fps), loop = 0)
dark_portfolio_path <- file.path(dark_dir, "00_portfolio_cumulative.gif")
image_write(dark_portfolio, path = dark_portfolio_path)

dark_mb <- round(file.info(dark_portfolio_path)$size / 1024^2, 1)
cat("  Saved: dark_mode/00_portfolio_cumulative.gif (", dark_mb, " MB)\n\n", sep = "")

# -----------------------------------------------------------------------------
cat("=== ALL DARK MODE RENDERING COMPLETE ===\n")
cat("Dark outputs saved to:", dark_dir, "\n")
cat("  Portfolio:     dark_mode/00_portfolio_cumulative.gif\n")
cat("  Static (16):   09-16, 23-26, 36, 38-39, proto_A (.png)\n")
cat("  Animated (19): 20-22, 27-37, 42-46 (.gif)\n")
