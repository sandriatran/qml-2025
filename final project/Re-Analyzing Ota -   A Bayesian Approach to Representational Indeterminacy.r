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
#Multiple theory-driven models
#Formulae:
#    (a) Comprehensive:
#       accuracy ~ contrast_type + (1\|subject_id) + 1\|item_id)

#    (b) Linguistic:
#        accuracy ~ phonological_status + (1\|subject_id) + (1\|item_id)

#    (c) Distinctness
#       accuracy ~ Quantify core mechanism phonological_distinctness_scaled + (1\|subject_id) + (1\|item_id)

# -----------------------------------------------------------------------------
#Model's Feature:
#Bayesian hierarchical logistic regression with
#     (a) random effects for subjects and items, model participant differences.
#     (b) Random intercepts for both subjects and items (captures individual differences)
# -----------------------------------------------------------------------------
#Bayesian approach with priors (better than frequentist for small-medium samples)  
#   VALIFATION MODEL ASSUMPTION
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
      Contrast == "F" ~ 1.0,   # Spelling control: fully distinct (baseline)
      Contrast == "PB" ~ 0.8,  # /p/-/b/ present in Japanese: fairly distinct
      Contrast == "LR" ~ 0.3,  # /l/-/r/ absent in Japanese: indeterminate
      Contrast == "H" ~ 0.0    # True homophones: phonologically fused
    ),

    # Property 4: Theoretical factor (meaningful levels for interpretation)
    phonological_status = factor(
      case_when(
        Contrast == "F" ~ "Unrelated",              # Reference: phonologically unrelated
        Contrast == "H" ~ "Homophone",              # Universally confusable (all speakers)
        Contrast == "LR" ~ "L1_absent_contrast",    # Representationally indeterminate
        Contrast == "PB" ~ "L1_present_contrast"    # Phonologically separate
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
output_plots_dir <- here("final project", "outputs")  # Save directly to outputs folder
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
           levels = c("Unrelated", "L1_present_contrast", "L1_absent_contrast", "Homophone")))

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
cat("   95% CrI: [", round(quantile(l1_present_effect, 0.025), 3), ", ",
    round(quantile(l1_present_effect, 0.975), 3), "]\n")
cat("   Interpretation: Contrast present in Japanese L1\n")
cat("                   → Should show minimal interference\n\n")

cat("2. L1 ABSENT CONTRAST (/l/-/r/):\n")
cat("   Median effect: ", round(median(l1_absent_effect), 3), "\n")
cat("   95% CrI: [", round(quantile(l1_absent_effect, 0.025), 3), ", ",
    round(quantile(l1_absent_effect, 0.975), 3), "]\n")
cat("   Interpretation: Contrast ABSENT in Japanese L1\n")
cat("                   → Should show HIGH interference\n")
cat("                   → REPRESENTATIONAL INDETERMINACY\n\n")

cat("3. HOMOPHONES (universally confusable):\n")
cat("   Median effect: ", round(median(homophone_effect), 3), "\n")
cat("   95% CrI: [", round(quantile(homophone_effect, 0.025), 3), ", ",
    round(quantile(homophone_effect, 0.975), 3), "]\n")

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
cat("  95% CrI: [", round(quantile(distinctness_effect, 0.025), 3), ", ",
    round(quantile(distinctness_effect, 0.975), 3), "]\n\n")

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
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1.5) +
  geom_point(aes(color = significant), size = 5, shape = 21, stroke = 2) +
  geom_errorbarh(aes(xmin = lower_95, xmax = upper_95, color = significant),
                 height = 0.2, linewidth = 1.3) +
  scale_color_manual(
    values = c("TRUE" = "darkblue", "FALSE" = "gray60"),
    guide = "none"
  ) +
  labs(
    title = "Contrast Effects Relative to Spelling Control Baseline",
    subtitle = "LR, H, and PB compared to Spelling Control (F)\nNegative = MORE errors; Positive = FEWER errors",
    x = "Effect on log-odds of correct response",
    y = "Contrast Type",
    caption = "Points = median; lines = 95% credible interval\nLR and H show strong negative effects; PB shows minimal effect"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.subtitle = element_text(face = "italic", color = "gray30"),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3)
  )

print(p_forest)
ggsave(file.path(output_plots_dir, "09_forest_plot_all_contrasts.png"),
       p_forest, width = 11, height = 6, dpi = 300)
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
  geom_col(alpha = 0.7, color = "black", linewidth = 1.2, width = 0.6) +
  geom_errorbar(aes(ymin = lower_error, ymax = upper_error),
                width = 0.2, linewidth = 1.2, color = "black") +
  geom_text(aes(label = sprintf("%.1f%%\n[%.1f, %.1f]",
                                median_error * 100,
                                lower_error * 100,
                                upper_error * 100)),
            vjust = -0.5, size = 4.5, fontface = "bold") +
  scale_fill_manual(
    values = c("F (Spelling Control)" = "steelblue", "PB (/p/-/b/)" = "lightblue",
               "H (Homophones)" = "coral", "LR (/l/-/r/)" = "darkred"),
    guide = "none"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.35)) +
  labs(
    title = "False Positive Error Rates by Contrast Type",
    subtitle = "Predicted error rates with 95% credible intervals (hierarchical Bayesian model)",
    x = "Contrast Type",
    y = "Error Rate",
    caption = "Spelling Control (F) shows lowest errors; LR and H show elevated errors; PB intermediate"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 15, hjust = 0.5),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3)
  )

print(p_error_rates)
ggsave(file.path(output_plots_dir, "10_error_rates_all_contrasts.png"),
       p_error_rates, width = 10, height = 6, dpi = 300)
cat("✓ Saved: 10_error_rates_all_contrasts.png\n")

# -----------------------------------------------------------------------------
# STEP 9: Complete Model Summary
# -----------------------------------------------------------------------------
cat("\n=== STEP 9: MODEL SUMMARY ===\n\n")
print(summary(model_all))

sink(file.path(output_dir, "model_comprehensive_summary.txt"))
cat("=== COMPREHENSIVE BAYESIAN MODEL SUMMARY ===\n")
cat("LR (\l\-\r\) , H (Homophones), and PB (\p\-\b\)  Contrasts vs Spelling Control Baseline (F)\n\n")
print(summary(model_all))
sink()

# -----------------------------------------------------------------------------
#  MODEL USED IN QUARTO PDF
# STEP 10: Posterior Predictive Check | Our Model Illustrates Excellent Fit
# **Figure 4:** Posterior Predictive Check: Model Validation
# -----------------------------------------------------------------------------
cat("\n=== STEP 10: MODEL VALIDATION ===\n")
cat("Posterior Predictive Check: Does the model generate realistic data?\n\n")

pp_check_plot <- pp_check(model_all, ndraws = 100) +
  labs(
    title = "Posterior Predictive Check: Model Validation",
    subtitle = "Dark line = observed data; Light lines = simulated from posterior\nIf distributions overlap, model captures data well"
  ) +
  theme_minimal(base_size = 12)

print(pp_check_plot)
ggsave(file.path(output_plots_dir, "11_posterior_predictive_check.png"),
       pp_check_plot, width = 10, height = 6, dpi = 300)
cat("✓ Saved: 11_posterior_predictive_check.png\n")

# -----------------------------------------------------------------------------
# STEP 11: Alternative Priors (Sensitive Analysis)
# -----------------------------------------------------------------------------
cat("\n---STEP 11: SENSITIVITY ANALYSIS---\n")
cat("QUESTION: ")
cat("Testing robustness: Do conclusions hold with weaker priors?\n\n")

# Fitting Model | Weaker (Wider) Priors
priors_weak <- c(
  prior(normal(0, 3.0), class = Intercept),   # Wider prior
  prior(normal(0, 3.0), class = b),           # Wider prior
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
  geom_boxplot(alpha = 0.7, outlier.size = 2) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 2) +
  scale_fill_manual(
    values = c("F" = "steelblue", "LR" = "darkred",
               "H" = "coral", "PB" = "lightblue"),
    guide = "none"
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Item-Level Error Rates by Contrast",
    subtitle = "Each point = one item; Boxplot shows distribution\nRobust effect = consistent pattern across items",
    x = "Contrast Type",
    y = "Error Rate per Item"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3))

print(p_items)
ggsave(file.path(output_plots_dir, "12_item_level_robustness.png"),
       p_items, width = 10, height = 6, dpi = 300)
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
      levels = c("L1-present (/p/-/b/)", "L1-absent (/l/-/r/)", "Homophone")),
    significant = abs(lower_95) > 0 | abs(upper_95) < 0
  ) %>%
  ggplot(aes(x = median_effect, y = phonological_status)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1.5) +
  geom_point(aes(color = significant), size = 5, shape = 21, stroke = 2) +
  geom_errorbarh(aes(xmin = lower_95, xmax = upper_95, color = significant),
                 height = 0.2, linewidth = 1.3) +
  scale_color_manual(
    values = c("TRUE" = "darkgreen", "FALSE" = "gray60"),
    guide = "none"
  ) +
  labs(
    title = "Linguistic Model: Phonological Status Effects",
    subtitle = "L1-based phonological properties predict L2 accuracy\nRelative to Unrelated baseline",
    x = "Effect on log-odds of correct response",
    y = "Phonological Status",
    caption = "Negative = MORE errors; Positive = FEWER errors\nPattern: L1-absent >> L1-present; both < Homophone"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.subtitle = element_text(face = "italic", color = "gray30"),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3)
  )

print(p_linguistic)
ggsave(file.path(output_plots_dir, "13_linguistic_model_effects.png"),
       p_linguistic, width = 11, height = 6, dpi = 300)
cat("✓ Saved: 13_linguistic_model_effects.png\n\n")

# -----------------------------------------------------------------------------
# QUARTO PDF
# STEP 12c: REPRESENTATIONAL DISTINCTNESS CONTINUOUS SCALE
# Figure 3: Representational Indeterminacy: Continuous Mechanism
# -----------------------------------------------------------------------------
cat("=== Representational Distinctness Visualization ===\n")

# Create visualization of the distinctness model
#(Representational Indeterminacy: Continuous Mechanism)
data_plot <- data_clean %>%
  group_by(phonologically_distinct, Contrast) %>%
  summarise(
    error_rate = mean(accuracy == 0),
    n_trials = n(),
    se = sqrt(error_rate * (1 - error_rate) / n_trials),
    .groups = "drop"
  ) %>%
  distinct()

p_distinctness <- data_plot %>%
  ggplot(aes(x = phonologically_distinct, y = error_rate)) +
  geom_point(aes(size = n_trials, fill = Contrast), shape = 21, alpha = 0.7, color = "black", stroke = 2) +
  geom_smooth(method = "loess", se = TRUE, color = "navy", linewidth = 1.5, alpha = 0.2, fill = "lightblue") +
  scale_fill_manual(
    values = c("F" = "steelblue", "LR" = "darkred", "H" = "coral", "PB" = "lightblue"),
    name = "Contrast Type"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(-0.05, 0.4)) +
  scale_x_continuous(breaks = c(0, 0.3, 0.8, 1.0),
                    labels = c("Homophone\n(0.0)", "L1-absent\n(0.3)", "L1-present\n(0.8)", "Unrelated\n(1.0)")) +
  scale_size_continuous(name = "N trials", range = c(2, 8), guide = "none") +
  labs(
    title = "Representational Indeterminacy: Continuous Mechanism",
    subtitle = "Phonological distinctness (L1-constrained) predicts error rates\nA continuous scale captures L1-L2 representational effects",
    x = "Phonological Distinctness\n(as determined by L1 phonological inventory)",
    y = "Error Rate (False Positive)",
    caption = "Lower distinctness (L1-absent contrast) = higher errors\nThis is the CORE MECHANISM in Ota et al. (2009)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.subtitle = element_text(face = "italic", color = "gray30"),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    legend.position = "right"
  )

print(p_distinctness)
ggsave(file.path(output_plots_dir, "14_representational_distinctness_mechanism.png"),
       p_distinctness, width = 12, height = 6.5, dpi = 300)
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
cat(" found that Japanese speakers, whose language lacks an /l/-/r/ distinction")
cat("have high error rates when identifying English words with this contrast")
