# ==============================================================================
# FINAL PROJECT: Bayesian Re-Analysis of Ota, Hartsuiker & Haywood (2009)
# Title: Re-Analyzing Near-Homophony Effects in L2 Visual Word Recognition
# Authors: Sandria Tran and Violet Manson
# Date: 11-25-2025
#
# OBJECTIVE:
# This script provides a reproducible, fully-documented analysis that:
# 1. Replicates Ota et al. (2009) findings using Bayesian methods
# 2. Investigates and explains any discrepancies from original
# 3. Properly identifies and compares the predictor variables
# 4. Includes both basic and advanced hierarchical models
# ==============================================================================

# ==============================================================================
# SECTION 1: PACKAGE LOADING AND SETUP
# ==============================================================================

library(tidyverse)           # Data manipulation and visualization
library(brms)                # Bayesian regression with Stan
library(tidybayes)           # Tidy Bayesian inference
library(bayesplot)           # Posterior visualization
library(posterior)           # Posterior sampling utilities
library(here)                # Project-relative file paths
library(ggplot2)             # Advanced visualization
library(knitr)               # Reporting
# Note: patchwork removed - not required for this analysis

# Set random seed for reproducibility
set.seed(2025)

cat("\n")
cat(paste(rep("=", 80), collapse=""), "\n")
cat("BAYESIAN RE-ANALYSIS OF OTA, HARTSUIKER & HAYWOOD (2009)\n")
cat("Near-Homophony Effects in L2 Visual Word Recognition\n")
cat(paste(rep("=", 80), collapse=""), "\n\n")

# ==============================================================================
# SECTION 2: DATA LOADING AND EXPLORATION
# ==============================================================================

cat("SECTION 1: DATA LOADING\n")
cat(paste(rep("-", 80), collapse=""), "\n\n")

# Load the Ota et al. (2009) data
# Note: Using full path since here() may not work correctly from script
data_path <- "/Users/s/Desktop/qml-2025/data/ota2009/key-rock.csv"
raw_data <- read_csv(data_path, show_col_types = FALSE)

cat("Dataset loaded successfully\n")
cat("File:", data_path, "\n")
cat("Dimensions:", nrow(raw_data), "rows ×", ncol(raw_data), "columns\n\n")

cat("Columns in dataset:\n")
print(names(raw_data))

# ==============================================================================
# SECTION 3: DATA PREPARATION
# ==============================================================================

cat("\n\n")
cat("SECTION 2: DATA PREPARATION\n")
cat(paste(rep("-", 80), collapse=""), "\n\n")

cat("A. Filtering to test trials (exclude practice)\n")
data_test <- raw_data %>%
  filter(Procedure == "TrialProc") %>%
  mutate(
    Words.ACC = as.numeric(Words.ACC),
    Contrast = factor(Contrast)
  )

cat("   Test trials:", nrow(data_test), "\n")
cat("   Practice trials excluded:", sum(raw_data$Procedure == "PracticeProc"), "\n\n")

cat("B. Contrast types in test trials:\n")
contrast_summary <- data_test %>%
  count(Contrast) %>%
  arrange(desc(n))
print(contrast_summary)

cat("\n   Contrast type explanations:\n")
cat("   F  = Filler items (phonologically unrelated baseline)\n")
cat("   H  = Homophone (true homophones, not analyzed here)\n")
cat("   LR = /l/-/r/ near-homophones (TEST CONDITION)\n")
cat("   P  = Phonological other\n")
cat("   PB = /p/-/b/ near-homophones (for Arabic speakers)\n\n")

# ==============================================================================
# SECTION 4: CRITICAL ANALYSIS - IDENTIFYING THE COMPARISON
# ==============================================================================

cat("\n")
cat("SECTION 3: IDENTIFYING THE CORRECT COMPARISON\n")
cat(paste(rep("=", 80), collapse=""), "\n\n")

cat("ORIGINAL STUDY (Ota et al., 2009):\n")
cat("  'In all participant groups, homophones elicited more false positive errors\n")
cat("   and slower processing than spelling controls. In the Japanese and Arabic\n")
cat("   groups, near-homophones also induced relatively more false positives...'\n\n")

cat("KEY CONCEPT: FALSE POSITIVES\n")
cat("  False positive = Incorrectly judging UNRELATED words as RELATED\n")
cat("  These errors can only occur in UNRELATED trials\n")
cat("  Accuracy = 0 means error (false positive)\n")
cat("  Accuracy = 1 means correct rejection\n\n")

cat("FILTERING TO UNRELATED TRIALS:\n")
unrelated_data <- data_test %>%
  filter(Condition == "Unrelated")

cat("Total unrelated trials:", nrow(unrelated_data), "\n")
cat("Unique subjects:", n_distinct(unrelated_data$Subject), "\n")
cat("Unique items:", n_distinct(unrelated_data$Item), "\n\n")

cat("ERROR RATES BY CONTRAST IN UNRELATED TRIALS:\n")
error_by_contrast <- unrelated_data %>%
  group_by(Contrast) %>%
  summarise(
    n_trials = n(),
    n_subjects = n_distinct(Subject),
    n_items = n_distinct(Item),
    correct_rejections = sum(Words.ACC == 1),
    false_positives = sum(Words.ACC == 0),
    error_rate = 1 - mean(Words.ACC),
    .groups = "drop"
  ) %>%
  arrange(desc(n_trials))

print(error_by_contrast)

cat("\n\nIDENTIFYING THE CONTROL CONDITION:\n")
cat("The original paper compared LR to 'spelling controls'\n")
cat("In this dataset, Spelling Control(S) serve as the baseline comparison:\n")
cat("  - Spelling Control (S): Phonologically unrelated pairs (baseline)\n")
cat("  - LR: /l/-/r/ near-homophones (test condition)\n")
cat("  - Both have same semantic relationship as test pairs\n")
cat("  - Both appear in unrelated condition\n\n")

# ==============================================================================
# SECTION 5: HYPOTHESIS AND EFFECT SIZE
# ==============================================================================

cat("\n")
cat("SECTION 4: HYPOTHESIS AND RAW EFFECT SIZE\n")
cat(paste(rep("=", 80), collapse=""), "\n\n")

cat("HYPOTHESIS:\n")
cat("  Japanese speakers will make more false positive errors\n")
cat("  for /l/-/r/ near-homophones (LR)\n")
cat("  compared to phonologically unrelated fillers (F)\n\n")

cat("PREDICTION:\n")
cat("  Error_rate(LR) > Error_rate(F)\n\n")

# Calculate raw effect size
comparison_data <- unrelated_data %>%
  filter(Contrast %in% c("LR", "F"))

raw_effects <- comparison_data %>%
  group_by(Contrast) %>%
  summarise(
    n_trials = n(),
    error_rate = 1 - mean(Words.ACC),
    correct_rejections = sum(Words.ACC == 1),
    false_positives = sum(Words.ACC == 0),
    .groups = "drop"
  )

print(raw_effects)

lr_error <- raw_effects$error_rate[raw_effects$Contrast == "LR"]
f_error <- raw_effects$error_rate[raw_effects$Contrast == "F"]
effect_size <- lr_error - f_error

cat(sprintf("\nRAW EFFECT SIZE:\n"))
cat(sprintf("  LR error rate:       %.3f (%.1f%%)\n", lr_error, lr_error*100))
cat(sprintf("  Filler error rate:   %.3f (%.1f%%)\n", f_error, f_error*100))
cat(sprintf("  Difference (LR - F): %.3f (%.1f percentage points)\n",
            effect_size, effect_size*100))

cat("\nINTERPRETATION:\n")
if (effect_size > 0) {
  cat("  ✓ LR has HIGHER error rate than fillers\n")
  cat("  ✓ SUPPORTS the original hypothesis\n")
  cat("  ✓ Near-homophones DO increase false positive errors\n")
} else {
  cat("  ✗ LR has LOWER error rate than fillers\n")
  cat("  ✗ CONTRADICTS the original hypothesis\n")
}

# ==============================================================================
# SECTION 6: EXPLORATORY VISUALIZATIONS
# ==============================================================================

cat("\n\n")
cat("SECTION 5: EXPLORATORY VISUALIZATIONS\n")
cat(paste(rep("=", 80), collapse=""), "\n\n")

# Create output directory
output_dir <- "/Users/s/Desktop/qml-2025/final project/final project/outputs"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

cat("Creating visualizations...\n\n")

# Plot 1: Raw error rates - stacked bar
p1 <- comparison_data %>%
  mutate(
    response = if_else(Words.ACC == 1, "Correct Rejection", "False Positive"),
    contrast_label = if_else(
      Contrast == "F",
      "Filler Control",
      "/l/-/r/ Ambiguity"
    )
  ) %>%
  ggplot(aes(x = contrast_label, fill = response)) +
  geom_bar(position = "fill", alpha = 0.85, color = "black", linewidth = 1) +
  scale_fill_manual(
    values = c("False Positive" = "#edb9efff", "Correct Rejection" = "#4DAF4A"),
    name = "Response Type"
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "False Positive Errors: /l/-/r/ vs Control",
    subtitle = "L2 Japanese Speakers - Ota et al. (2009) Data",
    x = "Contrast Type",
    y = "Proportion of Responses",
    caption = sprintf("Effect size: %.1f percentage points", effect_size*100)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.major.x = element_blank()
  )

print(p1)
ggsave(file.path(output_dir, "01_error_rates_stacked.png"),
       p1, width = 9, height = 6, dpi = 300)
cat("Saved: 01_error_rates_stacked.png\n")

# Plot 2: Error rates with error bars
p2 <- comparison_data %>%
  group_by(Contrast) %>%
  summarise(
    n = n(),
    error_rate = 1 - mean(Words.ACC),
    se = sqrt((error_rate * (1 - error_rate)) / n),
    ci_lower = error_rate - 1.96 * se,
    ci_upper = error_rate + 1.96 * se,
    .groups = "drop"
  ) %>%
  mutate(
    contrast_label = if_else(
      Contrast == "F",
      "Filler Control",
      "/l/-/r/ Ambiguity"
    )
  ) %>%
  ggplot(aes(x = contrast_label, y = error_rate, fill = contrast_label)) +
  geom_col(alpha = 0.7, width = 0.6, color = "black", linewidth = 1) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.2, color = "black", linewidth = 1.2) +
  geom_text(aes(label = sprintf("%.1f%%\n(n=%d)", error_rate*100, n)),
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_manual(
    values = c("Filler Control" = "#2E86AB", "/l/-/r/ Ambiguity" = "#A23B72"),
    guide = "none"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.35)) +
  labs(
    title = "False Positive Error Rates with 95% Confidence Intervals",
    subtitle = "Frequentist baseline for comparison",
    x = "Contrast Type",
    y = "Error Rate",
    caption = "Error bars: 95% CI (Frequentist)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.major.x = element_blank()
  )

print(p2)
ggsave(file.path(output_dir, "02_error_rates_with_ci.png"),
       p2, width = 9, height = 6, dpi = 300)
cat("Saved: 02_error_rates_with_ci.png\n")

# Plot 3: Subject-level error rates
p3 <- comparison_data %>%
  group_by(Subject, Contrast) %>%
  summarise(
    error_rate = 1 - mean(Words.ACC),
    .groups = "drop"
  ) %>%
  mutate(
    contrast_label = if_else(
      Contrast == "F",
      "Filler Control",
      "/l/-/r/ Ambiguity"
    )
  ) %>%
  ggplot(aes(x = contrast_label, y = error_rate, color = contrast_label)) +
  geom_jitter(width = 0.2, size = 3, alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", size = 6, color = "black",
               shape = 21, fill = "gold", stroke = 2) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1,
               color = "black", linewidth = 1.2) +
  scale_color_manual(
    values = c("Filler Control" = "#2E86AB", "/l/-/r/ Ambiguity" = "#A23B72"),
    guide = "none"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.7)) +
  labs(
    title = "Error Rates by Subject",
    subtitle = "Gold dot = mean | Error bars = SE",
    x = "Contrast Type",
    y = "Error Rate"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.major.x = element_blank()
  )

print(p3)
ggsave(file.path(output_dir, "03_error_rates_by_subject.png"),
       p3, width = 9, height = 6, dpi = 300)
cat("Saved: 03_error_rates_by_subject.png\n")

# ==============================================================================
# SECTION 7: BAYESIAN BERNOULLI REGRESSION - BASIC MODEL
# ==============================================================================

cat("\n\n")
cat("SECTION 6: BAYESIAN BERNOULLI REGRESSION - FIXED EFFECTS MODEL\n")
cat(paste(rep("=", 80), collapse=""), "\n\n")

cat("MODEL SPECIFICATION:\n")
cat("  Dependent variable: Words.ACC (1 = correct, 0 = false positive)\n")
cat("  Predictor: Contrast (LR vs. Filler)\n")
cat("  Model: accuracy ~ Bernoulli(p)\n")
cat("  Link: logit(p) = β₀ + β₁ * LR_coded\n")
cat("  Random effects: (1 | Subject) + (1 | Item)\n\n")

cat("INTERPRETATION:\n")
cat("  β₀ = log-odds of correct response in Filler condition\n")
cat("  β₁ = change in log-odds from Filler to LR\n")
cat("  Negative β₁ = LR DECREASES correct responses (increases errors)\n\n")

# Prepare data for modeling
model_data <- comparison_data %>%
  mutate(
    lr_coded = if_else(Contrast == "LR", 1, 0),
    subject_id = factor(Subject),
    item_id = factor(Item)
  )

cat("Fitting model with brms (MCMC sampling)...\n")
cat("This may take 1-2 minutes...\n\n")

model_fixed <- brm(
  formula = Words.ACC ~ lr_coded + (1 | subject_id) + (1 | item_id),
  family = bernoulli(link = "logit"),
  data = model_data,
  chains = 4,
  iter = 2000,
  warmup = 1000,
  cores = 4,
  seed = 42,
  refresh = 0,
  prior = c(
    prior(normal(0, 1.5), class = "Intercept"),
    prior(normal(0, 1.5), class = "b")
  )
)

cat("\nMODEL FITTING COMPLETE!\n\n")
cat("Model Summary:\n")
print(summary(model_fixed))

# ==============================================================================
# SECTION 8: POSTERIOR ANALYSIS - FIXED EFFECTS
# ==============================================================================

cat("\n\n")
cat("SECTION 7: POSTERIOR DISTRIBUTION ANALYSIS\n")
cat(paste(rep("=", 80), collapse=""), "\n\n")

# Extract posterior draws
posterior_df <- as_draws_df(model_fixed)

# Fixed effects
intercept_post <- posterior_df$b_Intercept
lr_effect_post <- posterior_df$b_lr_coded

cat("INTERCEPT (β₀) - FILLER BASELINE:\n")
cat("-----------\n")
cat(sprintf("Posterior Mean: %.3f\n", mean(intercept_post)))
cat(sprintf("Posterior Median: %.3f\n", median(intercept_post)))
cat(sprintf("95%% CrI: [%.3f, %.3f]\n",
            quantile(intercept_post, 0.025),
            quantile(intercept_post, 0.975)))
cat(sprintf("Implied P(correct | Filler): %.1f%%\n",
            plogis(mean(intercept_post))*100))
cat(sprintf("Implied Error Rate (Filler): %.1f%%\n",
            (1 - plogis(mean(intercept_post)))*100))

cat("\n\n/L/-/R/ EFFECT (β₁):\n")
cat("-----------\n")
cat(sprintf("Posterior Mean: %.3f\n", mean(lr_effect_post)))
cat(sprintf("Posterior Median: %.3f\n", median(lr_effect_post)))
cat(sprintf("95%% CrI: [%.3f, %.3f]\n",
            quantile(lr_effect_post, 0.025),
            quantile(lr_effect_post, 0.975)))

prob_negative <- mean(lr_effect_post < 0)
cat(sprintf("P(β₁ < 0 | data) = %.3f\n", prob_negative))
cat(sprintf("P(β₁ > 0 | data) = %.3f\n", 1 - prob_negative))

cat("\n\nINTERPRETATION:\n")
if (prob_negative > 0.95) {
  cat("✓ STRONG evidence that LR effect is negative\n")
  cat("✓ LR DECREASES probability of correct response\n")
  cat("✓ = LR INCREASES false positive error rate\n")
  cat("✓ SUPPORTS the original hypothesis\n")
} else if (prob_negative > 0.5) {
  cat("? Moderate evidence for LR effect\n")
} else {
  cat("✗ Evidence suggests LR may increase correct responses\n")
  cat("✗ = LR DECREASES error rate\n")
  cat("✗ CONTRADICTS hypothesis\n")
}

# ==============================================================================
# SECTION 9: POSTERIOR VISUALIZATION
# ==============================================================================

cat("\n\n")
cat("SECTION 8: POSTERIOR VISUALIZATION\n")
cat(paste(rep("=", 80), collapse=""), "\n\n")

cat("Creating posterior distribution plots...\n\n")

# Plot 4: Posterior of intercept
p4 <- posterior_df %>%
  ggplot(aes(x = b_Intercept)) +
  geom_density(fill = "skyblue", alpha = 0.7, linewidth = 1.2, color = "darkblue") +
  geom_vline(aes(xintercept = median(b_Intercept)),
             color = "darkblue", linetype = "dashed", linewidth = 1.5) +
  geom_vline(xintercept = 0, color = "red", linetype = "dotted", linewidth = 1) +
  annotate("text", x = Inf, y = Inf,
           label = sprintf("Median = %.3f", median(intercept_post)),
           hjust = 1.05, vjust = 1.5, size = 4, color = "darkblue",
           fontface = "bold") +
  labs(
    title = "Posterior Distribution of Intercept (β₀)",
    subtitle = "Log-odds of correct response in Filler condition",
    x = "β₀ (Log-odds)",
    y = "Density"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

print(p4)
ggsave(file.path(output_dir, "04_posterior_intercept.png"),
       p4, width = 9, height = 6, dpi = 300)
cat("Saved: 04_posterior_intercept.png\n")

# Plot 5: Posterior of slope
p5 <- posterior_df %>%
  ggplot(aes(x = b_lr_coded)) +
  geom_density(fill = "salmon", alpha = 0.7, linewidth = 1.2, color = "darkred") +
  geom_vline(aes(xintercept = median(b_lr_coded)),
             color = "darkred", linetype = "dashed", linewidth = 1.5) +
  geom_vline(xintercept = 0, color = "black", linetype = "dotted", linewidth = 1) +
  annotate("text", x = -Inf, y = Inf,
           label = sprintf("Median = %.3f\nP(β < 0) = %.3f",
                          median(lr_effect_post), prob_negative),
           hjust = -0.05, vjust = 1.5, size = 4, color = "darkred",
           fontface = "bold") +
  labs(
    title = "Posterior Distribution of Slope (β₁)",
    subtitle = "Effect of /l/-/r/ ambiguity on log-odds of correct response",
    x = "β₁ (Log-odds Effect)",
    y = "Density",
    caption = "Negative values support hypothesis: LR increases errors"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

print(p5)
ggsave(file.path(output_dir, "05_posterior_slope.png"),
       p5, width = 9, height = 6, dpi = 300)
cat("Saved: 05_posterior_slope.png\n")

# Plot 6: Joint posterior
p6 <- posterior_df %>%
  slice_sample(n = 1000) %>%
  ggplot(aes(x = b_Intercept, y = b_lr_coded)) +
  geom_point(alpha = 0.3, size = 2, color = "steelblue") +
  geom_density2d(color = "darkblue", linewidth = 0.8, alpha = 0.4) +
  labs(
    title = "Joint Posterior Distribution",
    subtitle = "Intercept vs. Slope",
    x = "β₀ (Intercept)",
    y = "β₁ (Slope)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14))

print(p6)
ggsave(file.path(output_dir, "06_posterior_joint.png"),
       p6, width = 9, height = 6, dpi = 300)
cat("Saved: 06_posterior_joint.png\n")

# Plot 7: Posterior Predictive Check
p7 <- pp_check(model_fixed, ndraws = 100) +
  labs(
    title = "Posterior Predictive Check",
    subtitle = "Dark line = observed data | Light lines = simulated from posterior",
    x = "Accuracy (0 = error, 1 = correct)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14))

print(p7)
ggsave(file.path(output_dir, "07_posterior_predictive_check.png"),
       p7, width = 9, height = 6, dpi = 300)
cat("Saved: 07_posterior_predictive_check.png\n")

# ==============================================================================
# SECTION 10: POSTERIOR PREDICTIONS
# ==============================================================================

cat("\n\n")
cat("SECTION 9: POSTERIOR PREDICTIVE INFERENCE\n")
cat(paste(rep("=", 80), collapse=""), "\n\n")

# Create prediction grid
pred_grid <- expand_grid(
  lr_coded = c(0, 1),
  subject_id = NA,
  item_id = NA
)

cat("Generating posterior predictions for population level...\n\n")

# Get expected predictions
preds <- epred_draws(
  model_fixed,
  newdata = pred_grid,
  re_formula = NA,  # Population-level
  ndraws = 4000
) %>%
  as_tibble() %>%
  rename(prob_correct = .epred) %>%
  mutate(
    error_rate = 1 - prob_correct,
    condition = if_else(lr_coded == 0, "Filler Control", "/l/-/r/ Ambiguity")
  )

# Summary
pred_summary <- preds %>%
  group_by(condition) %>%
  summarise(
    mean_prob_correct = mean(prob_correct),
    mean_error_rate = mean(error_rate),
    median_error_rate = median(error_rate),
    ci_lower = quantile(error_rate, 0.025),
    ci_upper = quantile(error_rate, 0.975),
    .groups = "drop"
  )

cat("PREDICTED ERROR RATES (Population Level):\n")
print(pred_summary)

# Effect size from predictions
pred_filler <- pred_summary$mean_error_rate[pred_summary$condition == "Filler Control"]
pred_lr <- pred_summary$mean_error_rate[pred_summary$condition == "/l/-/r/ Ambiguity"]
pred_effect <- pred_lr - pred_filler

cat(sprintf("\nPREDICTED EFFECT SIZE:\n"))
cat(sprintf("  Filler error rate:    %.3f (%.1f%%)\n", pred_filler, pred_filler*100))
cat(sprintf("  LR error rate:        %.3f (%.1f%%)\n", pred_lr, pred_lr*100))
cat(sprintf("  Difference (LR - F):  %.3f (%.1f percentage points)\n",
            pred_effect, pred_effect*100))

# Plot 8: Posterior predictions
p8 <- preds %>%
  ggplot(aes(x = condition, y = error_rate, fill = condition)) +
  geom_jitter(width = 0.15, alpha = 0.05, size = 1) +
  stat_summary(fun = mean, geom = "point", size = 5, color = "black",
               shape = 21, fill = "gold", stroke = 1.5) +
  stat_summary(fun.data = mean_qi, geom = "errorbar", width = 0.1,
               linewidth = 1.2, color = "black") +
  scale_fill_manual(
    values = c("Filler Control" = "#2E86AB", "/l/-/r/ Ambiguity" = "#A23B72"),
    guide = "none"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.35)) +
  labs(
    title = "Predicted Error Rates with 95% Credible Intervals",
    subtitle = "Bayesian posterior predictions (population-level)",
    x = "Contrast Type",
    y = "Predicted Error Rate",
    caption = "Gold dot = posterior mean | Error bars = 95% CrI"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.major.x = element_blank()
  )

print(p8)
ggsave(file.path(output_dir, "08_posterior_predictions.png"),
       p8, width = 9, height = 6, dpi = 300)
cat("Saved: 08_posterior_predictions.png\n")

# ==============================================================================
# SECTION 11: ADVANCED MODEL - RANDOM SLOPES
# ==============================================================================

cat("\n\n")
cat("SECTION 10: ADVANCED MODEL - RANDOM SLOPES BY SUBJECT\n")
cat(paste(rep("=", 80), collapse=""), "\n\n")

cat("MOTIVATION:\n")
cat("  Do all subjects show the same /l/-/r/ effect?\n")
cat("  Random slopes model allows subject-specific effects\n\n")

cat("MODEL SPECIFICATION:\n")
cat("  Formula: Words.ACC ~ lr_coded + (1 + lr_coded | subject_id) + (1 | item_id)\n")
cat("  Random intercepts: (1 | subject_id) - baseline performance varies by subject\n")
cat("  Random slopes: lr_coded - effect strength varies by subject\n\n")

cat("Fitting random slopes model (this may take 2-3 minutes)...\n\n")

model_slopes <- brm(
  formula = Words.ACC ~ lr_coded + (1 + lr_coded | subject_id) + (1 | item_id),
  family = bernoulli(link = "logit"),
  data = model_data,
  chains = 4,
  iter = 2000,
  warmup = 1000,
  cores = 4,
  seed = 42,
  refresh = 0,
  prior = c(
    prior(normal(0, 1.5), class = "Intercept"),
    prior(normal(0, 1.5), class = "b"),
    prior(exponential(1), class = "sd")
  ),
  control = list(adapt_delta = 0.95)
)

cat("\nRANDOM SLOPES MODEL FITTED!\n\n")
print(summary(model_slopes))

# ==============================================================================
# SECTION 12: MODEL COMPARISON
# ==============================================================================

cat("\n\n")
cat("SECTION 11: MODEL COMPARISON\n")
cat(paste(rep("=", 80), collapse=""), "\n\n")

cat("Comparing fixed effects vs. random slopes models using LOO...\n\n")

loo_fixed <- loo(model_fixed)
loo_slopes <- loo(model_slopes)

cat("Fixed Effects Model LOO:\n")
print(loo_fixed)

cat("\n\nRandom Slopes Model LOO:\n")
print(loo_slopes)

cat("\n\nComparison:\n")
loo_compare(loo_fixed, loo_slopes)

# ==============================================================================
# SECTION 13: COMPREHENSIVE SUMMARY AND REPORT
# ==============================================================================

cat("\n\n")
cat(paste(rep("=", 80), collapse=""), "\n")
cat("COMPREHENSIVE ANALYSIS SUMMARY\n")
cat(paste(rep("=", 80), collapse=""), "\n\n")

cat("RESEARCH QUESTION:\n")
cat("Do L2 Japanese speakers make more false positive errors\n")
cat("for /l/-/r/ near-homophones compared to phonologically unrelated fillers?\n\n")

cat("DATA:\n")
cat(sprintf("- Total trials analyzed: %d\n", nrow(comparison_data)))
cat(sprintf("- Participants: %d Japanese L2 speakers\n", n_distinct(comparison_data$Subject)))
cat(sprintf("- Items: %d word pairs\n", n_distinct(comparison_data$Item)))
cat(sprintf("- Condition: Unrelated trials only (false positives possible)\n\n"))

cat("RAW FINDINGS:\n")
cat(sprintf("- Filler error rate:   %.1f%%\n", f_error*100))
cat(sprintf("- LR error rate:       %.1f%%\n", lr_error*100))
cat(sprintf("- Raw effect size:     %.1f percentage points\n\n", effect_size*100))

cat("BAYESIAN MODEL RESULTS:\n")
cat(sprintf("- β₁ posterior mean:   %.3f\n", mean(lr_effect_post)))
cat(sprintf("- 95%% CrI:            [%.3f, %.3f]\n",
            quantile(lr_effect_post, 0.025),
            quantile(lr_effect_post, 0.975)))
cat(sprintf("- P(β₁ < 0 | data):    %.3f\n", prob_negative))
cat(sprintf("- Predicted effect:    %.1f percentage points\n\n", pred_effect*100))

cat("CONCLUSION:\n")
if (effect_size > 0 && prob_negative > 0.95) {
  cat("✓ STRONG SUPPORT for original hypothesis\n")
  cat("✓ /l/-/r/ near-homophones INCREASE false positive error rates\n")
  cat("✓ Bayesian analysis CONFIRMS Ota et al. (2009) finding\n")
  cat("✓ Effect is robust to modern Bayesian statistical approach\n")
} else if (effect_size < 0 && prob_negative < 0.05) {
  cat("✗ Results CONTRADICT original hypothesis\n")
  cat("✗ This discrepancy requires careful investigation\n")
  cat("✗ Consider: data filtering, coding, or methodological differences\n")
} else {
  cat("? MIXED or INCONCLUSIVE evidence\n")
  cat("? Further investigation recommended\n")
}

cat("\n\n")
cat(paste(rep("=", 80), collapse=""), "\n")
cat("ANALYSIS COMPLETE\n")
cat(paste(rep("=", 80), collapse=""), "\n\n")

cat("Output files saved to:\n")
cat(output_dir, "\n\n")

cat("Generated files:\n")
cat("VISUALIZATIONS (8 plots):\n")
cat("  01_error_rates_stacked.png\n")
cat("  02_error_rates_with_ci.png\n")
cat("  03_error_rates_by_subject.png\n")
cat("  04_posterior_intercept.png\n")
cat("  05_posterior_slope.png\n")
cat("  06_posterior_joint.png\n")
cat("  07_posterior_predictive_check.png\n")
cat("  08_posterior_predictions.png\n\n")

cat(paste(rep("=", 80), collapse=""), "\n\n")
