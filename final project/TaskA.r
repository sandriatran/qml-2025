# ==============================================================================
# BAYESIAN RE-ANALYSIS OF OTA, HARTSUIKER & HAYWOOD (2009)
# Complete, Easy-to-Read, Runnable Script
# Title: The KEY to the ROCK: Near-homophony in nonnative visual word recognition
# Authors: Sandria Tran and Violet Manson
# Date: November 2025
# ==============================================================================
#
# GOAL: Test the robustness of Ota et al. (2009) findings using Bayesian methods
#
# ORIGINAL STUDY:
# - Japanese speakers showed MORE false positive errors for /l/-/r/ near-homophones
#   (e.g., ROCK-LOCK) compared to spelling controls
# - This was interpreted as evidence that L1 phonology affects L2 lexical representations
#
# OUR APPROACH:
# - Translate the frequentist ANOVA into a Bayesian hierarchical model
# - Use a Bernoulli regression with crossed random effects for subjects and items
# - Carefully isolate the LR vs Control comparison
#
# ==============================================================================

# STEP 0: Load Packages ----
library(tidyverse)   # For data manipulation and visualization
library(brms)        # For Bayesian regression models
library(tidybayes)   # For working with posterior distributions
library(bayesplot)   # For diagnostic plots
library(here)        # For clean project-relative paths

# STEP 1: Load and Inspect the Data ----
cat("\n=== STEP 1: LOADING DATA ===\n")

# Read the data
data_raw <- read_csv("/Users/s/Desktop/qml-2025/data/ota2009/key-rock.csv",
                     show_col_types = FALSE)

# Quick look at the structure
cat("Total rows:", nrow(data_raw), "\n")
cat("Unique subjects:", n_distinct(data_raw$Subject), "\n")
cat("Contrast types:", paste(unique(data_raw$Contrast), collapse = ", "), "\n")
cat("Conditions:", paste(unique(data_raw$Condition), collapse = ", "), "\n\n")

# ==============================================================================
# STEP 2: Preprocess the Data ----
# ==============================================================================
#
# KEY DECISIONS:
# 1. We only analyze TEST trials (not practice)
# 2. We only analyze UNRELATED trials (where false positives can occur)
# 3. We compare LR (/l/-/r/ near-homophones) to F (spelling controls)
#
# IMPORTANT: In the Ota et al. (2009) data:
#   - LR = /l/-/r/ near-homophones (experimental condition)
#   - F = Filler/Control trials (baseline condition)
#   - H = Homophones
#   - PB = /p/-/b/ minimal pairs
#
# ==============================================================================

cat("=== STEP 2: DATA PREPROCESSING ===\n")

data_clean <- data_raw %>%
  # Filter for test trials only
  filter(Procedure == "TrialProc") %>%

  # Filter for unrelated trials only (where false positives can occur)
  filter(Condition == "Unrelated") %>%

  # Create clean variable names
  mutate(
    subject_id = factor(Subject),
    item_id = factor(Item),
    accuracy = Words.ACC,  # 1 = correct rejection, 0 = false positive (error)

    # Create the key predictor variable
    contrast_type = factor(Contrast, levels = c("F", "LR", "H", "PB"))
  )

# Check the data
cat("\nContrast distribution in UNRELATED trials:\n")
data_clean %>%
  group_by(contrast_type) %>%
  summarise(
    n_trials = n(),
    n_subjects = n_distinct(subject_id),
    n_items = n_distinct(item_id),
    error_rate = mean(accuracy == 0),
    .groups = "drop"
  ) %>%
  print()

# ==============================================================================
# STEP 3: Create the Analysis Dataset ----
# ==============================================================================
#
# For the MAIN analysis, we compare ONLY LR vs F (Control)
# This matches the original study's key comparison
# ==============================================================================

cat("\n=== STEP 3: CREATING ANALYSIS DATASET ===\n")

data_analysis <- data_clean %>%
  filter(contrast_type %in% c("F", "LR")) %>%
  mutate(
    # Create a simple binary predictor: 0 = Control, 1 = LR
    is_lr = ifelse(contrast_type == "LR", 1, 0),

    # Also keep the factor version for easier interpretation
    condition = factor(contrast_type, levels = c("F", "LR"),
                      labels = c("Control", "LR"))
  )

cat("\nAnalysis dataset (LR vs Control):\n")
summary_stats <- data_analysis %>%
  group_by(condition) %>%
  summarise(
    n_trials = n(),
    n_subjects = n_distinct(subject_id),
    n_items = n_distinct(item_id),
    n_errors = sum(accuracy == 0),
    error_rate = mean(accuracy == 0),
    .groups = "drop"
  )
print(summary_stats)

# Create output directory
output_dir <- here("final project", "outputs")
output_plots_dir <- here("final project", "outputs", "plots")
dir.create(output_dir, showWarnings = FALSE)
dir.create(output_plots_dir, showWarnings = FALSE)

# ==============================================================================
# STEP 4: Visualize the Raw Data ----
# ==============================================================================

cat("\n=== STEP 4: CREATING VISUALIZATIONS ===\n")

# Plot 1: Raw error rates
p1 <- data_analysis %>%
  mutate(response = ifelse(accuracy == 1, "Correct Rejection", "False Positive")) %>%
  ggplot(aes(x = condition, fill = response)) +
  geom_bar(position = "fill", alpha = 0.85, color = "black", linewidth = 1) +
  scale_fill_manual(values = c("False Positive" = "#E74C3C", "Correct Rejection" = "#2ECC71")) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "False Positive Error Rates: LR vs Control",
    subtitle = "Japanese L2 speakers, unrelated word pairs",
    x = "Condition",
    y = "Proportion of Responses",
    fill = "Response Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

print(p1)
ggsave(file.path(output_plots_dir, "01_raw_error_rates.png"), p1, width = 9, height = 6, dpi = 300)
cat("Saved: 01_raw_error_rates.png\n")

# Plot 2: Error rates with confidence intervals
p2 <- summary_stats %>%
  mutate(
    se = sqrt(error_rate * (1 - error_rate) / n_trials),
    ci_lower = error_rate - 1.96 * se,
    ci_upper = error_rate + 1.96 * se
  ) %>%
  ggplot(aes(x = condition, y = error_rate)) +
  geom_col(fill = "steelblue", alpha = 0.7, width = 0.6, color = "black", linewidth = 1) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, linewidth = 1.2) +
  geom_text(aes(label = sprintf("%.1f%%\n(n=%d)", error_rate * 100, n_trials)),
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.35)) +
  labs(
    title = "False Positive Error Rates with 95% CI",
    subtitle = "Frequentist point estimates (proportions)",
    x = "Condition",
    y = "Error Rate"
  ) +
  theme_minimal(base_size = 14)

print(p2)
ggsave(file.path(output_plots_dir, "02_error_rates_ci.png"), p2, width = 9, height = 6, dpi = 300)
cat("Saved: 02_error_rates_ci.png\n")

# ==============================================================================
# STEP 5: Fit the Bayesian Model ----
# ==============================================================================
#
# MODEL SPECIFICATION:
#
# accuracy ~ Bernoulli(p)
# logit(p) = β₀ + β₁ * is_lr + u_subject + u_item
#
# where:
#   - accuracy = 1 (correct rejection) or 0 (false positive/error)
#   - β₀ = intercept (log-odds of correct response in Control condition)
#   - β₁ = effect of LR condition (change in log-odds)
#   - u_subject = random intercept for each subject
#   - u_item = random intercept for each item
#
# INTERPRETATION:
#   - If β₁ is NEGATIVE: LR condition has LOWER log-odds of correct response
#     → HIGHER error rate → SUPPORTS the original hypothesis
#   - If β₁ is POSITIVE: LR condition has HIGHER log-odds of correct response
#     → LOWER error rate → CONTRADICTS the original hypothesis
#
# ==============================================================================

cat("\n=== STEP 5: FITTING BAYESIAN MODEL ===\n")
cat("This may take 5-10 minutes...\n\n")

# Set priors (weakly informative)
priors <- c(
  prior(normal(0, 1.5), class = Intercept),  # Intercept prior
  prior(normal(0, 1.5), class = b),          # Slope prior
  prior(exponential(1), class = sd)          # Random effects SD prior
)

# Fit the model
set.seed(2025)
model <- brm(
  formula = accuracy ~ is_lr + (1 | subject_id) + (1 | item_id),
  data = data_analysis,
  family = bernoulli(link = "logit"),
  prior = priors,
  iter = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  seed = 2025,
  refresh = 0,
  file = file.path(output_dir, "bayesian_model")  # Save the model
)

# Print summary
cat("\n=== MODEL SUMMARY ===\n\n")
print(summary(model))

# Save summary to file
sink(file.path(output_dir, "model_summary.txt"))
summary(model)
sink()

cat("\n✓ Model fitted successfully\n")

# ==============================================================================
# STEP 6: Extract and Visualize Posterior Distributions ----
# ==============================================================================

cat("\n=== STEP 6: POSTERIOR ANALYSIS ===\n")

# Extract posterior draws
posterior <- as_draws_df(model)

# Get the key parameters
intercept_draws <- posterior$b_Intercept
lr_effect_draws <- posterior$b_is_lr

# Summary statistics
cat("\nIntercept (β₀) - log-odds of correct response in Control:\n")
cat("  Median:", round(median(intercept_draws), 3), "\n")
cat("  95% CrI: [", round(quantile(intercept_draws, 0.025), 3), ",",
    round(quantile(intercept_draws, 0.975), 3), "]\n")

cat("\nLR Effect (β₁) - change in log-odds for LR condition:\n")
cat("  Median:", round(median(lr_effect_draws), 3), "\n")
cat("  95% CrI: [", round(quantile(lr_effect_draws, 0.025), 3), ",",
    round(quantile(lr_effect_draws, 0.975), 3), "]\n")
cat("  P(β₁ < 0) =", round(mean(lr_effect_draws < 0), 3),
    "(probability that LR increases errors)\n\n")

# Plot 3: Posterior of intercept
p3 <- ggplot(data.frame(x = intercept_draws), aes(x = x)) +
  geom_density(fill = "steelblue", alpha = 0.7, linewidth = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  geom_vline(xintercept = median(intercept_draws), color = "darkblue", linewidth = 1.5) +
  annotate("text", x = median(intercept_draws), y = Inf,
           label = sprintf("Median = %.3f", median(intercept_draws)),
           vjust = 1.5, hjust = -0.1, color = "darkblue", size = 5, fontface = "bold") +
  labs(
    title = "Posterior Distribution: Intercept (β₀)",
    subtitle = "Log-odds of correct response in Control condition",
    x = "β₀ (log-odds)",
    y = "Density"
  ) +
  theme_minimal(base_size = 14)

print(p3)
ggsave(file.path(output_plots_dir, "03_posterior_intercept.png"), p3, width = 9, height = 6, dpi = 300)
cat("Saved: 03_posterior_intercept.png\n")

# Plot 4: Posterior of LR effect
p4 <- ggplot(data.frame(x = lr_effect_draws), aes(x = x)) +
  geom_density(fill = "coral", alpha = 0.7, linewidth = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1.5) +
  geom_vline(xintercept = median(lr_effect_draws), color = "darkred", linewidth = 1.5) +
  annotate("text", x = median(lr_effect_draws), y = Inf,
           label = sprintf("Median = %.3f\nP(β₁ < 0) = %.3f",
                          median(lr_effect_draws),
                          mean(lr_effect_draws < 0)),
           vjust = 1.5,
           hjust = ifelse(median(lr_effect_draws) > 0, -0.1, 1.1),
           color = "darkred", size = 5, fontface = "bold") +
  labs(
    title = "Posterior Distribution: LR Effect (β₁)",
    subtitle = "Effect of /l/-/r/ ambiguity on log-odds of correct response",
    x = "β₁ (log-odds)",
    y = "Density",
    caption = "Negative values = MORE errors in LR (supports hypothesis)"
  ) +
  theme_minimal(base_size = 14)

print(p4)
ggsave(file.path(output_plots_dir, "04_posterior_lr_effect.png"), p4, width = 9, height = 6, dpi = 300)
cat("Saved: 04_posterior_lr_effect.png\n")

# Plot 5: Posterior predictive check
p5 <- pp_check(model, ndraws = 100) +
  labs(title = "Posterior Predictive Check") +
  theme_minimal(base_size = 14)

print(p5)
ggsave(file.path(output_plots_dir, "05_posterior_predictive_check.png"), p5, width = 9, height = 6, dpi = 300)
cat("Saved: 05_posterior_predictive_check.png\n")

# ==============================================================================
# STEP 7: Calculate Predicted Error Rates ----
# ==============================================================================
#
# This is the KEY analysis: What are the predicted error rates for each condition?
# ==============================================================================

cat("\n=== STEP 7: PREDICTED ERROR RATES ===\n")

# Convert log-odds to probabilities for each posterior draw
control_prob_correct <- plogis(intercept_draws)
lr_prob_correct <- plogis(intercept_draws + lr_effect_draws)

# Error rates are 1 - probability of correct
control_error_rate <- 1 - control_prob_correct
lr_error_rate <- 1 - lr_prob_correct

# Difference in error rates
error_rate_difference <- lr_error_rate - control_error_rate

# Summarize
cat("\nControl condition (predicted):\n")
cat("  Median:", round(median(control_error_rate), 3),
    "(", round(median(control_error_rate) * 100, 1), "%)\n")
cat("  95% CrI: [", round(quantile(control_error_rate, 0.025), 3), ",",
    round(quantile(control_error_rate, 0.975), 3), "]\n")

cat("\nLR condition (predicted):\n")
cat("  Median:", round(median(lr_error_rate), 3),
    "(", round(median(lr_error_rate) * 100, 1), "%)\n")
cat("  95% CrI: [", round(quantile(lr_error_rate, 0.025), 3), ",",
    round(quantile(lr_error_rate, 0.975), 3), "]\n")

cat("\nDifference (LR - Control):\n")
cat("  Median:", round(median(error_rate_difference), 3),
    "(", round(median(error_rate_difference) * 100, 1), "percentage points)\n")
cat("  95% CrI: [", round(quantile(error_rate_difference, 0.025), 3), ",",
    round(quantile(error_rate_difference, 0.975), 3), "]\n")
cat("  P(LR > Control) =", round(mean(error_rate_difference > 0), 3),
    "(", round(mean(error_rate_difference > 0) * 100, 1), "%)\n\n")

# Plot 6: Distribution of the difference
p6 <- ggplot(data.frame(x = error_rate_difference), aes(x = x)) +
  geom_density(fill = "purple", alpha = 0.7, linewidth = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1.5) +
  geom_vline(xintercept = median(error_rate_difference),
             color = "darkviolet", linewidth = 1.5) +
  annotate("text", x = median(error_rate_difference), y = Inf,
           label = sprintf("Median = %.1f%%\nP(LR > Control) = %.1f%%",
                          median(error_rate_difference) * 100,
                          mean(error_rate_difference > 0) * 100),
           vjust = 1.5,
           hjust = ifelse(median(error_rate_difference) > 0, -0.1, 1.1),
           color = "darkviolet", size = 5, fontface = "bold") +
  scale_x_continuous(labels = scales::percent) +
  labs(
    title = "Posterior Distribution: Difference in Error Rates",
    subtitle = "LR Error Rate - Control Error Rate",
    x = "Difference in Error Rate",
    y = "Density",
    caption = "Positive values = LR has MORE errors (supports hypothesis)"
  ) +
  theme_minimal(base_size = 14)

print(p6)
ggsave(file.path(output_plots_dir, "06_difference_distribution.png"), p6, width = 9, height = 6, dpi = 300)
cat("Saved: 06_difference_distribution.png\n")

# Plot 7: Predicted error rates with uncertainty
pred_data <- data.frame(
  condition = rep(c("Control", "LR"), each = length(control_error_rate)),
  error_rate = c(control_error_rate, lr_error_rate)
)

p7 <- pred_data %>%
  ggplot(aes(x = condition, y = error_rate)) +
  geom_jitter(alpha = 0.05, width = 0.2, color = "gray50") +
  stat_summary(fun = median, geom = "point", size = 5, color = "gold", shape = 21, stroke = 2) +
  stat_summary(fun.data = function(x) {
    data.frame(
      y = median(x),
      ymin = quantile(x, 0.025),
      ymax = quantile(x, 0.975)
    )
  }, geom = "errorbar", width = 0.2, linewidth = 1.2, color = "black") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Predicted Error Rates with 95% Credible Intervals",
    subtitle = "Bayesian posterior predictions",
    x = "Condition",
    y = "Predicted Error Rate",
    caption = "Gold dot = median | Error bars = 95% CrI"
  ) +
  theme_minimal(base_size = 14)

print(p7)
ggsave(file.path(output_plots_dir, "07_predicted_error_rates.png"), p7, width = 9, height = 6, dpi = 300)
cat("Saved: 07_predicted_error_rates.png\n")

# ==============================================================================
# STEP 8: Final Interpretation ----
# ==============================================================================

cat("\n")
cat(paste(rep("=", 80), collapse=""), "\n")
cat("FINAL INTERPRETATION\n")
cat(paste(rep("=", 80), collapse=""), "\n\n")

cat("ORIGINAL HYPOTHESIS (Ota et al., 2009):\n")
cat("  Japanese speakers should make MORE errors for /l/-/r/ near-homophones\n")
cat("  compared to spelling controls.\n\n")

cat("OUR BAYESIAN RESULTS:\n\n")
cat(sprintf("Control condition:     %.1f%% error rate [95%% CrI: %.1f%%, %.1f%%]\n",
            median(control_error_rate) * 100,
            quantile(control_error_rate, 0.025) * 100,
            quantile(control_error_rate, 0.975) * 100))

cat(sprintf("LR condition:          %.1f%% error rate [95%% CrI: %.1f%%, %.1f%%]\n\n",
            median(lr_error_rate) * 100,
            quantile(lr_error_rate, 0.025) * 100,
            quantile(lr_error_rate, 0.975) * 100))

cat(sprintf("Difference:            %.1f percentage points [95%% CrI: %.1f, %.1f]\n\n",
            median(error_rate_difference) * 100,
            quantile(error_rate_difference, 0.025) * 100,
            quantile(error_rate_difference, 0.975) * 100))

# Determine the conclusion
prob_supports <- mean(error_rate_difference > 0)

cat("CONCLUSION:\n")
if (prob_supports > 0.95) {
  cat("  ✓✓✓ STRONG SUPPORT for the original hypothesis\n")
  cat(sprintf("  There is a %.1f%% probability that LR increases error rates.\n\n",
              prob_supports * 100))
  cat("  INTERPRETATION:\n")
  cat("    - /l/-/r/ near-homophones DO increase false positive errors\n")
  cat("    - Effect is robust to Bayesian hierarchical modeling\n")
  cat("    - Supports L1 phonology affecting L2 lexical representations\n")
  cat("    - Confirms representational indeterminacy hypothesis\n")
} else if (prob_supports > 0.80) {
  cat("  ✓ MODERATE SUPPORT for the original hypothesis\n")
  cat(sprintf("  There is a %.1f%% probability that LR increases error rates.\n\n",
              prob_supports * 100))
} else if (prob_supports < 0.20) {
  cat("  ✗ EVIDENCE AGAINST the original hypothesis\n")
  cat(sprintf("  There is only a %.1f%% probability that LR increases error rates.\n\n",
              prob_supports * 100))
} else {
  cat("  ? INCONCLUSIVE EVIDENCE\n")
  cat(sprintf("  There is a %.1f%% probability that LR increases error rates.\n\n",
              prob_supports * 100))
}

cat("\nROBUSTNESS:\n")
cat("  The Bayesian hierarchical model accounts for:\n")
cat("  - Variability across subjects (random intercepts)\n")
cat("  - Variability across items (random intercepts)\n")
cat("  - Uncertainty in all parameter estimates\n")
cat("  - Proper handling of binary outcome data (Bernoulli likelihood)\n\n")

# Save interpretation to file
sink(file.path(output_dir, "interpretation.txt"))
cat("=== BAYESIAN RE-ANALYSIS OF OTA ET AL. (2009) ===\n\n")
cat(sprintf("Control error rate: %.1f%% [95%% CrI: %.1f%%, %.1f%%]\n",
            median(control_error_rate) * 100,
            quantile(control_error_rate, 0.025) * 100,
            quantile(control_error_rate, 0.975) * 100))
cat(sprintf("LR error rate: %.1f%% [95%% CrI: %.1f%%, %.1f%%]\n",
            median(lr_error_rate) * 100,
            quantile(lr_error_rate, 0.025) * 100,
            quantile(lr_error_rate, 0.975) * 100))
cat(sprintf("Difference: %.1f percentage points [95%% CrI: %.1f, %.1f]\n\n",
            median(error_rate_difference) * 100,
            quantile(error_rate_difference, 0.025) * 100,
            quantile(error_rate_difference, 0.975) * 100))
cat(sprintf("Probability that LR has MORE errors: %.1f%%\n", prob_supports * 100))
sink()

cat(paste(rep("=", 80), collapse=""), "\n")
cat("ANALYSIS COMPLETE!\n")
cat(paste(rep("=", 80), collapse=""), "\n\n")
cat("Results saved to:\n")
cat(paste("  - Plots:", file.path(output_plots_dir), "\n"))
cat(paste("  - Model:", file.path(output_dir, "bayesian_model"), "\n"))
cat(paste("  - Summary:", file.path(output_dir, "model_summary.txt"), "\n"))
cat(paste("  - Interpretation:", file.path(output_dir, "interpretation.txt"), "\n\n"))
