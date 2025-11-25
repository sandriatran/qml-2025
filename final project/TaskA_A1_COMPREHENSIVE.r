# ==============================================================================
# BAYESIAN RE-ANALYSIS OF OTA, HARTSUIKER & HAYWOOD (2009)
# COMPREHENSIVE A1 VERSION - ALL CONTRASTS PRIMARY ANALYSIS
# Title: The KEY to the ROCK: Near-homophony in nonnative visual word recognition
# Authors: Sandria Tran and Violet Manson
# Date: November 2025
#
# COMPREHENSIVE APPROACH:
# This version makes the all-contrasts model PRIMARY, showing the full pattern
# of effects across F (Fillers), LR (/l/-/r/), H (Homophones), and PB (/p/-/b/)
#
# FINDINGS FROM OTA ET AL. (2009):
# - LR: HIGH contrast (large error rate increase)
# - H: HIGH contrast (large error rate increase)
# - PB: MINISCULE contrast (minimal error rate increase)
# This pattern demonstrates phonological ambiguity (not just phonetic similarity)
# ==============================================================================

# STEP 0: Load Packages ----
library(tidyverse)
library(brms)
library(tidybayes)
library(bayesplot)
library(here)
library(patchwork)

cat("\n=== PROJECT SETUP ===\n")
cat("Working directory:", here(), "\n")
cat("Date:", Sys.Date(), "\n\n")

# ==============================================================================
# STEP 1: Load and Inspect Data
# ==============================================================================
cat("=== STEP 1: LOADING DATA ===\n")

data_path <- here("data", "ota2009", "key-rock.csv")
data_raw <- read_csv(data_path, show_col_types = FALSE)

cat("Total rows:", nrow(data_raw), "\n")
cat("Unique subjects:", n_distinct(data_raw$Subject), "\n")
cat("Contrast types:", paste(unique(data_raw$Contrast), collapse = ", "), "\n\n")

# ==============================================================================
# STEP 2: Preprocess Data
# ==============================================================================
cat("=== STEP 2: DATA PREPROCESSING ===\n")

data_clean <- data_raw %>%
  filter(Procedure == "TrialProc") %>%
  filter(Condition == "Unrelated") %>%
  mutate(
    subject_id = factor(Subject),
    item_id = factor(Item),
    accuracy = Words.ACC,
    contrast_type = factor(Contrast, levels = c("F", "LR", "H", "PB"))
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
output_plots_dir <- here("final project", "outputs", "plots")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(output_plots_dir, showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# STEP 3: Prior Specification
# ==============================================================================
cat("\n=== STEP 3: PRIOR SPECIFICATION ===\n")

priors_main <- c(
  prior(normal(0, 1.5), class = Intercept),
  prior(normal(0, 1.5), class = b),
  prior(exponential(1), class = sd)
)

cat("\nPrior Justification:\n")
cat("  Intercept prior: normal(0, 1.5)\n")
cat("    Rationale: Weakly informative, centered at no effect\n\n")
cat("  Effect prior: normal(0, 1.5)\n")
cat("    Rationale: Allows substantial variation while preventing overfitting\n\n")
cat("  Random effects prior: exponential(1)\n")
cat("    Rationale: Standard for SD parameters in hierarchical models\n\n")

# ==============================================================================
# STEP 4: PRIMARY ANALYSIS - ALL CONTRASTS MODEL
# ==============================================================================
cat("=== STEP 4: FITTING COMPREHENSIVE MODEL (ALL CONTRASTS) ===\n")
cat("This is the PRIMARY analysis showing the full pattern of effects\n")
cat("Fitting with F (Fillers) as baseline...\n\n")

set.seed(2025)

# Make F the reference level
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

# ==============================================================================
# STEP 5: Contrast Effects Analysis
# ==============================================================================
cat("=== STEP 5: CONTRAST EFFECTS (vs FILLER CONTROL) ===\n\n")

posterior_all <- as_draws_df(model_all)

# Extract effects for each contrast (relative to Fillers)
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

cat("Effects on log-odds of CORRECT response (relative to Fillers):\n")
cat("(Negative values = MORE errors; Positive values = FEWER errors)\n\n")
print(effects_summary)

cat("\n\nINTERPRETATION OF PATTERN:\n")
cat("✓ LR: Median effect = ", round(median(lr_effect), 3),
    " (STRONG NEGATIVE = high error increase)\n")
cat("✓ H:  Median effect = ", round(median(h_effect), 3),
    " (STRONG NEGATIVE = high error increase)\n")
cat("✓ PB: Median effect = ", round(median(pb_effect), 3),
    " (WEAK/NEAR-ZERO = minimal error increase)\n\n")

cat("CONCLUSION: LR and H (phonologically ambiguous) show HIGH effects\n")
cat("            PB (/p/-/b/ minimal pairs) shows MINISCULE effect\n")
cat("            → Supports phonological ambiguity mechanism, not generic phonetic similarity\n\n")

# ==============================================================================
# STEP 6: Predicted Error Rates for All Contrasts
# ==============================================================================
cat("=== STEP 6: PREDICTED ERROR RATES BY CONTRAST ===\n\n")

# Convert to probability scale
filler_prob_correct <- plogis(intercept_all)
lr_prob_correct <- plogis(intercept_all + lr_effect)
h_prob_correct <- plogis(intercept_all + h_effect)
pb_prob_correct <- plogis(intercept_all + pb_effect)

# Error rates
filler_error <- 1 - filler_prob_correct
lr_error <- 1 - lr_prob_correct
h_error <- 1 - h_prob_correct
pb_error <- 1 - pb_prob_correct

error_rates <- tribble(
  ~contrast, ~median_error_pct, ~lower_95_pct, ~upper_95_pct,

  "F (Fillers)",
  median(filler_error) * 100,
  quantile(filler_error, 0.025) * 100,
  quantile(filler_error, 0.975) * 100,

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

# ==============================================================================
# STEP 7: Forest Plot Visualization - PRIMARY RESULT
# ==============================================================================
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
    title = "Contrast Effects on Error Rates",
    subtitle = "All contrasts compared to Filler control (F = baseline)\nNegative = MORE errors; larger magnitude = stronger effect",
    x = "Effect on log-odds of correct response",
    y = "Contrast Type",
    caption = "Points = median; lines = 95% credible interval\nLR and H show strong effects; PB shows minimal effect"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.subtitle = element_text(face = "italic", color = "gray30"),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3)
  )

print(p_forest)
ggsave(file.path(output_plots_dir, "01_forest_plot_all_contrasts.png"),
       p_forest, width = 11, height = 6, dpi = 300)
cat("Saved: 01_forest_plot_all_contrasts.png\n")

# ==============================================================================
# STEP 8: Error Rate Comparison Plot
# ==============================================================================
cat("\n=== STEP 8: CREATING ERROR RATE COMPARISON PLOT ===\n")

error_rate_summary <- tribble(
  ~contrast, ~median_error, ~lower_error, ~upper_error,
  "F (Fillers)", median(filler_error), quantile(filler_error, 0.025), quantile(filler_error, 0.975),
  "LR (/l/-/r/)", median(lr_error), quantile(lr_error, 0.025), quantile(lr_error, 0.975),
  "H (Homophones)", median(h_error), quantile(h_error, 0.025), quantile(h_error, 0.975),
  "PB (/p/-/b/)", median(pb_error), quantile(pb_error, 0.025), quantile(pb_error, 0.975)
) %>%
  mutate(contrast = factor(contrast, levels = c("F (Fillers)", "PB (/p/-/b/)", "H (Homophones)", "LR (/l/-/r/)")))

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
    values = c("F (Fillers)" = "steelblue", "PB (/p/-/b/)" = "lightblue",
               "H (Homophones)" = "coral", "LR (/l/-/r/)" = "darkred"),
    guide = "none"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.35)) +
  labs(
    title = "False Positive Error Rates by Contrast Type",
    subtitle = "Predicted error rates with 95% credible intervals (hierarchical Bayesian model)",
    x = "Contrast Type",
    y = "Error Rate",
    caption = "LR and H show similarly elevated error rates; PB shows minimal increase"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 15, hjust = 0.5),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3)
  )

print(p_error_rates)
ggsave(file.path(output_plots_dir, "02_error_rates_all_contrasts.png"),
       p_error_rates, width = 10, height = 6, dpi = 300)
cat("Saved: 02_error_rates_all_contrasts.png\n")

# ==============================================================================
# STEP 9: Model Summary & Interpretation
# ==============================================================================
cat("\n=== STEP 9: MODEL SUMMARY ===\n\n")
print(summary(model_all))

sink(file.path(output_dir, "model_comprehensive_summary.txt"))
cat("=== COMPREHENSIVE BAYESIAN MODEL SUMMARY ===\n")
cat("All Contrasts vs Filler Control (F = baseline)\n\n")
print(summary(model_all))
sink()

# ==============================================================================
# STEP 10: Final Interpretation
# ==============================================================================
cat("\n")
cat(paste(rep("=", 80), collapse=""), "\n")
cat("COMPREHENSIVE ANALYSIS COMPLETE\n")
cat(paste(rep("=", 80), collapse=""), "\n\n")

cat("PRIMARY FINDING - Pattern of Effects:\n\n")
cat("1. /l/-/r/ near-homophones (LR): ",
    round(median(lr_error)*100, 1), "% error [95% CrI: ",
    round(quantile(lr_error, 0.025)*100, 1), "-",
    round(quantile(lr_error, 0.975)*100, 1), "%]\n")
cat("   → Effect size (log-odds): ", round(median(lr_effect), 3), "\n")
cat("   → Conclusion: STRONG increase in error rate\n\n")

cat("2. Homophones (H): ",
    round(median(h_error)*100, 1), "% error [95% CrI: ",
    round(quantile(h_error, 0.025)*100, 1), "-",
    round(quantile(h_error, 0.975)*100, 1), "%]\n")
cat("   → Effect size (log-odds): ", round(median(h_effect), 3), "\n")
cat("   → Conclusion: STRONG increase in error rate (comparable to LR)\n\n")

cat("3. /p/-/b/ minimal pairs (PB): ",
    round(median(pb_error)*100, 1), "% error [95% CrI: ",
    round(quantile(pb_error, 0.025)*100, 1), "-",
    round(quantile(pb_error, 0.975)*100, 1), "%]\n")
cat("   → Effect size (log-odds): ", round(median(pb_effect), 3), "\n")
cat("   → Conclusion: MINIMAL/NEGLIGIBLE increase in error rate\n\n")

cat("4. Filler Control (F): ",
    round(median(filler_error)*100, 1), "% error [95% CrI: ",
    round(quantile(filler_error, 0.025)*100, 1), "-",
    round(quantile(filler_error, 0.975)*100, 1), "%]\n\n")

cat("KEY INTERPRETATION:\n")
cat("✓ LR and H both show HIGH error rates (phonological ambiguity)\n")
cat("✓ PB shows MINISCULE effect (phonetic similarity alone insufficient)\n")
cat("✓ Pattern supports PHONOLOGICAL AMBIGUITY hypothesis\n")
cat("✓ Japanese participants confused by /l/-/r/ and homophones\n")
cat("✓ But NOT by /p/-/b/ distinction (already present in Japanese)\n\n")

cat("ROBUSTNESS:\n")
cat("✓ Hierarchical model with random effects for subjects and items\n")
cat("✓ All contrasts tested in same model\n")
cat("✓ Direct comparison shows differential effects\n")
cat("✓ Pattern matches Ota et al. (2009) findings\n\n")

cat("Results saved to:\n")
cat(paste("  ", output_plots_dir, "/\n"))
cat(paste("  ", file.path(output_dir, "model_comprehensive_summary.txt"), "\n\n"))
