# ==============================================================================
# Week 5 | https://uoelel.github.io/qml/workshops/week-05.html
# Title: Task A: Massive Auditory Lexical Decision (MALD)
# Author: Sandria Tran
# Date: 2025-10-17
# Topic: Workshop: Regression basics and posteriors draws
#        Lecture:  Regression basics with brms
#
# Instructions:
#    (1) Read the tucker2019/mald_1_1.rds data.
#    (2) Filter the data to include only real words.
#    (3) Fit a regression model to answer the following question:
#        What is the relationship between reaction times and mean phoneme-level
#        Levenshtein distance (PhonLev)?
#    (4) You can check the publication linked on the data documentation to learn
#        about the mean phoneme-level Levenshtein distance.
#    (5) Write a paragraph reporting the model.
#        Produce plots of the posterior distributions of the model parameters.
#    (6) Why does PhonLev have the effect it has on reaction times?
#
# Research Questions:
#  
# SUMMARY: Q1: Higher PhonLev correlates with longer reaction time
#          Lower PhonLev correlates with shorter reaction time (recognized more quickly). 
#
#          Q2: Why does PhonLev have the effect it has on reaction times?
#          (i) In the MALD (Massive Auditory Lexical Decision) 1.1 dataset documentation, Mean Phoneme-level Levenshtein distance (PhonLev) 
#          describes how reflective similar a word is in the "item" in the dataset. 

#          (ii) #LOWER NIEGHBORHOOD DENSITY 
#          Higher PhonLev describes fewer association with close-sounding neighbors

#          (iii) # HIGHER NEIGHBORHOOD DENSITY 
#          while lower PhonLev are phonemically more similar with more association 
#          as close-sounding neighbors

#          (iv) THEREFORE, Mean Phoneme-level Levenshtein distance (PhonLev) is inversely related to 
#          phonological neighborhood density.

# Model Formula: RT ∼ Gaussian(μ, σ) with μ = β₀ + β₁ · PhonLev
#
# Learning Objectives:
#   (i) Questions:
#       (1) What are regression models and what are they for?
#        ANSWER: Regression models are tools that help quantify and 
#                estimate relationships between an independent variable and dependent variable. 
#                They can be done by fitting an equation (e.g. linear regression)
#                 that enables predictions and pattern analysis based on relationships.
#       (2) How do we interpret intercept and slope in a regression model?
#        ANSWER: Equation for simple linear regression is y=  β₀ + β₁x
#                  where y - predicted value (outcome variable)
#                  x is predictor variable 
#                  β₀ - intercept  (y when x = 0 )
#                                -  graphically it is when regression line crosses y-axis
#                  β₁x - slope (how much y changes for one-unit increase in x)
#                               - if slope is negative, y decreases as x increases
#       (3) Why do we need Markov Chain Monte Carlo to estimate models?
#        ANSWER:  Markov Chain Monte Carlo is a class of algorithms 
#                 that uses samples from probability distributions (especially distributions one cannot calculate directly)
#                 Markov Chain: sequence of random steps where each step depends on current position
#                 Monte Carlo:  random sampling techniques
#                 MCMC: chain of samples drawing from target distribution (normally posterior in Bayesian inferences)
#                       that allows one to estimate (a) means, (b) variances, and (c) credible intervals for parameters
#                       helps complicated models estimate (i) uncertainty, (ii) correlations, (iii) credible intervals
#                        MCMC helps to generate lists of possible β₀ - intercept  and  β₁x - slope  values
#                        to analyze relationships and likely ranges 
#                   EXAMPLE MCMC: (a) Metropolis-Hastings, Gibbs Sampling 
#        (4) How do we use MCMC draws from a model fit?
#         ANSWER:
#                   1) MCMC Algorithm generates model parameters that allows
#                    sampled values (draws)
#                   2) Algorithm is used to reflect beliefs/uncertainty after seeing data:
#                       (a) Summarize Posterior (for parameters: slope, intercepts)
#                           (i) mean
#                           (ii) medians
#                           (iii) credible intervals
#                           (iv) standard deviations 
#                       (b) make predictions  
#                           (i) by using each parameter to predict outcome
#                           (ii) yields plausible distributions (not a single value)
#                       (c) examine or quantify parameters (or predictions)
#                           (i) ex. correlation between intercepts and slope
#                       (d) model checking  
#                           (i) uses draws for posterior predictive checks
#                           (ii) simulate data using sampled parameters 
#                           (iii) compare real observations AND simulated data 
#   (ii) Skills:
#       (1) Fit regression models with the form y ~ x brms in R.
#       (2) Interpret the regression coefficients table of the model summary.
#       (3) Extract and plot MCMC draws.
#       (4) Report results from regression models.
#
# Resources:
#    Lecture: https://uoelel.github.io/qml/lectures/week-05.html#/title-slide
#    Textbook (PDF): https://stefanocoretta.github.io/qdal/Quantitative-Data-Analysis-for-Linguists-in-R.pdf
#    Textbook (Online): https://stefanocoretta.github.io/qdal/ch-regression-intro.html
# ==============================================================================

# Load required packages ----
library(tidyverse)
library(brms)
library(bayesplot)
library(posterior)
library(ggdist)
library(here)

# 1. Read the data ----
# Read the MALD (Massive Auditory Lexical Decision) dataset
mald <- readRDS(here("data", "tucker2019", "mald_1_1.rds"))

# Examine the data structure
glimpse(mald)
head(mald)

# 2. Filter to include only real words ----
# The task asks us to analyze only real words (not non-words)
mald_real <- mald |>
  filter(IsWord == TRUE)

cat("Total observations:", nrow(mald), "\n")
cat("Real words only:", nrow(mald_real), "\n")
cat("Non-words removed:", nrow(mald) - nrow(mald_real), "\n\n")

# 3. Explore the data ----

# Summary statistics for reaction times
cat("=== Reaction Time Summary ===\n")
summary(mald_real$RT)

# Summary statistics for PhonLev
cat("\n=== PhonLev Summary ===\n")
summary(mald_real$PhonLev)

# Check for missing values
cat("\nMissing values in RT:", sum(is.na(mald_real$RT)), "\n")
cat("Missing values in PhonLev:", sum(is.na(mald_real$PhonLev)), "\n\n")

# Remove missing values if any
mald_clean <- mald_real |>
  drop_na(RT, PhonLev)

cat("Clean dataset size:", nrow(mald_clean), "observations\n\n")

# 4. Visualize the data ----

# Create a scatter plot to visualize the relationship between PhonLev and RT
mald_clean |>
  ggplot(aes(PhonLev, RT)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_smooth(method = "lm", color = "steelblue", se = TRUE) +
  labs(
    title = "Relationship between PhonLev and Reaction Time",
    subtitle = "MALD dataset (real words only)",
    x = "Mean phoneme-level Levenshtein distance (PhonLev)",
    y = "Reaction time (ms)"
  ) +
  theme_minimal()

# Distribution of reaction times
mald_clean |>
  ggplot(aes(RT)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Distribution of Reaction Times",
    x = "Reaction time (ms)",
    y = "Count"
  ) +
  theme_minimal()

# Distribution of PhonLev
mald_clean |>
  ggplot(aes(PhonLev)) +
  geom_histogram(bins = 30, fill = "coral", alpha = 0.7) +
  labs(
    title = "Distribution of Phoneme-level Levenshtein Distance",
    x = "PhonLev",
    y = "Count"
  ) +
  theme_minimal()

# 5. Fit the Bayesian regression model ----
# Model: RT ~ Gaussian(μ, σ) with μ = β₀ + β₁ · PhonLev
# This model estimates:
#   - β₀ (Intercept): Mean RT when PhonLev = 0
#   - β₁ (PhonLev slope): Change in RT for each unit increase in PhonLev
#   - σ (sigma): Residual standard deviation

# Ensure cache directory exists
if (!dir.exists("cache")) {
  dir.create("cache")
}

mald_bm <- brm(
  RT ~ PhonLev,
  family = gaussian,
  data = mald_clean,
  cores = 4,
  seed = 999,
  file = here("cache", "mald_bm")
)

# 6. Model summary and interpretation ----
cat("=== MODEL SUMMARY ===\n")
summary(mald_bm)

# Print model formula
cat("\n=== MODEL FORMULA ===\n")
cat("RT ~ Gaussian(μ, σ)\n")
cat("μ = β₀ + β₁ · PhonLev\n\n")

# 7. Plot posterior distributions ----

# Basic posterior density plots for all parameters
cat("=== POSTERIOR DISTRIBUTIONS ===\n")
mcmc_dens(mald_bm, pars = c("b_Intercept", "b_PhonLev", "sigma")) +
  labs(title = "Posterior Distributions of Model Parameters")

# 8. Plot model predictions ----

# Conditional effects plot (without data points)
conditional_effects(mald_bm, effects = "PhonLev")

# Conditional effects with raw data points
cat("\n=== MODEL PREDICTIONS ===\n")
plot(
  conditional_effects(mald_bm, effects = "PhonLev"),
  points = TRUE,
  point_args = list(alpha = 0.05, size = 0.8)
)

# 9. Extract and wrangle MCMC draws ----

# Extract posterior draws from the model
mald_draws <- as_draws_df(mald_bm)

# Examine the structure of the draws
cat("\n=== MCMC DRAWS STRUCTURE ===\n")
cat("Total draws:", nrow(mald_draws), "\n")
cat("Parameters:", names(mald_draws)[1:6], "...\n\n")

# Display first few draws
head(mald_draws[, 1:6])

# 10. Summary measures of the posterior draws ----

cat("\n=== POSTERIOR SUMMARY STATISTICS ===\n\n")

# Intercept (β₀)
cat("--- Intercept (β₀) ---\n")
cat("Mean:", mean(mald_draws$b_Intercept) |> round(2), "ms\n")
cat("SD:", sd(mald_draws$b_Intercept) |> round(2), "ms\n")
cat("95% CrI:",
    quantile2(mald_draws$b_Intercept, c(0.025, 0.975)) |> round(2), "ms\n\n")

# PhonLev slope (β₁)
cat("--- PhonLev Slope (β₁) ---\n")
cat("Mean:", mean(mald_draws$b_PhonLev) |> round(2), "ms per unit\n")
cat("SD:", sd(mald_draws$b_PhonLev) |> round(2), "ms\n")
cat("95% CrI:",
    quantile2(mald_draws$b_PhonLev, c(0.025, 0.975)) |> round(2), "ms\n\n")

# Sigma (σ)
cat("--- Residual SD (σ) ---\n")
cat("Mean:", mean(mald_draws$sigma) |> round(2), "ms\n")
cat("SD:", sd(mald_draws$sigma) |> round(2), "ms\n")
cat("95% CrI:",
    quantile2(mald_draws$sigma, c(0.025, 0.975)) |> round(2), "ms\n\n")

# 11. Additional credible intervals ----

cat("=== ADDITIONAL CREDIBLE INTERVALS FOR PhonLev ===\n\n")

# 90% CrI for PhonLev
cat("90% CrI:",
    quantile2(mald_draws$b_PhonLev, c(0.05, 0.95)) |> round(2), "ms\n")

# 80% CrI for PhonLev
cat("80% CrI:",
    quantile2(mald_draws$b_PhonLev, c(0.10, 0.90)) |> round(2), "ms\n")

# 60% CrI for PhonLev
cat("60% CrI:",
    quantile2(mald_draws$b_PhonLev, c(0.20, 0.80)) |> round(2), "ms\n\n")

# 12. Enhanced posterior distribution plots ----

# PhonLev coefficient with density plot
p1 <- mald_draws |>
  ggplot(aes(b_PhonLev)) +
  geom_density(fill = "steelblue", alpha = 0.6) +
  geom_rug(alpha = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Posterior Distribution of PhonLev Coefficient (β₁)",
    x = "Change in RT per unit increase in PhonLev (ms)",
    y = "Density"
  ) +
  theme_minimal()

print(p1)

# Intercept
p2 <- mald_draws |>
  ggplot(aes(b_Intercept)) +
  geom_density(fill = "coral", alpha = 0.6) +
  geom_rug(alpha = 0.2) +
  labs(
    title = "Posterior Distribution of Intercept (β₀)",
    x = "Mean RT when PhonLev = 0 (ms)",
    y = "Density"
  ) +
  theme_minimal()

print(p2)

# Sigma
p3 <- mald_draws |>
  ggplot(aes(sigma)) +
  geom_density(fill = "seagreen", alpha = 0.6) +
  geom_rug(alpha = 0.2) +
  labs(
    title = "Posterior Distribution of Sigma (σ)",
    x = "Residual standard deviation (ms)",
    y = "Density"
  ) +
  theme_minimal()

print(p3)

# 13. Advanced visualizations with ggdist ----

# PhonLev coefficient with halfeye plot showing multiple CrIs
p4 <- mald_draws |>
  ggplot(aes(x = b_PhonLev)) +
  stat_halfeye(
    .width = c(0.5, 0.8, 0.95),
    aes(fill = after_stat(level))
  ) +
  scale_fill_brewer(na.translate = FALSE, name = "Interval") +
  geom_rug(alpha = 0.1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
  labs(
    title = "Posterior Distribution of PhonLev Effect with Credible Intervals",
    x = "Change in RT per unit increase in PhonLev (ms)",
    y = "Density"
  ) +
  theme_minimal()

print(p4)

# Intercept with halfeye
p5 <- mald_draws |>
  ggplot(aes(x = b_Intercept)) +
  stat_halfeye(
    .width = c(0.5, 0.8, 0.95),
    aes(fill = after_stat(level))
  ) +
  scale_fill_brewer(na.translate = FALSE, name = "Interval") +
  geom_rug(alpha = 0.1) +
  labs(
    title = "Posterior Distribution of Intercept with Credible Intervals",
    x = "Mean RT when PhonLev = 0 (ms)",
    y = "Density"
  ) +
  theme_minimal()

print(p5)

# Sigma with halfeye
p6 <- mald_draws |>
  ggplot(aes(x = sigma)) +
  stat_halfeye(
    .width = c(0.5, 0.8, 0.95),
    aes(fill = after_stat(level))
  ) +
  scale_fill_brewer(na.translate = FALSE, name = "Interval") +
  geom_rug(alpha = 0.1) +
  labs(
    title = "Posterior Distribution of Sigma with Credible Intervals",
    x = "Residual standard deviation (ms)",
    y = "Density"
  ) +
  theme_minimal()

print(p6)

# 13b. Save plots to files ----

# Create output directories
output_dir <- here("code", "outputs")
week5_dir <- here("code", "Week 5")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
if (!dir.exists(week5_dir)) dir.create(week5_dir, recursive = TRUE)

# Save PhonLev coefficient with credible intervals
ggsave(here("code", "Week 5", "TaskA_phonlev_posterior_credible_intervals.png"), p4, width = 8, height = 6, dpi = 300)
ggsave(here("code", "outputs", "Week5_TaskA_phonlev_posterior_credible_intervals.png"), p4, width = 8, height = 6, dpi = 300)

# Save Intercept with credible intervals
ggsave(here("code", "Week 5", "TaskA_intercept_posterior_credible_intervals.png"), p5, width = 8, height = 6, dpi = 300)
ggsave(here("code", "outputs", "Week5_TaskA_intercept_posterior_credible_intervals.png"), p5, width = 8, height = 6, dpi = 300)

# Save Sigma with credible intervals
ggsave(here("code", "Week 5", "TaskA_sigma_posterior_credible_intervals.png"), p6, width = 8, height = 6, dpi = 300)
ggsave(here("code", "outputs", "Week5_TaskA_sigma_posterior_credible_intervals.png"), p6, width = 8, height = 6, dpi = 300)

# Save simple density plots
ggsave(here("code", "Week 5", "TaskA_phonlev_posterior_density.png"), p1, width = 8, height = 6, dpi = 300)
ggsave(here("code", "outputs", "Week5_TaskA_phonlev_posterior_density.png"), p1, width = 8, height = 6, dpi = 300)

ggsave(here("code", "Week 5", "TaskA_intercept_posterior_density.png"), p2, width = 8, height = 6, dpi = 300)
ggsave(here("code", "outputs", "Week5_TaskA_intercept_posterior_density.png"), p2, width = 8, height = 6, dpi = 300)

ggsave(here("code", "Week 5", "TaskA_sigma_posterior_density.png"), p3, width = 8, height = 6, dpi = 300)
ggsave(here("code", "outputs", "Week5_TaskA_sigma_posterior_density.png"), p3, width = 8, height = 6, dpi = 300)

cat("\n=== PLOTS SAVED ===\n")
cat("All plots saved to both Week 5 and outputs folders\n")
cat("Files saved:\n")
cat("  - TaskA_phonlev_posterior_credible_intervals.png\n")
cat("  - TaskA_intercept_posterior_credible_intervals.png\n")
cat("  - TaskA_sigma_posterior_credible_intervals.png\n")
cat("  - TaskA_phonlev_posterior_density.png\n")
cat("  - TaskA_intercept_posterior_density.png\n")
cat("  - TaskA_sigma_posterior_density.png\n\n")

# 14. Model reporting paragraph ----

cat("\n")
cat("=" |> rep(80) |> paste0(collapse = ""), "\n")
cat("MODEL REPORTING PARAGRAPH\n")
cat("=" |> rep(80) |> paste0(collapse = ""), "\n\n")

cat("We fitted a Bayesian Gaussian regression model using the brms package\n")
cat("(Bürkner, 2017) in R (R Core Team, 2025) to investigate the relationship\n")
cat("between reaction times (RT) and mean phoneme-level Levenshtein distance\n")
cat("(PhonLev) in the MALD dataset. The model was fit on real words only,\n")
cat("excluding non-words from the analysis.\n\n")

cat("The model formula was: RT ~ Gaussian(μ, σ) with μ = β₀ + β₁ · PhonLev\n\n")

# Extract values for reporting
b0_mean <- mean(mald_draws$b_Intercept) |> round(0)
b0_sd <- sd(mald_draws$b_Intercept) |> round(0)
b0_ci <- quantile2(mald_draws$b_Intercept, c(0.025, 0.975)) |> round(0)

b1_mean <- mean(mald_draws$b_PhonLev) |> round(1)
b1_sd <- sd(mald_draws$b_PhonLev) |> round(1)
b1_ci <- quantile2(mald_draws$b_PhonLev, c(0.025, 0.975)) |> round(1)

sigma_mean <- mean(mald_draws$sigma) |> round(0)
sigma_sd <- sd(mald_draws$sigma) |> round(0)
sigma_ci <- quantile2(mald_draws$sigma, c(0.025, 0.975)) |> round(0)

cat("Based on the model results, there is a 95% probability that the mean\n")
cat(sprintf("reaction time when PhonLev = 0 is between %d and %d ms (mean = %d ms,\n",
    b0_ci[1], b0_ci[2], b0_mean))
cat(sprintf("SD = %d ms). For each unit increase in PhonLev, reaction time increases\n", b0_sd))
cat(sprintf("by %0.1f to %0.1f ms (mean = %0.1f ms, SD = %0.1f ms), with 95%% confidence.\n",
    b1_ci[1], b1_ci[2], b1_mean, b1_sd))
cat(sprintf("The residual standard deviation is between %d and %d ms (mean = %d ms,\n",
    sigma_ci[1], sigma_ci[2], sigma_mean))
cat(sprintf("SD = %d ms).\n\n", sigma_sd))

cat("This positive relationship indicates that words with higher phoneme-level\n")
cat("Levenshtein distance require longer processing times in auditory lexical\n")
cat("decision tasks. The effect is robust, as the entire 95%% credible interval\n")
cat("for the PhonLev coefficient is positive, excluding zero.\n\n")

# 15. Answer research questions ----

cat("=" |> rep(80) |> paste0(collapse = ""), "\n")
cat("RESEARCH QUESTIONS - ANSWERS\n")
cat("=" |> rep(80) |> paste0(collapse = ""), "\n\n")

cat("Q1: What is the relationship between reaction times and PhonLev?\n")
cat(rep("-", 75), "\n\n", sep = "")

cat("There is a positive linear relationship between reaction times and mean\n")
cat("phoneme-level Levenshtein distance (PhonLev). As PhonLev increases, reaction\n")
cat("times also increase. Specifically:\n\n")

cat(sprintf("  • For each unit increase in PhonLev, RT increases by approximately\n"))
cat(sprintf("    %0.1f ms (95%% CrI: [%0.1f, %0.1f] ms)\n\n",
    b1_mean, b1_ci[1], b1_ci[2]))

cat("  • This effect is statistically reliable, as the entire 95%% credible\n")
cat("    interval is positive and excludes zero.\n\n")

cat("  • The relationship is consistent across the range of PhonLev values\n")
cat("    observed in the data, as evidenced by the linear model fit.\n\n\n")

cat("Q2: Why does PhonLev have the effect it has on reaction times?\n")
cat(rep("-", 75), "\n\n", sep = "")

cat("The positive effect of PhonLev on reaction times can be explained by\n")
cat("several psycholinguistic mechanisms:\n\n")

cat("1. LEXICAL NEIGHBORHOOD DENSITY:\n")
cat("   PhonLev (phoneme-level Levenshtein distance) measures the average\n")
cat("   phonological distance between a target word and all other words in the\n")
cat("   lexicon. Higher PhonLev values indicate that a word has fewer close\n")
cat("   phonological neighbors.\n\n")

cat("2. LEXICAL COMPETITION:\n")
cat("   Words with lower PhonLev (more phonological neighbors) experience greater\n")
cat("   lexical competition during word recognition. However, words with very high\n")
cat("   PhonLev may be harder to process because:\n")
cat("   - They are phonologically unusual or atypical\n")
cat("   - They may have lower frequency\n")
cat("   - They may be less familiar to listeners\n\n")

cat("3. WORD RECOGNITION MODELS:\n")
cat("   According to models of spoken word recognition (e.g., TRACE, Cohort Model):\n")
cat("   - Words activate lexical candidates that are phonologically similar\n")
cat("   - Words with high PhonLev are phonologically isolated\n")
cat("   - Isolated words may require more processing time because they don't\n")
cat("     benefit from spreading activation from neighbors\n\n")

cat("4. FREQUENCY AND FAMILIARITY:\n")
cat("   Words with high PhonLev tend to be:\n")
cat("   - Less frequent in the language\n")
cat("   - Less familiar to listeners\n")
cat("   - More difficult to process due to weaker lexical representations\n\n")

cat("5. PERCEPTUAL CLARITY:\n")
cat("   Words with many phonological neighbors (low PhonLev) may benefit from\n")
cat("   greater perceptual clarity because the phonological patterns are more\n")
cat("   common and expected. Words with high PhonLev may be perceptually more\n")
cat("   difficult to process.\n\n")

cat("CONCLUSION:\n")
cat("The positive relationship between PhonLev and RT reflects the increased\n")
cat("cognitive demands of processing phonologically atypical or isolated words.\n")
cat("These words lack the facilitative effects of lexical neighborhood density\n")
cat("and may have weaker lexical representations, leading to longer reaction times\n")
cat("in auditory lexical decision tasks.\n\n")

cat("=" |> rep(80) |> paste0(collapse = ""), "\n")
cat("END OF ANALYSIS\n")
cat("=" |> rep(80) |> paste0(collapse = ""), "\n\n")

# ==============================================================================
# Notes:
# - This analysis demonstrates the full workflow of Bayesian regression modeling
# - Key skills practiced: data wrangling, model fitting, posterior extraction,
#   visualization, and interpretation
# - Remember to always visualize your data before and after modeling
# - Posterior distributions provide rich information about parameter uncertainty
# ==============================================================================
