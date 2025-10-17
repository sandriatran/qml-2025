# ==============================================================================
# Week 5
# Title: NoteTask C: Reading fixation times (eye-tracking)
# Author: Sandria Tran
# Date: 2025-10-17
# Instructions:
#    (1) Read berzak2025/fix-dur.csv and familiarise yourself with the data documentation and study linked therein.
#    (2) With a regression model, answer the following research question: what is the relationship between the current fixation duration and the length of the word being fixated?
#    (3) Write a paragraph reporting the model. Produce plots of the posterior distributions of the model parameters.
#
# Research Questions/Analysis:
# (Q1) what is the relationship between the current fixation duration
#       and the length of the word being fixated?

# (A1) Write a paragraph reporting the model. 
#     Produce plots of the posterior distributions of the model parameters.

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
#    Important Terms & Information:
#            RT ∼ Gaussian(μ, σ) with μ = β₀ + β₁ · PhonLev
#            Lecture: https://uoelel.github.io/qml/lectures/week-05.html#/title-slide
#            Textbook (PDF): https://stefanocoretta.github.io/qdal/Quantitative-Data-Analysis-for-Linguists-in-R.pdf
#            Textbook (Online): https://stefanocoretta.github.io/qdal/ch-regression-intro.html
# ==============================================================================

# TaskC: Fixation Duration Analysis
# Research Question: What is the relationship between current fixation duration 
# and the length of the word being fixated?

# Load required packages
library(tidyverse)
library(brms)
library(bayesplot)
library(posterior)
library(ggdist)
library(here)

# Create output directories
output_dir <- here("code", "outputs")
week5_dir <- here("code", "Week 5")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
if (!dir.exists(week5_dir)) dir.create(week5_dir, recursive = TRUE)

# Read and explore the data
fix_dur <- read_csv(here("data", "berzak2025", "fix-dur.csv"))

# Explore the data
glimpse(fix_dur)
summary(fix_dur)

# Plot 1: Scatter plot with regression line
p_scatter <- fix_dur |>
  ggplot(aes(word_len, CURRENT_FIX_DURATION)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_smooth(method = "lm", color = "steelblue") +
  labs(
    title = "Relationship between Word Length and Fixation Duration",
    x = "Word Length (characters)",
    y = "Fixation Duration (ms)"
  ) +
  theme_minimal()

ggsave(here("code", "Week 5", "TaskC_scatter_plot.png"), p_scatter, width = 8, height = 6)
ggsave(here("code", "outputs", "Week5_TaskC_scatter_plot.png"), p_scatter, width = 8, height = 6)

# Fit the Gaussian regression model
fix_bm <- brm(
  CURRENT_FIX_DURATION ~ word_len,
  family = gaussian,
  data = fix_dur,
  cores = 4,
  seed = 12345,
  file = here("cache", "fix_bm")
)

# Model summary
summary(fix_bm)

# Plot 2: Posterior distributions
p_posterior <- mcmc_dens(fix_bm, pars = c("b_Intercept", "b_word_len", "sigma")) +
  labs(title = "Posterior Distributions of Model Parameters")

ggsave(here("code", "Week 5", "TaskC_posterior_dens.png"), p_posterior, width = 8, height = 6)
ggsave(here("code", "outputs", "Week5_TaskC_posterior_dens.png"), p_posterior, width = 8, height = 6)

# Plot 3: Conditional effects plot
ce_plot <- plot(
  conditional_effects(fix_bm, effects = "word_len"),
  points = TRUE,
  point_args = list(alpha = 0.1, size = 0.8)
)

ggsave(here("code", "Week 5", "TaskC_conditional_effects.png"), ce_plot[[1]], width = 8, height = 6)
ggsave(here("code", "outputs", "Week5_TaskC_conditional_effects.png"), ce_plot[[1]], width = 8, height = 6)

# Extract MCMC draws
fix_bm_draws <- as_draws_df(fix_bm)

# Plot 4: Posterior distribution with credible intervals
p_halfeye <- fix_bm_draws |>
  ggplot(aes(x = b_word_len)) +
  stat_halfeye(
    .width = c(0.5, 0.8, 0.95),
    aes(fill = after_stat(level))
  ) +
  scale_fill_brewer(na.translate = FALSE, name = "Interval") +
  geom_rug(alpha = 0.2) +
  labs(
    title = "Posterior Distribution of Word Length Effect",
    x = "Change in Fixation Duration per Character (ms)",
    y = "Density"
  ) +
  theme_minimal()

ggsave(here("code", "Week 5", "TaskC_word_len_posterior.png"), p_halfeye, width = 8, height = 6)
ggsave(here("code", "outputs", "Week5_TaskC_word_len_posterior.png"), p_halfeye, width = 8, height = 6)

# Calculate summary statistics
b0_mean <- mean(fix_bm_draws$b_Intercept) |> round(1)
b0_sd <- sd(fix_bm_draws$b_Intercept) |> round(1)
b0_ci <- quantile2(fix_bm_draws$b_Intercept, c(0.025, 0.975)) |> round(1)

b1_mean <- mean(fix_bm_draws$b_word_len) |> round(2)
b1_sd <- sd(fix_bm_draws$b_word_len) |> round(2)
b1_ci <- quantile2(fix_bm_draws$b_word_len, c(0.025, 0.975)) |> round(2)

sigma_mean <- mean(fix_bm_draws$sigma) |> round(1)
sigma_sd <- sd(fix_bm_draws$sigma) |> round(1)
sigma_ci <- quantile2(fix_bm_draws$sigma, c(0.025, 0.975)) |> round(1)

# Print summary statistics
cat("\n=== POSTERIOR SUMMARY STATISTICS ===\n\n")

cat("--- Intercept (β₀) ---\n")
cat("Mean:", b0_mean, "ms\n")
cat("SD:", b0_sd, "ms\n")
cat("95% CrI: [", b0_ci[1], ",", b0_ci[2], "] ms\n\n")

cat("--- Word Length Slope (β₁) ---\n")
cat("Mean:", b1_mean, "ms per character\n")
cat("SD:", b1_sd, "ms\n")
cat("95% CrI: [", b1_ci[1], ",", b1_ci[2], "] ms\n\n")

cat("--- Residual SD (σ) ---\n")
cat("Mean:", sigma_mean, "ms\n")
cat("SD:", sigma_sd, "ms\n")
cat("95% CrI: [", sigma_ci[1], ",", sigma_ci[2], "] ms\n\n")

# MODEL REPORTING PARAGRAPH
cat("\n")
cat(rep("=", 80), "\n", sep = "")
cat("MODEL REPORTING PARAGRAPH\n")
cat(rep("=", 80), "\n\n", sep = "")

cat("We fitted a Bayesian regression model using the brms package (Bürkner 2017)\n")
cat("in R (R Core Team 2025). We used a Gaussian distribution for the outcome\n")
cat("variable, fixation duration (in milliseconds). We included word length\n")
cat("(measured as number of characters) as the regression predictor.\n\n")

cat("Based on the model results, there is a 95% probability that the mean fixation\n")
cat(sprintf("duration when word length is 0 is between %0.1f and %0.1f ms (mean = %0.1f ms,\n",
    b0_ci[1], b0_ci[2], b0_mean))
cat(sprintf("SD = %0.1f ms). For each unit increase of word length (i.e., for each additional\n", b0_sd))
cat(sprintf("character), fixation duration increases by %0.2f to %0.2f ms (mean = %0.2f ms,\n",
    b1_ci[1], b1_ci[2], b1_mean))
cat(sprintf("SD = %0.2f ms). The residual standard deviation is between %0.1f and %0.1f ms\n",
    b1_sd, sigma_ci[1], sigma_ci[2]))
cat(sprintf("(mean = %0.1f ms, SD = %0.1f ms).\n\n", sigma_mean, sigma_sd))

cat("This positive relationship indicates that readers spend more time fixating on\n")
cat("longer words, which is consistent with theories of visual word recognition that\n")
cat("propose increased processing difficulty for longer words.\n\n")

cat(rep("=", 80), "\n", sep = "")
cat("END OF ANALYSIS\n")
cat(rep("=", 80), "\n", sep = "")
