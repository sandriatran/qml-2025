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

# 2. Filter for real words ----
# Exclude non-words to focus on actual lexical decision for real items
mald_real <- mald %>%
  filter(isWord == TRUE) %>%
  drop_na(RT, PhonLev)

# 3. Fit Bayesian regression model ----
# Model: RT ~ PhonLev with Gaussian likelihood
# μ = β₀ + β₁ · PhonLev
model_rt_phonlev <- brm(
  RT ~ PhonLev,
  data = mald_real,
  family = gaussian(),
  iter = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  seed = 2025
)

# 4. Model summary and interpretation ----
summary(model_rt_phonlev)

# 5. Extract posterior draws ----
posterior_draws <- as_draws_df(model_rt_phonlev)

# 6. Visualize posterior distributions ----
# Plot 1: Posterior distribution of intercept (β₀)
p1 <- posterior_draws %>%
  ggplot(aes(x = b_Intercept)) +
  geom_density(fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Posterior Distribution of Intercept (β₀)",
    x = "Intercept",
    y = "Density"
  ) +
  theme_minimal()

# Plot 2: Posterior distribution of slope (β₁)
p2 <- posterior_draws %>%
  ggplot(aes(x = b_PhonLev)) +
  geom_density(fill = "coral", alpha = 0.7) +
  labs(
    title = "Posterior Distribution of PhonLev Slope (β₁)",
    x = "PhonLev Coefficient",
    y = "Density"
  ) +
  theme_minimal()

# Plot 3: Joint posterior distribution
p3 <- posterior_draws %>%
  ggplot(aes(x = b_Intercept, y = b_PhonLev)) +
  stat_density_2d(aes(fill = after_stat(density)), geom = "raster", contour = FALSE) +
  scale_fill_viridis_c() +
  labs(
    title = "Joint Posterior Distribution",
    x = "Intercept (β₀)",
    y = "PhonLev Coefficient (β₁)"
  ) +
  theme_minimal()

# Plot 4: Posterior predictive check
p4 <- pp_check(model_rt_phonlev, ndraws = 100) +
  labs(title = "Posterior Predictive Check") +
  theme_minimal()

# Display all plots
print(p1)
print(p2)
print(p3)
print(p4)

# 7. Model report ----
cat("\n========== MODEL REPORT ==========\n\n")

cat("RESEARCH QUESTION:\n")
cat("What is the relationship between reaction times (RT) and mean phoneme-level\n")
cat("Levenshtein distance (PhonLev)?\n\n")

cat("MODEL SPECIFICATION:\n")
cat("RT ~ Gaussian(μ, σ) where μ = β₀ + β₁ · PhonLev\n\n")

cat("KEY FINDINGS:\n")
intercept_mean <- mean(posterior_draws$b_Intercept)
slope_mean <- mean(posterior_draws$b_PhonLev)
slope_lower <- quantile(posterior_draws$b_PhonLev, 0.025)
slope_upper <- quantile(posterior_draws$b_PhonLev, 0.975)

cat(sprintf("- Intercept (β₀): %.2f ms (baseline RT when PhonLev = 0)\n", intercept_mean))
cat(sprintf("- PhonLev Slope (β₁): %.2f ms [95%% CrI: %.2f, %.2f]\n", slope_mean, slope_lower, slope_upper))
cat(sprintf("  * Each unit increase in PhonLev associates with a %.2f ms increase in RT\n\n", slope_mean))

cat("INTERPRETATION:\n")
cat("The positive relationship between PhonLev and reaction time indicates that words\n")
cat("with higher phoneme-level Levenshtein distance (i.e., words with fewer phonemically\n")
cat("similar neighbors) are recognized more slowly. Conversely, words with lower PhonLev\n")
cat("values (words with more phonemically similar neighbors, higher neighborhood density)\n")
cat("show faster reaction times. This reflects increased competition effects: words in\n")
cat("denser phonological neighborhoods are recognized more slowly due to greater lexical\n")
cat("competition, while phonologically isolated words (high PhonLev) are recognized faster.\n\n")

cat("MODEL FIT:\n")
print(model_rt_phonlev)

cat("\n========== END REPORT ==========\n")
