# ==============================================================================
# Week 5 | https://uoelel.github.io/qml/workshops/week-05.html
# Title: Task B: Korean voice quality
# Author:
# Date: 2025-10-17
# Topic: Workshop: Regression basics and posteriors draws
#        Lecture:  Regression basics with brms
# Instructions:
#    (1) Read the winter2012/polite.csv data.
#    (2) Filter the data to include only female participants.
#    (3) Fit a regression model of mean f0 (f0mn)
#        and mean harmonic-to-noise ratio (HNRmn, the higher the HNR, the more modal the voice is).

#    (4) What is the relationship between mean f0 and mean HNR in female Korean speakers?
#      In Winter (2012), the research studies the politeness of (female) Korean speakers
#      which describes the relationship between (f0) fundamental frequency and (HNRmn)
#      harmonic-to-noise ratio. 
#      f0 is pitch and HNR is voice quality. 
#      With HNRmn, the higher the HNR the more modal voice is while with higher HNR 
#      is a reliable acoustic correlation with modal changes and lower HNR is 
#       a reliable acoustic correlation to creaky and breathy voice qualities in analysis of typical modal phonation.
#    (5) Write a paragraph reporting the model.
#     The model is described as HNRmn ~ Gaussian(μ, σ) where μ = β₀ + β₁ · f0mn
#     where positive slope correlates to tighter vocal fold closure or higher pitch (more modal voice).
#     Meanwhile negative slope describes higher voice correlated with breathier or creakier voice qualities
#    
#    (6) Produce plots of the posterior distributions of the model parameters.
#      SEE FOLLOWING IMAGES:
#       (a) Week5_TaskB_posterior_dens
#       (b) Week5_TaskB_posterior_f0mn
#       (c) Week5_TaskB_posterior_intercept
#       (d) Week5_TaskB_predictions
#       (e) Week5_TaskB_scatterplot
#Learning Objectives:
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
#        (1) (1) Fit regression models with the form y ~ x brms in R.
#        (2) Interpret the regression coefficients table of the model summary.
#        (3) Extract and plot MCMC draws.
#        (4) Report results from regression models.
#    Important Terms & Information:
#            Model: HNRmn ∼ Gaussian(μ, σ) with μ = β₀ + β₁ · f0mn
#            Lecture: https://uoelel.github.io/qml/lectures/week-05.html#/title-slide
#            Textbook (PDF): https://stefanocoretta.github.io/qdal/Quantitative-Data-Analysis-for-Linguists-in-R.pdf
#            Textbook (Online): https://stefanocoretta.github.io/qdal/ch-regression-intro.html
#            Data Documentation: https://uoelel.github.io/qml-data/data/winter2012/polite.html
#            seed(123) - why so random 
# ==============================================================================

# Load packages
library(tidyverse)
library(here)
library(brms)
library(bayesplot)
library(posterior)

# Create output directories
output_dir <- here("code", "outputs")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

week5_dir <- here("code", "Week 5")
if (!dir.exists(week5_dir)) dir.create(week5_dir, recursive = TRUE)

# Create cache directory if it doesn't exist
cache_dir <- here("cache")
if (!dir.exists(cache_dir)) dir.create(cache_dir)

# 1. Read the data
polite <- read_csv(here("data", "winter2012", "polite.csv"))

# 2. Filter to include only female participants
polite_female <- polite |>
  filter(gender == "F") |>
  drop_na(f0mn, HNRmn)

# Explore the data
glimpse(polite_female)
summary(polite_female$f0mn)
summary(polite_female$HNRmn)

# Visualize the relationship
polite_female |>
  ggplot(aes(f0mn, HNRmn)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  labs(
    x = "Mean fundamental frequency (f0, Hz)",
    y = "Mean harmonic-to-noise ratio (HNR, dB)"
  )

# Save plot
ggsave(here("code", "Week 5", "TaskB_scatterplot.png"))
ggsave(here("code", "outputs", "Week5_TaskB_scatterplot.png"))

# 3. Fit the regression model (3) 
# Model: HNRmn ~ Gaussian(μ, σ) where μ = β₀ + β₁ · f0mn
polite_bm <- brm(
  HNRmn ~ f0mn,
  family = gaussian,
  data = polite_female,
  cores = 4,
  seed = 4321,
  file = here("cache", "polite_bm")
)

# Model summary
summary(polite_bm)

# 4. Plot posterior distributions
mcmc_plot <- mcmc_dens(polite_bm, pars = c("b_Intercept", "b_f0mn", "sigma"))
print(mcmc_plot)

# Save plot
ggsave(here("code", "Week 5", "TaskB_posterior_dens.png"))
ggsave(here("code", "outputs", "Week5_TaskB_posterior_dens.png"))

# Plot model predictions
pred_plot <- plot(
  conditional_effects(polite_bm, effects = "f0mn"),
  points = TRUE,
  point_args = list(alpha = 0.3)
)
print(pred_plot)

# Save plot
ggsave(here("code", "Week 5", "TaskB_predictions.png"))
ggsave(here("code", "outputs", "Week5_TaskB_predictions.png"))

# Extract MCMC draws
polite_draws <- as_draws_df(polite_bm)

# Summary measures
cat("\nIntercept:\n")
cat("  Mean:", round(mean(polite_draws$b_Intercept), 2), "dB\n")
cat("  95% CrI:", round(quantile2(polite_draws$b_Intercept, c(0.025, 0.975)), 2), "\n")

cat("\nf0mn slope:\n")
cat("  Mean:", round(mean(polite_draws$b_f0mn), 4), "dB/Hz\n")
cat("  95% CrI:", round(quantile2(polite_draws$b_f0mn, c(0.025, 0.975)), 4), "\n")

cat("\nSigma:\n")
cat("  Mean:", round(mean(polite_draws$sigma), 2), "dB\n")
cat("  95% CrI:", round(quantile2(polite_draws$sigma, c(0.025, 0.975)), 2), "\n")

# Plot individual posteriors
polite_draws |>
  ggplot(aes(b_f0mn)) +
  geom_density() +
  geom_rug(alpha = 0.2) +
  labs(
    title = "Posterior distribution of f0 coefficient",
    x = "Change in HNR per Hz increase in f0 (dB/Hz)",
    y = "Density"
  )

# Save plot
ggsave(here("code", "Week 5", "TaskB_posterior_f0mn.png"))
ggsave(here("code", "outputs", "Week5_TaskB_posterior_f0mn.png"))

polite_draws |>
  ggplot(aes(b_Intercept)) +
  geom_density() +
  geom_rug(alpha = 0.2) +
  labs(
    title = "Posterior distribution of Intercept",
    x = "Mean HNR when f0 = 0 Hz (dB)",
    y = "Density"
  )

# Save plot
ggsave(here("code", "Week 5", "TaskB_posterior_intercept.png"))
ggsave(here("code", "outputs", "Week5_TaskB_posterior_intercept.png"))

polite_draws |>
  ggplot(aes(sigma)) +
  geom_density() +
  geom_rug(alpha = 0.2) +
  labs(
    title = "Posterior distribution of Sigma",
    x = "Residual standard deviation (dB)",
    y = "Density"
  )

# Save plot
ggsave(here("code", "Week 5", "TaskB_posterior_sigma.png"))
ggsave(here("code", "outputs", "Week5_TaskB_posterior_sigma.png"))

# ==============================================================================
# 5. REPORT
# ==============================================================================

cat("\n\n=== MODEL REPORT ===\n\n")

cat("We fitted a Bayesian Gaussian regression model using the brms package\n")
cat("(Bürkner 2017) in R (R Core Team 2025). We used a Gaussian distribution\n")
cat("for the outcome variable, mean harmonic-to-noise ratio (HNRmn, in dB),\n")
cat("and included mean fundamental frequency (f0mn, in Hz) as the predictor.\n")
cat("The data were filtered to include only female Korean speakers from the\n")
cat("politeness study.\n\n")

cat("Based on the model results, there is a 95% probability that the mean HNR\n")
cat("when f0 is 0 Hz is between",
    round(quantile2(polite_draws$b_Intercept, c(0.025, 0.975))[1], 1), "and",
    round(quantile2(polite_draws$b_Intercept, c(0.025, 0.975))[2], 1), "dB\n")
cat("(mean =", round(mean(polite_draws$b_Intercept), 1), "dB, SD =",
    round(sd(polite_draws$b_Intercept), 1), "dB).\n\n")

# Determine direction of relationship
f0_effect <- mean(polite_draws$b_f0mn)
f0_ci <- quantile2(polite_draws$b_f0mn, c(0.025, 0.975))

if (f0_effect > 0 && f0_ci[1] > 0) {
  cat("For each Hz increase in mean f0, HNR increases by",
      round(f0_ci[1], 4), "to",
      round(f0_ci[2], 4), "dB\n")
} else if (f0_effect < 0 && f0_ci[2] < 0) {
  cat("For each Hz increase in mean f0, HNR decreases by",
      abs(round(f0_ci[2], 4)), "to",
      abs(round(f0_ci[1], 4)), "dB\n")
} else {
  cat("For each Hz increase in mean f0, HNR changes by",
      round(f0_ci[1], 4), "to",
      round(f0_ci[2], 4), "dB\n")
}

cat("(mean =", round(f0_effect, 4), "dB/Hz, SD =",
    round(sd(polite_draws$b_f0mn), 4), "dB/Hz), at 95% confidence.\n\n")

cat("The residual standard deviation is between",
    round(quantile2(polite_draws$sigma, c(0.025, 0.975))[1], 1), "and",
    round(quantile2(polite_draws$sigma, c(0.025, 0.975))[2], 1), "dB\n")
cat("(mean =", round(mean(polite_draws$sigma), 1), "dB, SD =",
    round(sd(polite_draws$sigma), 1), "dB).\n\n")

# ==============================================================================
# 6. THEORETICAL INTERPRETATION
# ==============================================================================

cat("\n=== THEORETICAL EXPLANATION ===\n\n")

cat("Harmonic-to-noise ratio (HNR) is an acoustic measure of voice quality that\n")
cat("reflects the ratio of periodic (harmonic) to aperiodic (noise) energy in\n")
cat("the speech signal. Higher HNR values indicate more modal (clear, periodic)\n")
cat("voice quality, while lower values suggest breathier or creakier phonation.\n\n")

cat("Fundamental frequency (f0) is the rate of vocal fold vibration, perceived\n")
cat("as pitch. The relationship between f0 and HNR can reveal biomechanical\n")
cat("constraints on voice production:\n\n")

if (f0_effect > 0) {
  cat("The POSITIVE relationship between f0 and HNR suggests that higher pitch\n")
  cat("is associated with more modal voice quality in female Korean speakers.\n")
  cat("This pattern may reflect:\n\n")
  cat("1. VOCAL FOLD TENSION: Higher f0 requires increased vocal fold tension,\n")
  cat("   which can lead to more complete glottal closure and thus higher HNR.\n\n")
  cat("2. PHONATION TYPE: At lower f0, speakers may be more likely to use\n")
  cat("   creaky voice (vocal fry), which has lower HNR due to irregular\n")
  cat("   vocal fold vibration patterns.\n\n")
  cat("3. AERODYNAMIC FACTORS: The airflow and pressure conditions that support\n")
  cat("   higher f0 may also favor more periodic vocal fold vibration.\n")
} else if (f0_effect < 0) {
  cat("The NEGATIVE relationship between f0 and HNR suggests that higher pitch\n")
  cat("is associated with breathier voice quality in female Korean speakers.\n")
  cat("This pattern may reflect:\n\n")
  cat("1. GLOTTAL CONFIGURATION: Higher f0 may be accompanied by incomplete\n")
  cat("   glottal closure, allowing more turbulent airflow (noise).\n\n")
  cat("2. REGISTER EFFECTS: Some speakers may use different phonatory registers\n")
  cat("   at different pitch levels, with breathier quality at higher pitches.\n\n")
} else {
  cat("The relationship between f0 and HNR is uncertain (credible interval\n")
  cat("includes zero), suggesting that in this dataset, f0 may not be a strong\n")
  cat("predictor of voice quality for female Korean speakers, or that the\n")
  cat("relationship varies considerably across individuals or contexts.\n\n")
}

cat("\nThis finding contributes to our understanding of the phonetic implementation\n")
cat("of voice quality in Korean, which has been studied in relation to the\n")
cat("three-way laryngeal contrast in stops and the role of voice quality cues\n")
cat("in maintaining phonological distinctions.\n")

cat("\n", strrep("=", 78), "\n")
