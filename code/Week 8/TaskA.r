# ==============================================================================
# Week 8 | https://uoelel.github.io/qml/workshops/week-08.html
# Title: Task A: Vowel duration and vowel category
# Author: Sandria Tran
# Date: 2025-11-6
# Topic: Workshop:  Bernoulli regression
#        Lecture:   Bernoulli regression
#       NOTE: THIS USES A epred_draws() APPROACH
#
# (i) Instructions:
#   (1) Read the pankratz2021/si.csv data. (Data documentation)
#   (2) Fit a regression model to answer the following question: does the semantic distance (semdist) of the weak and strong adjective affect the probability of a scalar inference (SI)?
#   (3) Write a paragraph reporting the model. Produce plots of the posterior distributions of the model parameters and the expected predictions for each vowel.
#   (4) Discuss the results with your group (no need to write the discussion)
#
# (ii) Learning Objectives:
#  (1) How can we model binary outcomes with regression models?
#       (a)  We can use a Bernoulli regression (also known as a logistic               regression or binomial regression)
#       (b) Binary outcome variables are:
#            -  yes/no
#            -  correct / incorrect
#            -  grammatical / ungrammatical
#       (c)  Normally, when analyzing binary outcome variables,
#          we're interested in the p (probability) of obtaining of those             outcomes
#        (d) Bernoulli Distribution
#            -  we model the outcome using a Bernoulli distribution
#              x ~ Bernoulli (p)
#        (e) probability are BOUNDED between 0 and 1
#            regression models require an unbounded response variable
#            to use linear predictors effectively
#        (f) Logit Link Function
#            Logit function transform (p) into log-odds (unbounded)
#            logit(p) = log (p / 1 - p)
#        (g) Regression model
#            The general form of modelling a binary outcome
#            (e.g. categorical predictor)
#        (h) logit(p_{i})=β_{0} +β_{1} x_{1i} + + β_{2}x_{2i} . . .
#               here: p_{i} is:
#                     probability of "success"
#                     for observation i, and β coefficients are
#                     estimated by the model
#            ex. a model would estimate log-odds of the outcome and have 1 group as a reference
#        (i) LOG-ODDS back into Probabilities
#             - To understand results:
#                (i) transform the predicted log odds back to probabilities
#                    using the inverse logit (logistic) function
#                (ii) p = (1) / ( 1 + e ^ {-logit(p)}
#          **    (ii) IN R: plogis(log_odds)
#         (j) PRACTICAL MODELLING
#            - R
#                 specifies that the outcome variable is binary
#                (i) and to use a bernoulli (or binomial)
#                (ii) then use a logit link
#                (iii) the estimated coefficients
#                     -  intercepts, group effects
#                     -  describe log-odds
#                     - then you interpret the terms of probabilities for                         predictor level or group
#        (k) INTERPRETING RESULTS:
#            (i) output allows estiminating the probability of success in               each group (or predictor value)
#             (ii) you compare these probabilities by look at differences in            log-odds or probabilities
#        (l) SUMMARY:
#            BINARY REGRESSION MODELS use:
#            (a) logit links -> convert probabilities (bounded) to log-odds            (unbounded) so standard regression methods can be applied
#            (b) standard methods for binary outcomes are logistic                     (bernoulli, binomial)
#            - returns inteprretable probabilities
#            after transforming from log-odds
                    
#  (2) How can we model numeric outcome variables that are only positive?
#        - MOST COMMON:
#          - Generalized Linear Models (GLMs)
#          - with a family and link function
#          - positive-only data
#        (a) Log-Normal Regression
#            Assumption: the log of the outcome variable is normally                   distributed

#        model:
#        log(y_{i})=β_{0} +β_{1} x_{1i} + β_{2}x_{2i} +…+ε_{i}
#        - here: y_{i} > 0
#        - predictors enter linearly on log scales
#        interpretation: exponentiating predictors gives estimates on the          original scale
#        (b) Gamma Regression
#            VERY COMMON FOR:
#            (1) REACTION TIMES
#            (2) Cost
#            (3) Skewness is Present
#        model:
#            - uses a GLM with Gamma family
#            - y_{i} ~ Gamma (μ_{i},ϕ)
#            - often with log link
#                log link ensures predictions are ALWAYS positive (+)
#    R SYNTAX EX:
#    glm(y ~ predictors, family = Gamma(link = "log"), data = your_data)
#        (c) Inverse Gaussian Regression
#            - less common but useful for some response time data

#        (d) why NOT linear regressioN?
#            - standard linear regression assumes errors are normal
#            - can produce negative predictions
#              (invalid for strictly positive data)
#            - log or gamma regression prevents this and models the data more           accurately
#        SUMMARY:
#        (a) To model strictly positive,numeric outcomes:
#            (1) use log-normal regression
#            (2) Gamma regression
#            (3) or another positive-only family in a GLM
#                with an appropriate link (commonly log),
#            (4) ensuring your predictions respect the constraint
#                   y > 0.
#  (3) What are model diagnostics?
#        Model diagonistics are practice of thoroughly:
#        - checking
#        - visualizing
#        - testing a fitted statistical model
#        - so to test if the model appropriately represents your data
#        - and satisifies the assumption necessary for trustworthy/valid            inference and accurate prediction + interpretation
#        - faithfully represents the data
    

#        key aspects of model diagonistics
#        (1) checking assumptions of the statistical models are met
#        (2) for regression models this include checking:
#            (a) linearity
#            (b) normality of residuals
#            (c) homoscedasticity (constant variance)
#            (d) independence of errors
#        (3) detecting problems
#            (a) outliers
#            (b) influential points - (observations that have a large effet                 on results)
#            (c) multicollinearity (strong correlations between predictors)
#            (d) model misspecification
#        (4) assessing model fit
#            (a) diagnostics provide ways to assess if the model fits the                  data well
#            (b) if there are systematic patterns in residuals
#                which suggests improvement of model
#        (5) examples of residual analysis:
#           - they examine differences between observed and predicted values
#            - to identify patterns or violations of assumptions
#                although these statistical tests or goodness-of-fit                       diagnostics are  not specifically mentioned in the chapter              they are important to understand:
#            (i) residuals vs. fitted values plot (detects non-linearity, heteroscedasticity)
#           (ii) normal Q-Q plot (checks normality of residuals)
#            (iii) scale-location plot (variance across predictors)
#            (iv) leverage plots (detects influential points)

#        although outside of scope of chapter:
#        ex. Breusch-Pagan test (for heteroscedasticity), Durbin-Watson test (for independence), Variance Inflation Factor (VIF) (for multicollinearity), Goodness-of-Fit Statistics: R-squared, Adjusted R-squared, AIC, BIC, Logistic Regression Diagnostics: Hosmer-Lemeshow test, ROC curves, classification tables
#        (6) chapter focuses on: (1) model parameters, (2) confidence intervals (crl), (3) plotting model fits, (4) interpreting regression cofficients and contrats

#  (4) Which model diagnostics should I pay attention to?
#        (1) Visual inspection & summary of model results
#        (2) Reporting credible intervals & confidence intervals (CrIs/CIs)
#        (3) Checking the mean & variability of proportions by group
#        (4) Using bootstrapped confidence intervals (with functions like mean_cl_boot in R)
#        Focus on:
#        (5): Credible intervals/Confidence intervals for group probabilities or effects
#        (6): Group summaries and differences (comparing predicted probabilities/log-odds between groups)
#        (7) Visualizations such as:
#            - Bar plots and whiskers (with caution against misuse)
#            - Jitter plots showing participant-level proportions
#            - Bootstrapped confidence interval plots for group means
#        (8) Awareness of data structure (noting repeated measures and hierarchical/mixed models as future topics)
#        (9) Discussing the importance of correctly summarizing and visualizing participant variability and dependent data structures

# (iii) Skills:

#   (1) Fit and interpret regression models with a Bernoulli family in brms.
#   (2) Fit and interpret regression models with a log-normal family in brms.
#   (3) Use model diagnostics to understand the model’s goodness of fit.
#   (4) Apply standard solutions when models do not converge.

 Lecture: https://uoelel.github.io/qml/lectures/week-08.html

# Resources:
#    Lecture: https://uoelel.github.io/qml/lectures/week-08.html
#    Textbook (PDF): https://stefanocoretta.github.io/qdal/Quantitative-Data-Analysis-for-Linguists-in-R.pdf
#    Textbook (Online):https://stefanocoretta.github.io/qdal/ch-regression-bernoulli.html
# ==============================================================================

# Load required packages ----
library(tidyverse)
library(brms)
library(tidybayes)
library(bayesplot)
library(posterior)
library(here)

# 1. Read the data ----
# Scalar Inference (SI) data: binary outcome (SI or no SI) predicted by semantic distance
si_data <- read_csv(here("data", "pankratz2021", "si.csv"), show_col_types = FALSE) %>%
  mutate(SI = if_else(SI == "scalar", 1, 0))

# Check data structure
head(si_data)
glimpse(si_data)

# 2. Fit Bernoulli regression model ----
# Research question: does semantic distance (semdist) affect probability of scalar inference (SI)?
# Model: SI ~ Bernoulli(p) where logit(p) = β₀ + β₁ · semdist

model_si <- brm(
  SI ~ semdist,
  data = si_data,
  family = bernoulli(link = "logit"),
  iter = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  seed = 2025
)

# Model summary
summary(model_si)

# 3. Extract and visualize posterior distributions ----
posterior_draws <- as_draws_df(model_si)

# Plot 1: Posterior distribution of intercept (β₀)
p1 <- posterior_draws %>%
  ggplot(aes(x = b_Intercept)) +
  geom_density(fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Posterior Distribution of Intercept (β₀)",
    x = "Log-odds",
    y = "Density"
  ) +
  theme_minimal()

# Plot 2: Posterior distribution of slope (β₁)
p2 <- posterior_draws %>%
  ggplot(aes(x = b_semdist)) +
  geom_density(fill = "coral", alpha = 0.7) +
  labs(
    title = "Posterior Distribution of Semantic Distance Slope (β₁)",
    x = "Log-odds per unit semdist",
    y = "Density"
  ) +
  theme_minimal()

# Plot 3: Joint posterior
p3 <- posterior_draws %>%
  ggplot(aes(x = b_Intercept, y = b_semdist)) +
  stat_density_2d(aes(fill = after_stat(density)), geom = "raster", contour = FALSE) +
  scale_fill_viridis_c() +
  labs(
    title = "Joint Posterior Distribution",
    x = "Intercept (β₀)",
    y = "Semantic Distance Effect (β₁)"
  ) +
  theme_minimal()

# Plot 4: Posterior predictive check
p4 <- pp_check(model_si, ndraws = 100) +
  labs(title = "Posterior Predictive Check") +
  theme_minimal()

print(p1)
print(p2)
print(p3)
print(p4)

# 4. Expected predictions across semantic distance range ----
# Create new data for prediction across the semantic distance range
semdist_range <- si_data %>%
  summarise(
    min = min(semdist, na.rm = TRUE),
    max = max(semdist, na.rm = TRUE)
  )

newdata <- tibble(
  semdist = seq(semdist_range$min, semdist_range$max, length.out = 100)
)

# Get expected probability of SI at each semdist value using epred_draws
epreds <- model_si %>%
  epred_draws(newdata = newdata)

# Summarize predictions into mean and credible intervals
epreds_summary <- epreds %>%
  group_by(semdist) %>%
  summarize(
    mean = mean(.epred),
    lower = quantile(.epred, 0.05),
    upper = quantile(.epred, 0.95),
    .groups = "drop"
  )

# 5. Plot expected probability of SI across semantic distance ----
p5 <- ggplot(epreds_summary, aes(x = semdist, y = mean)) +
  geom_line(color = "darkblue", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "steelblue") +
  labs(
    title = "Expected Probability of Scalar Inference by Semantic Distance",
    x = "Semantic Distance",
    y = "Probability of SI",
    subtitle = "95% credible interval shown as ribbon"
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal() +
  theme(plot.subtitle = element_text(size = 10, color = "gray40"))

print(p5)

# 6. Model report ----
cat("\n========== BERNOULLI REGRESSION MODEL REPORT ==========\n\n")

cat("RESEARCH QUESTION:\n")
cat("Does semantic distance (semdist) of weak and strong adjectives affect\n")
cat("the probability of a scalar inference (SI)?\n\n")

cat("MODEL SPECIFICATION:\n")
cat("SI ~ Bernoulli(p) with logit link\n")
cat("logit(p) = β₀ + β₁ · semdist\n\n")

cat("INTERPRETATION OF COEFFICIENTS:\n")
intercept_mean <- mean(posterior_draws$b_Intercept)
slope_mean <- mean(posterior_draws$b_semdist)
slope_lower <- quantile(posterior_draws$b_semdist, 0.025)
slope_upper <- quantile(posterior_draws$b_semdist, 0.975)

cat(sprintf("Intercept (β₀): %.3f log-odds\n", intercept_mean))
cat(sprintf("  - This is the log-odds of SI when semdist = 0\n"))
cat(sprintf("  - Probability at semdist=0: %.3f\n\n", plogis(intercept_mean)))

cat(sprintf("Semantic Distance Slope (β₁): %.3f log-odds per unit [95%% CrI: %.3f, %.3f]\n",
            slope_mean, slope_lower, slope_upper))

if (slope_mean > 0) {
  cat("  - POSITIVE effect: greater semantic distance INCREASES probability of SI\n")
} else {
  cat("  - NEGATIVE effect: greater semantic distance DECREASES probability of SI\n")
}

cat("\nMODEL SUMMARY:\n")
print(model_si)

cat("\n========== END REPORT ==========\n")
