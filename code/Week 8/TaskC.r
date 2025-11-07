# ==============================================================================
# Week 8 | https://uoelel.github.io/qml/workshops/week-07.html
# Title: Task C: Simulate data
# Author: Sandria Tran
# Date: 2025-11-6
# Topic: Workshop:  Bernoulli regression
#        Lecture:   Bernoulli regression
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
#         Focus on:
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
# Shallow Structure data: accuracy and reaction times by relation type
shallow <- read_csv(here("data", "song2020", "shallow.csv"), show_col_types = FALSE)

# 2. Filter data: L2 speakers + critical trials + correct responses with valid RTs ----
shallow_l2_rt <- shallow %>%
  filter(Group == "L2", Critical_Filler == "Critical", ACC == 1) %>%
  filter(RT > 0, RT < 5000, !is.na(RT)) %>%
  drop_na(Relation_type)

cat("Filtered data: L2 speakers, critical trials, correct responses only\n")
cat("N =", nrow(shallow_l2_rt), "observations\n")
cat("Mean RT:", round(mean(shallow_l2_rt$RT), 0), "ms\n")
cat("Relation types:", unique(shallow_l2_rt$Relation_type), "\n\n")

# 3. Fit log-normal regression model ----
# Question: does relation_type affect reaction times (positive-valued outcome)?
# Model: log(RT) ~ Gaussian(μ, σ) where μ = β₀ + β_relation
# This is a log-normal family (continuous, positive outcomes)

model_rt <- brm(
  RT ~ Relation_type,
  data = shallow_l2_rt,
  family = lognormal(),
  iter = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  seed = 2025
)

# Model summary
summary(model_rt)

# 4. Extract posterior distributions ----
posterior_draws <- as_draws_df(model_rt)

# Plot 1: Intercept posterior
p1 <- posterior_draws %>%
  ggplot(aes(x = b_Intercept)) +
  geom_density(fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Posterior: Intercept (β₀)",
    x = "log(RT) (ms)",
    y = "Density"
  ) +
  theme_minimal()

# Plot 2: Relation type effects
p2 <- posterior_draws %>%
  select(starts_with("b_Relation_type")) %>%
  pivot_longer(everything(), names_to = "parameter", values_to = "value") %>%
  ggplot(aes(x = value, fill = parameter)) +
  geom_density(alpha = 0.6) +
  labs(
    title = "Posterior: Relation Type Effects (β₁, β₂, ...)",
    x = "log(RT) adjustment",
    y = "Density"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot 3: Sigma (error SD) posterior
p3 <- posterior_draws %>%
  ggplot(aes(x = sigma)) +
  geom_density(fill = "coral", alpha = 0.7) +
  labs(
    title = "Posterior: Error SD (σ)",
    x = "Sigma",
    y = "Density"
  ) +
  theme_minimal()

# Plot 4: Posterior predictive check
p4 <- pp_check(model_rt, ndraws = 100) +
  labs(title = "Posterior Predictive Check") +
  theme_minimal()

print(p1)
print(p2)
print(p3)
print(p4)

# 5. Expected reaction time predictions by relation type ----
relation_types <- shallow_l2_rt %>%
  distinct(Relation_type) %>%
  pull(Relation_type)

newdata <- tibble(Relation_type = relation_types)

# Get predicted RTs
epreds <- model_rt %>%
  epred_draws(newdata = newdata)

# Summarize predictions
epreds_summary <- epreds %>%
  group_by(Relation_type) %>%
  summarize(
    mean_rt = mean(.epred),
    lower = quantile(.epred, 0.05),
    upper = quantile(.epred, 0.95),
    .groups = "drop"
  )

# Plot expected RTs
p5 <- ggplot(epreds_summary, aes(x = Relation_type, y = mean_rt)) +
  geom_point(size = 3, color = "darkblue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "darkblue") +
  labs(
    title = "Expected Reaction Time by Relation Type (L2, Critical Trials)",
    x = "Relation Type",
    y = "Reaction Time (ms)",
    subtitle = "90% credible intervals shown as error bars"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p5)

# 6. Model report ----
cat("\n========== LOG-NORMAL REGRESSION: REACTION TIMES ==========\n\n")

cat("RESEARCH QUESTION:\n")
cat("How does relation type of prime-target affect reaction times in L2 speakers\n")
cat("on critical syntactic trials (correct responses only)?\n\n")

cat("DATA:\n")
cat("- L2 speakers only\n")
cat("- Critical (not filler) trials\n")
cat("- Correct responses only (ACC = 1)\n")
cat("- N =", nrow(shallow_l2_rt), "\n")
cat("- RT range:", round(min(shallow_l2_rt$RT), 0), "-", round(max(shallow_l2_rt$RT), 0), "ms\n")
cat("- Relation types:", paste(relation_types, collapse = ", "), "\n\n")

cat("MODEL SPECIFICATION:\n")
cat("RT ~ log-normal(μ, σ)\n")
cat("μ = β₀ + β₁·RelationType (on log scale)\n")
cat("Predictions are back-transformed to milliseconds\n\n")

cat("INTERPRETATION:\n")
cat("Log-normal family is appropriate for positive-valued outcomes like RT.\n")
cat("Coefficients are on the log scale; negative values indicate faster RTs.\n\n")

cat("PREDICTED REACTION TIMES BY RELATION TYPE:\n")
print(epreds_summary %>%
        select(Relation_type, mean_rt, lower, upper) %>%
        mutate(across(where(is.numeric), ~round(., 1))))

cat("\nMODEL SUMMARY:\n")
print(model_rt)

cat("\n========== END REPORT ==========\n")
