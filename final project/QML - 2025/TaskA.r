# ==============================================================================
# Final Project
# Title: Re-Analyzing Ota, Hartsuiker and Haywood (2009).docx
# Author: Sandria Tran and Violet Manson
# Date: 11-18-2025


# Notes from TA (Find Space Later)
# Methodology is similar
# analytical visual word
# model for reaction time (log - best for rection time)
# accuracy data - (which model is the best: Bernoulli
# TA's question:
# Methodology is similar
# analytical visual word
#For Accuracy Data: Bernoulli Distribution
#For accuracy data (correct/incorrect responses), the textbook recommends:
#Bernoulli Models for Binary Accuracy:

#When to use: When accuracy is coded as binary (correct/incorrect, yes/no)
#The model:

#r   brm(accuracy ~ predictor,
#       family = bernoulli,
#       data = your_data)

#Key characteristics:

#Uses logit link function
#Models probability of correct response
#Estimates are in log-odds



Alternative Models for Accuracy-Related Data:

Beta distribution: For proportions (e.g., proportion correct across multiple trials), when values are between 0 and 1 but not including 0 or 1
Zero/One-Inflated Beta (ZOIB): When proportion data includes many 0s and 1s
Binomial: When you have counts of successes out of a known number of trials

Important Theoretical Note:
The textbook emphasizes that model selection should be based on theoretical considerations, not empirical distribution checking. The choice should depend on:

The nature of the variable (can it be negative? zero?)
The measurement process
Prior theoretical knowledge about the variable

So in summary:

Reaction times → Log-normal model ✓
Binary accuracy data → Bernoulli model ✓
Proportion accuracy → Beta or ZOIB model (depending on whether 0s and 1s are present)

# ==============================================================================
#Should follow this general structure:
Paper: The KEY to the ROCK: Near-homophony in nonnative visual word recognition

Year: 2009

Authors: Mitsuhiko Ota, Robert J. Hartsuiker, Sara L. Haywood

Language: English

Population: Japanese

Abstract:
To test the hypothesis that native language (L1) phonology can affect the lexical representations of nonnative words, a visual semantic-relatedness decision task in English was given to native speakers and nonnative speakers whose L1 was Japanese or Arabic. In the critical conditions, the word pair contained a homophone or near-homophone of a semantically associated word, where a near-homophone was defined as a phonological neighbor involving a contrast absent in the speaker’s L1 (e.g., ROCK–LOCK for native speakers of Japanese). In all participant groups, homophones elicited more false positive errors and slower processing than spelling controls. In the Japanese and Arabic groups, near-homophones also induced relatively more false positives and slower processing. The results show that, even when auditory perception is not involved, recognition of nonnative words and, by implication, their lexical representations are affected by the L1 phonology.

Data Description:

    Subject : Participant ID
    Procedure: Whether the trial is practice(Practice Pro) of a test trial (TrialProc)
    Version: Trial Version
    Contrast: Type of contrast (F filler, H homophone, LR /l~r/, P phonological, PB /p~b/).
    Item: Item number.
    Condition: Trial condition (whether the pair contains Control, Related, or Unrelated words),
    WorldL: Word shown on the left-side of the screen.
    WordR: Word shown on the right-side of the screen.
    Words.ACC: Whether the participant correctly identified the pair being related or unrelated.
    Words.RT: Reaction time of response in milliseconds.

DOI: https://doi.org/10.1016/j.cognition.2008.12.007

KEYWORDS: Nonnative language
          phonology Visual
          Word Recognition
          Homophone
          Lexical representation
          Bilingualism
          Arabic
          Japanese
OUTLINE:
        I. Introduction & Background
            (a) Problem: L1-L2 phonemic mismatches
            (b) Example: Japanese /l/-/r/ difficulty
            (c) Prior research on spoken word recognition
            (d) Knowledge gap: effects on visual/written recognition

        II. Theoretical Framework
            (a) Phonological mediation in visual word recognition
            (b) Homophone interference effects in reading
            (c) Extension to nonnative speakers
            (d) Near-homophones as test case

        III. Methodology
            (a) Participants: 20 native English, 20 Japanese, 20 Arabic speakers
            (b) Screening task for phoneme identification
            (c) Semantic-relatedness decision task
            (d) Materials: 20 homophones, 20 /l-r/ pairs, 20 /p-b/ pairs
            (e) Lexical knowledge test (offline verification)

        IV. Results
            (a) Accuracy analysis: Error rates by group and condition
            (b) Reaction time analysis: Latency patterns
            (c) Key findings: Near-homophone effects match prediction patterns

        V. Discussion
            (a) Confirmation of hypotheses
            (b) Double dissociation evidence
            (c) Implications for lexical representation theory
            (d) Evidence against perceptual-only accounts

CRITICAL FINDING:
    (a) The critical finding: participants showed processing difficulties (more errors and slower reaction times) not only with homophones but also with "near-homophones"—word pairs differing by phonological contrasts absent in the speaker's L1.

    (b)For instance, Japanese speakers struggled with ROCK-LOCK pairs because Japanese lacks the /l/-/r/ contrast. Crucially, these effects occurred during silent visual word recognition, demonstrating that L1 phonology shapes L2 lexical representations independent of auditory perception. The results provide direct evidence that late bilinguals maintain indeterminate lexical entries for L2 words involving nonnative contrasts.
Key Vocabulary : Terms and Concepts
    (a) L1: Native Language
    (b) L2: Second Language
    (c) Homophone: Words identicial in pronunciation but different in meaning
    (d) Near-homophone: words differing by contrast abasent in speaker's L1
    (e) Phonoloigcal mediation: accessing sound information during visual word recognition
    (f) Lexical representation: mental storage of word properties including phonology
    (g) Representational indeterminacy: Failure to maintain separate lexical entries for minimal pairs
    (h) False positive error: Incorretly judging unrelated words as semantically related
    (i) Semantic interference Phonological activation causing meaning-based processing difficulty

Paper challenges:  modularity assumption in bilingual cognitive science—the notion that knowledge and processing related to bilingual's ' first language (L1) and second language (L2) are completely independent systems or modules
    The main idea of modularity is that cognitive processes involved in 1 language do not affect or overlap those involved in another langugae

TASK: The researchers used a visual semantic-relatedness decision task where participants judged if two words on a screen were related.
PARTICIPANTS: The study included native English speakers, native Japanese speakers (who lack the /l/–/r/ contrast), and native Arabic speakers (who lack the /p/–/b/ contrast). (However, only native Japanese speakers are avaialble)
KEY CONDITIONS: The critical manipulation involved "near-homophones"—word pairs that differ by a phonological                contrast absent in a speaker's L1 (e.g., LOCK and ROCK for Japanese speakers).

Analysis: The study relied on frequentist statistics, specifically mixed-design ANOVAs, to analyze error rates and reaction times, reporting results separately for participants (F1) and items (F2).

Conclusion: They found that near-homophones caused significantly more false positive errors and slower reaction times for the relevant nonnative groups. This supported the idea that the L1 phonology leads to "indeterminate" or overlapping lexical representations for these L2 words, an effect independent of auditory misperception.


Goals: (1) Test robustness of The KEY to the ROCK: Near-homophony in nonnative visual word recognition (Mitsuhiko Ota, Robert J. Hartsuiker, Sara L. Haywood, 2009) by replacing the original study's by-participant (F1) and by-item (F2) ANOVAS with a Bayesian hierarchical (or multilevel model) or Multivariate regression model
        (2) The original research Ota et al. (2009)  relied on frequentist statistics, specifically mixed-design ANOVAs, to analyze error rates and reaction times, reporting results separately for participants (F1) and items (F2) to find if the F-statistic for the interaction is large enough to yield a p-valie <.05, which allows the rejection of null hypothesis
        (3) Re-analyze their data using a Bayesian approach is a well-founded strategy for testing the robustness of their finding
        (4) New Bayesian model would provide nuanced interpretation of the evidence, moving beyond the binary significance testing of the original paper to estimate the magnitude and certainty of the near-homophone effect.
        (5) Main analysis is to re-analyze the original data, your project can contribute to this discussion by confirming whether the conclusions from Ota, Hartsuiker, and Haywood (2009) hold up under a different and more modern statistical framework.
        (6) Does the Bayesian uphold robustness of their influential findings on how L1 phonology shapes the L2 mental lexicon.
        (7) Bayesian goal of parameter estimation (quantifying the size and uncertainty of an effect)

        (8) Answer of the re-analyzed Bayesian model should:
            (a) Numerically demonstrate magnitude (e..g., "an increase in probability of X&")
            (b) compute certainty ("with a 95% credible interval of [X, Y]")

Re-Analyzing Ota, Hartsuiker and Haywood (2009)
Hypothesis: The phonological system of a speaker&#39;s native language (L1) causes representational ambiguity for L2 words that contain non-native sound contrasts. This ambiguity, termed &quot;near-homophony,&quot; will cause processing interference similar to that of true homophones, even in a purely visual task where no sound is involved  (Ota, Hartsuiker, &amp; Haywood, 2009, p. 1)  (Ota, Hartsuiker, &amp; Haywood, 2009, p. 2).


Task:  A visual semantic-relatedness decision task in English
Dependent Variables: Error Rates(false positives) )

Draft: TaskA.r: Word Accuracys of L2 Japanese Speakers in Visual Semantic-Relatedness Decision Task

    (1) Read key-rock.csv data. Filter the dataset to only include trials where the Condition is unrelated (Isolate False Positives)

    (2) Fit a Bayesian regression model approach to answer the following question: "When the addresser presents a message with an /l/-/r/ ambiguity, how much more likely is the addressee to misinterpret it (e.g. false positive error), compared to when the addresser presents a Spelling control (baseline message)?

    (3) Write a paragraph reporting the model. Produce plots of the posterior distributions of the model parameters and the expected predictions of what should be the output of this bayesian model (e.g. Produce plots of the posterior distributions of the model parameters and the expected predictions of accuracy of Japanese L2 speakers' words accuracy.

    (4) Discuss the results with your group (no need to write the discussion).



Data: in folder "data" -> "ota2009" -> "key-rock.csv"

Model Documentation:
    (1) Load required packages
    (2) Read the data | Read the ota2009/key-rock.csv (Data Documentation )
    (3) Check the data structure
    (4) Explore Visualization
       (a) Visualize the raw data relationship
       (b) Numeric (0/1) Conversion for visualisation
    (5) Fit Bernoulli regression model with brms (Bayesian Model) ----
        (a) Research question:
        (b) (e.g. Model: X ~ Bernoulli(p) where logit(p) = β₀ + β₁ · cofactor
    (6) Extract and visualize posterior distributions
        (a) Plot 1: Posterior distribution of intercept (β₀)
        (b) Plot 2: Posterior distribution of slope (β₁)
        (c) Plot 3: Joint posterior
        (d) Plot 4: Posterior predictive check
    (7) Save plots to both central outputs
    (8) Expected predictions | ???
    (9) Use epred_draws for predictions
    (10) Summarize predictions into mean and credible intervals
    (11) Save p5 to both central outputs
    (12)  Model report ----



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
si <- read_csv(here("data", "pankratz2021", "si.csv"), show_col_types = FALSE) %>%
  mutate(
    SI = factor(SI, levels = c("no_scalar", "scalar"))
  )

# Check data structure
head(si)
glimpse(si)

# 2. Exploratory visualization ----
# First, visualize the raw data relationship between semdist and SI
# Convert SI to numeric (0/1) for visualization purposes
si %>%
  mutate(SI = as.numeric(SI) - 1) %>%
  ggplot(aes(semdist, SI)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Scalar Inference by Semantic Distance",
    subtitle = "Frequentist logistic regression (glm with binomial family)",
    x = "Semantic Distance",
    y = "Probability of SI"
  ) +
  theme_minimal()

# Note: geom_smooth() with method = "glm" and family = "binomial" fits a
# Maximum Likelihood Estimation (MLE) Bernoulli model SI ~ semdist
# This provides a quick frequentist baseline before our Bayesian analysis

# 3. Fit Bernoulli regression model with brms ----
# Research question: does semantic distance (semdist) affect probability of scalar inference (SI)?
# Model: SI ~ Bernoulli(p) where logit(p) = β₀ + β₁ · semdist

model_si <- brm(
  SI ~ semdist,
  data = si %>% mutate(SI = as.numeric(SI) - 1),
  family = bernoulli(link = "logit"),
  iter = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  seed = 2025
)

# Model summary
summary(model_si)

# 4. Extract and visualize posterior distributions ----
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

# Save plots to both central outputs and Week 8 outputs folders


# 5. Expected predictions across semantic distance range ----
# Create new data for prediction across the semantic distance range


# Get expected probability of SI at each semdist value using epred_draws
epreds <- model_si %>%
  epred_draws(newdata = newdata)

# Summarize predictions into mean and credible intervals

  )

# 6. Plot expected probability of SI across semantic distance ----
p5 <- ggplot()

print(p5)

# Save p5 to both central outputs and Week 8 outputs folders
ggsave(here("code", "outputs", "Final_TaskA_p5.png"), p5,
       width = 8, height = 6, dpi = 300)
ggsave(here("code", "Week 8", "outputs", "Final_TaskA_p5.png"), p5,
       width = 8, height = 6, dpi = 300)

# 7. Model report ----










# ==============================================================================
(4) Discuss the results with your group (no need to write the discussion)
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

# Lecture: https://uoelel.github.io/qml/lectures/week-08.html

# Resources:
#    Lecture: https://uoelel.github.io/qml/lectures/week-08.html
#    Textbook (PDF): https://stefanocoretta.github.io/qdal/Quantitative-Data-Analysis-for-Linguists-in-R.pdf
#    Textbook (Online):https://stefanocoretta.github.io/qdal/ch-regression-bernoulli.html
# ==============================================================================
