# ----------------------------------------------------------------------
# Week 4
# Title: Task C: Reading fixation times (eye-tracking)
# Author: Sandria
# Date: 2025-10-10
# Lecture: Bayesian inference
# Workshop: Probabilities and Gaussian models | Gaussian models with brms
# Instructions:
#   (1) Read berzak2025/fix-dur.csv and familiarise yourself with data
#   documentation and study linked therein.
#   (2) Answer the following question using a Gaussian model:
#   (3) What is the mean and SD of fixation durations in the data set?
#   (4) Now run another model but with only 5 participants (sample 5 random
#   participants from the data set). What happens to the posterior
#   distributions of the mean and SD?
# SOLUTION: Sample size matters: 
#            the lower the sample size, the higher the uncertainty.
# Learning Objectives:
#    (i) Question:
#               (1) What are probabilities and probability distributions?
#                (a) probabilities are # between 0 and 1 that express how likely
#                    an event is to occurs
#                    (i) 0 means impossible
#                    (ii) 1 means certain 
#                (b) probability distribution describes how probabilities are
#                    assigned to all possible outcomes of a random process or variable
#                    (i) instead of focusing on a single event
#                    (ii) a distribution gives full picture by specifying the probability
#                        of every possible value a random variable can take
#                    (iii) discrete probability distribution
#                        (1) distinct separatable values
#                        (2) dice rolls, # of correct answers
#                        (3) probabilities for all possible outcomes must add up to 1
#                    (iv) continuous probability distribution 
#                        (1) take any value within a range (e.g. reaction time in lingusitics data)
#                        (2) we consider intervals than individual values
#                    (v) probabilities quantify uncertainy while probability distributions
#                        organize thse probabilities across all possible outcomes
#                        to analyze random phenomena 
#               (2) How do we describe and visualise probability distributions?
#                    (a) describing probability distributions involves:
#                        (i) summarizing likely different outcomes for random variable
#                        (ii) discrete variables: listing probability for each possible value
#                        (iii) continuous variable: describes the shape of the distribution
#                    (b) summarizing: using statistics -> mean, median, standard deviation
#                    (c) visualizing probability distributions: to see features/patterns of data:
#                       (i) HISTOGRAMS: frequency (or probability of data) falling into bins
#                          helps to see shape and spread of a Distribution
#                          STRENGTH: SEEING SKEWNESS AND MODALITY
#                       (ii) DENSITY PLOT: smooths out histograms
#                          show the estimated probability density for continuous data 
#                          STRENGTH: SEEING SKEWNESS AND MODALITY
#                      (iii) BOXPLOT: summarizes the distribution with 
#                            quartiles, median, and potential outliers
#                         STRENGTH: GOOD FOR COMPARING GROUPS
#                      (iv) EMPIRICAL CUMULATIVE DISTRIBUTION FUNCTION (ECDF):
#                          plots the proportion of data points >= to each value, giving stepwise curve
#                         STRENGTH: GOOD FOR COMPARING GROUPS
#                      (v) VIOLIN PLOT
#                          combines a boxplot + density plot to illustrate:
#                          (a) summary statistics + distribution's shape 
#               (3) How do we use Gaussian probability distributions to estimate a mean and standard deviation?
#                         (a) what is a Gaussian distribution?
#                             (i) bell-shaped curve that is symmetric around mean
#                                 defined by: (1) mean (μ): center of distribution
#                                             (2) standard deviation (σ): how spread out the values are 
#                             (ii) you can use Gaussian probability
#                               (a) summarize distribution (symmetrical, unimodal - bell shaped)
#                               (b) claim likelihood of values falling within certain ranges
#               (4) What is the Bayesian approach to probability and inference?
#                     (a) treats probability as a measure of belif or certainty about an event
#                     (b) does not treat as a long-run frequency
#                     (c) requires the following:
#                         (a) prior: initial belief in a parameters
#                         (b) evidence: data collected
#                         (c) updating belief using Bayes' Theorem 
#                               (i) P(A|B) = \frac{P(B|A)\,P(A)}{P(B)}
#                         (d) that results in a full posterior distribution:
#                               (i) new, updated belief after seeing data/likelihood/evidence 
#                               (ii) Bayesian inference doesn't produce a single stimate
#                               (iii) and produces a full probability distribution given parameter of choice
#                         (e) Frequentist Interpretation 
#                             (i) long-run freqeuncy of an event occurring in identicial, repeated experiments 
#                             (ii) mean &/|| proportion are considered fixed but unknown values
#                             (iii) frequentist confidence interval gives a range that would contain the true parameter 
#                             in a certain proportion of repeated samples, but does not assign a probability to the parameter itself
#                         (f) Bayesian Interpretation 
#                             (i) probability: measure of belief or certainty about event/parameters
#                             based on given available Information
#                             (ii) parameters: treated as random variables with probability distributions that reflect uncertainty
#                             (iii) combines: (1) prior beliefs (prior)
#                                           (2) observed data (the likelihood or evidence)
#                                          (3) an updated belief (the posterior distribution)
#                                         (4) VIEWPOINT: probability as degree of belief/certainty 
#                                                           about parameters oy hypotheses 
#                                                           updating/updated as new data becomes available 
#    (ii) Skills:
#               (1) Produce density plots.
#               (2)Use pnorm() and qnorm() to obtain probabilities and quantiles of                Gaussian distributions.
#               (3)Fit Bayesian Gaussian models with brms.
#                (4) Report results from Bayesian Gaussian models.
# Important Terms & Information:
#        (a) fix_dur = raw data (full dataset)
#        (b) fix_dur_bm = bayesian model fitted to full dataset 
#        (c) fix_dur_5 = raw data (5 participants only) 
#        (d) fix_dur_5_bm = bayesian model fitted to 5 participants
#                 _bm - suffix means "Bayesian model"
#        (e) set.seed(123) - ensures you get the same 5 participants 
#                        > important for: (1) comparing results across runs
#                                         (2) debugging       
#                                         (3) academic reproducibility
#  Lecture: https://uoelel.github.io/qml/lectures/week-04.html
#       Textbook (Online): https://stefanocoretta.github.io/qdal/ch-probability.html
#       Textbook (PDF): https://stefanocoretta.github.io/qdal/Quantitative-Data-Analysis-for-Linguists-in-R.pdf
# ----------------------------------------------------------------------

library(tidyverse)
library(here)
# Load brms for Bayesian modeling
library(brms)

# Create central outputs directory
output_dir <- here("code", "outputs")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

fix_dur <- read_csv(here("data", "berzak2025", "fix-dur.csv"))

# Full dataset
mean(fix_dur$CURRENT_FIX_DURATION)
sd(fix_dur$CURRENT_FIX_DURATION)

# Visualize raw data - full dataset
ggplot(fix_dur, aes(x = CURRENT_FIX_DURATION)) + geom_histogram(bins = 30)
ggsave(here("code", "Week 4", "TaskC_full_histogram.png"))
ggsave(here("code", "outputs", "Week4_TaskC_full_histogram.png"))

# Load brms for Bayesian modeling
library(brms)

# Fit Bayesian Gaussian model to FULL fixation duration data
fix_dur_bm <- brm(
  CURRENT_FIX_DURATION ~ 1,
  family = gaussian,
  data = fix_dur
)

# Display model summary for full dataset
summary(fix_dur_bm)
plot(fix_dur_bm)        # Check MCMC convergence
pp_check(fix_dur_bm)    # Check model fit
ggsave(here("code", "Week 4", "TaskC_full_ppcheck.png"))
ggsave(here("code", "outputs", "Week4_TaskC_full_ppcheck.png"))

# Sample 5 random participants
set.seed(123)  # For reproducibility | ensures same 5 participants
random_participants <- fix_dur |>
  distinct(participant_id) |>
  slice_sample(n = 5) |> # randomly select 5 participants
  pull(participant_id)

# Print which participants were selected
print(random_participants)

fix_dur_5 <- fix_dur |> filter(participant_id %in% random_participants)

# Calculate Gaussian parameters for 5 participants (for reference)
mean(fix_dur_5$CURRENT_FIX_DURATION)
sd(fix_dur_5$CURRENT_FIX_DURATION)

# Visualize raw data - 5 participants
ggplot(fix_dur_5, aes(x = CURRENT_FIX_DURATION)) + geom_histogram(bins = 30)
ggsave(here("code", "Week 4", "TaskC_5participants_histogram.png"))
ggsave(here("code", "outputs", "Week4_TaskC_5participants_histogram.png"))

# Fit Bayesian Gaussian model to 5 PARTICIPANTS fixation duration data
fix_dur_5_bm <- brm(
  CURRENT_FIX_DURATION ~ 1,
  family = gaussian,
  data = fix_dur_5
)

# Display model summary for 5 participants
summary(fix_dur_5_bm)
plot(fix_dur_5_bm)        # Check MCMC convergence
pp_check(fix_dur_5_bm)    # Check model fit
ggsave(here("code", "Week 4", "TaskC_5participants_ppcheck.png"))
ggsave(here("code", "outputs", "Week4_TaskC_5participants_ppcheck.png"))

# =============================================================================
# TASK C: READING FIXATION TIMES (EYE-TRACKING)
# FULL FIXATION DURATIONS IN DATA SET
# 5 RANDOM PARTICIPANTS (FIXED SEED SET.SEED(123)
# =============================================================================

#OUTPUT OF FULL DATASET MODEL:

  #summary(fix_dur_bm)
  #Sample size: 7,519 observations

  #Regression Coefficients:
  #          Estimate Est.Error l-95% CI u-95% CI
  #Intercept   199.62      1.08   197.54   201.75

  #Further Distributional Parameters:
  #      Estimate Est.Error l-95% CI u-95% CI
  #sigma    94.69      0.77    93.20    96.22

# GAUSSIAN MODEL PARAMETER
# Full Fixation Durations in the Data Set
# μ = 199.59 ms
# σ = 94.69 ms
      # μ = P_{μ} (Posterior Probability Distribution)
      # σ = P_{σ} (Posterior Probability Distribution)


# CALCULATIONS:
# μ - 3σ = 199.59 - (3 × 94.69) = 199.59 - 284.07 = -84.48 ms  (≈ 0, can't be negative)
# μ - 2σ = 199.59 - (2 × 94.69) = 199.59 - 189.38 = 10.21 ms
# μ - σ  = 199.59 - 94.69 = 104.90 ms
# μ      = 199.59 ms
# μ + σ  = 199.59 + 94.69 = 294.28 ms
# μ + 2σ = 199.59 + (2 × 94.69) = 199.59 + 189.38 = 388.97 ms
# μ + 3σ = 199.59 + (3 × 94.69) = 199.59 + 284.07 = 483.66 ms

# VERTICAL DISTRIBUTION:
#                   Gaussian  Distribution
#           Fixation Duration ~ Gaussian(μ = 199.59, σ = 94.69)
#             FULL DATASET (7,519 observations)
#                         Frequency
#       |
#       |                    ***
#       |                  *******
#       |                 *********
#       |                ***********
#       |               *************
#       |              ***************
#       |             *****************
#       |            *******************
#       |          *********************
#       |        *************************
#       |      *****************************
#       |    *********************************
#       |  *************************************
#       |*****************************************
#   ────┴────────┴────────┴────────┴────────┴───────── Duration (ms)
#       0      104.90    199.59   294.28   483.66
#       ↑         ↑       ↑         ↑        ↑
#     μ-3σ      μ-σ      μ        μ+σ      μ+3σ
#
#       |←────── 68% ──────→|      (104.90 - 294.28 ms)
#       |←────────── 95% ────────────→|   (10.21 - 388.97 ms)
#       |←─────────────── 99.7% ───────────────→| (≈0 - 483.66 ms)


# HORIZONTAL DISTRIBUTION:
#┌────────────────────────────────────────────────────────────────────────┐
#│          Fixation Duration Distribution (FULL DATASET)                 │
#│                  7,519 observations from all participants              │
#├────────────────────────────────────────────────────────────────────────┤
#│                                                                        │
#│   0.15%          2.35%             68%             2.35%        0.15%  │
#│     |              |                |                |            |    │
#│     ├──────────────┼────────────────┼────────────────┼────────────┤    │
#│     0            105              200              294          484    │
#│   μ-3σ           μ-σ               μ               μ+σ         μ+3σ    │
#│  (-84*)          (10**)                          (389***)              │
#│                                                                        │
#│                [██████████████████████████████]  ← 68% of data         │
#│           [██████████████████████████████████████]  ← 95% of data      │
#│     [██████████████████████████████████████████████]  ← 99.7% of data  │
#│                                                                        │
#│  * μ-3σ = -84.48 ms (truncated to 0, duration can't be negative)       │
#│  ** μ-2σ = 10.21 ms                                                    │
#│  *** μ+2σ = 388.97 ms                                                  │
#│  Fixation Duration in milliseconds (ms)                                │
#└────────────────────────────────────────────────────────────────────────┘

# OUTPUT OF 5 RANDOM PARTICIPANTS FROM THE DATA SET 

# summary(fix_dur_5_bm)
#Sample size: 616 observations (5 participants)

#Regression Coefficients:
#          Estimate Est.Error l-95% CI u-95% CI
#Intercept   203.11      4.39   194.29   211.63

#Further Distributional Parameters:
#      Estimate Est.Error l-95% CI u-95% CI
#sigma   110.87      3.14   104.78   117.04

 
# GAUSSIAN MODEL PARAMETER
# 5 Random Participants from the Data Set
# μ = 203.15 ms
# σ = 110.86 ms

# CALCULATIONS:
# μ - 3σ = 203.15 - (3 × 110.86) = 203.15 - 332.58 = -129.43 ms  (≈ 0, can't be negative)
# μ - 2σ = 203.15 - (2 × 110.86) = 203.15 - 221.72 = -18.57 ms   (≈ 0, can't be negative)
# μ - σ  = 203.15 - 110.86       = 92.29 ms
# μ      = 203.15 ms
# μ + σ  = 203.15 + 110.86       = 314.01 ms
# μ + 2σ = 203.15 + (2 × 110.86) = 203.15 + 221.72 = 424.87 ms
# μ + 3σ = 203.15 + (3 × 110.86) = 203.15 + 332.58 = 535.73 ms

# GAUSSIAN DISTRIBUTION
# 5 Random PARTICIPANTS (616 observations)
# Fixation Duration (5 Samples) ~ Gaussian(μ = 203.15, σ = 110.86)


# VERTICAL DISTRIBUTION:
#               Gaussian Distribution
#    Fixation Duration (5 Samples) ~ Gaussian(μ = 203.15, σ = 110.86)
#              5 Random PARTICIPANTS (616 observations)
#                    Frequency
#  |
#  |                    ***
#  |                  *******
#  |                 *********
#  |                ***********
#  |               *************
#  |              ***************
#  |             *****************
#  |            *******************
#  |          *********************
#  |        *************************
#  |      *****************************
#  |    *********************************
#  |  *************************************
#  |*****************************************
#  ────┴────────┴────────┴────────┴────────┴───────── Duration (ms)
#      0       92.29   203.15  314.01   535.73
#      ↑         ↑       ↑       ↑         ↑
#    μ-3σ      μ-σ      μ      μ+σ      μ+3σ
#
#   |←────── 68% ──────→|      (92.29 - 314.01 ms)
#   |←────────── 95% ────────────→|   (≈0 - 425 ms)
#   |←─────────────── 99.7% ───────────────→| (≈0 - 536 ms)

# HORIZONTAL DISTRIBUTION:
# ┌────────────────────────────────────────────────────────────────────────┐
# │        Fixation Duration Distribution (5 PARTICIPANTS SAMPLE)          │
# │              616 observations from 5 random participants               │
# │            Gaussian(μ = 203.15 ms, σ = 110.86 ms)                      │
# ├────────────────────────────────────────────────────────────────────────┤
# │                                                                        │
# │   0.15%          2.35%             68%             2.35%        0.15%  │
# │     |              |                |                |            |    │
# │     ├──────────────┼────────────────┼────────────────┼────────────┤    │
# │     0             92              203              314          536    │
# │   μ-3σ           μ-σ               μ               μ+σ         μ+3σ    │
# │  (-129*)         (-19**)                                      (425***) │
# │                                                                        │
# │                [██████████████████████████████]  ← 68% of data         │
# │           [██████████████████████████████████████]  ← 95% of data      │
# │     [██████████████████████████████████████████████]  ← 99.7% of data  │
# │                                                                        │
# │  * μ-3σ = -129.43 ms (truncated to 0, duration can't be negative)      │
# │  ** μ-2σ = -18.57 ms (truncated to 0)                                  │
# │  *** μ+2σ = 424.87 ms                                                  │
# │  Fixation Duration in milliseconds (ms)                                │
# └────────────────────────────────────────────────────────────────────────┘


# INTERPRETATION
# 68% of fixations (μ ± σ):
#   Full Data Set: Between 105 and 294 ms (range: 189 ms)
#   5 participants: Between 92 and 314 ms (range: 222 ms)
#   Smaller sample has WIDER range = more uncertainty
#
# 95% of fixations (μ ± 2σ):
#   Full Data Set: Between 10 and 389 ms (range: 379 ms)
#   5 participants: Between ≈0 and 425 ms (range: 425 ms)
#
# 99.7% of fixations (μ ± 3σ):
#   Full Data Set: Between ≈0 and 484 ms (range: 484 ms)
#   5 participants: Between ≈0 and 536 ms (range: 536 ms)

# SUMMARY
# Larger Sample Sizes are More Preferred
# The Distribution with 5 participants becomes wider (illustrating uncertainty)
# Smaller sample Size Also Shifts Slightly (Sampling Variability)
# There is an indication of less confidence of "true" (population) μ / mean
