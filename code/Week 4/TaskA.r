# ----------------------------------------------------------------------
# Week 4
# Title: Task A: Mixean Basque Voice Onset Time
# Author: Sandria Tran
# Class Quantitative Methods in Linguistics and Language Studies - Fall 2025 
# Date: 2025-10-10
# Lecture: Bayesian inference
# Workshop: Probabilities and Gaussian models | Gaussian models with brms
# Instructions:
#   Read the egurtzegi2020/eu_vot.csv data.
#   Filter the data to include only aspirated consonants.
#   Answer the following question using a Gaussian model:
#   What is the mean and SD of aspirated VOT in Mixean Basque?
#   Solution: μ = 32.54 ms 
#             σ = 14.76 ms
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
#       Lecture: https://uoelel.github.io/qml/lectures/week-04.html
#       Textbook (Online): https://stefanocoretta.github.io/qdal/ch-probability.html
#       Textbook (PDF): https://stefanocoretta.github.io/qdal/Quantitative-Data-Analysis-for-Linguists-in-R.pdf
# ----------------------------------------------------------------------

# Week 4 | 22  Fitting Gaussian models with brms
# https://stefanocoretta.github.io/qdal/ch-fit-model.html
# install.packages("brms")  # Run once to install
library(tidyverse)
library(here)
library(brms)

# Create central outputs directory
output_dir <- here("code", "outputs")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

vot_data <- read_csv(here("data", "egurtzegi2020", "eu_vot.csv"))

# Filter to include only aspirated consonants (ph, th, kh)
aspirated <- vot_data |> filter(phone %in% c("ph", "th", "kh"))

# Calculate Gaussian parameters (for reference)
mean(aspirated$VOT)
sd(aspirated$VOT)

# Visualize raw data
ggplot(aspirated, aes(x = VOT)) + geom_histogram(bins = 30)
ggsave(here("code", "Week 4", "TaskA_histogram.png"))
ggsave(here("code", "outputs", "Week4_TaskA_histogram.png"))

# Load brms for Bayesian modeling
library(brms)

# Fit Bayesian Gaussian model to aspirated VOT data
vot_bm <- brm(
  VOT ~ 1,
  family = gaussian,
  data = aspirated
)

# Display model summary with regression coefficients and sigma
summary(vot_bm)
plot(vot_bm)        # Check MCMC convergence
pp_check(vot_bm)    # Check model fit
ggsave(here("code", "Week 4", "TaskA_ppcheck.png"))
ggsave(here("code", "outputs", "Week4_TaskA_ppcheck.png"))


# =============================================================================
# TASK A: MIXEAN BASQUE VOICE ONSET TIME
# Aspirated Consonants (ph, th, kh)
# =============================================================================

# MODEL RESULTS: 
# (noted that model displays in seconds and not milliseconds)

# summary(vot_bm):
#Family: gaussian 
#  Links: mu = identity 
#Formula: VOT ~ 1 
# Data: aspirated (Number of observations: 135) 
#  Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
         # total post-warmup draws = 4000

#Regression Coefficients:
          # Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#Intercept     0.03      0.00     0.03     0.04 1.00     4401     3064

#Further Distributional Parameters:
      #Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#sigma     0.01      0.00     0.01     0.02 1.00     1125     1320

# GAUSSIAN MODEL PARAMETERS:
# μ = 32.54 ms 
# σ = 14.76 ms

# The Voice Onset Time for aspirated consonants follows
# a Gaussian distribution:
# VOT ~ Gaussian(μ = 32.54 ms, σ = 14.76 ms)

# CALCULATIONS:
# μ - 3σ = 32.54 - (3 × 14.76) = 32.54 - 44.28 = -11.74 ms  (≈ 0, can't be negative)
# μ - 2σ = 32.54 - (2 × 14.76) = 32.54 - 29.52 = 3.02 ms
# μ - σ  = 32.54 - 14.76       = 17.78 ms
# μ      = 32.54 ms
# μ + σ  = 32.54 + 14.76       = 47.30 ms
# μ + 2σ = 32.54 + (2 × 14.76) = 32.54 + 29.52 = 62.06 ms
# μ + 3σ = 32.54 + (3 × 14.76) = 32.54 + 44.28 = 76.82 ms

# VERTICAL DISTRIBUTION:
#                     Gaussian Distribution
#                VOT ~ Gaussian(μ=32.54, σ=14.76)
#              Aspirated Consonants (ph, th, kh)
#
#                           Frequency
#         |
#         |                    ***
#         |                  *******
#         |                 *********
#         |                ***********
#         |               *************
#         |              ***************
#         |             *****************
#         |            *******************
#         |          *********************
#         |        *************************
#         |      *****************************
#         |    *********************************
#         |  *************************************
#         |*****************************************
#     ────┴─────────┴─────────┴─────────┴─────────┴───── VOT (ms)
#         0        17.78    32.54    47.30     76.82
#         ↑          ↑        ↑        ↑          ↑
#       μ-3σ       μ-σ       μ       μ+σ       μ+3σ
#
#         |←────── 68% ──────→|      (17.78 - 47.30 ms)
#         |←────────── 95% ────────────→|   (3.02 - 62.06 ms)
#         |←─────────────── 99.7% ───────────────→| (≈0 - 76.82 ms)

# HORIZONTAL DISTRIBUTION:
# ┌─────────────────────────────────────────────────────────────────────────┐
# │    VOT Distribution (Aspirated Consonants: ph, th, kh)                  │
# │    Mixean Basque Voice Onset Time                                       │
# │    Gaussian(μ = 32.54 ms, σ = 14.76 ms)                                 │
# ├─────────────────────────────────────────────────────────────────────────┤
# │                                                                         │
# │   0.15%          2.35%             68%             2.35%        0.15%   │
# │     |              |                |                |            |     │
# │     ├──────────────┼────────────────┼────────────────┼────────────┤     │
# │     0              3               33               47          77      │
# │   μ-3σ           μ-2σ              μ              μ+σ          μ+3σ     │
# │  (-12*)                                            (62**)               │
# │                                                                         │
# │                [██████████████████████████████]  ← 68% of data          │
# │           [██████████████████████████████████████]  ← 95% of data       │
# │     [██████████████████████████████████████████████]  ← 99.7% of data   │
# │                                                                         │
# │  * μ-3σ = -11.74 ms (truncated to 0, VOT can't be negative)             │
# │  ** μ+2σ = 62.06 ms                                                     │
# │  Voice Onset Time in milliseconds (ms)                                  │
# └─────────────────────────────────────────────────────────────────────────┘

# INTERPRETATION:
# 68% of VOTs (μ ± σ):
#  Aspirated consonants have VOT between 18 and 47 ms
#  Range: 29 ms

# 95% of VOTs (μ ± 2σ):
#  Aspirated consonants have VOT between 3 and 62 ms
#  Range: 59 ms

# 99.7% of VOTs (μ ± 3σ):
#  Aspirated consonants have VOT between ≈0 and 77 ms
#  Range: 77 ms

# SUMMARY:
# On average, aspirated consonants (ph, th, kh) take about 33 ms to start voicing
# The variation is moderate (SD = 15 ms is 45% of the mean), showing that
# while most aspirated sounds are fairly consistent in timing,
# there is some natural variation in how quickly voicing begins
# Most aspirations (68%) happen between 18 and 47 milliseconds after release 
