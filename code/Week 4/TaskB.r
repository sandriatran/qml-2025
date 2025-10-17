# ----------------------------------------------------------------------
# Week 4
# Title: Task B: Shallow morphological parsing (RT)
# Author: Sandria Tran
# Date: 2025-10-10
# Lecture: Bayesian inference
# Workshop: Probabilities and Gaussian models | Gaussian models with brms
# Instructions:
#   Read the song2020/shallow.csv data.
#   Familiarise yourself with the study (https://uoelel.github.io/qml-data/data/song2020/shallow.html).
#   Filter the data to include only L2 data from critical trials.
#   Answer the following question using a Gaussian model:
#   What is the mean and SD of reaction times in L2 participants?
#   SOLUTION: μ = 967.75 ms
#             σ = 354.06 ms
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
library(tidyverse)
library(here)
# Load brms for Bayesian modeling
library(brms)

# Create central outputs directory
output_dir <- here("code", "outputs")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

shallow <- read_csv(here("data", "song2020", "shallow.csv"))

# Filter to L2 participants in critical trials only
l2_critical <- shallow |> filter(Group == "L2", Critical_Filler == "Critical")

# Calculate Gaussian parameters (for reference)
mean(l2_critical$RT)
sd(l2_critical$RT)

# Visualize raw data
ggplot(l2_critical, aes(x = RT)) + geom_histogram(bins = 30)
ggsave(here("code", "Week 4", "TaskB_histogram.png"))
ggsave(here("code", "outputs", "Week4_TaskB_histogram.png"))

# Fit Bayesian Gaussian model to L2 critical trial RT data
rt_bm <- brm(
  RT ~ 1,
  family = gaussian,
  data = l2_critical
)

# Display model summary with regression coefficients and sigma
summary(rt_bm)
plot(rt_bm)        # Check MCMC convergence
pp_check(rt_bm)    # Check model fit
ggsave(here("code", "Week 4", "TaskB_ppcheck.png"))
ggsave(here("code", "outputs", "Week4_TaskB_ppcheck.png"))

# =============================================================================
# TASK B: SHALLOW MORPHOLOGICAL PARSING (REACTION TIME)
# L2 Participants in Critical Trials
# =============================================================================

#SUMMARY(RT_BM) Output 

# Family: gaussian 
#  Links: mu = identity 
#Formula: RT ~ 1 
#   Data: l2_critical (Number of observations: 1080) 
#  Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#         total post-warmup draws = 4000

#Regression Coefficients:
#          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#Intercept   967.60     10.52   947.44   988.02 1.00     3493     2897

#Further Distributional Parameters:
#      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#sigma   354.21      7.69   340.09   369.82 1.00     4027     2544

# GAUSSIAN MODEL PARAMETERS:
# 
# μ = 967.75 ms
# σ = 354.06 ms

# The reaction times for L2 participants in critical trials follow
# a Gaussian distribution:
# RT ~ Gaussian(μ = 967.75 ms, σ = 354.06 ms)
                # μ = P_{μ} (Posterior Probability Distribution)
                # σ = P_{σ} (Posterior Probability Distribution)

# CALCULATIONS:
# μ - 3σ = 967.75 - (3 × 354.06) = 967.75 - 1062.18 = -94.43 ms   (≈ 0, can't be negative)
# μ - 2σ = 967.75 - (2 × 354.06) = 967.75 - 708.12  = 259.63 ms
# μ - σ  = 967.75 - 354.06       = 613.69 ms
# μ      = 967.75 ms
# μ + σ  = 967.75 + 354.06       = 1321.81 ms
# μ + 2σ = 967.75 + (2 × 354.06) = 967.75 + 708.12  = 1675.87 ms
# μ + 3σ = 967.75 + (3 × 354.06) = 967.75 + 1062.18 = 2029.93 ms

# VERTICAL DISTRIBUTION:
#                     Gaussian Distribution
#                RT ~ Gaussian(μ=967.75, σ=354.06)
#              L2 Participants - Critical Trials
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
#     ────┴─────────┴─────────┴─────────┴─────────┴───── RT (ms)
#         0       613.69   967.75   1321.81   2029.93
#         ↑         ↑        ↑         ↑          ↑
#       μ-3σ      μ-σ       μ        μ+σ       μ+3σ
#
#         |←────── 68% ──────→|      (613.69 - 1321.81 ms)
#         |←────────── 95% ────────────→|   (259.63 - 1675.87 ms)
#         |←─────────────── 99.7% ───────────────→| (≈0 - 2029.93 ms)

# HORIZONTAL DISTRIBUTION:
# ┌─────────────────────────────────────────────────────────────────────────┐
# │         RT Distribution (L2 Critical Trials)                            │
# │         Shallow Morphological Parsing Task                              │
# │         Gaussian(μ = 967.75 ms, σ = 354.06 ms)                          │
# ├─────────────────────────────────────────────────────────────────────────┤
# │                                                                         │
# │   0.15%          2.35%             68%             2.35%        0.15%   │
# │     |              |                |                |            |     │
# │     ├──────────────┼────────────────┼────────────────┼────────────┤     │
# │     0            260              968             1322         2030     │
# │   μ-3σ           μ-2σ              μ              μ+σ          μ+3σ     │
# │  (-94*)                                          (1676**)               │
# │                                                                         │
# │                [██████████████████████████████]  ← 68% of data          │
# │           [██████████████████████████████████████]  ← 95% of data       │
# │     [██████████████████████████████████████████████]  ← 99.7% of data   │
# │                                                                         │
# │  * μ-3σ = -94.43 ms (truncated to 0, RT can't be negative)              │
# │  ** μ+2σ = 1675.87 ms                                                   │
# │  Reaction Time in milliseconds (ms)                                     │
# └─────────────────────────────────────────────────────────────────────────┘

# INTERPRETATION:

# 68% of RTs (μ ± σ):
#  L2 participants responded between 614 and 1322 ms
#  Range: 708 ms

# 95% of RTs (μ ± 2σ):
#  L2 participants responded between 260 and 1676 ms
#  Range: 1416 ms

# 99.7% of RTs (μ ± 3σ):
#  L2 participants responded between ≈0 and 2030 ms
#  Range: 2030 ms

# SUMMARY:
# Regression Coefficients (Intercept = μ - Mean RT): Estimate: 967.60 ms
# Further Distributional Parameters (sigma = σ - SD of RT): Estimate: 354.21 ms 
# On average, L2 participants took about ≈1 second (968 ms) to respond
# There was high variation: some responded quickly, others took much longer
# The spread is large (SD = 354 ms is 37% of the mean), showing that
# L2 learners process language at very different speeds from each other
# Most people (68%) responded somewhere between 0.6 and 1.3 seconds
