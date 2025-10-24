# ==============================================================================
# Week 2
# Title: Task C: Massive Auditory Lexical Decision
# Author: Sandria Tran
# Date: 2025-9-26
# Lecture: Inference and Uncertainty
# Workshop: Summarise, count and group data | R scripts, read and summarise data
# Instructions:
#   1) Read the tucker2019/mald_1_1.rds data.
#   2) Summarise the data with appropriate summary functions:
#   number of participants,
#    number of words,
#    number of words by lexical status (real vs nonce word, IsWord),
#    reaction times by lexical status,
#    reaction times by lexical status and accuracy,
#    accuracy by lexical status.
#   3) What can you tell about reaction times and accuracy based on lexical status?
#   What about individual variation?
# Learning Objectives:
#    (i) Question:
#               (1) What are the components of research methods?
#                    (a) research process
#                    (b) project management
#                    (c) digital Skills
#                    (d) philosophy
#                    (e) ethics 
#                    (via https://stefanocoretta.github.io/qdal/ch-research-methods.html)
#               (2) What makes a good research question and research hypothesis?
#                    Good research questions have these qualities:
#                    (a) clean, precise focus
#                    (b) answerable: using observation, analysis, and/or data
#                    (c) research-relevant: connected to theories or existing that can address gap/needing
#                    (d) significance: related to application, theory, or academic understanding
#                    (e) specific: narrow in detail yet abroad for meaningful findings
#                    
#                    Good research hypothesis has these qualities:
#                    (a) testable
#                    (b) direct and specific
#                    (c) prior knowledge: based on existing theory/evidence
#                    (d) falsifiable: with a possibility to be disproven
#                    (e) research-relevant question: central question of study
#                     In essence, feasible, meaningfully, and research-oriented to be scientifically robust. 
#               (3) What are the three steps of quantitative data analysis?
#                     (a) data acquisition
#                     (b) data analysis
#                     (c) interpretation
#               (4) What is the computational workflow of quantitative data analysis?
#                       (a) Import Data: bring data into analysis environment 
#                       (b) Tidy Data: (i) reshape + organize data to easily work with 
#                       (c) Transform Data: (i) filter observations
#                                           (ii) modify/create columns
#                                           (iii) summarize
#                                           (iv) join data as needed
#                       (d) Visualize Data: Create Graphs + Plots to Reveal Patterns/Relationships
#                       (e) Model Data: Test Hypotheses via Statistical Models
#                       (f) Communicate Results: share via presentation, publications, reports
#                       (via https://stefanocoretta.github.io/qdal/ch-quantitative.html)
#    (ii) Skills:
#                (1)  Think critically about research methods and research questions.
#                (2)  Master the basics of RStudio.
#                (3)  Master the basics of the programming language R.
#                (4)  Learn how to install and use R packages.
# Important Terms & Information:
#       Course Website: https://uoelel.github.io/qml/
#       Install R: Install R from https://cloud.r-project.org
#       Install RStudio from https://posit.co/download/rstudio-desktop/
#       Lecture: https://uoelel.github.io/qml/lectures/week-01.html
#       Textbook (PDF): https://stefanocoretta.github.io/qdal/Quantitative-Data-Analysis-for-Linguists-in-R.pdf
#       Textbook(Online):https://stefanocoretta.github.io/qdal/
#
# ==============================================================================

library(tidyverse)
library(here)

# Create outputs directory
output_dir <- here("code", "outputs")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Step 1: Read the MALD data
mald <- readRDS(here("data", "tucker2019", "mald_1_1.rds"))

# Explore the data structure
glimpse(mald)

# Step 2: Summarise the data

# Number of participants
n_participants <- mald %>%
  summarise(n_participants = n_distinct(Subject))
print(n_participants)

# Number of words/items
n_words <- mald %>%
  summarise(n_words = n_distinct(Item))
print(n_words)

# Number of words by lexical status (IsWord)
words_by_status <- mald %>%
  distinct(Item, IsWord) %>%
  group_by(IsWord) %>%
  summarise(n_words = n())
print(words_by_status)

# Reaction times by lexical status
rt_by_status <- mald %>%
  group_by(IsWord) %>%
  summarise(
    mean_RT = mean(RT, na.rm = TRUE),
    sd_RT = sd(RT, na.rm = TRUE),
    median_RT = median(RT, na.rm = TRUE),
    n = n()
  )
print(rt_by_status)

# Reaction times by lexical status AND accuracy
rt_by_status_accuracy <- mald %>%
  group_by(IsWord, ACC) %>%
  summarise(
    mean_RT = mean(RT, na.rm = TRUE),
    sd_RT = sd(RT, na.rm = TRUE),
    median_RT = median(RT, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )
print(rt_by_status_accuracy)

# Accuracy by lexical status
# ACC is a factor with levels "correct"/"incorrect", convert to numeric (1/0)
accuracy_by_status <- mald %>%
  mutate(ACC_numeric = as.numeric(ACC == "correct")) %>%
  group_by(IsWord) %>%
  summarise(
    mean_accuracy = mean(ACC_numeric, na.rm = TRUE),
    sd_accuracy = sd(ACC_numeric, na.rm = TRUE),
    n = n()
  )
print(accuracy_by_status)

# Step 3: Analysis and interpretation

# Individual variation: RT and accuracy by participant and lexical status
individual_variation <- mald %>%
  mutate(ACC_numeric = as.numeric(ACC == "correct")) %>%
  group_by(Subject, IsWord) %>%
  summarise(
    mean_RT = mean(RT, na.rm = TRUE),
    mean_accuracy = mean(ACC_numeric, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )
print(individual_variation)

# Summary statistics of individual variation
individual_variation_summary <- individual_variation %>%
  group_by(IsWord) %>%
  summarise(
    mean_of_mean_RT = mean(mean_RT, na.rm = TRUE),
    sd_of_mean_RT = sd(mean_RT, na.rm = TRUE),
    mean_of_mean_accuracy = mean(mean_accuracy, na.rm = TRUE),
    sd_of_mean_accuracy = sd(mean_accuracy, na.rm = TRUE)
  )
print(individual_variation_summary)

# ==============================================================================
# INTERPRETATION
# ==============================================================================
# Based on lexical status:
# - Real words (IsWord = TRUE) typically have faster RTs and higher accuracy
# - Nonce words (IsWord = FALSE) typically have slower RTs and lower accuracy
# - This is expected: real words are recognized faster and more accurately
#
# Individual variation:
# - Check sd_of_mean_RT to see variability across participants
# - Some participants may be consistently faster/slower
# - Some participants may be more/less accurate
# - This shows importance of accounting for individual differences in analysis
# ==============================================================================

# Visualization 1: RT distribution by lexical status
p1 <- ggplot(mald, aes(x = RT, fill = IsWord)) +
  geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
  facet_wrap(~IsWord, labeller = labeller(IsWord = c("FALSE" = "Nonce Words", "TRUE" = "Real Words"))) +
  labs(
    title = "Reaction Time Distribution by Lexical Status",
    x = "Reaction Time (ms)",
    y = "Count",
    fill = "Real Word"
  ) +
  theme_minimal()

ggsave(here("code", "Week 2", "TaskC_rt_distribution.png"), p1, width = 10, height = 5)
ggsave(here("code", "outputs", "Week2_TaskC_rt_distribution.png"), p1, width = 10, height = 5)

# Visualization 2: RT and accuracy by lexical status
p2 <- ggplot(rt_by_status, aes(x = IsWord, y = mean_RT, fill = IsWord)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_RT - sd_RT, ymax = mean_RT + sd_RT), width = 0.2) +
  scale_x_discrete(labels = c("FALSE" = "Nonce Words", "TRUE" = "Real Words")) +
  labs(
    title = "Mean Reaction Time by Lexical Status",
    x = "Lexical Status",
    y = "Mean RT (ms)",
    fill = "Real Word"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(here("code", "Week 2", "TaskC_mean_rt.png"), p2, width = 7, height = 5)
ggsave(here("code", "outputs", "Week2_TaskC_mean_rt.png"), p2, width = 7, height = 5)

# Visualization 3: Accuracy by lexical status
# For binary accuracy data, use barplot with confidence intervals
# First, calculate participant-level accuracy for individual variation visualization
participant_accuracy <- mald %>%
  mutate(ACC_numeric = as.numeric(ACC == "correct")) %>%
  group_by(Subject, IsWord) %>%
  summarise(mean_accuracy = mean(ACC_numeric, na.rm = TRUE), .groups = "drop")

# Calculate group-level means and 95% confidence intervals
accuracy_summary <- participant_accuracy %>%
  group_by(IsWord) %>%
  summarise(
    mean_acc = mean(mean_accuracy),
    se_acc = sd(mean_accuracy) / sqrt(n()),
    ci_lower = mean_acc - 1.96 * se_acc,
    ci_upper = mean_acc + 1.96 * se_acc,
    .groups = "drop"
  )

# Create barplot with individual participant points and 95% CI
p3 <- ggplot(accuracy_summary, aes(x = IsWord, y = mean_acc, fill = IsWord)) +
  geom_col(alpha = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, linewidth = 0.8) +
  geom_jitter(data = participant_accuracy,
              aes(x = IsWord, y = mean_accuracy),
              width = 0.15, alpha = 0.3, size = 1, color = "black") +
  scale_x_discrete(labels = c("FALSE" = "Nonce Words", "TRUE" = "Real Words")) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent, expand = c(0, 0)) +
  labs(
    title = "Accuracy by Lexical Status",
    subtitle = "Bars show group means with 95% CI; points show individual participants",
    x = "Lexical Status",
    y = "Proportion Correct",
    fill = "Real Word"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(here("code", "Week 2", "TaskC_accuracy.png"), p3, width = 7, height = 5)
ggsave(here("code", "outputs", "Week2_TaskC_accuracy.png"), p3, width = 7, height = 5)
