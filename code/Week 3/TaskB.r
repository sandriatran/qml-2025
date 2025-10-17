# ==============================================================================
# Week 3
# Title: Task B: Albanian Vowel Space and Duration Analysis
# Author: Sandria Tran
# Lecture: Questionable Research Practices 
# Workshop: Plot Basics
# Workshop: Summarise, count and group data 
# Date: 2025-10-03
# Topic:  Research cycle | Data viz principles | Quarto documents, data transformation and plottting
# Instructions:
#   Read the coretta2021/alb_formants.rds data.
#   Calculate vowel duration (end - start).
#   Create Plot 1: F1 vs F2 (vowel space).
#   Create Plot 2: F1 vs Duration.
# Learning Objectives:
#    (i) Question:
#         (1) What does the research cycle entail?
#            (a) Research Question / Hypothesis 
#            (b) Study Design
#            (c) Data Acquisiton 
#            (d) Data Analysis
#            (e) interpretation
#            (f) Publication / Next Study 
#            (via https://stefanocoretta.github.io/qdal/ch-research-cycle.html)
#          (2) How do the researcher’s degrees of freedom affect research?
#              describes the flexible choices researchers have to:
#              (a) select data for analysis
#              (b) choose statistical methods/models
#              (c) report results 
#              (d) however, excessive degrees of freedom can:
#                  (i) undermine robustness and reproducibility 
#                  (ii) make published findings less generalizable and reliable
#              (e) open research practices + replication studies increases reproducille, credible science
#           (via https://stefanocoretta.github.io/qdal/ch-research-cycle.html)
#          (3) What are Questionable Research Practices?
#              (a) practices (intentional or not) that undermine robustness of research
#                  (via https://stefanocoretta.github.io/qdal/ch-research-cycle.html)
#          (4) Which characteristic do compelling graphics have?
#              (a) it contains reliable Information
#              (b) patterns become noticable (based on design)
#              (c) attractive, but represents honesty, clarity, and depth
#              (d) enables exploration when its organization is appropriate 
#              (via https://stefanocoretta.github.io/qdal/ch-plotting.html)
#    (ii) Skills:
#              (1) Use Quarto files to create dynamic reports.
#              (2) Filtering rows using filter().
#              (3) Using logical operators to filter data.
#              (4) Create or modify columns with mutate().
# Important Terms & Information:
#       Lecture: https://uoelel.github.io/qml/lectures/week-03.html#/title-slide
#       Textbook (online): https://stefanocoretta.github.io/qdal/ch-transform.html
#       Textbook (PDF): https://stefanocoretta.github.io/qdal/Quantitative-Data-Analysis-for-Linguists-in-R.pdf
# ==============================================================================

library(tidyverse)
library(here)

# Create outputs directory
output_dir <- here("code", "outputs")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Load data and calculate vowel duration
vowel_dat <- readRDS(here("data", "coretta2021", "alb_formants.rds")) %>%
  mutate(duration = end - start)

# ==============================================================================
# PLOT 1: F1 vs F2 (Vowel Space)
# ==============================================================================

plot_F1_F2 <- ggplot(vowel_dat, aes(x = F2, y = F1, colour = vowel)) +
  geom_point(alpha = 0.4) +
  scale_y_reverse() +
  scale_x_reverse() +
  labs(
    title = "Albanian Vowel Space: F1 vs F2",
    x = "F2 (Hz) - Backness (Front on Left)",
    y = "F1 (Hz) - Height (High at Top)",
    colour = "Vowel"
  ) +
  theme_minimal()

ggsave(here("code", "Week 3", "TaskB_f1_f2_plot.png"), plot_F1_F2, width = 7, height = 5)
ggsave(here("code", "outputs", "Week3_TaskB_f1_f2_plot.png"), plot_F1_F2, width = 7, height = 5)

# ==============================================================================
# PLOT 2: F1 vs Duration
# ==============================================================================

plot_F1_Duration <- ggplot(vowel_dat, aes(x = duration, y = F1, colour = vowel)) +
  geom_point(alpha = 0.4) +
  scale_y_reverse() +
  labs(
    title = "Vowel Height (F1) vs. Duration",
    x = "Vowel Duration (s)",
    y = "F1 (Hz) - Height (High at Top)",
    colour = "Vowel"
  ) +
  theme_minimal()

ggsave(here("code", "Week 3", "TaskB_f1_duration_plot.png"), plot_F1_Duration, width = 7, height = 5)
ggsave(here("code", "outputs", "Week3_TaskB_f1_duration_plot.png"), plot_F1_Duration, width = 7, height = 5)

# Display plots
print(plot_F1_F2)
print(plot_F1_Duration)
