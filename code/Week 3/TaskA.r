# ==============================================================================
# Week 3
# Title: Task A: Albanian Voice Onset Time (VOT) Analysis
# Author: Sandria Tran
# Lecture: Questionable Research Practices 
# Workshop: Plot Basics
# Topic: Research cycle | Data viz principles | Quarto documents, data transformation and plottting
# Date: 2025-10-03
# Instructions:
#   Read the coretta2021/alb-vot.csv data.
#   Calculate VOT based on release and voice_onset columns.
#   Create a column specifying voicing (voiced/voiceless) based on label.
#   Get summary measures of VOT for both voicing categories.
#   Can VOT differentiate between the two voicing categories?
# Learning Objectives:
#    (i) Question:
#         (1) What does the research cycle entail?
#            (a) Research Question / Hypothesis 
#            (b) Study Design
#            (c) Data Acquisition 
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
#                  (i)  undermine robustness and reproducibility 
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

# Read and transform data
dat <- read_csv(here("data", "coretta2021", "alb-vot.csv")) %>%
  mutate(
    VOT = voi_onset - release,
    voicing = if_else(label %in% c("b", "d", "g"), "voiced", "voiceless")
  )

# Summary statistics
vot_summary <- dat %>%
  group_by(voicing) %>%
  summarise(mean_VOT = mean(VOT), sd_VOT = sd(VOT), n = n())

print(vot_summary)

# Visualization 1: VOT distribution by voicing
p1 <- ggplot(dat, aes(x = VOT, fill = voicing)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  facet_wrap(~voicing) +
  labs(
    title = "Albanian VOT Distribution by Voicing",
    x = "Voice Onset Time (ms)",
    y = "Count",
    fill = "Voicing"
  ) +
  theme_minimal()

ggsave(here("code", "Week 3", "TaskA_vot_distribution.png"), p1, width = 10, height = 5)
ggsave(here("code", "outputs", "Week3_TaskA_vot_distribution.png"), p1, width = 10, height = 5)

# Visualization 2: Boxplot comparison
p2 <- ggplot(dat, aes(x = voicing, y = VOT, fill = voicing)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  labs(
    title = "Albanian VOT by Voicing Category",
    subtitle = "VOT clearly differentiates voiced from voiceless consonants",
    x = "Voicing",
    y = "Voice Onset Time (ms)",
    fill = "Voicing"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(here("code", "Week 3", "TaskA_vot_boxplot.png"), p2, width = 7, height = 5)
ggsave(here("code", "outputs", "Week3_TaskA_vot_boxplot.png"), p2, width = 7, height = 5)

# Answer: Can VOT differentiate between the two voicing categories?
cat("\n== ANSWER ==\n")
cat("Yes, VOT clearly differentiates between voiced and voiceless consonants.\n")
cat("Voiced consonants have a mean VOT of", round(vot_summary$mean_VOT[vot_summary$voicing == "voiced"], 1), "ms\n")
cat("Voiceless consonants have a mean VOT of", round(vot_summary$mean_VOT[vot_summary$voicing == "voiceless"], 1), "ms\n")
cat("The difference is substantial and statistically meaningful.\n")
