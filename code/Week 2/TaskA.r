# ==============================================================================
# Week 2
# Title: Task A: Italian Voice Onset Time
# Author: Sandria Tran
# Lecture: Inference and Uncertainty
# Workshop: Summarise, count and group data | R scripts, read and summarise data
# Date: 2025-9-26
# Topic: Inference and uncertainty
# Instructions: Summarise, count and group data
#   Read the coretta2018a/ita_egg.rda data. .rda files are read using load().
#   This function works without needing to assign the output to a variable
#   (so it’s #different from the other read*() functions,
#   which do require you to assign the output to a variable).
#   Summarise relevant columns,
#   with a particular focus on how VOT (voice onset time, column vot)
#   differs depending on phonological features like place, vowel and so on.
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

# Load the .rda file
load(here("data", "coretta2018a", "ita_egg.rda"))

# Basic summary of the dataset structure
glimpse(ita_egg)

# Create voicing category from consonant (c2)
ita_egg <- ita_egg %>%
  mutate(voicing = if_else(c2 %in% c("b", "d", "g"), "voiced", "voiceless"))

# Summary of VOT by voicing
vot_by_voicing <- ita_egg %>%
  group_by(voicing) %>%
  summarise(
    mean_VOT = mean(vot, na.rm = TRUE),
    sd_VOT = sd(vot, na.rm = TRUE),
    n = n()
  )
print(vot_by_voicing)

# Summary of VOT by place of articulation
vot_by_place <- ita_egg %>%
  group_by(place) %>%
  summarise(
    mean_VOT = mean(vot, na.rm = TRUE),
    sd_VOT = sd(vot, na.rm = TRUE),
    n = n()
  )
print(vot_by_place)

# Summary of VOT by vowel
vot_by_vowel <- ita_egg %>%
  group_by(vowel) %>%
  summarise(
    mean_VOT = mean(vot, na.rm = TRUE),
    sd_VOT = sd(vot, na.rm = TRUE),
    n = n()
  )
print(vot_by_vowel)

# Combined summary: VOT by voicing AND place
vot_by_voicing_place <- ita_egg %>%
  group_by(voicing, place) %>%
  summarise(
    mean_VOT = mean(vot, na.rm = TRUE),
    sd_VOT = sd(vot, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )
print(vot_by_voicing_place)

# Combined summary: VOT by voicing AND vowel
vot_by_voicing_vowel <- ita_egg %>%
  group_by(voicing, vowel) %>%
  summarise(
    mean_VOT = mean(vot, na.rm = TRUE),
    sd_VOT = sd(vot, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )
print(vot_by_voicing_vowel)

# Visualization 1: VOT distribution by voicing
p1 <- ggplot(ita_egg, aes(x = vot, fill = voicing)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  facet_wrap(~voicing) +
  labs(
    title = "Italian VOT Distribution by Voicing",
    x = "Voice Onset Time (ms)",
    y = "Count",
    fill = "Voicing"
  ) +
  theme_minimal()

ggsave(here("code", "Week 2", "TaskA_vot_by_voicing.png"), p1, width = 8, height = 5)
ggsave(here("code", "outputs", "Week2_TaskA_vot_by_voicing.png"), p1, width = 8, height = 5)

# Visualization 2: VOT by place of articulation and voicing
p2 <- ggplot(vot_by_voicing_place, aes(x = place, y = mean_VOT, fill = voicing)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = mean_VOT - sd_VOT, ymax = mean_VOT + sd_VOT),
                position = position_dodge(0.9), width = 0.25) +
  labs(
    title = "Mean VOT by Place of Articulation and Voicing",
    x = "Place of Articulation",
    y = "Mean VOT (ms)",
    fill = "Voicing"
  ) +
  theme_minimal()

ggsave(here("code", "Week 2", "TaskA_vot_by_place_voicing.png"), p2, width = 8, height = 5)
ggsave(here("code", "outputs", "Week2_TaskA_vot_by_place_voicing.png"), p2, width = 8, height = 5)
