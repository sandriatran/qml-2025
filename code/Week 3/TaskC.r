# ==============================================================================
# Week 3
# Title: Task C: Acceptability of "Everywhere" Constructions
# Author: Sandria Tran
# Lecture: Questionable Research Practices 
# Workshop: Plot Basics
# Workshop: Summarise, count and group data 
# Date: 2025-10-03
# Topic:  Research cycle | Data viz principles | Quarto documents, data transformation and plottting
# Instructions:
#     (1) Read the sluckin2022/everywhere-loc.csv data.
#     (2) Mutate Restrictor: 0 = "no restrictor", 1 = "restrictor".
#     (3) Mutate Value column to character.
#     (4) Create a filled stacked bar chart by Restrictor, fill by Value,
#         panels by Age.
#     (5) Can you spot differences in rating across age brackets?
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
judgements <- read_csv(here("data", "sluckin2022", "everywhere-loc.csv"), show_col_types = FALSE) %>%
  mutate(
    Restrictor = if_else(Restrictor == 0, "no restrictor", "restrictor"),
    Value = as.character(Value)
  )

# Create visualization
p1 <- ggplot(judgements, aes(x = Restrictor, fill = Value)) +
  geom_bar(position = "fill") +
  facet_wrap(~Age) +
  labs(
    title = "Acceptability of 'Everywhere' Constructions by Age",
    x = "Restrictor",
    y = "Proportion",
    fill = "Rating"
  ) +
  theme_minimal()

ggsave(here("code", "Week 3", "TaskC_acceptability_plot.png"), p1, width = 10, height = 6)
ggsave(here("code", "outputs", "Week3_TaskC_acceptability_plot.png"), p1, width = 10, height = 6)

cat("\nPlots saved to:\n")
cat("- code/Week 3/TaskC_acceptability_plot.png\n")
cat("- code/outputs/Week3_TaskC_acceptability_plot.png\n")
