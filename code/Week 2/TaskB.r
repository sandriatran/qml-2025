# ==============================================================================
# Week 2
# Title: Task B: World Atlas of Language Structures
# Author: Sandria Tran
# Date: 2025-9-26
# Lecture: Inference and Uncertainty
# Workshop: Summarise, count and group data | R scripts, read and summarise data
# Instructions:
#   1) The data from WALS are available through the R package ritwals.
#   Install the package (this is done in two steps:
#   first install devtools and then install ritwals with devtools,
#   see linked package documentation).
#   2) Then attach it and load the data with data("WALS").
#   3) Get the number of languages by value (value)
#   for each feature (feature_ID and feature). The features are listed here.
#   Focus on features you are interested in.
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
#        Citation:  Dryer, Matthew S. & Haspelmath, Martin (eds.) 2013. The World Atlas of Language Structures Online. Leipzig: Max Planck Institute for Evolutionary Anthropology.
#        (Available online at https://wals.info/, Accessed on 2025-10-17.)

#         @book{wals,
#          address   = {Leipzig},
#          editor    = {Matthew S. Dryer and Martin Haspelmath},
#          publisher = {Max Planck Institute for Evolutionary Anthropology},
#          title     = {WALS Online},
#          url       = {https://wals.info/},
#          year      = {2013}

# ==============================================================================

library(tidyverse)
library(here)

# Create outputs directory
output_dir <- here("code", "outputs")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Step 1: Install and load linguistic typology package with WALS data
# NOTE: ritwals package doesn't exist, but lingtypology has WALS data
# To install: install.packages("lingtypology")

# Step 2: Load lingtypology package (contains WALS database)
if (!require("lingtypology", quietly = TRUE)) {
  cat("Installing lingtypology package (contains WALS data)...\n")
  install.packages("lingtypology", repos = "https://cloud.r-project.org")
}

library(lingtypology)

# Load WALS data from lingtypology
# Fetch specific features of interest
features_to_fetch <- c("1A", "13A", "81A", "87A")
wals_data <- wals.feature(features_to_fetch)

# Convert wide format to long format (tidy data)
# Each row = one language-feature-value combination
WALS <- wals_data %>%
  select(language, `1A`, `13A`, `81A`, `87A`) %>%
  pivot_longer(
    cols = -language,
    names_to = "feature_ID",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  mutate(
    feature = case_when(
      feature_ID == "1A" ~ "Consonant Inventories",
      feature_ID == "13A" ~ "Tone",
      feature_ID == "81A" ~ "Order of Subject, Object and Verb",
      feature_ID == "87A" ~ "Order of Adjective and Noun"
    )
  )

cat("WALS data loaded from lingtypology package\n")
cat("Available features:", length(unique(WALS$feature_ID)), "\n")
cat("Total observations:", nrow(WALS), "\n\n")

# Explore the data structure
glimpse(WALS)

# Step 3: Count languages by value for each feature

# Example 1: Feature 1A - Consonant Inventories
consonant_inv <- WALS %>%
  filter(feature_ID == "1A") %>%
  group_by(feature, value) %>%
  summarise(n_languages = n(), .groups = "drop") %>%
  arrange(desc(n_languages))
print(consonant_inv)

# Example 2: Feature 13A - Tone
tone <- WALS %>%
  filter(feature_ID == "13A") %>%
  group_by(feature, value) %>%
  summarise(n_languages = n(), .groups = "drop") %>%
  arrange(desc(n_languages))
print(tone)

# Example 3: Feature 81A - Order of Subject, Object and Verb
sov_order <- WALS %>%
  filter(feature_ID == "81A") %>%
  group_by(feature, value) %>%
  summarise(n_languages = n(), .groups = "drop") %>%
  arrange(desc(n_languages))
print(sov_order)

# Example 4: Feature 87A - Order of Adjective and Noun
adj_noun <- WALS %>%
  filter(feature_ID == "87A") %>%
  group_by(feature, value) %>%
  summarise(n_languages = n(), .groups = "drop") %>%
  arrange(desc(n_languages))
print(adj_noun)

# Summary for all fetched features
all_features_summary <- WALS %>%
  group_by(feature_ID, feature, value) %>%
  summarise(n_languages = n(), .groups = "drop") %>%
  arrange(feature_ID, desc(n_languages))
print(all_features_summary)

# Visualization 1: Consonant Inventories
p1 <- ggplot(consonant_inv, aes(x = reorder(value, n_languages), y = n_languages)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "WALS: Consonant Inventory Sizes",
    x = "Size Category",
    y = "Number of Languages"
  ) +
  theme_minimal()

ggsave(here("code", "Week 2", "TaskB_consonant_inventories.png"), p1, width = 8, height = 5)
ggsave(here("code", "outputs", "Week2_TaskB_consonant_inventories.png"), p1, width = 8, height = 5)

# Visualization 2: Word Order (SOV)
p2 <- ggplot(sov_order, aes(x = reorder(value, n_languages), y = n_languages)) +
  geom_col(fill = "coral") +
  coord_flip() +
  labs(
    title = "WALS: Word Order Patterns (Subject, Object, Verb)",
    x = "Word Order",
    y = "Number of Languages"
  ) +
  theme_minimal()

ggsave(here("code", "Week 2", "TaskB_word_order.png"), p2, width = 8, height = 5)
ggsave(here("code", "outputs", "Week2_TaskB_word_order.png"), p2, width = 8, height = 5)

# Visualization 3: Tone Systems
p3 <- ggplot(tone, aes(x = reorder(value, n_languages), y = n_languages)) +
  geom_col(fill = "seagreen") +
  coord_flip() +
  labs(
    title = "WALS: Tone Systems",
    x = "Tone Category",
    y = "Number of Languages"
  ) +
  theme_minimal()

ggsave(here("code", "Week 2", "TaskB_tone_systems.png"), p3, width = 8, height = 5)
ggsave(here("code", "outputs", "Week2_TaskB_tone_systems.png"), p3, width = 8, height = 5)
