# ==============================================================================
# Week 9 | https://uoelel.github.io/qml/lectures/week-09.html
# Title: Task B: Pupil Width with Age × Density Interaction
# Author: Sandria Tran
# Date: 2025-11-14
# Topic: Workshop: Multiple predictors and interactions
#        Lecture: Open research
#
# (i) Instructions:
#   (1) Read the mclaughlin2023/pupil-width.csv data.
#   (2) Fit a regression model to answer: Does the density of the neighbourhood
#       affect maximum pupil size in old vs young adults differently?
#   (3) Write a paragraph reporting the model.
#   (4) Produce plots of the posterior distributions of the model parameters and
#       the expected predictions for the dense and sparse condition in old vs
#       young participants.
#   (5) Discuss the results with your group.
#
# (ii) Learning Objectives:
#   (1) What is Open Research?
#        - Principles: honest, transparent, reproducible, accessible research
#        - Avoids questionable research behaviors
#        - Promotes openness throughout research cycle
#        - Makes outputs publicly accessible
#
#   (2) Research reliability: reproducible, replicable, robust, generalisable
#        - Reproducible: same data + same pipeline = same results
#        - Replicable: new data + same protocol = similar results
#        - Robust: alternative methods (same data) = same findings
#        - Generalisable: new data + new workflows = consistent findings
#
#   (3) Registered Reports
#        - Peer-review BEFORE data collection starts
#        - Stage 1: Review research plan
#        - Stage 2: Review adherence to plan after analysis
#        - Benefits: Prevent p-hacking, selective reporting, publication bias
#
#   (4) Modeling interactions in regression
#        - Add interaction term: Y ~ X + Z + X:Z or Y ~ X * Z
#        - Allows effect of one predictor to depend on another
#        - Reference level matters for interpretation
#        - Mean-centering reduces multicollinearity
#
# (iii) Skills:
#      (1) Assess research in light of Open Research practices
#      (2) Distinguish four ways research can be reliable
#      (3) Fit and interpret categorical × categorical interactions in brms
#      (4) Visualize and interpret interaction effects
#
# Resources:
#    Lecture: https://uoelel.github.io/qml/lectures/week-09.html
#    Textbook: https://stefanocoretta.github.io/qdal/ch-regression-interaction.html
# ==============================================================================

# Load required packages ----
library(tidyverse)
library(brms)
library(tidybayes)
library(bayesplot)
library(posterior)
library(here)

# 1. Read the data ----
# Pupil width data: maximum pupil size by age group and neighbourhood density
pupil <- read_csv(here("data", "mclaughlin2023", "pupil-width.csv"), show_col_types = FALSE) %>%
  mutate(
    Age = factor(Age, levels = c("YA", "OA")),
    Condition = factor(Condition, levels = c("Sparse", "Dense"))
  ) %>%
  rename(age_group = Age, density = Condition, pupil_width = pupil_max)

# Check data structure
head(pupil)
glimpse(pupil)

# 2. Exploratory visualization ----
# Visualize pupil width patterns by density and age group
pupil %>%
  ggplot(aes(x = density, y = pupil_width, fill = age_group)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2),
              alpha = 0.3, size = 2) +
  geom_boxplot(position = position_dodge(0.75), alpha = 0.6) +
  labs(
    title = "Maximum Pupil Width by Density and Age Group",
    subtitle = "Exploratory visualization",
    x = "Neighbourhood Density",
    y = "Maximum Pupil Width",
    fill = "Age Group"
  ) +
  scale_fill_manual(values = c("young" = "steelblue", "old" = "coral")) +
  theme_minimal()

# 3. Prepare data ----
# Remove any missing values in key variables
pupil_clean <- pupil %>%
  drop_na(pupil_width, age_group, density)

cat("Clean data: Pupil width measurements\n")
cat("N =", nrow(pupil_clean), "observations\n")
cat("Age groups:", paste(levels(pupil_clean$age_group), collapse = ", "), "\n")
cat("Density levels:", paste(levels(pupil_clean$density), collapse = ", "), "\n")
cat("Mean pupil width:", round(mean(pupil_clean$pupil_width), 2), "\n")
cat("\nPupil width by Group and Density:\n")
print(pupil_clean %>%
        group_by(age_group, density) %>%
        summarize(
          mean = mean(pupil_width),
          sd = sd(pupil_width),
          n = n(),
          .groups = "drop"
        ))

# 4. Fit regression model with interaction ----
# Research question: Does density affect pupil width differently in old vs young?
# Model: pupil_width ~ Gaussian(μ, σ) where μ = β₀ + β_age + β_density + β_interaction
#
# Using * expands to: pupil_width ~ age_group + density + age_group:density
# This allows the effect of density to differ by age group

model_pupil <- brm(
  pupil_width ~ age_group * density,
  data = pupil_clean,
  family = gaussian(),
  iter = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  seed = 2025
)

# Model summary
summary(model_pupil)

# 5. Extract and visualize posterior distributions ----
posterior_draws <- as_draws_df(model_pupil)

# Plot 1: Posterior distribution of intercept (β₀)
p1 <- posterior_draws %>%
  ggplot(aes(x = b_Intercept)) +
  geom_density(fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Posterior: Intercept (β₀)",
    subtitle = "Baseline pupil width (Young, Sparse)",
    x = "Pupil Width",
    y = "Density"
  ) +
  theme_minimal()

# Plot 2: Main effects (Age group and Density)
p2 <- posterior_draws %>%
  select(starts_with("b_age_group"), starts_with("b_density")) %>%
  pivot_longer(everything(), names_to = "parameter", values_to = "value") %>%
  mutate(parameter = str_remove(parameter, "^b_")) %>%
  ggplot(aes(x = value, fill = parameter)) +
  geom_density(alpha = 0.6) +
  labs(
    title = "Posterior: Main Effects",
    x = "Effect Size",
    y = "Density",
    fill = "Parameter"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot 3: Interaction effect
p3 <- posterior_draws %>%
  select(starts_with("b_age_group") & ends_with(":")) %>%
  pivot_longer(everything(), names_to = "parameter", values_to = "value") %>%
  mutate(parameter = str_remove(parameter, "^b_")) %>%
  ggplot(aes(x = value, fill = parameter)) +
  geom_density(alpha = 0.7, fill = "coral") +
  labs(
    title = "Posterior: Interaction Effect",
    subtitle = "Age group × Density",
    x = "Effect Size",
    y = "Density"
  ) +
  theme_minimal()

# Plot 4: Posterior predictive check
p4 <- pp_check(model_pupil, ndraws = 100) +
  labs(title = "Posterior Predictive Check") +
  theme_minimal()

# Plot 5: Sigma (error SD)
p5_sigma <- posterior_draws %>%
  ggplot(aes(x = sigma)) +
  geom_density(fill = "lightgreen", alpha = 0.7) +
  labs(
    title = "Posterior: Error SD (σ)",
    x = "Sigma",
    y = "Density"
  ) +
  theme_minimal()

print(p1)
print(p2)
print(p3)
print(p4)
print(p5_sigma)

# 6. Expected pupil width predictions by Age Group and Density ----
# Create newdata for all combinations of age_group × density
newdata <- expand_grid(
  age_group = levels(pupil_clean$age_group),
  density = levels(pupil_clean$density)
)

# Get predicted pupil widths using epred_draws
epreds <- model_pupil %>%
  epred_draws(newdata = newdata)

# Summarize predictions with credible intervals
epreds_summary <- epreds %>%
  group_by(age_group, density) %>%
  summarize(
    mean_width = mean(.epred),
    lower = quantile(.epred, 0.05),
    upper = quantile(.epred, 0.95),
    .groups = "drop"
  )

# Plot expected pupil widths with interaction visualization
p6 <- ggplot(epreds_summary, aes(x = density, y = mean_width, color = age_group)) +
  geom_point(size = 3) +
  geom_line(aes(group = age_group), linewidth = 1) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, linewidth = 1) +
  labs(
    title = "Expected Maximum Pupil Width by Density and Age Group",
    subtitle = "Interaction effect shown by non-parallel lines",
    x = "Neighbourhood Density",
    y = "Maximum Pupil Width",
    color = "Age Group"
  ) +
  scale_color_manual(values = c("young" = "steelblue", "old" = "coral")) +
  theme_minimal()

print(p6)

# 7. Model report ----
cat("\n========== GAUSSIAN REGRESSION: AGE GROUP × DENSITY INTERACTION ==========\n\n")

cat("RESEARCH QUESTION:\n")
cat("Does the density of the neighbourhood affect maximum pupil size differently\n")
cat("in old vs young adults?\n\n")

cat("DATA:\n")
cat("- N =", nrow(pupil_clean), "observations\n")
cat("- Age groups:", paste(levels(pupil_clean$age_group), collapse = ", "), "\n")
cat("- Density levels:", paste(levels(pupil_clean$density), collapse = ", "), "\n")
cat("- Mean pupil width:", round(mean(pupil_clean$pupil_width), 2), "\n")
cat("- SD:", round(sd(pupil_clean$pupil_width), 2), "\n\n")

cat("MODEL SPECIFICATION:\n")
cat("pupil_width ~ Gaussian(μ, σ)\n")
cat("μ = β₀ + β₁·age_groupOld + β₂·densityDense + β₃·age_groupOld:densityDense\n")
cat("σ = residual error SD\n\n")

cat("INTERPRETATION:\n")
cat("- β₀ (Intercept): Baseline pupil width for young adults in sparse neighbourhoods\n")
cat("- β₁ (age_groupOld): Difference in pupil width between old and young in sparse neighbourhoods\n")
cat("- β₂ (densityDense): Difference in pupil width between dense and sparse for young adults\n")
cat("- β₃ (Interaction): How the density effect differs between old and young adults\n\n")

cat("PREDICTED PUPIL WIDTHS BY AGE GROUP AND DENSITY:\n")
print(epreds_summary %>%
        mutate(across(where(is.numeric), ~round(., 3))))

cat("\n\nKEY FINDINGS:\n")
# Calculate pupil width difference by age group for each density
for (dens in levels(pupil_clean$density)) {
  young_width <- epreds_summary %>%
    filter(age_group == "young", density == dens) %>%
    pull(mean_width)

  old_width <- epreds_summary %>%
    filter(age_group == "old", density == dens) %>%
    pull(mean_width)

  diff <- old_width - young_width

  cat(sprintf("- %s: Young=%.3f, Old=%.3f, Difference=%.3f\n",
              dens, young_width, old_width, diff))
}

cat("\n\nMODEL SUMMARY:\n")
print(model_pupil)

cat("\n========== END REPORT ==========\n")
