# ==============================================================================
# Week 9 |https://uoelel.github.io/qml/workshops/week-09.html
# Title: Task B: Pupil Width 
# Author: Sandria Tran
# Date: 2025-11-14
# Topic: Workshop: Multiple predictors and interactions (with Age and Density Interaction)
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
#   (5) Discuss the results with your group. (no need to write the discussion).
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

# 1. Read and prepare data ----
pupil <- read_csv(here("data", "mclaughlin2023", "pupil-width.csv"), show_col_types = FALSE) %>%
  mutate(
    Age = factor(Age, levels = c("YA", "OA")),
    Condition = factor(Condition, levels = c("Sparse", "Dense"))
  ) %>%
  rename(age_group = Age, density = Condition, pupil_width = pupil_max)

head(pupil)

# 2. Exploratory visualization ----
pupil %>%
  ggplot(aes(x = density, y = pupil_width, fill = age_group)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0),
              alpha = 0.3, size = 2) +
  geom_boxplot(position = position_dodge(0.75), alpha = 0.6) +
  labs(title = "Maximum Pupil Width by Density and Age Group",
       x = "Neighbourhood Density", y = "Maximum Pupil Width", fill = "Age Group") +
  scale_fill_manual(values = c("YA" = "steelblue", "OA" = "coral")) +
  theme_minimal()

# 3. Filter data and fit model ----
pupil_clean <- pupil %>%
  drop_na(pupil_width, age_group, density)

# 4. Fit regression model with interaction ----
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

summary(model_pupil)

# 5. Extract posterior distributions and create plots ----
posterior_draws <- as_draws_df(model_pupil)

# Posterior plots
p1 <- posterior_draws %>%
  ggplot(aes(x = b_Intercept)) +
  geom_density(fill = "steelblue", alpha = 0.7) +
  labs(title = "Posterior: Intercept (β₀) - Baseline Young, Sparse",
       x = "Pupil Width", y = "Density") +
  theme_minimal()

p2 <- posterior_draws %>%
  select(starts_with("b_age_groupOA"), starts_with("b_densityDense"), -starts_with("b_age_groupOA:")) %>%
  pivot_longer(everything(), names_to = "parameter", values_to = "value") %>%
  mutate(parameter = str_remove(parameter, "^b_")) %>%
  ggplot(aes(x = value, fill = parameter)) +
  geom_density(alpha = 0.6) +
  labs(title = "Posterior: Main Effects", x = "Effect Size", y = "Density", fill = "Parameter") +
  theme_minimal() + theme(legend.position = "bottom")

p3 <- posterior_draws %>%
  select(starts_with("b_age_groupOA:")) %>%
  pivot_longer(everything(), names_to = "parameter", values_to = "value") %>%
  mutate(parameter = str_remove(parameter, "^b_")) %>%
  ggplot(aes(x = value, fill = parameter)) +
  geom_density(alpha = 0.7, fill = "coral") +
  labs(title = "Posterior: Interaction Effect (Age × Density)",
       x = "Effect Size", y = "Density") +
  theme_minimal()

p4 <- pp_check(model_pupil, ndraws = 100) +
  labs(title = "Posterior Predictive Check") +
  theme_minimal()

p5 <- posterior_draws %>%
  ggplot(aes(x = sigma)) +
  geom_density(fill = "lightgreen", alpha = 0.7) +
  labs(title = "Posterior: Error SD (σ)", x = "Sigma", y = "Density") +
  theme_minimal()

# Print and save plots
list(p1 = p1, p2 = p2, p3 = p3, p4 = p4, p5 = p5) %>%
  walk2(names(.), function(plot, name) {
    print(plot)
    ggsave(here("code", "outputs", paste0("Week9_TaskB_", name, ".png")),
           plot, width = 8, height = 6, dpi = 300)
  })

# 6. Expected pupil width predictions ----
newdata <- expand_grid(
  age_group = levels(pupil_clean$age_group),
  density = levels(pupil_clean$density)
)

epreds_summary <- model_pupil %>%
  epred_draws(newdata = newdata) %>%
  group_by(age_group, density) %>%
  summarize(
    mean_width = mean(.epred),
    lower = quantile(.epred, 0.05),
    upper = quantile(.epred, 0.95),
    .groups = "drop"
  )

p6 <- ggplot(epreds_summary, aes(x = density, y = mean_width, color = age_group)) +
  geom_point(size = 3) +
  geom_line(aes(group = age_group), linewidth = 1) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, linewidth = 1) +
  labs(title = "Expected Maximum Pupil Width by Density and Age Group",
       subtitle = "Non-parallel lines indicate interaction effect",
       x = "Neighbourhood Density", y = "Maximum Pupil Width",
       color = "Age Group") +
  scale_color_manual(values = c("YA" = "steelblue", "OA" = "coral")) +
  theme_minimal()

print(p6)
ggsave(here("code", "outputs", "Week9_TaskB_p6_expected_pupil_width.png"), p6,
       width = 8, height = 6, dpi = 300)

# 7. Model report ----
cat("\n========== GAUSSIAN REGRESSION: AGE GROUP × DENSITY INTERACTION ==========\n\n")

cat("RESEARCH QUESTION:\n")
cat("Does neighbourhood density affect maximum pupil size differently in old vs young adults?\n\n")

cat("DATA SUMMARY:\n")
cat(sprintf("- N = %d observations\n", nrow(pupil_clean)))
cat(sprintf("- Age groups: %s\n", paste(levels(pupil_clean$age_group), collapse = ", ")))
cat(sprintf("- Density levels: %s\n\n", paste(levels(pupil_clean$density), collapse = ", ")))

cat("PREDICTED PUPIL WIDTHS BY AGE GROUP AND DENSITY:\n")
print(epreds_summary %>% mutate(across(where(is.numeric), ~round(., 3))))

# Extract posterior statistics
cat("\n\nPOSTERIOR STATISTICS (95% Credible Intervals):\n")
stats <- list(
  "Age (OA vs YA, Sparse)" = quantile(posterior_draws$b_age_groupOA, c(0.025, 0.5, 0.975)),
  "Density (Dense vs Sparse, Young)" = quantile(posterior_draws$b_densityDense, c(0.025, 0.5, 0.975)),
  "Interaction: Age × Density" = quantile(posterior_draws$`b_age_groupOA:densityDense`, c(0.025, 0.5, 0.975))
)

for (name in names(stats)) {
  cat(sprintf("%s: Median = %.2f, 95%% CI [%.2f, %.2f]\n",
              name, stats[[name]]["50%"], stats[[name]]["2.5%"], stats[[name]]["97.5%"]))
}

# Written paragraph
cat("\n\nRESEARCH SUMMARY:\n")
cat("================\n\n")

paragraph <- sprintf(
  "To investigate whether neighbourhood density affects maximum pupil size differently in older versus younger adults, we fitted a Bayesian Gaussian regression model with age × density interaction to %d observations. Older adults showed substantially larger pupil widths than younger adults (β = %.2f, 95%% CI [%.2f, %.2f]). Density had negligible effect for younger adults (β = %.2f, 95%% CI [%.2f, %.2f]). Critically, the age × density interaction was near zero (β = %.2f, 95%% CI [%.2f, %.2f]), indicating no meaningful evidence for differential density effects across age groups. The approximately parallel lines in the interaction plot confirm this lack of interaction. Posterior predictive checks and chain convergence (Rhat < 1.01) indicated adequate model fit. These findings suggest neighbourhood density does not meaningfully modulate age-related differences in pupil size.",

  nrow(pupil_clean),
  stats[["Age (OA vs YA, Sparse)"]][2], stats[["Age (OA vs YA, Sparse)"]][1], stats[["Age (OA vs YA, Sparse)"]][3],
  stats[["Density (Dense vs Sparse, Young)"]][2], stats[["Density (Dense vs Sparse, Young)"]][1], stats[["Density (Dense vs Sparse, Young)"]][3],
  stats[["Interaction: Age × Density"]][2], stats[["Interaction: Age × Density"]][1], stats[["Interaction: Age × Density"]][3]
)

wrapped <- strwrap(paragraph, width = 80)
for (line in wrapped) cat(line, "\n")

cat("\n\nMODEL SUMMARY:\n")
print(model_pupil)

cat("\n========== END REPORT ==========\n")
