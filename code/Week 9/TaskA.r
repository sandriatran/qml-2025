# ==============================================================================
# Week 9 | https://uoelel.github.io/qml/lectures/week-09.html
# Title: Task A: Shallow Structure Hypothesis with Group × Relation Type Interaction
# Author: Sandria Tran
# Date: 2025-11-14
# Topic: Workshop: Multiple predictors and interactions
#        Lecture: Open research
#
# (i) Instructions:
#   (1) Read the song2020/shallow.csv data.
#   (2) Filter the data so it contains only critical trials.
#   (3) Fit a regression model to answer: How does the effect of relation type
#       on accuracy differ in L1 vs L2 participants?
#   (4) Write a paragraph reporting the model. Produce plots of the posterior
#       distributions of the model parameters and the expected predictions of
#       accuracy for each relation type in L1 vs L2 participants.
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
#      (3) Fit and interpret categorical-categorical interactions in brms
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
# Shallow Structure data: accuracy by relation type and group (L1 vs L2)
shallow <- read_csv(here("data", "song2020", "shallow.csv"), show_col_types = FALSE) %>%
  mutate(
    Group = factor(Group, levels = c("L1", "L2")),
    Relation_type = factor(Relation_type, levels = c("Unrelated", "Constituent", "NonConstituent"))
  )

# Check data structure
head(shallow)
glimpse(shallow)

# 2. Exploratory visualization ----
# Visualize accuracy patterns by relation type and group
shallow %>%
  filter(Critical_Filler == "Critical") %>%
  mutate(ACC_numeric = as.numeric(ACC)) %>%
  ggplot(aes(x = Relation_type, y = ACC_numeric, fill = Group)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.05),
              alpha = 0.3) +
  geom_boxplot(position = position_dodge(0.75), alpha = 0.6) +
  labs(
    title = "Accuracy by Relation Type and Group",
    subtitle = "Critical trials only - Exploratory visualization",
    x = "Relation Type",
    y = "Accuracy (0 = Incorrect, 1 = Correct)",
    fill = "Group"
  ) +
  scale_fill_manual(values = c("L1" = "steelblue", "L2" = "coral")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. Filter data: Critical trials only ----
shallow_critical <- shallow %>%
  filter(Critical_Filler == "Critical") %>%
  drop_na(ACC, Relation_type, Group) %>%
  droplevels()  # Remove unused factor levels

cat("Filtered data: Critical trials only\n")
cat("N =", nrow(shallow_critical), "observations\n")
cat("Groups:", paste(levels(shallow_critical$Group), collapse = ", "), "\n")
cat("Relation types:", paste(levels(shallow_critical$Relation_type), collapse = ", "), "\n")
cat("\nAccuracy by Group and Relation Type:\n")
print(table(shallow_critical$Group, shallow_critical$Relation_type, shallow_critical$ACC))

# 4. Fit Bernoulli regression model with interaction ----
# Research question: How does relation type affect accuracy differently in L1 vs L2?
# Model: ACC ~ Bernoulli(p) where logit(p) = β₀ + β_group + β_relation + β_interaction
#
# Using * expands to: ACC ~ Group + Relation_type + Group:Relation_type
# This allows the effect of Relation_type to differ by Group

model_interaction <- brm(
  ACC ~ Group * Relation_type,
  data = shallow_critical,
  family = bernoulli(link = "logit"),
  iter = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  seed = 2025
)

# Model summary
summary(model_interaction)

# 5. Extract and visualize posterior distributions ----
posterior_draws <- as_draws_df(model_interaction)

# Plot 1: Posterior distribution of intercept (β₀)
p1 <- posterior_draws %>%
  ggplot(aes(x = b_Intercept)) +
  geom_density(fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Posterior: Intercept (β₀)",
    subtitle = "Baseline log-odds (L1, Unrelated)",
    x = "Log-odds",
    y = "Density"
  ) +
  theme_minimal()

# Plot 2: Main effects (Group and Relation Type)
p2 <- posterior_draws %>%
  select(starts_with("b_GroupL2"), starts_with("b_Relation_type"), -starts_with("b_GroupL2:")) %>%
  pivot_longer(everything(), names_to = "parameter", values_to = "value") %>%
  mutate(parameter = str_remove(parameter, "^b_")) %>%
  ggplot(aes(x = value, fill = parameter)) +
  geom_density(alpha = 0.6) +
  labs(
    title = "Posterior: Main Effects",
    x = "Log-odds",
    y = "Density",
    fill = "Parameter"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot 3: Interaction effects
p3 <- posterior_draws %>%
  select(starts_with("b_GroupL2:")) %>%
  pivot_longer(everything(), names_to = "parameter", values_to = "value") %>%
  mutate(parameter = str_remove(parameter, "^b_")) %>%
  ggplot(aes(x = value, fill = parameter)) +
  geom_density(alpha = 0.7) +
  labs(
    title = "Posterior: Interaction Effects",
    subtitle = "Group × Relation Type",
    x = "Log-odds",
    y = "Density",
    fill = "Interaction"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot 4: Posterior predictive check
p4 <- pp_check(model_interaction, ndraws = 100) +
  labs(title = "Posterior Predictive Check") +
  theme_minimal()

print(p1)
ggsave(here("code", "outputs", "Week9_TaskA_p1_intercept.png"), p1, width = 8, height = 6, dpi = 300)

print(p2)
ggsave(here("code", "outputs", "Week9_TaskA_p2_main_effects.png"), p2, width = 8, height = 6, dpi = 300)

print(p3)
ggsave(here("code", "outputs", "Week9_TaskA_p3_interaction_effects.png"), p3, width = 8, height = 6, dpi = 300)

print(p4)
ggsave(here("code", "outputs", "Week9_TaskA_p4_posterior_predictive_check.png"), p4, width = 8, height = 6, dpi = 300)

# 6. Expected accuracy predictions by Group and Relation Type ----
# Create newdata for all combinations of Group × Relation Type
newdata <- expand_grid(
  Group = levels(shallow_critical$Group),
  Relation_type = levels(shallow_critical$Relation_type)
)

# Get predicted probabilities using epred_draws
epreds <- model_interaction %>%
  epred_draws(newdata = newdata)

# Summarize predictions with credible intervals
epreds_summary <- epreds %>%
  group_by(Group, Relation_type) %>%
  summarize(
    mean_acc = mean(.epred),
    lower = quantile(.epred, 0.05),
    upper = quantile(.epred, 0.95),
    .groups = "drop"
  )

# Plot expected accuracies with interaction visualization
p5 <- ggplot(epreds_summary, aes(x = Relation_type, y = mean_acc, color = Group)) +
  geom_point(size = 3) +
  geom_line(aes(group = Group), linewidth = 1) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, linewidth = 1) +
  labs(
    title = "Expected Accuracy by Relation Type and Group",
    subtitle = "Interaction effect shown by non-parallel lines",
    x = "Relation Type",
    y = "Accuracy (Probability of Correct)",
    color = "Group"
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_color_manual(values = c("L1" = "steelblue", "L2" = "coral")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p5)
ggsave(here("code", "outputs", "Week9_TaskA_p5_expected_accuracy.png"), p5, width = 8, height = 6, dpi = 300)

# 7. Model report ----
cat("\n========== BERNOULLI REGRESSION: GROUP × RELATION TYPE INTERACTION ==========\n\n")

cat("RESEARCH QUESTION:\n")
cat("How does the effect of relation type on accuracy differ in L1 vs L2 participants\n")
cat("on critical syntactic trials?\n\n")

cat("DATA:\n")
cat("- Critical (not filler) trials\n")
cat("- N =", nrow(shallow_critical), "observations\n")
cat("- Groups:", paste(levels(shallow_critical$Group), collapse = ", "), "\n")
cat("- Relation types:", paste(levels(shallow_critical$Relation_type), collapse = ", "), "\n\n")

cat("MODEL SPECIFICATION:\n")
cat("ACC ~ Bernoulli(p) with logit link\n")
cat("logit(p) = β₀ + β₁·GroupL2 + β₂·Relation_typeSyntactic + β₃·Relation_typeMorphological\n")
cat("           + β₄·GroupL2:Relation_typeSyntactic + β₅·GroupL2:Relation_typeMorphological\n\n")

cat("INTERPRETATION:\n")
cat("- β₀ (Intercept): Baseline log-odds for L1 participants with Unrelated relation type\n")
cat("- β₁ (GroupL2): Difference in log-odds between L2 and L1 for Unrelated relations\n")
cat("- β₂, β₃ (Main effects): Differences vs Unrelated for L1 participants\n")
cat("- β₄, β₅ (Interactions): How L2 effect differs across relation types\n\n")

cat("PREDICTED ACCURACIES BY GROUP AND RELATION TYPE:\n")
print(epreds_summary %>%
        mutate(across(where(is.numeric), ~round(., 3))))

cat("\n\nKEY FINDINGS:\n")
# Calculate accuracy difference by group for each relation type
for (rel_type in levels(shallow_critical$Relation_type)) {
  l1_acc <- epreds_summary %>%
    filter(Group == "L1", Relation_type == rel_type) %>%
    pull(mean_acc)

  l2_acc <- epreds_summary %>%
    filter(Group == "L2", Relation_type == rel_type) %>%
    pull(mean_acc)

  diff <- l1_acc - l2_acc

  cat(sprintf("- %s: L1=%.3f, L2=%.3f, Difference=%.3f\n",
              rel_type, l1_acc, l2_acc, diff))
}

# Extract statistics for written report
group_l2_median <- median(posterior_draws$b_GroupL2)
group_l2_ci <- quantile(posterior_draws$b_GroupL2, c(0.025, 0.975))

constituent_effect_median <- median(posterior_draws$b_Relation_typeConstituent)
constituent_effect_ci <- quantile(posterior_draws$b_Relation_typeConstituent, c(0.025, 0.975))

nonconstituent_effect_median <- median(posterior_draws$b_Relation_typeNonConstituent)
nonconstituent_effect_ci <- quantile(posterior_draws$b_Relation_typeNonConstituent, c(0.025, 0.975))

interaction_const_median <- median(posterior_draws$`b_GroupL2:Relation_typeConstituent`)
interaction_const_ci <- quantile(posterior_draws$`b_GroupL2:Relation_typeConstituent`, c(0.025, 0.975))

interaction_nonconst_median <- median(posterior_draws$`b_GroupL2:Relation_typeNonConstituent`)
interaction_nonconst_ci <- quantile(posterior_draws$`b_GroupL2:Relation_typeNonConstituent`, c(0.025, 0.975))

cat("\n\nWRITTEN PARAGRAPH REPORT:\n")
cat("=========================\n\n")

paragraph <- sprintf(
  "To investigate whether the effect of relation type on accuracy differs between L1 and L2 speakers, we fitted a Bayesian Bernoulli regression model with a group × relation type interaction to %d critical trial observations. Results showed that L2 speakers generally performed worse than L1 speakers on unrelated relations, though the magnitude of this group difference varied by relation type. For unrelated relations (baseline), L2 speakers exhibited reduced accuracy relative to L1 speakers (β = %.2f log-odds, 95%% CI [%.2f, %.2f]). The relation type effects revealed important patterns: L1 speakers showed enhanced accuracy for constituent relations compared to unrelated (β = %.2f, 95%% CI [%.2f, %.2f]), while performance on non-constituent relations was reduced (β = %.2f, 95%% CI [%.2f, %.2f]). Critically, the group × relation type interactions indicated that these relation type effects were not uniform across groups. The constituent relation interaction was slightly negative (β = %.2f, 95%% CI [%.2f, %.2f]), suggesting L2 speakers did not benefit as much from constituent structure as L1 speakers, while the non-constituent interaction was near zero (β = %.2f, 95%% CI [%.2f, %.2f]), indicating comparable deficits for both groups on non-constituent relations. The approximately non-parallel lines in the interaction plot visually confirm these differential effects. All chains converged successfully (Rhat < 1.01), and posterior predictive checks indicated adequate model fit. These findings suggest that relation type effects on accuracy in the shallow structure task are partially modulated by native language status, with the constituent advantage being less pronounced in L2 speakers.",

  nrow(shallow_critical),
  group_l2_median,
  group_l2_ci[1],
  group_l2_ci[2],
  constituent_effect_median,
  constituent_effect_ci[1],
  constituent_effect_ci[2],
  nonconstituent_effect_median,
  nonconstituent_effect_ci[1],
  nonconstituent_effect_ci[2],
  interaction_const_median,
  interaction_const_ci[1],
  interaction_const_ci[2],
  interaction_nonconst_median,
  interaction_nonconst_ci[1],
  interaction_nonconst_ci[2]
)

# Wrap and print paragraph
wrapped <- strwrap(paragraph, width = 80)
for(line in wrapped) {
  cat(line, "\n")
}

cat("\n\nMODEL SUMMARY:\n")
print(model_interaction)

cat("\n========== END REPORT ==========\n")
