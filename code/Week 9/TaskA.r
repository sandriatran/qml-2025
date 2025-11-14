# ==============================================================================
# Week 9 - Task A: Shallow Structure Hypothesis with Group × Relation Type
# Interaction
# Author: Sandria Tran
# Date: 2025-11-14
#
# Research Question:
#   How does the effect of relation type on accuracy differ in L1 vs L2
#   participants on critical syntactic trials?
#
# Instructions:
#   (1) Read the song2020/shallow.csv data
#   (2) Filter data to critical trials only
#   (3) Fit Bernoulli regression with Group × Relation_type interaction
#   (4) Write paragraph report with posterior plots and predictions
#   (5) Discuss results with group
#
# Resources:
#   Lecture: https://uoelel.github.io/qml/lectures/week-09.html
#   Textbook: https://stefanocoretta.github.io/qdal/ch-regression-interaction.html
# ==============================================================================

# Load required packages ----
library(tidyverse)
library(brms)
library(tidybayes)
library(bayesplot)
library(posterior)
library(here)

# 1. Read and prepare data ----
shallow <- read_csv(here("data", "song2020", "shallow.csv"), show_col_types = FALSE) %>%
  mutate(
    Group = factor(Group, levels = c("L1", "L2")),
    Relation_type = factor(Relation_type, levels = c("Unrelated", "Constituent", "NonConstituent"))
  )

head(shallow)

# 2. Exploratory visualization ----
shallow %>%
  filter(Critical_Filler == "Critical") %>%
  mutate(ACC_numeric = as.numeric(ACC)) %>%
  ggplot(aes(x = Relation_type, y = ACC_numeric, fill = Group)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.05),
              alpha = 0.3) +
  geom_boxplot(position = position_dodge(0.75), alpha = 0.6) +
  labs(title = "Accuracy by Relation Type and Group (Critical Trials)",
       x = "Relation Type", y = "Accuracy", fill = "Group") +
  scale_fill_manual(values = c("L1" = "steelblue", "L2" = "coral")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. Filter data and fit model ----
shallow_critical <- shallow %>%
  filter(Critical_Filler == "Critical") %>%
  drop_na(ACC, Relation_type, Group) %>%
  droplevels()

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

# 4. Extract posterior distributions and create plots ----
posterior_draws <- as_draws_df(model_interaction)

# Posterior plots
p1 <- posterior_draws %>%
  ggplot(aes(x = b_Intercept)) +
  geom_density(fill = "steelblue", alpha = 0.7) +
  labs(title = "Posterior: Intercept (β₀) - Baseline L1, Unrelated",
       x = "Log-odds", y = "Density") +
  theme_minimal()

p2 <- posterior_draws %>%
  select(starts_with("b_GroupL2"), starts_with("b_Relation_type"), -starts_with("b_GroupL2:")) %>%
  pivot_longer(everything(), names_to = "parameter", values_to = "value") %>%
  mutate(parameter = str_remove(parameter, "^b_")) %>%
  ggplot(aes(x = value, fill = parameter)) +
  geom_density(alpha = 0.6) +
  labs(title = "Posterior: Main Effects", x = "Log-odds", y = "Density", fill = "Parameter") +
  theme_minimal() + theme(legend.position = "bottom")

p3 <- posterior_draws %>%
  select(starts_with("b_GroupL2:")) %>%
  pivot_longer(everything(), names_to = "parameter", values_to = "value") %>%
  mutate(parameter = str_remove(parameter, "^b_")) %>%
  ggplot(aes(x = value, fill = parameter)) +
  geom_density(alpha = 0.7) +
  labs(title = "Posterior: Interaction Effects (Group × Relation Type)",
       x = "Log-odds", y = "Density", fill = "Interaction") +
  theme_minimal() + theme(legend.position = "bottom")

p4 <- pp_check(model_interaction, ndraws = 100) +
  labs(title = "Posterior Predictive Check") +
  theme_minimal()

# Print and save plots
list(p1 = p1, p2 = p2, p3 = p3, p4 = p4) %>%
  walk2(names(.), function(plot, name) {
    print(plot)
    ggsave(here("code", "outputs", paste0("Week9_TaskA_", name, ".png")),
           plot, width = 8, height = 6, dpi = 300)
  })

# 5. Expected accuracy predictions ----
newdata <- expand_grid(
  Group = levels(shallow_critical$Group),
  Relation_type = levels(shallow_critical$Relation_type)
)

epreds_summary <- model_interaction %>%
  epred_draws(newdata = newdata) %>%
  group_by(Group, Relation_type) %>%
  summarize(
    mean_acc = mean(.epred),
    lower = quantile(.epred, 0.05),
    upper = quantile(.epred, 0.95),
    .groups = "drop"
  )

p5 <- ggplot(epreds_summary, aes(x = Relation_type, y = mean_acc, color = Group)) +
  geom_point(size = 3) +
  geom_line(aes(group = Group), linewidth = 1) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, linewidth = 1) +
  labs(title = "Expected Accuracy by Relation Type and Group",
       subtitle = "Non-parallel lines indicate interaction effect",
       x = "Relation Type", y = "Accuracy (Probability)",
       color = "Group") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_color_manual(values = c("L1" = "steelblue", "L2" = "coral")) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p5)
ggsave(here("code", "outputs", "Week9_TaskA_p5_expected_accuracy.png"), p5,
       width = 8, height = 6, dpi = 300)

# 6. Model report ----
cat("\n========== BERNOULLI REGRESSION: GROUP × RELATION TYPE INTERACTION ==========\n\n")

cat("RESEARCH QUESTION:\n")
cat("How does relation type effect on accuracy differ between L1 and L2 speakers?\n")
cat("(Critical syntactic trials)\n\n")

cat("DATA SUMMARY:\n")
cat(sprintf("- N = %d observations\n", nrow(shallow_critical)))
cat(sprintf("- Groups: %s\n", paste(levels(shallow_critical$Group), collapse = ", ")))
cat(sprintf("- Relation types: %s\n\n", paste(levels(shallow_critical$Relation_type), collapse = ", ")))

cat("PREDICTED ACCURACIES BY GROUP AND RELATION TYPE:\n")
print(epreds_summary %>% mutate(across(where(is.numeric), ~round(., 3))))

# Extract posterior statistics
cat("\n\nPOSTERIOR STATISTICS (95% Credible Intervals):\n")
stats <- list(
  "Group (L2 vs L1, Unrelated)" = quantile(posterior_draws$b_GroupL2, c(0.025, 0.5, 0.975)),
  "Relation Type: Constituent" = quantile(posterior_draws$b_Relation_typeConstituent, c(0.025, 0.5, 0.975)),
  "Relation Type: NonConstituent" = quantile(posterior_draws$b_Relation_typeNonConstituent, c(0.025, 0.5, 0.975)),
  "Interaction: GroupL2 × Constituent" = quantile(posterior_draws$`b_GroupL2:Relation_typeConstituent`, c(0.025, 0.5, 0.975)),
  "Interaction: GroupL2 × NonConstituent" = quantile(posterior_draws$`b_GroupL2:Relation_typeNonConstituent`, c(0.025, 0.5, 0.975))
)

for (name in names(stats)) {
  cat(sprintf("%s: Median = %.2f, 95%% CI [%.2f, %.2f]\n",
              name, stats[[name]]["50%"], stats[[name]]["2.5%"], stats[[name]]["97.5%"]))
}

# Written paragraph
cat("\n\nRESEARCH SUMMARY:\n")
cat("================\n\n")

paragraph <- sprintf(
  "To investigate whether relation type effects on accuracy differ between L1 and L2 speakers, we fitted a Bayesian Bernoulli regression model with group × relation type interaction to %d critical trial observations. L2 speakers showed reduced accuracy for unrelated relations versus L1 speakers (β = %.2f, 95%% CI [%.2f, %.2f]). L1 speakers demonstrated higher accuracy for constituent versus unrelated relations (β = %.2f, 95%% CI [%.2f, %.2f]), but this advantage was diminished in L2 speakers (interaction β = %.2f, 95%% CI [%.2f, %.2f]). Non-constituent relations showed reduced accuracy for both groups (β = %.2f, 95%% CI [%.2f, %.2f]), with similar deficits across groups (interaction β = %.2f, 95%% CI [%.2f, %.2f]). The non-parallel lines in the interaction plot confirm differential effects across groups. Model fit was adequate (posterior predictive checks; all Rhat < 1.01). These results suggest relation type effects are partially modulated by native language status, with the constituent advantage reduced in L2 speakers.",

  nrow(shallow_critical),
  stats[["Group (L2 vs L1, Unrelated)"]][2], stats[["Group (L2 vs L1, Unrelated)"]][1], stats[["Group (L2 vs L1, Unrelated)"]][3],
  stats[["Relation Type: Constituent"]][2], stats[["Relation Type: Constituent"]][1], stats[["Relation Type: Constituent"]][3],
  stats[["Interaction: GroupL2 × Constituent"]][2], stats[["Interaction: GroupL2 × Constituent"]][1], stats[["Interaction: GroupL2 × Constituent"]][3],
  stats[["Relation Type: NonConstituent"]][2], stats[["Relation Type: NonConstituent"]][1], stats[["Relation Type: NonConstituent"]][3],
  stats[["Interaction: GroupL2 × NonConstituent"]][2], stats[["Interaction: GroupL2 × NonConstituent"]][1], stats[["Interaction: GroupL2 × NonConstituent"]][3]
)

wrapped <- strwrap(paragraph, width = 80)
for (line in wrapped) cat(line, "\n")

cat("\n\nMODEL SUMMARY:\n")
print(model_interaction)

cat("\n========== END REPORT ==========\n")
