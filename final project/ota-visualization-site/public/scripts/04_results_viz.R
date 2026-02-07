# -----------------------------------------------------------------------------
# STEP 4: Results & Visualization
# -----------------------------------------------------------------------------

# Main Effect: Predicted Error Rates
data_all %>%
    add_epred_draws(model_all) %>%
    mutate(error_rate = 1 - .epred) %>%
    ggplot(aes(x = contrast_type, y = error_rate, fill = contrast_type)) +
    stat_halfeye() +
    scale_fill_manual(values = contrast_fills) +
    labs(title = "Posterior Predicted Error Rates", x = "Contrast", y = "Error Rate") +
    theme_ota()

# Pairwise Comparisons & ROPE
# (Testing LR vs H equivalence)
lr_vs_h <- hypothesis(model_all, "contrast_typeLR = contrast_typeH")
print(lr_vs_h)
