# -----------------------------------------------------------------------------
# STEP 3: Model Diagnostics & Validation
# -----------------------------------------------------------------------------

# Convergence Checks
cat("--- MCMC Convergence (Comprehensive Model) ---\n")
print(rhat(model_all))
mcmc_plot(model_all, type = "trace")

# Posterior Predictive Checks
pp_check(model_all, ndraws = 100) +
    ggtitle("Posterior Predictive Check: Observed vs Simulated Accuracy") +
    theme_ota()

# Model Comparison (LOO-CV)
loo_all <- loo(model_all)
loo_ling <- loo(model_linguistic)
loo_dist <- loo(model_distinctness)

loo_compare(loo_all, loo_ling, loo_dist)
