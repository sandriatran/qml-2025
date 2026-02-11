# -----------------------------------------------------------------------------
# STEP 2: Bayesian Model Fitting
# -----------------------------------------------------------------------------
set.seed(2025)

# 1. Priors
priors_main <- c(
    prior(normal(0, 1.5), class = Intercept),
    prior(normal(0, 1.5), class = b),
    prior(exponential(1), class = sd)
)

# 2. Comprehensive Model (Factor: contrast_type)
model_all <- brm(
    formula = accuracy ~ contrast_type + (1 | subject_id) + (1 | item_id),
    data = data_clean,
    family = bernoulli(link = "logit"),
    prior = priors_main,
    iter = 2000, warmup = 1000, chains = 4, cores = 4,
    file = file.path(output_dir, "model_comprehensive")
)

# 3. Linguistic Model (Factor: phonological_status)
model_linguistic <- brm(
    formula = accuracy ~ phonological_status + (1 | subject_id) + (1 | item_id),
    data = data_clean,
    family = bernoulli(link = "logit"),
    prior = priors_main,
    iter = 2000, warmup = 1000, chains = 4, cores = 4,
    file = file.path(output_dir, "model_linguistic")
)

# 4. Distinctness Model (Continuous: phonologically_distinct)
model_distinctness <- brm(
    formula = accuracy ~ phonologically_distinct + (1 | subject_id) + (1 | item_id),
    data = data_clean,
    family = bernoulli(link = "logit"),
    prior = priors_main,
    iter = 2000, warmup = 1000, chains = 4, cores = 4,
    file = file.path(output_dir, "model_distinctness")
)
