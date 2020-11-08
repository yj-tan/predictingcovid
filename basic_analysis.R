library("mgcv")
library("BAS")
library("tidyverse")

LOG_RDS_CASES_PATH <- "final_logcases_dat.rds"
N_MODELS <- 50000

dataset <-
    readRDS(LOG_RDS_CASES_PATH) %>%
    as_tibble() %>%
    mutate(log_population_density_per_sqmi = log(population_density_per_sqmi)) %>%
    mutate(across(!starts_with("log_mean") & where(is.numeric), scale))

basic_gam <- gam(
    log_mean_confirmed_7d_total ~ s(never_rarely_mask)
    + s(retail_and_recreation_percent_change_from_baseline)
        + s(workplaces_percent_change_from_baseline)
        + stay_at_home + s(population_density_per_sqmi)
        + s(percent_uninsured) + s(percent_non_hispanic_white)
        + s(per_capita_income)
        + s(percent_65_and_over)
        + s(ELEV_M)
        + s(mean_temp_15d_avg),
    data = dataset
)

summary(basic_gam)

basic_bas <- bas.lm(
    formula = log_mean_confirmed_7d_total ~ . - population_density_per_sqmi,
    data = dataset,
    method = "MCMC",
    prior = "JZS",
    update = 100,
    n.models = N_MODELS
)

poly_bas <- bas.lm(
    formula = log_mean_confirmed_7d_total ~ poly(never_rarely_mask, 3)
    + poly(retail_and_recreation_percent_change_from_baseline, 3)
        + poly(workplaces_percent_change_from_baseline, 3)
        + stay_at_home + log_population_density_per_sqmi
        + poly(percent_uninsured, 3) + poly(percent_non_hispanic_white, 3)
        + poly(per_capita_income, 3) + poly(percent_65_and_over, 3)
        + poly(ELEV_M, 3) + poly(mean_temp_15d_avg),
    data = dataset,
    method = "MCMC",
    prior = "JZS",
    update = 100,
    n.models = N_MODELS
)

gprior_bas <- bas.lm(
    formula = log_mean_confirmed_7d_total ~ . - population_density_per_sqmi,
    data = dataset,
    method = "MCMC",
    prior = "g-prior",
    alpha = 4,
    update = 100,
    n.models = N_MODELS
)
