library("tidyverse")
library("BAS")

LOG_RDS_CASES_PATH <- "final_logcases_dat.rds"
N_MODELS <- 50000

dataset <-
    readRDS(LOG_RDS_CASES_PATH) %>%
    as_tibble() %>%
    mutate(log_population_density_per_sqmi = log(population_density_per_sqmi)) %>%
    select(-population_density_per_sqmi) %>%
    mutate(across(!starts_with("log_mean") & where(is.numeric), scale))

inter_1o_bas <- bas.lm(
    formula = log_mean_confirmed_7d_total ~ poly(never_rarely_mask, 3)
    + poly(retail_and_recreation_percent_change_from_baseline, 3)
        + poly(workplaces_percent_change_from_baseline, 3)
        + stay_at_home
        + log_population_density_per_sqmi
        + poly(percent_uninsured, 3)
        + poly(percent_non_hispanic_white, 3)
        + poly(per_capita_income, 3)
        + poly(percent_65_and_over, 3)
        + poly(ELEV_M, 3)
        + poly(mean_temp_15d_avg)
        + ((.)^2 - .),
    data = dataset,
    method = "MCMC",
    prior = "JZS",
    update = 100,
    n.models = N_MODELS,
    bigmem = T
)

smaller_bas <- bas.lm(
    formula =
        log_mean_confirmed_7d_total ~
        poly(workplaces_percent_change_from_baseline, 3)
        + stay_at_home
            + log_population_density_per_sqmi
            + percent_uninsured
            + poly(percent_non_hispanic_white, 3)
            + percent_65_and_over
            + stay_at_home:percent_uninsured
            + stay_at_home:percent_non_hispanic_white
            + percent_uninsured:percent_non_hispanic_white
            + percent_non_hispanic_white:percent_65_and_over
            + log_population_density_per_sqmi:per_capita_income
            + percent_65_and_over:mean_temp_15d_avg
            + log_population_density_per_sqmi:ELEV_M,
    data = dataset,
    method = "MCMC",
    prior = "JZS",
    update = 100,
    n.models = N_MODELS,
    bigmem = T
)

coef_smaller_bas <- coef(smaller_bas, estimator = "BMA")

plot(coef_smaller_bas)
plot(confint(coef_smaller_bas))

diagnostics(coef_smaller_bas)
