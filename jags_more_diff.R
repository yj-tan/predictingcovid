library("tidyverse")
library("rjags")

LOG_RDS_DIFFCASESWTOTAL_PATH <- "final_logdiffcaseswtotal_dat.rds"
JAGS_MODEL_PATH <- "bugs/ols_covid.txt"

diff_dataset2 <-
    readRDS(LOG_RDS_DIFFCASESWTOTAL_PATH) %>%
    as_tibble() %>%
    mutate(log_population_density_per_sqmi = log(population_density_per_sqmi)) %>%
    select(-population_density_per_sqmi) %>%
    mutate(across(!starts_with("log_mean") & where(is.numeric), scale))

response <- diff_dataset2$log_mean_confirmed_7d_diff

predictors <- as.matrix(
    model.matrix(
        log_mean_confirmed_7d_diff ~
        I(never_rarely_mask^2)
        + I(never_rarely_mask^3)
            + workplaces_percent_change_from_baseline
            + log_mean_confirmed_7d_total
            + I(percent_non_hispanic_white^2)
            + I(mean_temp_15d_avg^2)
            + log_mean_confirmed_7d_total:workplaces_percent_change_from_baseline
            + log_mean_confirmed_7d_total:percent_uninsured
            + log_mean_confirmed_7d_total:mean_temp_15d_avg
            + stay_at_home:ELEV_M
            + percent_uninsured:percent_65_and_over
            + percent_uninsured:ELEV_M
            + log_population_density_per_sqmi:percent_non_hispanic_white,
        data = diff_dataset2
    )[, -1]
)
jags_data <- list(
    x = predictors,
    y = response,
    N = length(response),
    K = ncol(predictors)
)

init_lm <- lm(
    log_mean_confirmed_7d_diff ~
    I(never_rarely_mask^2)
    + I(never_rarely_mask^3)
        + workplaces_percent_change_from_baseline
        + log_mean_confirmed_7d_total
        + I(percent_non_hispanic_white^2)
        + I(mean_temp_15d_avg^2)
        + log_mean_confirmed_7d_total:workplaces_percent_change_from_baseline
        + log_mean_confirmed_7d_total:percent_uninsured
        + log_mean_confirmed_7d_total:mean_temp_15d_avg
        + stay_at_home:ELEV_M
        + percent_uninsured:percent_65_and_over
        + percent_uninsured:ELEV_M
        + log_population_density_per_sqmi:percent_non_hispanic_white,
    data = diff_dataset2
)

beta_inits <- replace_na(init_lm$coefficients[-1], 0.1)
tau_init <- length(response) / sum(init_lm$residuals^2)

inits <- list(beta0 = 0, beta = beta_inits, tau = tau_init)

model <- jags.model(JAGS_MODEL_PATH, data = jags_data, inits = inits, n.chains = 4, quiet = F)

update(model, 10000, progress.bar = "text")

params <- c("beta0", "beta", "sigma")

samples <- coda.samples(
    model,
    variable.names = params,
    n.iter = 30000, progress.bar = "text"
)
