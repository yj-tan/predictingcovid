library("tidyverse")
library("rjags")

LOG_RDS_CASES_PATH <- "final_logcases_dat.rds"
JAGS_MODEL_PATH <- "bugs/ols_covid.txt"

dataset <-
    readRDS(LOG_RDS_CASES_PATH) %>%
    as_tibble() %>%
    mutate(log_population_density_per_sqmi = log(population_density_per_sqmi)) %>%
    select(-population_density_per_sqmi) %>%
    mutate(across(!starts_with("log_mean") & where(is.numeric), scale))

response <- dataset$log_mean_confirmed_7d_total

predictors <- as.matrix(
    model.matrix(
        log_mean_confirmed_7d_total ~ poly(workplaces_percent_change_from_baseline, 3)
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
        data = dataset
    )[, -1]
)

jags_data <- list(
    x = predictors,
    y = response,
    N = length(response),
    K = ncol(predictors)
)

init_lm <- lm(
    formula = log_mean_confirmed_7d_total ~ poly(workplaces_percent_change_from_baseline, 3)
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
    data = dataset
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

qt <- data.table::setDT(as.data.frame(summary(samples)$quantiles), keep.rownames = "coeff")

ggplot(qt, aes(x = coeff, y = `50%`)) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymax = `97.5%`, ymin = `2.5%`)) +
    ggtitle("Confidence intervals for JAGS total confirmed cases") +
    xlab("Estimated Coefficient") +
    ylab("Value") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# summary(samples)

# plot(samples)
