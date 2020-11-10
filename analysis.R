library("tidyverse")
library("BAS")

LOG_RDS_CASES_PATH <- "final_logcases_dat.rds"
LOG_RDS_DIFFCASES_PATH <- "final_logdiffcases_dat.rds"
LOG_RDS_DIFFCASESWTOTAL_PATH <- "final_logdiffcaseswtotal_dat.rds"
N_MODELS <- 10000

dataset <-
    readRDS(LOG_RDS_CASES_PATH) %>%
    as_tibble() %>%
    mutate(log_population_density_per_sqmi = log(population_density_per_sqmi)) %>%
    select(-population_density_per_sqmi) %>%
    mutate(across(!starts_with("log_mean") & where(is.numeric), scale))

diff_dataset <-
    readRDS(LOG_RDS_DIFFCASES_PATH) %>%
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

diff_inter_1o_bas <- bas.lm(
    formula = log_mean_confirmed_7d_diff ~ poly(never_rarely_mask, 3)
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
    data = diff_dataset,
    method = "MCMC",
    prior = "JZS",
    update = 100,
    n.models = N_MODELS,
    bigmem = T
)

# predictors at >0.8 inclusion probability without using total cases as an adjuster
predictors <- as.matrix(
    model.matrix(
        log_mean_confirmed_7d_diff ~ 
            poly(never_rarely_mask, 2)
        + I(retail_and_recreation_percent_change_from_baseline^3)
        + poly(workplaces_percent_change_from_baseline, 3)
        + stay_at_home
        + log_population_density_per_sqmi
        + percent_uninsured
        + poly(percent_non_hispanic_white, 3)
        + I(per_capita_income^2)
        + percent_65_and_over
        + I(ELEV_M^3) # poly(ELEV_M, 3)
        + workplaces_percent_change_from_baseline:percent_non_hispanic_white
        + percent_uninsured:percent_65_and_over
        + per_capita_income:mean_temp_15d_avg
        + log_population_density_per_sqmi:percent_uninsured
        + log_population_density_per_sqmi:percent_non_hispanic_white
        + log_population_density_per_sqmi:per_capita_income,
        data = diff_dataset
    )[, -1]
)

difflm <- lm(log_mean_confirmed_7d_diff ~ 
                 poly(never_rarely_mask, 2)
             + I(retail_and_recreation_percent_change_from_baseline^3)
             + poly(workplaces_percent_change_from_baseline, 3)
             + stay_at_home
             + log_population_density_per_sqmi
             + percent_uninsured
             + poly(percent_non_hispanic_white, 3)
             + I(per_capita_income^2)
             + percent_65_and_over
             + I(ELEV_M^3) # poly(ELEV_M, 3)
             + workplaces_percent_change_from_baseline:percent_non_hispanic_white
             + percent_uninsured:percent_65_and_over
             + per_capita_income:mean_temp_15d_avg
             + log_population_density_per_sqmi:percent_uninsured
             + log_population_density_per_sqmi:percent_non_hispanic_white
             + log_population_density_per_sqmi:per_capita_income,
             data = diff_dataset[-c(301, 1506, 446),]
             )

####################################################################################################
# with total as predictor

# with total cases are predictor 
diff_dataset2 <-
    readRDS(LOG_RDS_DIFFCASESWTOTAL_PATH) %>%
    as_tibble() %>%
    mutate(log_population_density_per_sqmi = log(population_density_per_sqmi)) %>%
    select(-population_density_per_sqmi) %>%
    mutate(across(!starts_with("log_mean") & where(is.numeric), scale))

diff_inter_1o_bas2 <- bas.lm(
    formula = log_mean_confirmed_7d_diff ~ poly(never_rarely_mask, 3)
    + poly(retail_and_recreation_percent_change_from_baseline, 3)
    + poly(workplaces_percent_change_from_baseline, 3)
    + stay_at_home
    + log_population_density_per_sqmi
    + log_mean_confirmed_7d_total
    + poly(percent_uninsured, 3)
    + poly(percent_non_hispanic_white, 3)
    + poly(per_capita_income, 3)
    + poly(percent_65_and_over, 3)
    + poly(ELEV_M, 3)
    + poly(mean_temp_15d_avg, 3)
    + ((.)^2 - .),
    data = diff_dataset2,
    method = "MCMC",
    prior = "JZS",
    update = 100,
    n.models = N_MODELS,
    bigmem = T
)
# predictors at >0.9 inclusion probability without using total cases as an adjuster
predictors2 <- as.matrix(
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
difflm2 <- lm(log_mean_confirmed_7d_diff ~ 
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



############################################################ smaller bas
### smaller 
smaller_bas <- bas.lm(
    formula =
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
    data = diff_dataset2,
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

# posterior predictive checks
BPM <- predict(diff_inter_1o_bas2, estimator = "BPM", se.fit = TRUE)
conf.fit <- confint(BPM, parm = "mean")
conf.pred <- confint(BPM, parm = "pred")
plot(conf.fit)
plot(conf.pred)
