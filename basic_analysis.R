library("mgcv")
library("tidyverse")

LOG_RDS_CASES_PATH <- "final_logcases_dat.rds"

dataset <-
    readRDS(LOG_RDS_CASES_PATH) %>%
    as_tibble() %>%
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
