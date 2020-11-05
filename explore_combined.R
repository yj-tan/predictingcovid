library("tidyverse")

COMBINED_CSV_PATH <- "all_combined.csv"

combined <- read_csv(
    file = COMBINED_CSV_PATH,
    col_types = cols(
        countyfips = col_factor(),
        mean_confirmed_7d_total = col_double(),
        mean_deaths_7d_total = col_double(),
        mean_confirmed_7d_diff = col_double(),
        mean_deaths_7d_diff = col_double(),
        never_rarely_mask = col_double(),
        retail_and_recreation_percent_change_from_baseline = col_double(),
        workplaces_percent_change_from_baseline = col_double(),
        stay_at_home = col_factor(),
        population_density_per_sqmi = col_double(),
        percent_uninsured = col_double(),
        percent_non_hispanic_white = col_double(),
        per_capita_income = col_double(),
        percent_65_and_over = col_double(),
        ELEV_M = col_double(),
        mean_temp_15d_avg = col_double()
    )
)

hist(
    x = log(combined$mean_confirmed_7d_total),
    main = "Histogram of Log Mean Confirmed 7 Day Total",
    xlab = "log of Mean Confirmed 7 Day Total",
    freq = F
)

boxplot(
    x = log(combined$mean_deaths_7d_total),
    main = "Boxplot of Log Mean 7 Day Total Deaths",
    ylab = "Log Mean 7 Day Total Deaths"
)

hist(
    x = combined$never_rarely_mask,
    main = "Histogram of Rate of Mask Disuse",
    xlab = "Mask Disuse Proportion",
    freq = F
)

boxplot(
    x = combined$never_rarely_mask,
    main = "Boxplot of Rate of Mask Disuse",
    ylab = "Mask Disuse Proportion"
)

hist(
    x = combined$retail_and_recreation_percent_change_from_baseline,
    main = "Histogram of Percent Retail Change from Baseline",
    xlab = "Percent Retail Change from Baseline",
    freq = F
)

boxplot(
    x = combined$retail_and_recreation_percent_change_from_baseline,
    main = "Boxplot of Percent Retail Change from Baseline",
    ylab = "Percent Retail Change from Baseline"
)
