library("tidyverse")

COMBINED_CSV_PATH <- "all_combined.csv"
CLEANED_COMBINED_PATH <- "cleaned_all_combined.csv"

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

# Mean seven day confirmed cases

hist(
    x = log(combined$mean_confirmed_7d_total),
    main = "Histogram of Log Mean Confirmed 7 Day Total",
    xlab = "log of Mean Confirmed 7 Day Total",
    freq = F
)

boxplot(
    x = log(combined$mean_deaths_7d_total),
    main = "Boxplot of Log Mean 7 Day Total",
    ylab = "log Mean 7 Day Total Cases"
)

boxplot(
    formula = log(mean_deaths_7d_total) ~ stay_at_home,
    data = combined,
    main = "Boxplot of Mean 7 Day Total Deaths by Stay at Home Order",
    ylab = "log Mean 7 Day Total Cases",
    xlab = "Stay at Home Order (Yes = 1, No = 0)"
)

# Proportion of mask disuse

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

# Percentage retail and recreation mobility change from baseline

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

# Remove percent retail change from baseline > 100

# Percentage workplace mobility change from baseline

hist(
    x = combined$workplaces_percent_change_from_baseline,
    main = "Histogram of Percent Workplace Mobility Change from Baseline",
    xlab = "Percent Workplace Mobility Change from Baseline",
    freq = F
)

boxplot(
    x = combined$workplaces_percent_change_from_baseline,
    main = "Boxplot of Percent Workplace Mobility Change from Baseline",
    ylab = "Percent Workplace Mobility Change from Baseline"
)

# Remove percent workplace change from baseline < -60

# Population density per square mile

hist(
    x = combined$population_density_per_sqmi,
    main = "Histogram of Population Density per Sq. Mi.",
    xlab = "Population Density per Sq. Mi.",
    freq = F
)

boxplot(
    x = combined$population_density_per_sqmi,
    main = "Boxplot of Population Density per Sq. Mi.",
    ylab = "Population Density per Sq. Mi."
)

# A LOT OF OUTLIERS FOR POP DENSITY

# Percent Uninsured

hist(
    x = combined$percent_uninsured,
    main = "Histogram of Percent Uninsured",
    xlab = "Percent Uninsured",
    freq = F
)

boxplot(
    x = combined$percent_uninsured,
    main = "Boxplot of Percent Uninsured",
    ylab = "Percent Uninsured"
)

# Percent Non-Hispanic White

hist(
    x = combined$percent_non_hispanic_white,
    main = "Histogram of Percent Non-Hispanic White",
    xlab = "Percent Non-Hispanic White",
    freq = F
)

boxplot(
    x = combined$percent_non_hispanic_white,
    main = "Boxplot of Percent Non-Hispanic White",
    ylab = "Percent Non-Hispanic White"
)

# Cleaned data

dataset <- read_csv(
    file = CLEANED_COMBINED_PATH,
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

dataset %<>% mutate(across(!starts_with("log_mean") & where(is.numeric), scale))
