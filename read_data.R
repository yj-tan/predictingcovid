library("tidyverse")

MASK_USE_PATH <- "covardat/mask-use-by-county.csv"
MOBILITY_PATH <- "covardat/2020_US_Region_Mobility_Report.csv"
PROTEST_PATH <- "covardat/protests.csv"

START_DATE <- "2020-07-21"
END_DATE <- "2020-07-28"

# Kaggle dataset

# Percent uninsured, Percent non-white or Hispanic,
# Per capita income, Percent 65 and older,
# Elevation, Mean 15 day temperature, Stay at home order (Y/n)

# Our other datasets

# Mask use, Population density, Mobility

mask_use <- read.csv(MASK_USE_PATH,
    colClasses = c("COUNTYFP" = "factor")
) %>%
    as_tibble()

mobility <- read.csv(MOBILITY_PATH,
    colClasses = c(
        "country_region_code" = "factor",
        "country_region" = "factor",
        "sub_region_1" = "factor",
        "sub_region_2" = "factor",
        "iso_3166_2_code" = "factor",
        "census_fips_code" = "factor",
        "date" = "Date"
    )
) %>%
    as_tibble()

mobility %<>%
  filter(census_fips_code != ""
  & date >= START_DATE
  & date <= END_DATE) %>%
  group_by(census_fips_code) %>%
  select(-c(
    country_region_code,
    country_region,
    sub_region_1,
    sub_region_2,
    metro_area,
    iso_3166_2_code,
    date
  )) %>%
  summarise(across(.fns = ~ mean(.x, na.rm = TRUE)))

both_mask_mobility <-
    left_join(mobility, mask_use, by = c("census_fips_code" = "COUNTYFP")) %>%
    mutate(proportion_mask_disuse = NEVER + RARELY) %>%
    select(
        -c(
            NEVER,
            RARELY,
            SOMETIMES,
            FREQUENTLY,
            ALWAYS
        ),
    ) %>%
    select(
        census_fips_code,
        proportion_mask_disuse,
        retail_and_recreation_percent_change_from_baseline,
        workplaces_percent_change_from_baseline,
        residential_percent_change_from_baseline
    ) %>%
  drop_na()


protests <- read.csv(PROTEST_PATH, colClasses = c("Date" = "Date"))
protests <- filter(
  protests,
  (Event..legacy..see.tags. == "Racial Injustice"
  | Event..legacy..see.tags. == "Civil Rights")
  & Date >= START_DATE
  & Date <= END_DATE
)
