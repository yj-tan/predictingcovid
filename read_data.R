library("tidyverse")

MASK_USE_PATH <- "mask-use-by-county.csv"
MOBILITY_PATH <- "2020_US_Region_Mobility_Report.csv"
PROTEST_PATH <- "protests.csv"

mask_use <- read.csv(MASK_USE_PATH, colClasses = c("COUNTYFP" = "factor"))

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
)

mobility %<>%
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
    summarise(across(.fns = ~ mean()))

protests <- read.csv(PROTEST_PATH, colClasses = c("Date" = "Date"))
protests <- filter(
    protests,
    (Event..legacy..see.tags. == "Racial Injustice"
    | Event..legacy..see.tags. == "Civil Rights")
    & Date >= "2020-03-13"
    & Date <= "2020-10-20"
)
