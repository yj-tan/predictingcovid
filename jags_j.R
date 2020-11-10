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

stay_map <- model.matrix(
  log_mean_confirmed_7d_total ~ stay_at_home + log_population_density_per_sqmi,
  data = dataset
)

jags_data <- list(
  cases = dataset$log_mean_confirmed_7d_total,
  work = as.numeric(dataset$workplaces_percent_change_from_baseline),
  stay = as.numeric(stay_map[, 2]),
  pop = as.numeric(dataset$log_population_density_per_sqmi),
  insur = as.numeric(dataset$percent_uninsured),
  nonhis = as.numeric(dataset$percent_non_hispanic_white),
  older = as.numeric(dataset$percent_65_and_over),
  income = as.numeric(dataset$per_capita_income),
  temp = as.numeric(dataset$mean_temp_15d_avg),
  elev = as.numeric(dataset$ELEV_M),
  N = length(dataset$log_mean_confirmed_7d_total)
)

inits <- list(
  b0 = rnorm(1),
  b1 = rnorm(1),
  b2 = rnorm(1),
  b3 = rnorm(1),
  b4 = rnorm(1),
  b5 = rnorm(1),
  b6 = rnorm(1),
  b7 = rnorm(1),
  b8 = rnorm(1),
  b9 = rnorm(1),
  b10 = rnorm(1),
  b11 = rnorm(1),
  b12 = rnorm(1),
  b13 = rnorm(1),
  b14 = rnorm(1),
  b15 = rnorm(1),
  b16 = rnorm(1),
  tau = 10
)

model <- jags.model(JAGS_MODEL_PATH, data = jags_data, inits = inits, n.chains = 10, quiet = F)

update(model, 10000, progress.bar = "text")

params <- c(
  "b0", "b1", "b2", "b3", "b4", "b5", "b6",
  "b7", "b8", "b9", "b10", "b11", "b12",
  "b13", "b14", "b15", "b16", "sigma"
)

samples <- coda.samples(
  model,
  variable.names = params,
  n.iter = 30000, progress.bar = "text"
)

# summary(samples)

# plot(samples)
