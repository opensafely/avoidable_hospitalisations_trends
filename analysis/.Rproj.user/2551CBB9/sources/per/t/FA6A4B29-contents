############################
### Age-standardise data ###
############################


# Purpose: To age- and sex-standardise avoidable hospitalisation trends data by deprivation, region and urban-rural areas.


# Load libraries
library(data.table)
# library(ggplot2)
library(readr)
library(here)


## 1. Load all data into R and tidy ##


# Load data (individually at a time - suggestions for more efficient code welcome!)
input_19_1 <- read_csv(gsub("analysis", "", here("output/measures","input_2019-01-01.csv.gz"))) # Load
input_19_1$date <- "2019-01-01" # Add date

input_19_2 <- read_csv(gsub("analysis", "", here("output/measures","input_2019-02-01.csv.gz"))) # Load
input_19_2$date <- "2019-02-01" # Add date

input_19_3 <- read_csv(gsub("analysis", "", here("output/measures","input_2019-03-01.csv.gz"))) # Load
input_19_3$date <- "2019-03-01" # Add date

input_19_4 <- read_csv(gsub("analysis", "", here("output/measures","input_2019-04-01.csv.gz"))) # Load
input_19_4$date <- "2019-04-01" # Add date

input_19_5 <- read_csv(gsub("analysis", "", here("output/measures","input_2019-05-01.csv.gz"))) # Load
input_19_5$date <- "2019-05-01" # Add date

input_19_6 <- read_csv(gsub("analysis", "", here("output/measures","input_2019-06-01.csv.gz"))) # Load
input_19_6$date <- "2019-06-01" # Add date

input_19_7 <- read_csv(gsub("analysis", "", here("output/measures","input_2019-07-01.csv.gz"))) # Load
input_19_7$date <- "2019-07-01" # Add date

input_19_8 <- read_csv(gsub("analysis", "", here("output/measures","input_2019-08-01.csv.gz"))) # Load
input_19_8$date <- "2019-08-01" # Add date

input_19_9 <- read_csv(gsub("analysis", "", here("output/measures","input_2019-09-01.csv.gz"))) # Load
input_19_9$date <- "2019-09-01" # Add date

input_19_10 <- read_csv(gsub("analysis", "", here("output/measures","input_2019-10-01.csv.gz"))) # Load
input_19_10$date <- "2019-10-01" # Add date

input_19_11 <- read_csv(gsub("analysis", "", here("output/measures","input_2019-11-01.csv.gz"))) # Load
input_19_11$date <- "2019-11-01" # Add date

input_19_12 <- read_csv(gsub("analysis", "", here("output/measures","input_2019-12-01.csv.gz"))) # Load
input_19_12$date <- "2019-12-01" # Add date

input_20_1 <- read_csv(gsub("analysis", "", here("output/measures","input_2020-01-01.csv.gz"))) # Load
input_20_1$date <- "2020-01-01" # Add date

input_20_2 <- read_csv(gsub("analysis", "", here("output/measures","input_2020-02-01.csv.gz"))) # Load
input_20_2$date <- "2020-02-01" # Add date

input_20_3 <- read_csv(gsub("analysis", "", here("output/measures","input_2020-03-01.csv.gz"))) # Load
input_20_3$date <- "2020-03-01" # Add date

input_20_4 <- read_csv(gsub("analysis", "", here("output/measures","input_2020-04-01.csv.gz"))) # Load
input_20_4$date <- "2020-04-01" # Add date

input_20_5 <- read_csv(gsub("analysis", "", here("output/measures","input_2020-05-01.csv.gz"))) # Load
input_20_5$date <- "2020-05-01" # Add date

input_20_6 <- read_csv(gsub("analysis", "", here("output/measures","input_2020-06-01.csv.gz"))) # Load
input_20_6$date <- "2020-06-01" # Add date

input_20_7 <- read_csv(gsub("analysis", "", here("output/measures","input_2020-07-01.csv.gz"))) # Load
input_20_7$date <- "2020-07-01" # Add date

input_20_8 <- read_csv(gsub("analysis", "", here("output/measures","input_2020-08-01.csv.gz"))) # Load
input_20_8$date <- "2020-08-01" # Add date

input_20_9 <- read_csv(gsub("analysis", "", here("output/measures","input_2020-09-01.csv.gz"))) # Load
input_20_9$date <- "2020-09-01" # Add date

input_20_10 <- read_csv(gsub("analysis", "", here("output/measures","input_2020-10-01.csv.gz"))) # Load
input_20_10$date <- "2020-10-01" # Add date

input_20_11 <- read_csv(gsub("analysis", "", here("output/measures","input_2020-11-01.csv.gz"))) # Load
input_20_11$date <- "2020-11-01" # Add date

input_20_12 <- read_csv(gsub("analysis", "", here("output/measures","input_2020-12-01.csv.gz"))) # Load
input_20_12$date <- "2020-12-01" # Add date

input_21_1 <- read_csv(gsub("analysis", "", here("output/measures","input_2021-01-01.csv.gz"))) # Load
input_21_1$date <- "2021-01-01" # Add date

input_21_2 <- read_csv(gsub("analysis", "", here("output/measures","input_2021-02-01.csv.gz"))) # Load
input_21_2$date <- "2021-02-01" # Add date

input_21_3 <- read_csv(gsub("analysis", "", here("output/measures","input_2021-03-01.csv.gz"))) # Load
input_21_3$date <- "2021-03-01" # Add date

input_21_4 <- read_csv(gsub("analysis", "", here("output/measures","input_2021-04-01.csv.gz"))) # Load
input_21_4$date <- "2021-04-01" # Add date

input_21_5 <- read_csv(gsub("analysis", "", here("output/measures","input_2021-05-01.csv.gz"))) # Load
input_21_5$date <- "2021-05-01" # Add date

input_21_6 <- read_csv(gsub("analysis", "", here("output/measures","input_2021-06-01.csv.gz"))) # Load
input_21_6$date <- "2021-06-01" # Add date

input_21_7 <- read_csv(gsub("analysis", "", here("output/measures","input_2021-07-01.csv.gz"))) # Load
input_21_7$date <- "2021-07-01" # Add date

input_21_8 <- read_csv(gsub("analysis", "", here("output/measures","input_2021-08-01.csv.gz"))) # Load
input_21_8$date <- "2021-08-01" # Add date

input_21_9 <- read_csv(gsub("analysis", "", here("output/measures","input_2021-09-01.csv.gz"))) # Load
input_21_9$date <- "2021-09-01" # Add date

input_21_10 <- read_csv(gsub("analysis", "", here("output/measures","input_2021-10-01.csv.gz"))) # Load
input_21_10$date <- "2021-10-01" # Add date

input_21_11 <- read_csv(gsub("analysis", "", here("output/measures","input_2021-11-01.csv.gz"))) # Load
input_21_11$date <- "2021-11-01" # Add date

input_21_12 <- read_csv(gsub("analysis", "", here("output/measures","input_2021-12-01.csv.gz"))) # Load
input_21_12$date <- "2021-12-01" # Add date

input_22_1 <- read_csv(gsub("analysis", "", here("output/measures","input_2022-01-01.csv.gz"))) # Load
input_22_1$date <- "2022-01-01" # Add date

input_22_2 <- read_csv(gsub("analysis", "", here("output/measures","input_2022-02-01.csv.gz"))) # Load
input_22_2$date <- "2022-02-01" # Add date

input_22_3 <- read_csv(gsub("analysis", "", here("output/measures","input_2022-03-01.csv.gz"))) # Load
input_22_3$date <- "2022-03-01" # Add date

# Join together into single file
dfs <- sapply(.GlobalEnv, is.data.frame) # Get a list of all objects for below
input_all <- do.call(rbind, mget(names(dfs)[dfs])) # Join all files together
rm(list = setdiff(ls(), "input_all")) # Remove all files bar combined one
input_all <- data.table(input_all) # Convert to data.table format as I think the library is ace, sorry not sorry

# Calculate age group
input_all[, age_group:=cut(age, breaks = c(0,9,19,29,39,49,59,69,79,120), include.lowest=T)] # Define age groups
input_all[age_group=="[0,9]", age_group:="0-9"] # Rename labels (plus below lines)
input_all[age_group=="(9,19]", age_group:="10-19"]
input_all[age_group=="(19,29]", age_group:="20-29"]
input_all[age_group=="(29,39]", age_group:="30-39"]
input_all[age_group=="(39,49]", age_group:="40-49"]
input_all[age_group=="(49,59]", age_group:="50-59"]
input_all[age_group=="(59,69]", age_group:="60-69"]
input_all[age_group=="(69,79]", age_group:="70-79"]
input_all[age_group=="(79,120]", age_group:="80+"]


## 2. Age- and sex-standardise data ##


# a. Calculate the standard population #

std_pop <- input_all[, list(admitted = sum(admitted, na.rm =T), admitted_acs_all = sum(admitted_acs_all, na.rm = T), admitted_acs_acute = sum(admitted_acs_acute, na.rm = T), admitted_acs_chronic = sum(admitted_acs_chronic, na.rm = T), admitted_acs_vaccine = sum(admitted_acs_vaccine, na.rm = T), admitted_eucs = sum(admitted_eucs, na.rm = T), std_pop = .N), by = c("age_group", "sex", "date")] # Aggregate data by age group, sex and date
std_pop$admitted_rate <- std_pop$admitted / std_pop$std_pop # Calculate rate for admissions overall
std_pop$admitted_acs_all_rate <- std_pop$admitted_acs_all / std_pop$std_pop # Repeat rate calculation but by measure of avoidable hospitalisation
std_pop$admitted_acs_acute_rate <- std_pop$admitted_acs_acute / std_pop$std_pop
std_pop$admitted_acs_chronic_rate <- std_pop$admitted_acs_chronic / std_pop$std_pop
std_pop$admitted_acs_vaccine_rate <- std_pop$admitted_acs_vaccine / std_pop$std_pop
std_pop$admitted_eucs_rate <- std_pop$admitted_eucs / std_pop$std_pop
std_pop <- std_pop[, c("age_group", "sex", "date", "std_pop", "admitted_rate", "admitted_acs_all_rate", "admitted_acs_acute_rate", "admitted_acs_chronic_rate", "admitted_acs_vaccine_rate", "admitted_eucs_rate")] # Keep required variables


# b. Age- and sex-standardise data by deprivation quintile #

# Aggregate by deprivation quntile, age and sex over time
input_imd <- input_all[, list(admitted = sum(admitted, na.rm =T), admitted_acs_all = sum(admitted_acs_all, na.rm = T), admitted_acs_acute = sum(admitted_acs_acute, na.rm = T), admitted_acs_chronic = sum(admitted_acs_chronic, na.rm = T), admitted_acs_vaccine = sum(admitted_acs_vaccine, na.rm = T), admitted_eucs = sum(admitted_eucs, na.rm = T), pop = .N), by = c("age_group", "sex", "imd_quintile", "date")]

# Join on standard population data
input_imd <- merge(input_imd, std_pop, by = c("age_group", "sex", "date"), all.x = TRUE) # Join expected rates onto the main dataset

# Calculate expected number of admissions (indirect method)
input_imd$iexp_admitted <- input_imd$pop * input_imd$admitted_rate # Multiple expected rate by population size - repeat for all measures
input_imd$iexp_admitted_acs_all <- input_imd$pop * input_imd$admitted_acs_all_rate 
input_imd$iexp_admitted_acs_acute <- input_imd$pop * input_imd$admitted_acs_acute_rate 
input_imd$iexp_admitted_acs_chronic <- input_imd$pop * input_imd$admitted_acs_chronic_rate 
input_imd$iexp_admitted_acs_vaccine <- input_imd$pop * input_imd$admitted_acs_vaccine_rate 
input_imd$iexp_admitted_eucs <- input_imd$pop * input_imd$admitted_eucs_rate 

# Calculate expected number of admissions (direct method)
input_imd$dexp_admitted <- (input_imd$admitted / input_imd$pop) * input_imd$std_pop # Multiple observed rate by standard population - repeat for all measures
input_imd$dexp_admitted_acs_all <- (input_imd$admitted_acs_all / input_imd$pop) * input_imd$std_pop
input_imd$dexp_admitted_acs_acute <- (input_imd$admitted_acs_acute / input_imd$pop) * input_imd$std_pop
input_imd$dexp_admitted_acs_chronic <- (input_imd$admitted_acs_chronic / input_imd$pop) * input_imd$std_pop
input_imd$dexp_admitted_acs_vaccine <- (input_imd$admitted_acs_vaccine / input_imd$pop) * input_imd$std_pop
input_imd$dexp_admitted_eucs <- (input_imd$admitted_eucs / input_imd$pop) * input_imd$std_pop

# Sum data by age
input_imd <- input_imd[, list(admitted = sum(admitted, na.rm =T), iexp_admitted = sum(iexp_admitted , na.rm = T), dexp_admitted = sum(dexp_admitted , na.rm = T), admitted_acs_all = sum(admitted_acs_all, na.rm =T), iexp_admitted_acs_all = sum(iexp_admitted_acs_all , na.rm = T), dexp_admitted_acs_all = sum(dexp_admitted_acs_all , na.rm = T), admitted_acs_acute = sum(admitted_acs_acute, na.rm =T), iexp_admitted_acs_acute = sum(iexp_admitted_acs_acute , na.rm = T), dexp_admitted_acs_acute = sum(dexp_admitted_acs_acute , na.rm = T), admitted_acs_chronic = sum(admitted_acs_chronic, na.rm =T), iexp_admitted_acs_chronic = sum(iexp_admitted_acs_chronic , na.rm = T), dexp_admitted_acs_chronic = sum(dexp_admitted_acs_chronic , na.rm = T), admitted_acs_vaccine = sum(admitted_acs_vaccine, na.rm =T), iexp_admitted_acs_vaccine = sum(iexp_admitted_acs_vaccine , na.rm = T), dexp_admitted_acs_vaccine = sum(dexp_admitted_acs_vaccine , na.rm = T), admitted_eucs = sum(admitted_eucs, na.rm =T), iexp_admitted_eucs = sum(iexp_admitted_eucs , na.rm = T), dexp_admitted_eucs = sum(dexp_admitted_eucs , na.rm = T), pop = sum(pop, na.rm = T), std_pop = sum(std_pop, na.rm = T)), by = c("sex", "imd_quintile", "date")]

# To calculate rates (for reference) #

# # Calculate indirect standardised ratios
# input_imd$smr_admitted <- input_imd$admitted / input_imd$exp_admitted # Ratio
# 
# # Note: Confidence intervals are estimated using the Vandenbroucke method
# input_imd$smr_admitted_low <- ((sqrt(input_imd$admitted) - 1.96 * 0.5) ^ 2)/ input_imd$exp_admitted # Lower bound 
# input_imd$smr_admitted_upp <- ((sqrt(input_imd$admitted) + 1.96 * 0.5) ^ 2)/ input_imd$exp_admitted # Upper bound 
# 
# # Classical method
# input_imd$smr_admitted_low <- input_imd$sir_admitted - (1.96 * (sqrt(input_imd$admitted)/ input_imd$exp_admitted)) # Lower bound 
# input_imd$smr_admitted_upp <- input_imd$sir_admitted + (1.96 * (sqrt(input_imd$admitted)/ input_imd$exp_admitted)) # Upper bound 
# 
# # Calculate indirect standardised rates
# input_imd$sir_admitted <- input_imd$smr_admitted * (input_imd$admitted / input_imd$pop) # Rate
# 
# # Calculate direct standardised rates
# 
# input_imd$dsr_admitted <- (input_imd$mk / input_imd$total_pop) * 100000 # Rate
# input_imd$dsr_admitted_low <- input_imd$dsr_admitted - (1.96 * (sqrt(input_imd$dsr_admitted))) # Lower bound 
# input_imd$dsr_admitted_upp <- input_imd$dsr_admitted + (1.96 * (sqrt(input_imd$dsr_admitted))) # Upper bound 

# Save
write.csv(input_imd, file = gsub("analysis", "", here("output/measures","standardised_imd_trends.csv")))



# c. Age- and sex-standardise data by region #

# Aggregate by deprivation quntile, age and sex over time
input_region <- input_all[, list(admitted = sum(admitted, na.rm =T), admitted_acs_all = sum(admitted_acs_all, na.rm = T), admitted_acs_acute = sum(admitted_acs_acute, na.rm = T), admitted_acs_chronic = sum(admitted_acs_chronic, na.rm = T), admitted_acs_vaccine = sum(admitted_acs_vaccine, na.rm = T), admitted_eucs = sum(admitted_eucs, na.rm = T), pop = .N), by = c("age_group", "sex", "region", "date")]

# Join on standard population data
input_region <- merge(input_region, std_pop, by = c("age_group", "sex", "date"), all.x = TRUE) # Join expected rates onto the main dataset

# Calculate expected number of admissions (indirect method)
input_region$iexp_admitted <- input_region$pop * input_region$admitted_rate # Multiple expected rate by population size - repeat for all measures
input_region$iexp_admitted_acs_all <- input_region$pop * input_region$admitted_acs_all_rate 
input_region$iexp_admitted_acs_acute <- input_region$pop * input_region$admitted_acs_acute_rate 
input_region$iexp_admitted_acs_chronic <- input_region$pop * input_region$admitted_acs_chronic_rate 
input_region$iexp_admitted_acs_vaccine <- input_region$pop * input_region$admitted_acs_vaccine_rate 
input_region$iexp_admitted_eucs <- input_region$pop * input_region$admitted_eucs_rate 

# Calculate expected number of admissions (direct method)
input_region$dexp_admitted <- (input_region$admitted / input_region$pop) * input_region$std_pop # Multiple observed rate by standard population - repeat for all measures
input_region$dexp_admitted_acs_all <- (input_region$admitted_acs_all / input_region$pop) * input_region$std_pop
input_region$dexp_admitted_acs_acute <- (input_region$admitted_acs_acute / input_region$pop) * input_region$std_pop
input_region$dexp_admitted_acs_chronic <- (input_region$admitted_acs_chronic / input_region$pop) * input_region$std_pop
input_region$dexp_admitted_acs_vaccine <- (input_region$admitted_acs_vaccine / input_region$pop) * input_region$std_pop
input_region$dexp_admitted_eucs <- (input_region$admitted_eucs / input_region$pop) * input_region$std_pop

# Sum data by age
input_region <- input_region[, list(admitted = sum(admitted, na.rm =T), iexp_admitted = sum(iexp_admitted , na.rm = T), dexp_admitted = sum(dexp_admitted , na.rm = T), admitted_acs_all = sum(admitted_acs_all, na.rm =T), iexp_admitted_acs_all = sum(iexp_admitted_acs_all , na.rm = T), dexp_admitted_acs_all = sum(dexp_admitted_acs_all , na.rm = T), admitted_acs_acute = sum(admitted_acs_acute, na.rm =T), iexp_admitted_acs_acute = sum(iexp_admitted_acs_acute , na.rm = T), dexp_admitted_acs_acute = sum(dexp_admitted_acs_acute , na.rm = T), admitted_acs_chronic = sum(admitted_acs_chronic, na.rm =T), iexp_admitted_acs_chronic = sum(iexp_admitted_acs_chronic , na.rm = T), dexp_admitted_acs_chronic = sum(dexp_admitted_acs_chronic , na.rm = T), admitted_acs_vaccine = sum(admitted_acs_vaccine, na.rm =T), iexp_admitted_acs_vaccine = sum(iexp_admitted_acs_vaccine , na.rm = T), dexp_admitted_acs_vaccine = sum(dexp_admitted_acs_vaccine , na.rm = T), admitted_eucs = sum(admitted_eucs, na.rm =T), iexp_admitted_eucs = sum(iexp_admitted_eucs , na.rm = T), dexp_admitted_eucs = sum(dexp_admitted_eucs , na.rm = T), pop = sum(pop, na.rm = T), std_pop = sum(std_pop, na.rm = T)), by = c("sex", "region", "date")]

# Save
write.csv(input_region, file = gsub("analysis", "", here("output/measures","standardised_region_trends.csv")))


# d. Age- and sex-standardise data by urban-rural #

# Aggregate by deprivation quntile, age and sex over time
input_urbrur <- input_all[, list(admitted = sum(admitted, na.rm =T), admitted_acs_all = sum(admitted_acs_all, na.rm = T), admitted_acs_acute = sum(admitted_acs_acute, na.rm = T), admitted_acs_chronic = sum(admitted_acs_chronic, na.rm = T), admitted_acs_vaccine = sum(admitted_acs_vaccine, na.rm = T), admitted_eucs = sum(admitted_eucs, na.rm = T), pop = .N), by = c("age_group", "sex", "urban_rural", "date")]

# Join on standard population data
input_urbrur <- merge(input_urbrur, std_pop, by = c("age_group", "sex", "date"), all.x = TRUE) # Join expected rates onto the main dataset

# Calculate expected number of admissions (indirect method)
input_urbrur$iexp_admitted <- input_urbrur$pop * input_urbrur$admitted_rate # Multiple expected rate by population size - repeat for all measures
input_urbrur$iexp_admitted_acs_all <- input_urbrur$pop * input_urbrur$admitted_acs_all_rate 
input_urbrur$iexp_admitted_acs_acute <- input_urbrur$pop * input_urbrur$admitted_acs_acute_rate 
input_urbrur$iexp_admitted_acs_chronic <- input_urbrur$pop * input_urbrur$admitted_acs_chronic_rate 
input_urbrur$iexp_admitted_acs_vaccine <- input_urbrur$pop * input_urbrur$admitted_acs_vaccine_rate 
input_urbrur$iexp_admitted_eucs <- input_urbrur$pop * input_urbrur$admitted_eucs_rate 

# Calculate expected number of admissions (direct method)
input_urbrur$dexp_admitted <- (input_urbrur$admitted / input_urbrur$pop) * input_urbrur$std_pop # Multiple observed rate by standard population - repeat for all measures
input_urbrur$dexp_admitted_acs_all <- (input_urbrur$admitted_acs_all / input_urbrur$pop) * input_urbrur$std_pop
input_urbrur$dexp_admitted_acs_acute <- (input_urbrur$admitted_acs_acute / input_urbrur$pop) * input_urbrur$std_pop
input_urbrur$dexp_admitted_acs_chronic <- (input_urbrur$admitted_acs_chronic / input_urbrur$pop) * input_urbrur$std_pop
input_urbrur$dexp_admitted_acs_vaccine <- (input_urbrur$admitted_acs_vaccine / input_urbrur$pop) * input_urbrur$std_pop
input_urbrur$dexp_admitted_eucs <- (input_urbrur$admitted_eucs / input_urbrur$pop) * input_urbrur$std_pop

# Sum data by age
input_urbrur <- input_urbrur[, list(admitted = sum(admitted, na.rm =T), iexp_admitted = sum(iexp_admitted , na.rm = T), dexp_admitted = sum(dexp_admitted , na.rm = T), admitted_acs_all = sum(admitted_acs_all, na.rm =T), iexp_admitted_acs_all = sum(iexp_admitted_acs_all , na.rm = T), dexp_admitted_acs_all = sum(dexp_admitted_acs_all , na.rm = T), admitted_acs_acute = sum(admitted_acs_acute, na.rm =T), iexp_admitted_acs_acute = sum(iexp_admitted_acs_acute , na.rm = T), dexp_admitted_acs_acute = sum(dexp_admitted_acs_acute , na.rm = T), admitted_acs_chronic = sum(admitted_acs_chronic, na.rm =T), iexp_admitted_acs_chronic = sum(iexp_admitted_acs_chronic , na.rm = T), dexp_admitted_acs_chronic = sum(dexp_admitted_acs_chronic , na.rm = T), admitted_acs_vaccine = sum(admitted_acs_vaccine, na.rm =T), iexp_admitted_acs_vaccine = sum(iexp_admitted_acs_vaccine , na.rm = T), dexp_admitted_acs_vaccine = sum(dexp_admitted_acs_vaccine , na.rm = T), admitted_eucs = sum(admitted_eucs, na.rm =T), iexp_admitted_eucs = sum(iexp_admitted_eucs , na.rm = T), dexp_admitted_eucs = sum(dexp_admitted_eucs , na.rm = T), pop = sum(pop, na.rm = T), std_pop = sum(std_pop, na.rm = T)), by = c("sex", "urban_rural", "date")]

# Save
write.csv(input_urbrur, file = gsub("analysis", "", here("output/measures","standardised_urbrur_trends.csv")))



## 3. Plot data ##


# # Deprivation quintile rates over time
# input_agg <- input_agg[order(input_agg$sex, input_agg$imd_quintile, input_agg$date)]
# plot1 <- ggplot(input_agg[input_agg$imd_quintile != 0], aes(x = as.Date(date), y = sir_admitted, color = factor(imd_quintile))) +
#   geom_path() +
#   geom_errorbar(aes(ymin = sir_admitted_low, ymax = sir_admitted_upp), width=.2,
#                 position=position_dodge(.9), alpha = 0.1) +
#   facet_wrap(~sex) +
#   xlab("Date") +
#   ylab("Age-Standardised incidence ratio") +
#   labs(color = "Quintile")
# plot1


## 4. Tidy ##

rm(list = ls())
gc()
