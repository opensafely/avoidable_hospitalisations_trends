####################################
### Age-standardise monthly data ###
####################################


# Purpose: To create age- and sex-standardise avoidable hospitalisation estimates for each individual month of data data by deprivation, region and urban-rural areas. This file is run multiple times and called from age_standardise_trends.R



## 1. Tidy data ##


# Convert to data.table format as I think the library is ace, sorry not sorry
input <- data.table(input)

# Calculate age group
input[, age_group:=cut(age, breaks = c(0,9,19,29,39,49,59,69,79,120), include.lowest=T)] # Define age groups
input[age_group=="[0,9]", age_group:="0-9"] # Rename labels (plus below lines)
input[age_group=="(9,19]", age_group:="10-19"]
input[age_group=="(19,29]", age_group:="20-29"]
input[age_group=="(29,39]", age_group:="30-39"]
input[age_group=="(39,49]", age_group:="40-49"]
input[age_group=="(49,59]", age_group:="50-59"]
input[age_group=="(59,69]", age_group:="60-69"]
input[age_group=="(69,79]", age_group:="70-79"]
input[age_group=="(79,120]", age_group:="80+"]



## 2. Age- and sex-standardise data ##


# a. Calculate the standard population #

std_pop <- input[, list(admitted = sum(admitted, na.rm =T), admitted_acs_all = sum(admitted_acs_all, na.rm = T), admitted_acs_acute = sum(admitted_acs_acute, na.rm = T), admitted_acs_chronic = sum(admitted_acs_chronic, na.rm = T), admitted_acs_vaccine = sum(admitted_acs_vaccine, na.rm = T), admitted_eucs = sum(admitted_eucs, na.rm = T), std_pop = .N), by = c("age_group", "sex", "date")] # Aggregate data by age group, sex and date
std_pop$admitted_rate <- std_pop$admitted / std_pop$std_pop # Calculate rate for admissions overall
std_pop$admitted_acs_all_rate <- std_pop$admitted_acs_all / std_pop$std_pop # Repeat rate calculation but by measure of avoidable hospitalisation
std_pop$admitted_acs_acute_rate <- std_pop$admitted_acs_acute / std_pop$std_pop
std_pop$admitted_acs_chronic_rate <- std_pop$admitted_acs_chronic / std_pop$std_pop
std_pop$admitted_acs_vaccine_rate <- std_pop$admitted_acs_vaccine / std_pop$std_pop
std_pop$admitted_eucs_rate <- std_pop$admitted_eucs / std_pop$std_pop
std_pop <- std_pop[, c("age_group", "sex", "date", "std_pop", "admitted_rate", "admitted_acs_all_rate", "admitted_acs_acute_rate", "admitted_acs_chronic_rate", "admitted_acs_vaccine_rate", "admitted_eucs_rate")] # Keep required variables


# b. Age- and sex-standardise data by deprivation quintile #

# Aggregate by deprivation quntile, age and sex over time
input_imd <- input[, list(admitted = sum(admitted, na.rm =T), admitted_acs_all = sum(admitted_acs_all, na.rm = T), admitted_acs_acute = sum(admitted_acs_acute, na.rm = T), admitted_acs_chronic = sum(admitted_acs_chronic, na.rm = T), admitted_acs_vaccine = sum(admitted_acs_vaccine, na.rm = T), admitted_eucs = sum(admitted_eucs, na.rm = T), pop = .N), by = c("age_group", "sex", "imd_quintile", "date")]

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

# Drop small counts (i.e., <=5) so data can be released
input_imd <- input_imd[input_imd$pop > 5] # Drop any rows where population is <= 5

input_imd$admitted[input_imd$admitted <= 5] <- NA # So if numnber of admitted is 0-5, then we replace the value as missing
input_imd$iexp_admitted[is.na(input_imd$admitted)] <- NA # Repeat for both expected counts as derived from above
input_imd$dexp_admitted[is.na(input_imd$admitted)] <- NA 

input_imd$admitted_acs_all[input_imd$admitted_acs_all <= 5] <- NA # Now repeat process for each measure individually
input_imd$iexp_admitted_acs_all[is.na(input_imd$admitted_acs_all)] <- NA
input_imd$dexp_admitted_acs_all[is.na(input_imd$admitted_acs_all)] <- NA

input_imd$admitted_acs_acute[input_imd$admitted_acs_acute <= 5] <- NA 
input_imd$iexp_admitted_acs_acute[is.na(input_imd$admitted_acs_all)] <- NA
input_imd$dexp_admitted_acs_acute[is.na(input_imd$admitted_acs_all)] <- NA

input_imd$admitted_acs_chronic[input_imd$admitted_acs_chronic <= 5] <- NA 
input_imd$iexp_admitted_acs_chronic[is.na(input_imd$admitted_acs_chronic)] <- NA
input_imd$dexp_admitted_acs_chronic[is.na(input_imd$admitted_acs_chronic)] <- NA

input_imd$admitted_acs_vaccine[input_imd$admitted_acs_vaccine <= 5] <- NA 
input_imd$iexp_admitted_acs_vaccine[is.na(input_imd$admitted_acs_vaccine)] <- NA
input_imd$dexp_admitted_acs_vaccine[is.na(input_imd$admitted_acs_vaccine)] <- NA

input_imd$admitted_eucs[input_imd$admitted_eucs <= 5] <- NA 
input_imd$iexp_admitted_eucs[is.na(input_imd$admitted_eucs)] <- NA
input_imd$dexp_admitted_eucs[is.na(input_imd$admitted_eucs)] <- NA


# c. Age- and sex-standardise data by region #

# Aggregate by region, age and sex over time
input_region <- input[, list(admitted = sum(admitted, na.rm =T), admitted_acs_all = sum(admitted_acs_all, na.rm = T), admitted_acs_acute = sum(admitted_acs_acute, na.rm = T), admitted_acs_chronic = sum(admitted_acs_chronic, na.rm = T), admitted_acs_vaccine = sum(admitted_acs_vaccine, na.rm = T), admitted_eucs = sum(admitted_eucs, na.rm = T), pop = .N), by = c("age_group", "sex", "region", "date")]

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

# Drop small counts (i.e., <=5) so data can be released
input_region <- input_region[input_region$pop > 5] # Drop any rows where population is <= 5

input_region$admitted[input_region$admitted <= 5] <- NA # So if numnber of admitted is 0-5, then we replace the value as missing
input_region$iexp_admitted[is.na(input_region$admitted)] <- NA # Repeat for both expected counts as derived from above
input_region$dexp_admitted[is.na(input_region$admitted)] <- NA 

input_region$admitted_acs_all[input_region$admitted_acs_all <= 5] <- NA # Now repeat process for each measure individually
input_region$iexp_admitted_acs_all[is.na(input_region$admitted_acs_all)] <- NA
input_region$dexp_admitted_acs_all[is.na(input_region$admitted_acs_all)] <- NA

input_region$admitted_acs_acute[input_region$admitted_acs_acute <= 5] <- NA 
input_region$iexp_admitted_acs_acute[is.na(input_region$admitted_acs_all)] <- NA
input_region$dexp_admitted_acs_acute[is.na(input_region$admitted_acs_all)] <- NA

input_region$admitted_acs_chronic[input_region$admitted_acs_chronic <= 5] <- NA 
input_region$iexp_admitted_acs_chronic[is.na(input_region$admitted_acs_chronic)] <- NA
input_region$dexp_admitted_acs_chronic[is.na(input_region$admitted_acs_chronic)] <- NA

input_region$admitted_acs_vaccine[input_region$admitted_acs_vaccine <= 5] <- NA 
input_region$iexp_admitted_acs_vaccine[is.na(input_region$admitted_acs_vaccine)] <- NA
input_region$dexp_admitted_acs_vaccine[is.na(input_region$admitted_acs_vaccine)] <- NA

input_region$admitted_eucs[input_region$admitted_eucs <= 5] <- NA 
input_region$iexp_admitted_eucs[is.na(input_region$admitted_eucs)] <- NA
input_region$dexp_admitted_eucs[is.na(input_region$admitted_eucs)] <- NA


# d. Age- and sex-standardise data by urban-rural #

# Aggregate by urban-rural, age and sex over time
input_urbrur <- input[, list(admitted = sum(admitted, na.rm =T), admitted_acs_all = sum(admitted_acs_all, na.rm = T), admitted_acs_acute = sum(admitted_acs_acute, na.rm = T), admitted_acs_chronic = sum(admitted_acs_chronic, na.rm = T), admitted_acs_vaccine = sum(admitted_acs_vaccine, na.rm = T), admitted_eucs = sum(admitted_eucs, na.rm = T), pop = .N), by = c("age_group", "sex", "urban_rural", "date")]

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

# Drop small counts (i.e., <=5) so data can be released
input_urbrur <- input_urbrur[input_urbrur$pop > 5] # Drop any rows where population is <= 5

input_urbrur$admitted[input_urbrur$admitted <= 5] <- NA # So if numnber of admitted is 0-5, then we replace the value as missing
input_urbrur$iexp_admitted[is.na(input_urbrur$admitted)] <- NA # Repeat for both expected counts as derived from above
input_urbrur$dexp_admitted[is.na(input_urbrur$admitted)] <- NA 

input_urbrur$admitted_acs_all[input_urbrur$admitted_acs_all <= 5] <- NA # Now repeat process for each measure individually
input_urbrur$iexp_admitted_acs_all[is.na(input_urbrur$admitted_acs_all)] <- NA
input_urbrur$dexp_admitted_acs_all[is.na(input_urbrur$admitted_acs_all)] <- NA

input_urbrur$admitted_acs_acute[input_urbrur$admitted_acs_acute <= 5] <- NA 
input_urbrur$iexp_admitted_acs_acute[is.na(input_urbrur$admitted_acs_all)] <- NA
input_urbrur$dexp_admitted_acs_acute[is.na(input_urbrur$admitted_acs_all)] <- NA

input_urbrur$admitted_acs_chronic[input_urbrur$admitted_acs_chronic <= 5] <- NA 
input_urbrur$iexp_admitted_acs_chronic[is.na(input_urbrur$admitted_acs_chronic)] <- NA
input_urbrur$dexp_admitted_acs_chronic[is.na(input_urbrur$admitted_acs_chronic)] <- NA

input_urbrur$admitted_acs_vaccine[input_urbrur$admitted_acs_vaccine <= 5] <- NA 
input_urbrur$iexp_admitted_acs_vaccine[is.na(input_urbrur$admitted_acs_vaccine)] <- NA
input_urbrur$dexp_admitted_acs_vaccine[is.na(input_urbrur$admitted_acs_vaccine)] <- NA

input_urbrur$admitted_eucs[input_urbrur$admitted_eucs <= 5] <- NA 
input_urbrur$iexp_admitted_eucs[is.na(input_urbrur$admitted_eucs)] <- NA
input_urbrur$dexp_admitted_eucs[is.na(input_urbrur$admitted_eucs)] <- NA

# e. Age- and sex-standardise data by ethnicity #

# Aggregate by ethnicity, age and sex over time
input_ethnicity <- input[, list(admitted = sum(admitted, na.rm =T), admitted_acs_all = sum(admitted_acs_all, na.rm = T), admitted_acs_acute = sum(admitted_acs_acute, na.rm = T), admitted_acs_chronic = sum(admitted_acs_chronic, na.rm = T), admitted_acs_vaccine = sum(admitted_acs_vaccine, na.rm = T), admitted_eucs = sum(admitted_eucs, na.rm = T), pop = .N), by = c("age_group", "sex", "ethnicity", "date")]

# Join on standard population data
input_ethnicity <- merge(input_ethnicity, std_pop, by = c("age_group", "sex", "date"), all.x = TRUE) # Join expected rates onto the main dataset

# Calculate expected number of admissions (indirect method)
input_ethnicity$iexp_admitted <- input_ethnicity$pop * input_ethnicity$admitted_rate # Multiple expected rate by population size - repeat for all measures
input_ethnicity$iexp_admitted_acs_all <- input_ethnicity$pop * input_ethnicity$admitted_acs_all_rate 
input_ethnicity$iexp_admitted_acs_acute <- input_ethnicity$pop * input_ethnicity$admitted_acs_acute_rate 
input_ethnicity$iexp_admitted_acs_chronic <- input_ethnicity$pop * input_ethnicity$admitted_acs_chronic_rate 
input_ethnicity$iexp_admitted_acs_vaccine <- input_ethnicity$pop * input_ethnicity$admitted_acs_vaccine_rate 
input_ethnicity$iexp_admitted_eucs <- input_ethnicity$pop * input_ethnicity$admitted_eucs_rate 

# Calculate expected number of admissions (direct method)
input_ethnicity$dexp_admitted <- (input_ethnicity$admitted / input_ethnicity$pop) * input_ethnicity$std_pop # Multiple observed rate by standard population - repeat for all measures
input_ethnicity$dexp_admitted_acs_all <- (input_ethnicity$admitted_acs_all / input_ethnicity$pop) * input_ethnicity$std_pop
input_ethnicity$dexp_admitted_acs_acute <- (input_ethnicity$admitted_acs_acute / input_ethnicity$pop) * input_ethnicity$std_pop
input_ethnicity$dexp_admitted_acs_chronic <- (input_ethnicity$admitted_acs_chronic / input_ethnicity$pop) * input_ethnicity$std_pop
input_ethnicity$dexp_admitted_acs_vaccine <- (input_ethnicity$admitted_acs_vaccine / input_ethnicity$pop) * input_ethnicity$std_pop
input_ethnicity$dexp_admitted_eucs <- (input_ethnicity$admitted_eucs / input_ethnicity$pop) * input_ethnicity$std_pop

# Sum data by age
input_ethnicity <- input_ethnicity[, list(admitted = sum(admitted, na.rm =T), iexp_admitted = sum(iexp_admitted , na.rm = T), dexp_admitted = sum(dexp_admitted , na.rm = T), admitted_acs_all = sum(admitted_acs_all, na.rm =T), iexp_admitted_acs_all = sum(iexp_admitted_acs_all , na.rm = T), dexp_admitted_acs_all = sum(dexp_admitted_acs_all , na.rm = T), admitted_acs_acute = sum(admitted_acs_acute, na.rm =T), iexp_admitted_acs_acute = sum(iexp_admitted_acs_acute , na.rm = T), dexp_admitted_acs_acute = sum(dexp_admitted_acs_acute , na.rm = T), admitted_acs_chronic = sum(admitted_acs_chronic, na.rm =T), iexp_admitted_acs_chronic = sum(iexp_admitted_acs_chronic , na.rm = T), dexp_admitted_acs_chronic = sum(dexp_admitted_acs_chronic , na.rm = T), admitted_acs_vaccine = sum(admitted_acs_vaccine, na.rm =T), iexp_admitted_acs_vaccine = sum(iexp_admitted_acs_vaccine , na.rm = T), dexp_admitted_acs_vaccine = sum(dexp_admitted_acs_vaccine , na.rm = T), admitted_eucs = sum(admitted_eucs, na.rm =T), iexp_admitted_eucs = sum(iexp_admitted_eucs , na.rm = T), dexp_admitted_eucs = sum(dexp_admitted_eucs , na.rm = T), pop = sum(pop, na.rm = T), std_pop = sum(std_pop, na.rm = T)), by = c("sex", "ethnicity", "date")]

# Drop small counts (i.e., <=5) so data can be released
input_ethnicity <- input_ethnicity[input_ethnicity$pop > 5] # Drop any rows where population is <= 5

input_ethnicity$admitted[input_ethnicity$admitted <= 5] <- NA # So if numnber of admitted is 0-5, then we replace the value as missing
input_ethnicity$iexp_admitted[is.na(input_ethnicity$admitted)] <- NA # Repeat for both expected counts as derived from above
input_ethnicity$dexp_admitted[is.na(input_ethnicity$admitted)] <- NA 

input_ethnicity$admitted_acs_all[input_ethnicity$admitted_acs_all <= 5] <- NA # Now repeat process for each measure individually
input_ethnicity$iexp_admitted_acs_all[is.na(input_ethnicity$admitted_acs_all)] <- NA
input_ethnicity$dexp_admitted_acs_all[is.na(input_ethnicity$admitted_acs_all)] <- NA

input_ethnicity$admitted_acs_acute[input_ethnicity$admitted_acs_acute <= 5] <- NA 
input_ethnicity$iexp_admitted_acs_acute[is.na(input_ethnicity$admitted_acs_all)] <- NA
input_ethnicity$dexp_admitted_acs_acute[is.na(input_ethnicity$admitted_acs_all)] <- NA

input_ethnicity$admitted_acs_chronic[input_ethnicity$admitted_acs_chronic <= 5] <- NA 
input_ethnicity$iexp_admitted_acs_chronic[is.na(input_ethnicity$admitted_acs_chronic)] <- NA
input_ethnicity$dexp_admitted_acs_chronic[is.na(input_ethnicity$admitted_acs_chronic)] <- NA

input_ethnicity$admitted_acs_vaccine[input_ethnicity$admitted_acs_vaccine <= 5] <- NA 
input_ethnicity$iexp_admitted_acs_vaccine[is.na(input_ethnicity$admitted_acs_vaccine)] <- NA
input_ethnicity$dexp_admitted_acs_vaccine[is.na(input_ethnicity$admitted_acs_vaccine)] <- NA

input_ethnicity$admitted_eucs[input_ethnicity$admitted_eucs <= 5] <- NA 
input_ethnicity$iexp_admitted_eucs[is.na(input_ethnicity$admitted_eucs)] <- NA
input_ethnicity$dexp_admitted_eucs[is.na(input_ethnicity$admitted_eucs)] <- NA



## 3. Tidy up R ##

rm(std_pop, input)
