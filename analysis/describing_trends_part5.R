############################################
### Trends in avoidable hospitalisations ###
##### Part 5: Descriptives for report ######
############################################

# Purpose: To create a series of descriptive summary statistic tables for inclusion in the report.

# Libraries
library(dplyr)
library(data.table)

## 1. Load and tidy data ##

# Load data
trends_imd <- fread("../output/measures/standardised_imd_trends.csv") # Load
trends_imd <- trends_imd[trends_imd$date < "2022-04-01"] # Drop last period
trends_region <- fread("../output/measures/standardised_region_trends.csv") # Repeat
trends_region <- trends_region[trends_region$date < "2022-04-01"] 
trends_eth <- fread("../output/measures/standardised_ethnicity_trends.csv") # And again
trends_eth <- trends_eth[trends_eth$date < "2022-04-01"] 
trends_urbrur <- fread("../output/measures/standardised_urbrur_trends.csv") # Last time
trends_urbrur <- trends_urbrur[trends_urbrur$date < "2022-04-01"] 

# Convert to date format
trends_imd$date <- as.Date(trends_imd$date) 
trends_region$date <- as.Date(trends_region$date)

# Shift dates to mid-point of month since 1st of month refers to whole month (akes plots nicer to look at)
trends_imd$date <- trends_imd$date + 14

## 2. Create tables ##

# Outcome summary statistics #

# Sum values to totals
all_trends <- trends_imd[, list(admitted = sum(admitted), admitted_acs_all = sum(admitted_acs_all), admitted_acs_acute = sum(admitted_acs_acute), admitted_acs_chronic = sum(admitted_acs_chronic), admitted_acs_vaccine = sum(admitted_acs_vaccine), admitted_eucs = sum(admitted_eucs), pop = sum(pop)), by = c("date")]

# Total admissions over study period
sum(all_trends$admitted)
sum(all_trends$admitted_acs_all)
sum(all_trends$admitted_acs_acute)
sum(all_trends$admitted_acs_chronic)
sum(all_trends$admitted_acs_vaccine)
sum(all_trends$admitted_eucs)
sum(all_trends$pop)

# Mean monthly rates
mean(all_trends$admitted)
mean(all_trends$admitted_acs_all)
mean(all_trends$admitted_acs_acute)
mean(all_trends$admitted_acs_chronic)
mean(all_trends$admitted_acs_vaccine)
mean(all_trends$admitted_eucs)
mean(all_trends$pop)


# Missing data # 
(sum(trends_imd$pop[trends_imd$imd_quintile == 0 | is.na(trends_imd$imd_quintile)]) / sum(trends_imd$pop)) * 100 # IMD
(sum(trends_eth$pop[trends_eth$ethnicity == 0]) / sum(trends_eth$pop)) * 100 # Ethnicity
# (sum(trends_eth$pop[trends_eth$ethnicity == 5]) / sum(trends_eth$pop)) * 100
(sum(trends_region$pop[is.na(trends_region$region) ) / sum(trends_region$pop)) * 100 # Region
(sum(trends_urbrur$pop[trends_urbrur$urban_rural == 0 | trends_urbrur$urban_rural == -1]) / sum(trends_urbrur$pop)) * 100 # Urban/rural


