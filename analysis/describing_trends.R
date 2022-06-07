############################################
### Trends in avoidable hospitalisations ###
############################################

# Libraries
library(ggplot2)
library(PHEindicatormethods)

# Load data
trends_imd <- read.csv("../output/measures/standardised_imd_trends.csv") # Trends by neighbourhood deprivation
trends_eth <- read.csv("../output/measures/standardised_ethnicity_trends.csv")
trends_urbrur <- read.csv("../output/measures/standardised_urbrur_trends.csv")
trends_region <- read.csv("../output/measures/standardised_region_trends.csv")


# Plot
# IMD
trends_imd$smr <- trends_imd$admitted / trends_imd$iexp_admitted # SMR (indirect)
trends_imd$date <- as.Date(trends_imd$date)
# trends_imd$dsr_admitted <- (trends_imd$mk / trends_imd$total_pop) * 100000  # Rate (direct)
ggplot(trends_imd[trends_imd$imd_quintile != 0,], aes(x = date, y = smr, group = imd_quintile, color = imd_quintile)) +
  geom_line() +
  facet_wrap(~sex) +
  scale_x_date(date_breaks = "6 months", date_labels = "%m-%y")

trends_imd$smr <- trends_imd$admitted_acs_all / trends_imd$iexp_admitted_acs_all # SMR (indirect)
ggplot(trends_imd[trends_imd$imd_quintile != 0,], aes(x = date, y = smr, group = imd_quintile, color = imd_quintile)) +
  geom_line() +
  facet_wrap(~sex) +
  scale_x_date(date_breaks = "6 months", date_labels = "%m-%y")

trends_imd$smr <- trends_imd$admitted_acs_acute / trends_imd$iexp_admitted_acs_acute # SMR (indirect)
ggplot(trends_imd[trends_imd$imd_quintile != 0,], aes(x = date, y = smr, group = imd_quintile, color = imd_quintile)) +
  geom_line() +
  facet_wrap(~sex) +
  scale_x_date(date_breaks = "6 months", date_labels = "%m-%y")

trends_imd$smr <- trends_imd$admitted_acs_chronic / trends_imd$iexp_admitted_acs_chronic # SMR (indirect)
ggplot(trends_imd[trends_imd$imd_quintile != 0,], aes(x = date, y = smr, group = imd_quintile, color = imd_quintile)) +
  geom_line() +
  facet_wrap(~sex) +
  scale_x_date(date_breaks = "6 months", date_labels = "%m-%y")

trends_imd$smr <- trends_imd$admitted_acs_vaccine / trends_imd$iexp_admitted_acs_vaccine # SMR (indirect)
ggplot(trends_imd[trends_imd$imd_quintile != 0,], aes(x = date, y = smr, group = imd_quintile, color = imd_quintile)) +
  geom_line() +
  facet_wrap(~sex) +
  scale_x_date(date_breaks = "6 months", date_labels = "%m-%y")

trends_imd$smr <- trends_imd$admitted_eucs / trends_imd$iexp_admitted_eucs # SMR (indirect)
ggplot(trends_imd[trends_imd$imd_quintile != 0,], aes(x = date, y = smr, group = imd_quintile, color = imd_quintile)) +
  geom_line() +
  facet_wrap(~sex) +
  scale_x_date(date_breaks = "6 months", date_labels = "%m-%y")

trends_imd$sir <- trends_imd$smr * (trends_imd$admitted / trends_imd$pop) # As rate
ggplot(trends_imd[trends_imd$imd_quintile != 0,], aes(x = date, y = sir, group = imd_quintile, color = imd_quintile)) +
  geom_line() +
  facet_wrap(~sex) +
  scale_x_date(date_breaks = "6 months", date_labels = "%m-%y")

trends_imd$sir_low <- trends_imd$sir - (1.96 * (sqrt(trends_imd$admitted_eucs)/ trends_imd$iexp_admitted_eucs)) # Lower bound 
trends_imd$sir_upp <- trends_imd$sir + (1.96 * (sqrt(trends_imd$admitted_eucs)/ trends_imd$iexp_admitted_eucs)) # Upper bound 

test <- phe_sii(data = trends_imd, quantile = imd_quintile, population = pop, value = sir, value_type = 0, lower_cl = sir_low, upper_cl = sir_upp, confidence = 0.95, rii = FALSE, type = "standard")

# Ethnicity
trends_eth$smr <- trends_eth$admitted / trends_eth$iexp_admitted # SMR (indirect)
ggplot(trends_eth[trends_eth$ethnicity != 0,], aes(x = date, y = smr, group = ethnicity, color = ethnicity)) +
  geom_line() +
  facet_wrap(~sex)

# Urban v rural
trends_urbrur$smr <- trends_urbrur$admitted / trends_urbrur$iexp_admitted # SMR (indirect)
ggplot(trends_urbrur[trends_urbrur$urban_rural != 0,], aes(x = date, y = smr, group = urban_rural, color = urban_rural)) +
  geom_line() +
  facet_wrap(~sex)

# Region
trends_region$smr <- trends_region$admitted / trends_region$iexp_admitted # SMR (indirect)
trends_region$date <- as.Date(trends_region$date)
ggplot(trends_region[!is.na(trends_region$region),], aes(x = date, y = smr, group = region, color = region)) +
  geom_line() +
  facet_wrap(~sex)  +
  scale_x_date(date_breaks = "6 months", date_labels = "%m-%y")
