############################################
### Trends in avoidable hospitalisations ###
###### Part 2: Urban/rural patterns ########
############################################

# Purpose: To create a series of descriptive time series plots to visualise and describe how each of our outcome measures have changed between January 2019 and April 2022. The analyses here focus on trends by urban/rural status of areas. 

# Libraries
library(ggplot2)
library(viridis)
library(dplyr)
library(data.table)
library(patchwork)
library(tidyquant)

## 1. Load and tidy data ##

# Load data
trends_urbrur <- fread("../output/measures/standardised_urbrur_trends.csv") # Load
trends_urbrur <- trends_urbrur[trends_urbrur$date < "2022-04-01"] # Drop last period

# Convert to date format
trends_urbrur$date <- as.Date(trends_urbrur$date)

# Shift dates to mid-point of month since 1st of month refers to whole month (akes plots nicer to look at)
trends_urbrur$date <- trends_urbrur$date + 14


## 2. Plot trends ##

# Define labels for sex (used in plots)
labels <- c("Females", "Males")
names(labels) <- c("F", "M")

## 2a. Urban vs rural - 8 classification measure ##

# All hospital admissions #

# Create measures
trends_urbrur$dsr_admitted <- (trends_urbrur$dexp_admitted / trends_urbrur$std_pop) * 100000 # Rate (directly standardised)
trends_urbrur$dsr_admitted_low <- trends_urbrur$dsr_admitted - (1.96 * (sqrt(trends_urbrur$dsr_admitted))) # Lower bound 
trends_urbrur$dsr_admitted_upp <- trends_urbrur$dsr_admitted + (1.96 * (sqrt(trends_urbrur$dsr_admitted))) # Upper bound 

# Definitions
# 1 = urban major conurbation
# 2 = urban minor conurbation
# 3 = urban city and town
# 4 = urban city and town in a sparse setting
# 5 = rural town and fringe
# 6 = rural town and fringe in a sparse setting
# 7 = rural village and dispersed
# 8 = rural village and dispersed in a sparse setting

# Plot
ggplot(trends_urbrur[trends_urbrur$urban_rural > 0,],) +
  geom_ribbon(aes(x = date, ymin = dsr_admitted_low, ymax = dsr_admitted_upp, fill = factor(urban_rural)), alpha = 0.2) +
  geom_line(aes(x = date, y = dsr_admitted, group = factor(urban_rural), color = factor(urban_rural))) +
  facet_wrap(~sex) + 
  scale_colour_viridis_d(option = "magma", begin = 0.1, end = 0.9, labels = c("Urban major conurbation", "Urban minor conurbation", "Urban city and town", "Urban city and town in a sparse setting", "Rural town and fringe", "Rural town and fringe in a sparse setting", "Rural village and dispersed", "Rural village and dispersed in a sparse setting")) +
  scale_fill_viridis_d(option = "magma", begin = 0.1, end = 0.9, labels = c("Urban major conurbation", "Urban minor conurbation", "Urban city and town", "Urban city and town in a sparse setting", "Rural town and fringe", "Rural town and fringe in a sparse setting", "Rural village and dispersed", "Rural village and dispersed in a sparse setting")) +
  scale_x_date(date_labels = "%Y") 


## 2b. Urban v rural - binary classification ##
  
# Define urban/rural
trends_urbrur$urban_rural_new <- NA
trends_urbrur$urban_rural_new[trends_urbrur$urban_rural >= 1 & trends_urbrur$urban_rural <= 4] <- "Urban"
trends_urbrur$urban_rural_new[trends_urbrur$urban_rural >= 5 & trends_urbrur$urban_rural <= 8] <- "Rural"

# Aggregate data
trends_urbrur2 <- trends_urbrur[, list(dexp_admitted = sum(dexp_admitted, na.rm = T), dexp_admitted_acs_all = sum(dexp_admitted_acs_all, na.rm = T), dexp_admitted_acs_acute = sum(dexp_admitted_acs_acute, na.rm = T), dexp_admitted_acs_chronic = sum(dexp_admitted_acs_chronic, na.rm = T), dexp_admitted_acs_vaccine = sum(dexp_admitted_acs_vaccine, na.rm = T), dexp_admitted_eucs = sum(dexp_admitted_eucs, na.rm = T), std_pop = sum(std_pop, na.rm = T)), by = c("sex", "date", "urban_rural_new")]

# All hospital admissions #

# Create measures
trends_urbrur2$dsr_admitted <- (trends_urbrur2$dexp_admitted / trends_urbrur2$std_pop) * 100000 # Direct rate
trends_urbrur2$dsr_admitted_low <- trends_urbrur2$dsr_admitted - (1.96 * (sqrt(trends_urbrur2$dsr_admitted))) # Lower bound 
trends_urbrur2$dsr_admitted_upp <- trends_urbrur2$dsr_admitted + (1.96 * (sqrt(trends_urbrur2$dsr_admitted))) # Upper bound 

# Plot
urb1_f <- ggplot(trends_urbrur2[!is.na(trends_urbrur2$urban_rural_new) & trends_urbrur2$sex == "F"],) +
  # Plot lockdown periods
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Plot main data
  geom_ribbon(aes(x = date, ymin = dsr_admitted_low, ymax = dsr_admitted_upp, fill = factor(urban_rural_new)), alpha = 0.2) +
  geom_line(aes(x = date, y = dsr_admitted, group = factor(urban_rural_new), color = factor(urban_rural_new))) +
  # facet_wrap(~sex, labeller = labeller(sex = labels)) + 
  scale_colour_viridis_d(option = "magma", begin = 0.1, end = 0.9, labels = c("Rural", "Urban")) +
  scale_fill_viridis_d(option = "magma", begin = 0.1, end = 0.9, labels = c("Rural", "Urban")) +
  scale_x_date(date_labels = "%Y")+
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Location", fill = "Location") # Edit legend titles

urb1_m <- ggplot(trends_urbrur2[!is.na(trends_urbrur2$urban_rural_new) & trends_urbrur2$sex == "M"],) +
  # Plot lockdown periods
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Plot main data
  geom_ribbon(aes(x = date, ymin = dsr_admitted_low, ymax = dsr_admitted_upp, fill = factor(urban_rural_new)), alpha = 0.2) +
  geom_line(aes(x = date, y = dsr_admitted, group = factor(urban_rural_new), color = factor(urban_rural_new))) +
  # facet_wrap(~sex, labeller = labeller(sex = labels)) + 
  scale_colour_viridis_d(option = "magma", begin = 0.1, end = 0.9, labels = c("Rural", "Urban")) +
  scale_fill_viridis_d(option = "magma", begin = 0.1, end = 0.9, labels = c("Rural", "Urban")) +
  scale_x_date(date_labels = "%Y")+
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Location", fill = "Location") # Edit legend titles

# All avoidable admissions #

# Create measures
trends_urbrur2$dsr_acs_all_admitted <- (trends_urbrur2$dexp_admitted_acs_all / trends_urbrur2$std_pop) * 100000 # Direct rate
trends_urbrur2$dsr_acs_all_low <- trends_urbrur2$dsr_acs_all_admitted - (1.96 * (sqrt(trends_urbrur2$dsr_acs_all_admitted))) # Lower bound 
trends_urbrur2$dsr_acs_all_upp <- trends_urbrur2$dsr_acs_all_admitted + (1.96 * (sqrt(trends_urbrur2$dsr_acs_all_admitted))) # Upper bound 

# Plot
urb2_f <- ggplot(trends_urbrur2[!is.na(trends_urbrur2$urban_rural_new) & trends_urbrur2$sex == "F"],) +
  # Plot lockdown periods
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Plot main data
  geom_ribbon(aes(x = date, ymin = dsr_acs_all_low, ymax = dsr_acs_all_upp, fill = factor(urban_rural_new)), alpha = 0.2) +
  geom_line(aes(x = date, y = dsr_acs_all_admitted, group = factor(urban_rural_new), color = factor(urban_rural_new))) +
  # facet_wrap(~sex, labeller = labeller(sex = labels)) + 
  scale_colour_viridis_d(option = "magma", begin = 0.1, end = 0.9, labels = c("Rural", "Urban")) +
  scale_fill_viridis_d(option = "magma", begin = 0.1, end = 0.9, labels = c("Rural", "Urban")) +
  scale_x_date(date_labels = "%Y")+
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Location", fill = "Location") # Edit legend titles

urb2_m <- ggplot(trends_urbrur2[!is.na(trends_urbrur2$urban_rural_new) &trends_urbrur2$sex == "M"],) +
  # Plot lockdown periods
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Plot main data
  geom_ribbon(aes(x = date, ymin = dsr_acs_all_low, ymax = dsr_acs_all_upp, fill = factor(urban_rural_new)), alpha = 0.2) +
  geom_line(aes(x = date, y = dsr_acs_all_admitted, group = factor(urban_rural_new), color = factor(urban_rural_new))) +
  # facet_wrap(~sex, labeller = labeller(sex = labels)) + 
  scale_colour_viridis_d(option = "magma", begin = 0.1, end = 0.9, labels = c("Rural", "Urban")) +
  scale_fill_viridis_d(option = "magma", begin = 0.1, end = 0.9, labels = c("Rural", "Urban")) +
  scale_x_date(date_labels = "%Y")+
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Location", fill = "Location") # Edit legend titles

# Acute avoidable hospital admissions #

# Create measures
trends_urbrur2$dsr_acs_acute_admitted <- (trends_urbrur2$dexp_admitted_acs_acute / trends_urbrur2$std_pop) * 100000 # Direct rate
trends_urbrur2$dsr_acs_acute_low <- trends_urbrur2$dsr_acs_acute_admitted - (1.96 * (sqrt(trends_urbrur2$dsr_acs_acute_admitted))) # Lower bound 
trends_urbrur2$dsr_acs_acute_upp <- trends_urbrur2$dsr_acs_acute_admitted + (1.96 * (sqrt(trends_urbrur2$dsr_acs_acute_admitted))) # Upper bound 

# Plot by sex
urb3_f <- ggplot(trends_urbrur2[!is.na(trends_urbrur2$urban_rural_new) & trends_urbrur2$sex == "F"],) +
  # Plot lockdown periods
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Plot main data
  geom_ribbon(aes(x = date, ymin = dsr_acs_acute_low, ymax = dsr_acs_acute_upp, fill = factor(urban_rural_new)), alpha = 0.2) +
  geom_line(aes(x = date, y = dsr_acs_acute_admitted, group = factor(urban_rural_new), color = factor(urban_rural_new))) +
  # facet_wrap(~sex, labeller = labeller(sex = labels)) + 
  scale_colour_viridis_d(option = "magma", begin = 0.1, end = 0.9, labels = c("Rural", "Urban")) +
  scale_fill_viridis_d(option = "magma", begin = 0.1, end = 0.9, labels = c("Rural", "Urban")) +
  scale_x_date(date_labels = "%Y")+
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Location", fill = "Location") # Edit legend titles

urb3_m <- ggplot(trends_urbrur2[!is.na(trends_urbrur2$urban_rural_new) & trends_urbrur2$sex == "M"],) +
  # Plot lockdown periods
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Plot main data
  geom_ribbon(aes(x = date, ymin = dsr_acs_acute_low, ymax = dsr_acs_acute_upp, fill = factor(urban_rural_new)), alpha = 0.2) +
  geom_line(aes(x = date, y = dsr_acs_acute_admitted, group = factor(urban_rural_new), color = factor(urban_rural_new))) +
  # facet_wrap(~sex, labeller = labeller(sex = labels)) + 
  scale_colour_viridis_d(option = "magma", begin = 0.1, end = 0.9, labels = c("Rural", "Urban")) +
  scale_fill_viridis_d(option = "magma", begin = 0.1, end = 0.9, labels = c("Rural", "Urban")) +
  scale_x_date(date_labels = "%Y")+
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Location", fill = "Location") # Edit legend titles

# Chronic avoidable hospital admissions #

# Create measures
trends_urbrur2$dsr_acs_chronic_admitted <- (trends_urbrur2$dexp_admitted_acs_chronic / trends_urbrur2$std_pop) * 100000 # Direct rate
trends_urbrur2$dsr_acs_chronic_low <- trends_urbrur2$dsr_acs_chronic_admitted - (1.96 * (sqrt(trends_urbrur2$dsr_acs_chronic_admitted))) # Lower bound 
trends_urbrur2$dsr_acs_chronic_upp <- trends_urbrur2$dsr_acs_chronic_admitted + (1.96 * (sqrt(trends_urbrur2$dsr_acs_chronic_admitted))) # Upper bound 

# Plot
urb4_f <- ggplot(trends_urbrur2[!is.na(trends_urbrur2$urban_rural_new) & trends_urbrur2$sex == "F"],) +
  # Plot lockdown periods
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Plot main data
  geom_ribbon(aes(x = date, ymin = dsr_acs_chronic_low, ymax = dsr_acs_chronic_upp, fill = factor(urban_rural_new)), alpha = 0.2) +
  geom_line(aes(x = date, y = dsr_acs_chronic_admitted, group = factor(urban_rural_new), color = factor(urban_rural_new))) +
  # facet_wrap(~sex, labeller = labeller(sex = labels)) + 
  scale_colour_viridis_d(option = "magma", begin = 0.1, end = 0.9, labels = c("Rural", "Urban")) +
  scale_fill_viridis_d(option = "magma", begin = 0.1, end = 0.9, labels = c("Rural", "Urban")) +
  scale_x_date(date_labels = "%Y")+
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Location", fill = "Location") # Edit legend titles

urb4_m <- ggplot(trends_urbrur2[!is.na(trends_urbrur2$urban_rural_new) & trends_urbrur2$sex == "M"],) +
  # Plot lockdown periods
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Plot main data
  geom_ribbon(aes(x = date, ymin = dsr_acs_chronic_low, ymax = dsr_acs_chronic_upp, fill = factor(urban_rural_new)), alpha = 0.2) +
  geom_line(aes(x = date, y = dsr_acs_chronic_admitted, group = factor(urban_rural_new), color = factor(urban_rural_new))) +
  # facet_wrap(~sex, labeller = labeller(sex = labels)) + 
  scale_colour_viridis_d(option = "magma", begin = 0.1, end = 0.9, labels = c("Rural", "Urban")) +
  scale_fill_viridis_d(option = "magma", begin = 0.1, end = 0.9, labels = c("Rural", "Urban")) +
  scale_x_date(date_labels = "%Y")+
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Location", fill = "Location") # Edit legend titles

# Vaccine preventable admissions #

# Create measures
trends_urbrur2$dsr_acs_vaccine_admitted <- (trends_urbrur2$dexp_admitted_acs_vaccine / trends_urbrur2$std_pop) * 100000 # Direct rate
trends_urbrur2$dsr_acs_vaccine_low <- trends_urbrur2$dsr_acs_vaccine_admitted - (1.96 * (sqrt(trends_urbrur2$dsr_acs_vaccine_admitted))) # Lower bound 
trends_urbrur2$dsr_acs_vaccine_upp <- trends_urbrur2$dsr_acs_vaccine_admitted + (1.96 * (sqrt(trends_urbrur2$dsr_acs_vaccine_admitted))) # Upper bound 

# Plot by sex
urb5_f <- ggplot(trends_urbrur2[!is.na(trends_urbrur2$urban_rural_new) & trends_urbrur2$sex == "F"],) +
  # Plot lockdown periods
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Plot main data
  geom_ribbon(aes(x = date, ymin = dsr_acs_vaccine_low, ymax = dsr_acs_vaccine_upp, fill = factor(urban_rural_new)), alpha = 0.2) +
  geom_line(aes(x = date, y = dsr_acs_vaccine_admitted, group = factor(urban_rural_new), color = factor(urban_rural_new))) +
  # facet_wrap(~sex, labeller = labeller(sex = labels)) + 
  scale_colour_viridis_d(option = "magma", begin = 0.1, end = 0.9, labels = c("Rural", "Urban")) +
  scale_fill_viridis_d(option = "magma", begin = 0.1, end = 0.9, labels = c("Rural", "Urban")) +
  scale_x_date(date_labels = "%Y")+
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Location", fill = "Location") # Edit legend titles

urb5_m <- ggplot(trends_urbrur2[!is.na(trends_urbrur2$urban_rural_new) & trends_urbrur2$sex == "M"],) +
  # Plot lockdown periods
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Plot main data
  geom_ribbon(aes(x = date, ymin = dsr_acs_vaccine_low, ymax = dsr_acs_vaccine_upp, fill = factor(urban_rural_new)), alpha = 0.2) +
  geom_line(aes(x = date, y = dsr_acs_vaccine_admitted, group = factor(urban_rural_new), color = factor(urban_rural_new))) +
  # facet_wrap(~sex, labeller = labeller(sex = labels)) + 
  scale_colour_viridis_d(option = "magma", begin = 0.1, end = 0.9, labels = c("Rural", "Urban")) +
  scale_fill_viridis_d(option = "magma", begin = 0.1, end = 0.9, labels = c("Rural", "Urban")) +
  scale_x_date(date_labels = "%Y")+
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Location", fill = "Location") # Edit legend titles

# Emergency Urgent Sensitive conditions #

# Create measures
trends_urbrur2$dsr_eucs_admitted <- (trends_urbrur2$dexp_admitted_eucs / trends_urbrur2$std_pop) * 100000 # Direct rate
trends_urbrur2$dsr_eucs_low <- trends_urbrur2$dsr_eucs_admitted - (1.96 * (sqrt(trends_urbrur2$dsr_eucs_admitted))) # Lower bound 
trends_urbrur2$dsr_eucs_upp <- trends_urbrur2$dsr_eucs_admitted + (1.96 * (sqrt(trends_urbrur2$dsr_eucs_admitted))) # Upper bound 

# Plot by sex
urb6_f <- ggplot(trends_urbrur2[!is.na(trends_urbrur2$urban_rural_new) & trends_urbrur2$sex == "F"],) +
  # Plot lockdown periods
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Plot main data
  geom_ribbon(aes(x = date, ymin = dsr_eucs_low, ymax = dsr_eucs_upp, fill = factor(urban_rural_new)), alpha = 0.2) +
  geom_line(aes(x = date, y = dsr_eucs_admitted, group = factor(urban_rural_new), color = factor(urban_rural_new))) +
  # facet_wrap(~sex, labeller = labeller(sex = labels)) + 
  scale_colour_viridis_d(option = "magma", begin = 0.1, end = 0.9, labels = c("Rural", "Urban")) +
  scale_fill_viridis_d(option = "magma", begin = 0.1, end = 0.9, labels = c("Rural", "Urban")) +
  scale_x_date(date_labels = "%Y")+
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Location", fill = "Location") # Edit legend titles

urb6_m <- ggplot(trends_urbrur2[!is.na(trends_urbrur2$urban_rural_new) & trends_urbrur2$sex == "M"],) +
  # Plot lockdown periods
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Plot main data
  geom_ribbon(aes(x = date, ymin = dsr_eucs_low, ymax = dsr_eucs_upp, fill = factor(urban_rural_new)), alpha = 0.2) +
  geom_line(aes(x = date, y = dsr_eucs_admitted, group = factor(urban_rural_new), color = factor(urban_rural_new))) +
  # facet_wrap(~sex, labeller = labeller(sex = labels)) + 
  scale_colour_viridis_d(option = "magma", begin = 0.1, end = 0.9, labels = c("Rural", "Urban")) +
  scale_fill_viridis_d(option = "magma", begin = 0.1, end = 0.9, labels = c("Rural", "Urban")) +
  scale_x_date(date_labels = "%Y")+
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Location", fill = "Location") # Edit legend titles

# Combine into a single plot (seperate by sex)
overall_urb_plot_f <- urb1_f + urb2_f + urb3_f + urb4_f + urb5_f + urb6_f + # Combine these plots
  plot_annotation(tag_levels = 'A') + # Give plot labels
  plot_annotation(title = "Trends in female hospital admissions by location",
                  subtitle = "Directly age-standardised admission rates per 100,000 population",
                  caption = "A = Total emergency hospital admissions, B = All ambulatory care admissions, \nC = Acute ambulatory care admissions, D = Chronic ambulatory care admissions, \nE = Vaccine-preventable ambulatory care admissions, F = Emergency urgent care sensitive conditions.") + # Add title and descriptions
  plot_layout(guides = "collect") & theme(legend.position = "bottom") # Use same legend and place at bottom
overall_urb_plot_f
ggsave(overall_urb_plot_f, filename = "../output/plots/urbrur_trends_f.jpeg")

overall_urb_plot_m <- urb1_m + urb2_m + urb3_m + urb4_m + urb5_m + urb6_m + # Combine these plots
  plot_annotation(tag_levels = 'A') + # Give plot labels
  plot_annotation(title = "Trends in male hospital admissions by location",
                  subtitle = "Directly age-standardised admission rates per 100,000 population",
                  caption = "A = Total emergency hospital admissions, B = All ambulatory care admissions, \nC = Acute ambulatory care admissions, D = Chronic ambulatory care admissions, \nE = Vaccine-preventable ambulatory care admissions, F = Emergency urgent care sensitive conditions.") + # Add title and descriptions
  plot_layout(guides = "collect") & theme(legend.position = "bottom") # Use same legend and place at bottom
overall_urb_plot_m
ggsave(overall_urb_plot_m, filename = "../output/plots/urbrur_trends_m.jpeg")


## 2c. Differences by urban/rural locations ##

# Overall hospital admissions #

# Estimate absolute inequalities
test <- dcast(trends_urbrur2, date ~ sex + urban_rural_new, value.var = "dsr_admitted")
test$Female <- test$F_Urban - test$F_Rural
test$Male <- test$M_Urban - test$M_Rural
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
urb_abs_1 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "magma", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Rates per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Estimate relative inequalities
test$Female <- test$F_Urban / test$F_Rural
test$Male <- test$M_Urban / test$M_Rural
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
urb_rel_1 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = -Inf, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = -Inf, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = -Inf, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "magma", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Ratio") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles


# All avoidable admissions #

# Estimate absolute inequalities
test <- dcast(trends_urbrur2, date ~ sex + urban_rural_new, value.var = "dsr_acs_all_admitted")
test$Female <- test$F_Urban - test$F_Rural
test$Male <- test$M_Urban - test$M_Rural
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
urb_abs_2 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "magma", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Rates per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Estimate relative inequalities
test$Female <- test$F_Urban / test$F_Rural
test$Male <- test$M_Urban / test$M_Rural
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
urb_rel_2 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = -Inf, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = -Inf, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = -Inf, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "magma", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Ratio") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Acute avoidable admissions #

# Estimate absolute inequalities
test <- dcast(trends_urbrur2, date ~ sex + urban_rural_new, value.var = "dsr_acs_acute_admitted")
test$Female <- test$F_Urban - test$F_Rural
test$Male <- test$M_Urban - test$M_Rural
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
urb_abs_3 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = -Inf, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = -Inf, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = -Inf, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "magma", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Rates per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Estimate relative inequalities
test$Female <- test$F_Urban / test$F_Rural
test$Male <- test$M_Urban / test$M_Rural
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
urb_rel_3 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = -Inf, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = -Inf, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = -Inf, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "magma", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Ratio") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Chronic avoidable admissions #

# Estimate absolute inequalities
test <- dcast(trends_urbrur2, date ~ sex + urban_rural_new, value.var = "dsr_acs_chronic_admitted")
test$Female <- test$F_Urban - test$F_Rural
test$Male <- test$M_Urban - test$M_Rural
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
urb_abs_4 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "magma", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Rates per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Estimate relative inequalities
test$Female <- test$F_Urban / test$F_Rural
test$Male <- test$M_Urban / test$M_Rural
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
urb_rel_4 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = -Inf, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = -Inf, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = -Inf, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "magma", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Ratio") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Vaccine preventable avoidable admissions #

# Estimate absolute inequalities
test <- dcast(trends_urbrur2, date ~ sex + urban_rural_new, value.var = "dsr_acs_vaccine_admitted")
test$Female <- test$F_Urban - test$F_Rural
test$Male <- test$M_Urban - test$M_Rural
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
urb_abs_5 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "magma", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Rates per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Estimate relative inequalities
test$Female <- test$F_Urban / test$F_Rural
test$Male <- test$M_Urban / test$M_Rural
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
urb_rel_5 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = -Inf, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = -Inf, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = -Inf, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "magma", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Ratio") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Emergency Urgent Sensitive #

# Estimate absolute inequalities
test <- dcast(trends_urbrur2, date ~ sex + urban_rural_new, value.var = "dsr_eucs_admitted")
test$Female <- test$F_Urban - test$F_Rural
test$Male <- test$M_Urban - test$M_Rural
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
urb_abs_6 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "magma", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Rates per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Estimate relative inequalities
test$Female <- test$F_Urban / test$F_Rural
test$Male <- test$M_Urban / test$M_Rural
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
urb_rel_6 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = -Inf, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = -Inf, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = -Inf, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "magma", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Ratio") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Combine into a single plot 
overall_abs_plot <- urb_abs_1 + urb_abs_2 + urb_abs_3 + urb_abs_4 + urb_abs_5 + urb_abs_6 + # Combine these plots
  plot_annotation(tag_levels = 'A') + # Give plot labels
  plot_annotation(title = "Trends in hospital admissions by location",
                  subtitle = "Absolute difference in max/min rates (3 month moving average)",
                  caption = "A = Total emergency hospital admissions, B = All ambulatory care admissions, \nC = Acute ambulatory care admissions, D = Chronic ambulatory care admissions, \nE = Vaccine-preventable ambulatory care admissions, F = Emergency urgent care sensitive conditions.") + # Add title and descriptions
  plot_layout(guides = "collect") & theme(legend.position = "bottom") # Use same legend and place at bottom
overall_abs_plot
ggsave(overall_abs_plot, filename = "../output/plots/urbrur_abs.jpeg")

overall_rel_plot <- urb_rel_1 + urb_rel_2 + urb_rel_3 + urb_rel_4 + urb_rel_5 + urb_rel_6 + # Combine these plots
  plot_annotation(tag_levels = 'A') + # Give plot labels
  plot_annotation(title = "Trends in hospital admissions by location",
                  subtitle = "Relative difference in max/min rates (3 month moving average)",
                  caption = "A = Total emergency hospital admissions, B = All ambulatory care admissions, \nC = Acute ambulatory care admissions, D = Chronic ambulatory care admissions, \nE = Vaccine-preventable ambulatory care admissions, F = Emergency urgent care sensitive conditions.") + # Add title and descriptions
  plot_layout(guides = "collect") & theme(legend.position = "bottom") # Use same legend and place at bottom
overall_rel_plot
ggsave(overall_rel_plot, filename = "../output/plots/urbrur_rel.jpeg")


