############################################
### Trends in avoidable hospitalisations ###
######## Part 4: Regional patterns #########
############################################

# Purpose: To create a series of descriptive time series plots to visualise and describe how each of our outcome measures have changed between January 2019 and April 2022. The analyses here focus on trends by English region. 

# Libraries
library(ggplot2)
library(viridis)
library(dplyr)
library(data.table)
library(patchwork)
library(tidyquant)

## 1. Load and tidy data ##

# Load data
trends_region <- fread("../output/measures/standardised_region_trends.csv") # Load
trends_region <- trends_region[trends_region$date < "2022-04-01"] # Drop last period

# Convert to date format
trends_region$date <- as.Date(trends_region$date)

# Shift dates to mid-point of month since 1st of month refers to whole month (akes plots nicer to look at)
trends_region$date <- trends_region$date + 14

## 2. Plot trends ##

# Define labels for sex (used in plots)
labels <- c("Females", "Males")
names(labels) <- c("F", "M")


## 2a. Regional patterns ##

# All hospital admissions #

# Create measures
trends_region$dsr_admitted <- (trends_region$dexp_admitted / trends_region$std_pop) * 100000 # Direct rate
trends_region$dsr_admitted_low <- trends_region$dsr_admitted - (1.96 * (sqrt(trends_region$dsr_admitted))) # Lower bound 
trends_region$dsr_admitted_upp <- trends_region$dsr_admitted + (1.96 * (sqrt(trends_region$dsr_admitted))) # Upper bound 

# Plot
reg1_f <- ggplot(trends_region[!is.na(trends_region$region) & trends_region$sex == "F"],) +
  # Plot lockdown periods
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Plot main data
  geom_ribbon(aes(x = date, ymin = dsr_admitted_low, ymax = dsr_admitted_upp, fill = factor(region)), alpha = 0.2) +
  geom_line(aes(x = date, y = dsr_admitted, group = factor(region), color = factor(region))) +
  # facet_wrap(~sex, labeller = labeller(sex = labels)) + 
  scale_colour_viridis_d(option = "turbo", labels = c("East", "East Midlands", "London", "North East", "North West", "South East", "South West", "West Midlands", "Yorkshire")) +
  scale_fill_viridis_d(option = "turbo", labels = c("East", "East Midlands", "London", "North East", "North West", "South East", "South West", "West Midlands", "Yorkshire")) +
  scale_x_date(date_labels = "%Y")+
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Region", fill = "Region") # Edit legend titles

reg1_m <- ggplot(trends_region[!is.na(trends_region$region) & trends_region$sex == "M"],) +
  # Plot lockdown periods
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Plot main data
  geom_ribbon(aes(x = date, ymin = dsr_admitted_low, ymax = dsr_admitted_upp, fill = factor(region)), alpha = 0.2) +
  geom_line(aes(x = date, y = dsr_admitted, group = factor(region), color = factor(region))) +
  # facet_wrap(~sex, labeller = labeller(sex = labels)) + 
  scale_colour_viridis_d(option = "turbo", labels = c("East", "East Midlands", "London", "North East", "North West", "South East", "South West", "West Midlands", "Yorkshire")) +
  scale_fill_viridis_d(option = "turbo", labels = c("East", "East Midlands", "London", "North East", "North West", "South East", "South West", "West Midlands", "Yorkshire")) +
  scale_x_date(date_labels = "%Y")+
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Region", fill = "Region") # Edit legend titles

# All avoidable admissions #

# Create measures
trends_region$dsr_acs_all_admitted <- (trends_region$dexp_admitted_acs_all / trends_region$std_pop) * 100000 # Direct rate
trends_region$dsr_acs_all_low <- trends_region$dsr_acs_all_admitted - (1.96 * (sqrt(trends_region$dsr_acs_all_admitted))) # Lower bound 
trends_region$dsr_acs_all_upp <- trends_region$dsr_acs_all_admitted + (1.96 * (sqrt(trends_region$dsr_acs_all_admitted))) # Upper bound 

# Plot
reg2_f <- ggplot(trends_region[!is.na(trends_region$region) & trends_region$sex == "F"],) +
  # Plot lockdown periods
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Plot main data
  geom_ribbon(aes(x = date, ymin = dsr_acs_all_low, ymax = dsr_acs_all_upp, fill = factor(region)), alpha = 0.2) +
  geom_line(aes(x = date, y = dsr_acs_all_admitted, group = factor(region), color = factor(region))) +
  # facet_wrap(~sex, labeller = labeller(sex = labels)) + 
  scale_colour_viridis_d(option = "turbo", labels = c("East", "East Midlands", "London", "North East", "North West", "South East", "South West", "West Midlands", "Yorkshire")) +
  scale_fill_viridis_d(option = "turbo", labels = c("East", "East Midlands", "London", "North East", "North West", "South East", "South West", "West Midlands", "Yorkshire")) +
  scale_x_date(date_labels = "%Y")+
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Region", fill = "Region") # Edit legend titles

reg2_m <- ggplot(trends_region[!is.na(trends_region$region) &trends_region$sex == "M"],) +
  # Plot lockdown periods
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Plot main data
  geom_ribbon(aes(x = date, ymin = dsr_acs_all_low, ymax = dsr_acs_all_upp, fill = factor(region)), alpha = 0.2) +
  geom_line(aes(x = date, y = dsr_acs_all_admitted, group = factor(region), color = factor(region))) +
  # facet_wrap(~sex, labeller = labeller(sex = labels)) + 
  scale_colour_viridis_d(option = "turbo", labels = c("East", "East Midlands", "London", "North East", "North West", "South East", "South West", "West Midlands", "Yorkshire")) +
  scale_fill_viridis_d(option = "turbo", labels = c("East", "East Midlands", "London", "North East", "North West", "South East", "South West", "West Midlands", "Yorkshire")) +
  scale_x_date(date_labels = "%Y")+
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Region", fill = "Region") # Edit legend titles

# Acute avoidable hospital admissions #

# Create measures
trends_region$dsr_acs_acute_admitted <- (trends_region$dexp_admitted_acs_acute / trends_region$std_pop) * 100000 # Direct rate
trends_region$dsr_acs_acute_low <- trends_region$dsr_acs_acute_admitted - (1.96 * (sqrt(trends_region$dsr_acs_acute_admitted))) # Lower bound 
trends_region$dsr_acs_acute_upp <- trends_region$dsr_acs_acute_admitted + (1.96 * (sqrt(trends_region$dsr_acs_acute_admitted))) # Upper bound 

# Plot by sex
reg3_f <- ggplot(trends_region[!is.na(trends_region$region) & trends_region$sex == "F"],) +
  # Plot lockdown periods
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Plot main data
  geom_ribbon(aes(x = date, ymin = dsr_acs_acute_low, ymax = dsr_acs_acute_upp, fill = factor(region)), alpha = 0.2) +
  geom_line(aes(x = date, y = dsr_acs_acute_admitted, group = factor(region), color = factor(region))) +
  # facet_wrap(~sex, labeller = labeller(sex = labels)) + 
  scale_colour_viridis_d(option = "turbo", labels = c("East", "East Midlands", "London", "North East", "North West", "South East", "South West", "West Midlands", "Yorkshire")) +
  scale_fill_viridis_d(option = "turbo", labels = c("East", "East Midlands", "London", "North East", "North West", "South East", "South West", "West Midlands", "Yorkshire")) +
  scale_x_date(date_labels = "%Y")+
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Region", fill = "Region") # Edit legend titles

reg3_m <- ggplot(trends_region[!is.na(trends_region$region) & trends_region$sex == "M"],) +
  # Plot lockdown periods
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Plot main data
  geom_ribbon(aes(x = date, ymin = dsr_acs_acute_low, ymax = dsr_acs_acute_upp, fill = factor(region)), alpha = 0.2) +
  geom_line(aes(x = date, y = dsr_acs_acute_admitted, group = factor(region), color = factor(region))) +
  # facet_wrap(~sex, labeller = labeller(sex = labels)) + 
  scale_colour_viridis_d(option = "turbo", labels = c("East", "East Midlands", "London", "North East", "North West", "South East", "South West", "West Midlands", "Yorkshire")) +
  scale_fill_viridis_d(option = "turbo", labels = c("East", "East Midlands", "London", "North East", "North West", "South East", "South West", "West Midlands", "Yorkshire")) +
  scale_x_date(date_labels = "%Y")+
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Region", fill = "Region") # Edit legend titles

# Chronic avoidable hospital admissions #

# Create measures
trends_region$dsr_acs_chronic_admitted <- (trends_region$dexp_admitted_acs_chronic / trends_region$std_pop) * 100000 # Direct rate
trends_region$dsr_acs_chronic_low <- trends_region$dsr_acs_chronic_admitted - (1.96 * (sqrt(trends_region$dsr_acs_chronic_admitted))) # Lower bound 
trends_region$dsr_acs_chronic_upp <- trends_region$dsr_acs_chronic_admitted + (1.96 * (sqrt(trends_region$dsr_acs_chronic_admitted))) # Upper bound 

# Plot
reg4_f <- ggplot(trends_region[!is.na(trends_region$region) & trends_region$sex == "F"],) +
  # Plot lockdown periods
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Plot main data
  geom_ribbon(aes(x = date, ymin = dsr_acs_chronic_low, ymax = dsr_acs_chronic_upp, fill = factor(region)), alpha = 0.2) +
  geom_line(aes(x = date, y = dsr_acs_chronic_admitted, group = factor(region), color = factor(region))) +
  # facet_wrap(~sex, labeller = labeller(sex = labels)) + 
  scale_colour_viridis_d(option = "turbo", labels = c("East", "East Midlands", "London", "North East", "North West", "South East", "South West", "West Midlands", "Yorkshire")) +
  scale_fill_viridis_d(option = "turbo", labels = c("East", "East Midlands", "London", "North East", "North West", "South East", "South West", "West Midlands", "Yorkshire")) +
  scale_x_date(date_labels = "%Y")+
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Region", fill = "Region") # Edit legend titles

reg4_m <- ggplot(trends_region[!is.na(trends_region$region) & trends_region$sex == "M"],) +
  # Plot lockdown periods
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Plot main data
  geom_ribbon(aes(x = date, ymin = dsr_acs_chronic_low, ymax = dsr_acs_chronic_upp, fill = factor(region)), alpha = 0.2) +
  geom_line(aes(x = date, y = dsr_acs_chronic_admitted, group = factor(region), color = factor(region))) +
  # facet_wrap(~sex, labeller = labeller(sex = labels)) + 
  scale_colour_viridis_d(option = "turbo", labels = c("East", "East Midlands", "London", "North East", "North West", "South East", "South West", "West Midlands", "Yorkshire")) +
  scale_fill_viridis_d(option = "turbo", labels = c("East", "East Midlands", "London", "North East", "North West", "South East", "South West", "West Midlands", "Yorkshire")) +
  scale_x_date(date_labels = "%Y")+
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Region", fill = "Region") # Edit legend titles

# Vaccine preventable admissions #

# Create measures
trends_region$dsr_acs_vaccine_admitted <- (trends_region$dexp_admitted_acs_vaccine / trends_region$std_pop) * 100000 # Direct rate
trends_region$dsr_acs_vaccine_low <- trends_region$dsr_acs_vaccine_admitted - (1.96 * (sqrt(trends_region$dsr_acs_vaccine_admitted))) # Lower bound 
trends_region$dsr_acs_vaccine_upp <- trends_region$dsr_acs_vaccine_admitted + (1.96 * (sqrt(trends_region$dsr_acs_vaccine_admitted))) # Upper bound 

# Plot by sex
reg5_f <- ggplot(trends_region[!is.na(trends_region$region) & trends_region$sex == "F"],) +
  # Plot lockdown periods
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Plot main data
  geom_ribbon(aes(x = date, ymin = dsr_acs_vaccine_low, ymax = dsr_acs_vaccine_upp, fill = factor(region)), alpha = 0.2) +
  geom_line(aes(x = date, y = dsr_acs_vaccine_admitted, group = factor(region), color = factor(region))) +
  # facet_wrap(~sex, labeller = labeller(sex = labels)) + 
  scale_colour_viridis_d(option = "turbo", labels = c("East", "East Midlands", "London", "North East", "North West", "South East", "South West", "West Midlands", "Yorkshire")) +
  scale_fill_viridis_d(option = "turbo", labels = c("East", "East Midlands", "London", "North East", "North West", "South East", "South West", "West Midlands", "Yorkshire")) +
  scale_x_date(date_labels = "%Y")+
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Region", fill = "Region") # Edit legend titles

reg5_m <- ggplot(trends_region[!is.na(trends_region$region) & trends_region$sex == "M"],) +
  # Plot lockdown periods
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Plot main data
  geom_ribbon(aes(x = date, ymin = dsr_acs_vaccine_low, ymax = dsr_acs_vaccine_upp, fill = factor(region)), alpha = 0.2) +
  geom_line(aes(x = date, y = dsr_acs_vaccine_admitted, group = factor(region), color = factor(region))) +
  # facet_wrap(~sex, labeller = labeller(sex = labels)) + 
  scale_colour_viridis_d(option = "turbo", labels = c("East", "East Midlands", "London", "North East", "North West", "South East", "South West", "West Midlands", "Yorkshire")) +
  scale_fill_viridis_d(option = "turbo", labels = c("East", "East Midlands", "London", "North East", "North West", "South East", "South West", "West Midlands", "Yorkshire")) +
  scale_x_date(date_labels = "%Y")+
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Region", fill = "Region") # Edit legend titles

# Emergency Urgent Sensitive conditions #

# Create measures
trends_region$dsr_eucs_admitted <- (trends_region$dexp_admitted_eucs / trends_region$std_pop) * 100000 # Direct rate
trends_region$dsr_eucs_low <- trends_region$dsr_eucs_admitted - (1.96 * (sqrt(trends_region$dsr_eucs_admitted))) # Lower bound 
trends_region$dsr_eucs_upp <- trends_region$dsr_eucs_admitted + (1.96 * (sqrt(trends_region$dsr_eucs_admitted))) # Upper bound 

# Plot by sex
reg6_f <- ggplot(trends_region[!is.na(trends_region$region) & trends_region$sex == "F"],) +
  # Plot lockdown periods
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Plot main data
  geom_ribbon(aes(x = date, ymin = dsr_eucs_low, ymax = dsr_eucs_upp, fill = factor(region)), alpha = 0.2) +
  geom_line(aes(x = date, y = dsr_eucs_admitted, group = factor(region), color = factor(region))) +
  # facet_wrap(~sex, labeller = labeller(sex = labels)) + 
  scale_colour_viridis_d(option = "turbo", labels = c("East", "East Midlands", "London", "North East", "North West", "South East", "South West", "West Midlands", "Yorkshire")) +
  scale_fill_viridis_d(option = "turbo", labels = c("East", "East Midlands", "London", "North East", "North West", "South East", "South West", "West Midlands", "Yorkshire")) +
  scale_x_date(date_labels = "%Y")+
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Region", fill = "Region") # Edit legend titles

reg6_m <- ggplot(trends_region[!is.na(trends_region$region) & trends_region$sex == "M"],) +
  # Plot lockdown periods
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Plot main data
  geom_ribbon(aes(x = date, ymin = dsr_eucs_low, ymax = dsr_eucs_upp, fill = factor(region)), alpha = 0.2) +
  geom_line(aes(x = date, y = dsr_eucs_admitted, group = factor(region), color = factor(region))) +
  # facet_wrap(~sex, labeller = labeller(sex = labels)) + 
  scale_colour_viridis_d(option = "turbo", labels = c("East", "East Midlands", "London", "North East", "North West", "South East", "South West", "West Midlands", "Yorkshire")) +
  scale_fill_viridis_d(option = "turbo", labels = c("East", "East Midlands", "London", "North East", "North West", "South East", "South West", "West Midlands", "Yorkshire")) +
  scale_x_date(date_labels = "%Y")+
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Region", fill = "Region") # Edit legend titles

# Combine into a single plot (seperate by sex)
overall_reg_plot_f <- reg1_f + reg2_f + reg3_f + reg4_f + reg5_f + reg6_f + # Combine these plots
  plot_annotation(tag_levels = 'A') + # Give plot labels
  plot_annotation(title = "Trends in female hospital admissions by region",
                  subtitle = "Directly age-standardised admission rates per 100,000 population",
                  caption = "A = Total emergency hospital admissions, B = All ambulatory care admissions, \nC = Acute ambulatory care admissions, D = Chronic ambulatory care admissions, \nE = Vaccine-preventable ambulatory care admissions, F = Emergency urgent care sensitive conditions.") + # Add title and descriptions
  plot_layout(guides = "collect") & theme(legend.position = "bottom") # Use same legend and place at bottom
overall_reg_plot_f
ggsave(overall_reg_plot_f, filename = "../output/plots/region_trends_f.jpeg")

overall_reg_plot_m <- reg1_m + reg2_m + reg3_m + reg4_m + reg5_m + reg6_m + # Combine these plots
  plot_annotation(tag_levels = 'A') + # Give plot labels
  plot_annotation(title = "Trends in male hospital admissions by region",
                  subtitle = "Directly age-standardised admission rates per 100,000 population",
                  caption = "A = Total emergency hospital admissions, B = All ambulatory care admissions, \nC = Acute ambulatory care admissions, D = Chronic ambulatory care admissions, \nE = Vaccine-preventable ambulatory care admissions, F = Emergency urgent care sensitive conditions.") + # Add title and descriptions
  plot_layout(guides = "collect") & theme(legend.position = "bottom") # Use same legend and place at bottom
overall_reg_plot_m
ggsave(overall_reg_plot_m, filename = "../output/plots/region_trends_m.jpeg")



## 2b. Differences between regions ##

# Overall hospital admissions #

# Estimate absolute inequalities
test <- dcast(trends_region, date ~ sex + region, value.var = "dsr_admitted")
test$Female <- pmax(test$F_East, test$`F_East Midlands`, test$F_London, test$`F_North East`, test$`F_North West`, test$`F_South East`, test$`F_South West`, test$`F_West Midlands`, test$`F_Yorkshire and The Humber`, na.rm = TRUE) - pmin(test$F_East, test$`F_East Midlands`, test$F_London, test$`F_North East`, test$`F_North West`, test$`F_South East`, test$`F_South West`, test$`F_West Midlands`, test$`F_Yorkshire and The Humber`, na.rm = TRUE)
test$Male <- pmax(test$M_East, test$`M_East Midlands`, test$M_London, test$`M_North East`, test$`M_North West`, test$`M_South East`, test$`M_South West`, test$`M_West Midlands`, test$`M_Yorkshire and The Humber`, na.rm = TRUE) - pmin(test$M_East, test$`M_East Midlands`, test$M_London, test$`M_North East`, test$`M_North West`, test$`M_South East`, test$`M_South West`, test$`M_West Midlands`, test$`M_Yorkshire and The Humber`, na.rm = TRUE)
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
reg_abs_1 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Rates per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Estimate relative inequalities
test$Female <- pmax(test$F_East, test$`F_East Midlands`, test$F_London, test$`F_North East`, test$`F_North West`, test$`F_South East`, test$`F_South West`, test$`F_West Midlands`, test$`F_Yorkshire and The Humber`, na.rm = TRUE) / pmin(test$F_East, test$`F_East Midlands`, test$F_London, test$`F_North East`, test$`F_North West`, test$`F_South East`, test$`F_South West`, test$`F_West Midlands`, test$`F_Yorkshire and The Humber`, na.rm = TRUE)
test$Male <- pmax(test$M_East, test$`M_East Midlands`, test$M_London, test$`M_North East`, test$`M_North West`, test$`M_South East`, test$`M_South West`, test$`M_West Midlands`, test$`M_Yorkshire and The Humber`, na.rm = TRUE) / pmin(test$M_East, test$`M_East Midlands`, test$M_London, test$`M_North East`, test$`M_North West`, test$`M_South East`, test$`M_South West`, test$`M_West Midlands`, test$`M_Yorkshire and The Humber`, na.rm = TRUE)
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))


# Plot
reg_rel_1 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Ratio") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles


# All avoidable admissions #

# Estimate absolute inequalities
test <- dcast(trends_region, date ~ sex + region, value.var = "dsr_acs_all_admitted")
test$Female <- pmax(test$F_East, test$`F_East Midlands`, test$F_London, test$`F_North East`, test$`F_North West`, test$`F_South East`, test$`F_South West`, test$`F_West Midlands`, test$`F_Yorkshire and The Humber`, na.rm = TRUE) - pmin(test$F_East, test$`F_East Midlands`, test$F_London, test$`F_North East`, test$`F_North West`, test$`F_South East`, test$`F_South West`, test$`F_West Midlands`, test$`F_Yorkshire and The Humber`, na.rm = TRUE)
test$Male <- pmax(test$M_East, test$`M_East Midlands`, test$M_London, test$`M_North East`, test$`M_North West`, test$`M_South East`, test$`M_South West`, test$`M_West Midlands`, test$`M_Yorkshire and The Humber`, na.rm = TRUE) - pmin(test$M_East, test$`M_East Midlands`, test$M_London, test$`M_North East`, test$`M_North West`, test$`M_South East`, test$`M_South West`, test$`M_West Midlands`, test$`M_Yorkshire and The Humber`, na.rm = TRUE)
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
reg_abs_2 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Rates per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Estimate relative inequalities
test$Female <- pmax(test$F_East, test$`F_East Midlands`, test$F_London, test$`F_North East`, test$`F_North West`, test$`F_South East`, test$`F_South West`, test$`F_West Midlands`, test$`F_Yorkshire and The Humber`, na.rm = TRUE) / pmin(test$F_East, test$`F_East Midlands`, test$F_London, test$`F_North East`, test$`F_North West`, test$`F_South East`, test$`F_South West`, test$`F_West Midlands`, test$`F_Yorkshire and The Humber`, na.rm = TRUE)
test$Male <- pmax(test$M_East, test$`M_East Midlands`, test$M_London, test$`M_North East`, test$`M_North West`, test$`M_South East`, test$`M_South West`, test$`M_West Midlands`, test$`M_Yorkshire and The Humber`, na.rm = TRUE) / pmin(test$M_East, test$`M_East Midlands`, test$M_London, test$`M_North East`, test$`M_North West`, test$`M_South East`, test$`M_South West`, test$`M_West Midlands`, test$`M_Yorkshire and The Humber`, na.rm = TRUE)
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
reg_rel_2 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Ratio") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Acute avoidable admissions #

# Estimate absolute inequalities
test <- dcast(trends_region, date ~ sex + region, value.var = "dsr_acs_acute_admitted")
test$Female <- pmax(test$F_East, test$`F_East Midlands`, test$F_London, test$`F_North East`, test$`F_North West`, test$`F_South East`, test$`F_South West`, test$`F_West Midlands`, test$`F_Yorkshire and The Humber`, na.rm = TRUE) - pmin(test$F_East, test$`F_East Midlands`, test$F_London, test$`F_North East`, test$`F_North West`, test$`F_South East`, test$`F_South West`, test$`F_West Midlands`, test$`F_Yorkshire and The Humber`, na.rm = TRUE)
test$Male <- pmax(test$M_East, test$`M_East Midlands`, test$M_London, test$`M_North East`, test$`M_North West`, test$`M_South East`, test$`M_South West`, test$`M_West Midlands`, test$`M_Yorkshire and The Humber`, na.rm = TRUE) - pmin(test$M_East, test$`M_East Midlands`, test$M_London, test$`M_North East`, test$`M_North West`, test$`M_South East`, test$`M_South West`, test$`M_West Midlands`, test$`M_Yorkshire and The Humber`, na.rm = TRUE)
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
reg_abs_3 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Rates per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Estimate relative inequalities
test$Female <- pmax(test$F_East, test$`F_East Midlands`, test$F_London, test$`F_North East`, test$`F_North West`, test$`F_South East`, test$`F_South West`, test$`F_West Midlands`, test$`F_Yorkshire and The Humber`, na.rm = TRUE) / pmin(test$F_East, test$`F_East Midlands`, test$F_London, test$`F_North East`, test$`F_North West`, test$`F_South East`, test$`F_South West`, test$`F_West Midlands`, test$`F_Yorkshire and The Humber`, na.rm = TRUE)
test$Male <- pmax(test$M_East, test$`M_East Midlands`, test$M_London, test$`M_North East`, test$`M_North West`, test$`M_South East`, test$`M_South West`, test$`M_West Midlands`, test$`M_Yorkshire and The Humber`, na.rm = TRUE) / pmin(test$M_East, test$`M_East Midlands`, test$M_London, test$`M_North East`, test$`M_North West`, test$`M_South East`, test$`M_South West`, test$`M_West Midlands`, test$`M_Yorkshire and The Humber`, na.rm = TRUE)
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
reg_rel_3 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Ratio") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Chronic avoidable admissions #

# Estimate absolute inequalities
test <- dcast(trends_region, date ~ sex + region, value.var = "dsr_acs_chronic_admitted")
test$Female <- pmax(test$F_East, test$`F_East Midlands`, test$F_London, test$`F_North East`, test$`F_North West`, test$`F_South East`, test$`F_South West`, test$`F_West Midlands`, test$`F_Yorkshire and The Humber`, na.rm = TRUE) - pmin(test$F_East, test$`F_East Midlands`, test$F_London, test$`F_North East`, test$`F_North West`, test$`F_South East`, test$`F_South West`, test$`F_West Midlands`, test$`F_Yorkshire and The Humber`, na.rm = TRUE)
test$Male <- pmax(test$M_East, test$`M_East Midlands`, test$M_London, test$`M_North East`, test$`M_North West`, test$`M_South East`, test$`M_South West`, test$`M_West Midlands`, test$`M_Yorkshire and The Humber`, na.rm = TRUE) - pmin(test$M_East, test$`M_East Midlands`, test$M_London, test$`M_North East`, test$`M_North West`, test$`M_South East`, test$`M_South West`, test$`M_West Midlands`, test$`M_Yorkshire and The Humber`, na.rm = TRUE)
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
reg_abs_4 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Rates per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Estimate relative inequalities
test$Female <- pmax(test$F_East, test$`F_East Midlands`, test$F_London, test$`F_North East`, test$`F_North West`, test$`F_South East`, test$`F_South West`, test$`F_West Midlands`, test$`F_Yorkshire and The Humber`, na.rm = TRUE) / pmin(test$F_East, test$`F_East Midlands`, test$F_London, test$`F_North East`, test$`F_North West`, test$`F_South East`, test$`F_South West`, test$`F_West Midlands`, test$`F_Yorkshire and The Humber`, na.rm = TRUE)
test$Male <- pmax(test$M_East, test$`M_East Midlands`, test$M_London, test$`M_North East`, test$`M_North West`, test$`M_South East`, test$`M_South West`, test$`M_West Midlands`, test$`M_Yorkshire and The Humber`, na.rm = TRUE) / pmin(test$M_East, test$`M_East Midlands`, test$M_London, test$`M_North East`, test$`M_North West`, test$`M_South East`, test$`M_South West`, test$`M_West Midlands`, test$`M_Yorkshire and The Humber`, na.rm = TRUE)
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
reg_rel_4 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Ratio") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Vaccine preventable avoidable admissions #

# Estimate absolute inequalities
test <- dcast(trends_region, date ~ sex + region, value.var = "dsr_acs_vaccine_admitted")
test$Female <- pmax(test$F_East, test$`F_East Midlands`, test$F_London, test$`F_North East`, test$`F_North West`, test$`F_South East`, test$`F_South West`, test$`F_West Midlands`, test$`F_Yorkshire and The Humber`, na.rm = TRUE) - pmin(test$F_East, test$`F_East Midlands`, test$F_London, test$`F_North East`, test$`F_North West`, test$`F_South East`, test$`F_South West`, test$`F_West Midlands`, test$`F_Yorkshire and The Humber`, na.rm = TRUE)
test$Male <- pmax(test$M_East, test$`M_East Midlands`, test$M_London, test$`M_North East`, test$`M_North West`, test$`M_South East`, test$`M_South West`, test$`M_West Midlands`, test$`M_Yorkshire and The Humber`, na.rm = TRUE) - pmin(test$M_East, test$`M_East Midlands`, test$M_London, test$`M_North East`, test$`M_North West`, test$`M_South East`, test$`M_South West`, test$`M_West Midlands`, test$`M_Yorkshire and The Humber`, na.rm = TRUE)
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
reg_abs_5 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Rates per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Estimate relative inequalities
test$Female <- pmax(test$F_East, test$`F_East Midlands`, test$F_London, test$`F_North East`, test$`F_North West`, test$`F_South East`, test$`F_South West`, test$`F_West Midlands`, test$`F_Yorkshire and The Humber`, na.rm = TRUE) / pmin(test$F_East, test$`F_East Midlands`, test$F_London, test$`F_North East`, test$`F_North West`, test$`F_South East`, test$`F_South West`, test$`F_West Midlands`, test$`F_Yorkshire and The Humber`, na.rm = TRUE)
test$Male <- pmax(test$M_East, test$`M_East Midlands`, test$M_London, test$`M_North East`, test$`M_North West`, test$`M_South East`, test$`M_South West`, test$`M_West Midlands`, test$`M_Yorkshire and The Humber`, na.rm = TRUE) / pmin(test$M_East, test$`M_East Midlands`, test$M_London, test$`M_North East`, test$`M_North West`, test$`M_South East`, test$`M_South West`, test$`M_West Midlands`, test$`M_Yorkshire and The Humber`, na.rm = TRUE)
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
reg_rel_5 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Ratio") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Emergency Urgent Sensitive #

# Estimate absolute inequalities
test <- dcast(trends_region, date ~ sex + region, value.var = "dsr_eucs_admitted")
test$Female <- pmax(test$F_East, test$`F_East Midlands`, test$F_London, test$`F_North East`, test$`F_North West`, test$`F_South East`, test$`F_South West`, test$`F_West Midlands`, test$`F_Yorkshire and The Humber`, na.rm = TRUE) - pmin(test$F_East, test$`F_East Midlands`, test$F_London, test$`F_North East`, test$`F_North West`, test$`F_South East`, test$`F_South West`, test$`F_West Midlands`, test$`F_Yorkshire and The Humber`, na.rm = TRUE)
test$Male <- pmax(test$M_East, test$`M_East Midlands`, test$M_London, test$`M_North East`, test$`M_North West`, test$`M_South East`, test$`M_South West`, test$`M_West Midlands`, test$`M_Yorkshire and The Humber`, na.rm = TRUE) - pmin(test$M_East, test$`M_East Midlands`, test$M_London, test$`M_North East`, test$`M_North West`, test$`M_South East`, test$`M_South West`, test$`M_West Midlands`, test$`M_Yorkshire and The Humber`, na.rm = TRUE)
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
reg_abs_6 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Rates per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Estimate relative inequalities
test$Female <- pmax(test$F_East, test$`F_East Midlands`, test$F_London, test$`F_North East`, test$`F_North West`, test$`F_South East`, test$`F_South West`, test$`F_West Midlands`, test$`F_Yorkshire and The Humber`, na.rm = TRUE) / pmin(test$F_East, test$`F_East Midlands`, test$F_London, test$`F_North East`, test$`F_North West`, test$`F_South East`, test$`F_South West`, test$`F_West Midlands`, test$`F_Yorkshire and The Humber`, na.rm = TRUE)
test$Male <- pmax(test$M_East, test$`M_East Midlands`, test$M_London, test$`M_North East`, test$`M_North West`, test$`M_South East`, test$`M_South West`, test$`M_West Midlands`, test$`M_Yorkshire and The Humber`, na.rm = TRUE) / pmin(test$M_East, test$`M_East Midlands`, test$M_London, test$`M_North East`, test$`M_North West`, test$`M_South East`, test$`M_South West`, test$`M_West Midlands`, test$`M_Yorkshire and The Humber`, na.rm = TRUE)
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
reg_rel_6 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Ratio") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Combine into a single plot 
overall_abs_plot <- reg_abs_1 + reg_abs_2 + reg_abs_3 + reg_abs_4 + reg_abs_5 + reg_abs_6 + # Combine these plots
  plot_annotation(tag_levels = 'A') + # Give plot labels
  plot_annotation(title = "Trends in hospital admissions by region",
                  subtitle = "Absolute difference in max/min rates (3 month moving average)",
                  caption = "A = Total emergency hospital admissions, B = All ambulatory care admissions, \nC = Acute ambulatory care admissions, D = Chronic ambulatory care admissions, \nE = Vaccine-preventable ambulatory care admissions, F = Emergency urgent care sensitive conditions.") + # Add title and descriptions
  plot_layout(guides = "collect") & theme(legend.position = "bottom") # Use same legend and place at bottom
overall_abs_plot
ggsave(overall_abs_plot, filename = "../output/plots/region_abs.jpeg")
ggsave(overall_abs_plot, filename = "../output/plots/figure4.jpeg", dpi = 300)

overall_rel_plot <- reg_rel_1 + reg_rel_2 + reg_rel_3 + reg_rel_4 + reg_rel_5 + reg_rel_6 + # Combine these plots
  plot_annotation(tag_levels = 'A') + # Give plot labels
  plot_annotation(title = "Trends in hospital admissions by region",
                  subtitle = "Relative difference in max/min rates (3 month moving average)",
                  caption = "A = Total emergency hospital admissions, B = All ambulatory care admissions, \nC = Acute ambulatory care admissions, D = Chronic ambulatory care admissions, \nE = Vaccine-preventable ambulatory care admissions, F = Emergency urgent care sensitive conditions.") + # Add title and descriptions
  plot_layout(guides = "collect") & theme(legend.position = "bottom") # Use same legend and place at bottom
overall_rel_plot
ggsave(overall_rel_plot, filename = "../output/plots/region_rel.jpeg")
ggsave(overall_rel_plot, filename = "../output/plots/figure5.jpeg", dpi = 300)


## 2c. North vs South Divide ##

# Define north and south
trends_region$north_south <- NA
trends_region$north_south[trends_region$region == "North East" | trends_region$region == "North West" | trends_region$region == "Yorkshire and The Humber"] <- "North"
trends_region$north_south[trends_region$region == "South East" | trends_region$region == "South West" | trends_region$region == "London" | trends_region$region == "East"] <- "South"
# trends_region$north_south[trends_region$region == "East Midlands" | trends_region$region == "West Midlands"] <- "Midlands"
trends_region$north_south[trends_region$region == "East Midlands" | trends_region$region == "West Midlands"] <- "North"

# Aggregate data
trends_region2 <- trends_region[, list(dexp_admitted = sum(dexp_admitted, na.rm = T), dexp_admitted_acs_all = sum(dexp_admitted_acs_all, na.rm = T), dexp_admitted_acs_acute = sum(dexp_admitted_acs_acute, na.rm = T), dexp_admitted_acs_chronic = sum(dexp_admitted_acs_chronic, na.rm = T), dexp_admitted_acs_vaccine = sum(dexp_admitted_acs_vaccine, na.rm = T), dexp_admitted_eucs = sum(dexp_admitted_eucs, na.rm = T), std_pop = sum(std_pop, na.rm = T)), by = c("sex", "date", "north_south")]

# All hospital admissions #

# Create measures
trends_region2$dsr_admitted <- (trends_region2$dexp_admitted / trends_region2$std_pop) * 100000 # Direct rate
trends_region2$dsr_admitted_low <- trends_region2$dsr_admitted - (1.96 * (sqrt(trends_region2$dsr_admitted))) # Lower bound 
trends_region2$dsr_admitted_upp <- trends_region2$dsr_admitted + (1.96 * (sqrt(trends_region2$dsr_admitted))) # Upper bound 

# Plot
nor1_f <- ggplot(trends_region2[!is.na(trends_region2$north_south) & trends_region2$sex == "F"],) +
  # Plot lockdown periods
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Plot main data
  geom_ribbon(aes(x = date, ymin = dsr_admitted_low, ymax = dsr_admitted_upp, fill = factor(north_south)), alpha = 0.2) +
  geom_line(aes(x = date, y = dsr_admitted, group = factor(north_south), color = factor(north_south))) +
  # facet_wrap(~sex, labeller = labeller(sex = labels)) + 
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9, labels = c("North", "South")) +
  scale_fill_viridis_d(option = "turbo", begin = 0.1, end = 0.9, labels = c("North", "South")) +
  scale_x_date(date_labels = "%Y") +
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Region", fill = "Region") # Edit legend titles

nor1_m <- ggplot(trends_region2[!is.na(trends_region2$north_south) & trends_region2$sex == "M"],) +
  # Plot lockdown periods
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Plot main data
  geom_ribbon(aes(x = date, ymin = dsr_admitted_low, ymax = dsr_admitted_upp, fill = factor(north_south)), alpha = 0.2) +
  geom_line(aes(x = date, y = dsr_admitted, group = factor(north_south), color = factor(north_south))) +
  # facet_wrap(~sex, labeller = labeller(sex = labels)) + 
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9, labels = c("North", "South")) +
  scale_fill_viridis_d(option = "turbo", begin = 0.1, end = 0.9, labels = c("North", "South")) +
  scale_x_date(date_labels = "%Y") +
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Region", fill = "Region") # Edit legend titles

# All avoidable admissions #

# Create measures
trends_region2$dsr_acs_all_admitted <- (trends_region2$dexp_admitted_acs_all / trends_region2$std_pop) * 100000 # Direct rate
trends_region2$dsr_acs_all_low <- trends_region2$dsr_acs_all_admitted - (1.96 * (sqrt(trends_region2$dsr_acs_all_admitted))) # Lower bound 
trends_region2$dsr_acs_all_upp <- trends_region2$dsr_acs_all_admitted + (1.96 * (sqrt(trends_region2$dsr_acs_all_admitted))) # Upper bound 

# Plot
nor2_f <- ggplot(trends_region2[!is.na(trends_region2$north_south) & trends_region2$sex == "F"],) +
  # Plot lockdown periods
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Plot main data
  geom_ribbon(aes(x = date, ymin = dsr_acs_all_low, ymax = dsr_acs_all_upp, fill = factor(north_south)), alpha = 0.2) +
  geom_line(aes(x = date, y = dsr_acs_all_admitted, group = factor(north_south), color = factor(north_south))) +
  # facet_wrap(~sex, labeller = labeller(sex = labels)) + 
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9, labels = c("North", "South")) +
  scale_fill_viridis_d(option = "turbo", begin = 0.1, end = 0.9, labels = c("North", "South")) +
  scale_x_date(date_labels = "%Y")+
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Region", fill = "Region") # Edit legend titles

nor2_m <- ggplot(trends_region2[!is.na(trends_region2$north_south) &trends_region2$sex == "M"],) +
  # Plot lockdown periods
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Plot main data
  geom_ribbon(aes(x = date, ymin = dsr_acs_all_low, ymax = dsr_acs_all_upp, fill = factor(north_south)), alpha = 0.2) +
  geom_line(aes(x = date, y = dsr_acs_all_admitted, group = factor(north_south), color = factor(north_south))) +
  # facet_wrap(~sex, labeller = labeller(sex = labels)) + 
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9, labels = c("North", "South")) +
  scale_fill_viridis_d(option = "turbo", begin = 0.1, end = 0.9, labels = c("North", "South")) +
  scale_x_date(date_labels = "%Y")+
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Region", fill = "Region") # Edit legend titles

# Acute avoidable hospital admissions #

# Create measures
trends_region2$dsr_acs_acute_admitted <- (trends_region2$dexp_admitted_acs_acute / trends_region2$std_pop) * 100000 # Direct rate
trends_region2$dsr_acs_acute_low <- trends_region2$dsr_acs_acute_admitted - (1.96 * (sqrt(trends_region2$dsr_acs_acute_admitted))) # Lower bound 
trends_region2$dsr_acs_acute_upp <- trends_region2$dsr_acs_acute_admitted + (1.96 * (sqrt(trends_region2$dsr_acs_acute_admitted))) # Upper bound 

# Plot by sex
nor3_f <- ggplot(trends_region2[!is.na(trends_region2$north_south) & trends_region2$sex == "F"],) +
  # Plot lockdown periods
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Plot main data
  geom_ribbon(aes(x = date, ymin = dsr_acs_acute_low, ymax = dsr_acs_acute_upp, fill = factor(north_south)), alpha = 0.2) +
  geom_line(aes(x = date, y = dsr_acs_acute_admitted, group = factor(north_south), color = factor(north_south))) +
  # facet_wrap(~sex, labeller = labeller(sex = labels)) + 
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9, labels = c("North", "South")) +
  scale_fill_viridis_d(option = "turbo", begin = 0.1, end = 0.9, labels = c("North", "South")) +
  scale_x_date(date_labels = "%Y") +
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Region", fill = "Region") # Edit legend titles

nor3_m <- ggplot(trends_region2[!is.na(trends_region2$north_south) & trends_region2$sex == "M"],) +
  # Plot lockdown periods
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Plot main data
  geom_ribbon(aes(x = date, ymin = dsr_acs_acute_low, ymax = dsr_acs_acute_upp, fill = factor(north_south)), alpha = 0.2) +
  geom_line(aes(x = date, y = dsr_acs_acute_admitted, group = factor(north_south), color = factor(north_south))) +
  # facet_wrap(~sex, labeller = labeller(sex = labels)) + 
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9, labels = c("North", "South")) +
  scale_fill_viridis_d(option = "turbo", begin = 0.1, end = 0.9, labels = c("North", "South")) +
  scale_x_date(date_labels = "%Y")+
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Region", fill = "Region") # Edit legend titles

# Chronic avoidable hospital admissions #

# Create measures
trends_region2$dsr_acs_chronic_admitted <- (trends_region2$dexp_admitted_acs_chronic / trends_region2$std_pop) * 100000 # Direct rate
trends_region2$dsr_acs_chronic_low <- trends_region2$dsr_acs_chronic_admitted - (1.96 * (sqrt(trends_region2$dsr_acs_chronic_admitted))) # Lower bound 
trends_region2$dsr_acs_chronic_upp <- trends_region2$dsr_acs_chronic_admitted + (1.96 * (sqrt(trends_region2$dsr_acs_chronic_admitted))) # Upper bound 

# Plot
nor4_f <- ggplot(trends_region2[!is.na(trends_region2$north_south) & trends_region2$sex == "F"],) +
  # Plot lockdown periods
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Plot main data
  geom_ribbon(aes(x = date, ymin = dsr_acs_chronic_low, ymax = dsr_acs_chronic_upp, fill = factor(north_south)), alpha = 0.2) +
  geom_line(aes(x = date, y = dsr_acs_chronic_admitted, group = factor(north_south), color = factor(north_south))) +
  # facet_wrap(~sex, labeller = labeller(sex = labels)) + 
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9, labels = c("North", "South")) +
  scale_fill_viridis_d(option = "turbo", begin = 0.1, end = 0.9, labels = c("North", "South")) +
  scale_x_date(date_labels = "%Y")+
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Region", fill = "Region") # Edit legend titles

nor4_m <- ggplot(trends_region2[!is.na(trends_region2$north_south) & trends_region2$sex == "M"],) +
  # Plot lockdown periods
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Plot main data
  geom_ribbon(aes(x = date, ymin = dsr_acs_chronic_low, ymax = dsr_acs_chronic_upp, fill = factor(north_south)), alpha = 0.2) +
  geom_line(aes(x = date, y = dsr_acs_chronic_admitted, group = factor(north_south), color = factor(north_south))) +
  # facet_wrap(~sex, labeller = labeller(sex = labels)) + 
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9, labels = c("North", "South")) +
  scale_fill_viridis_d(option = "turbo", begin = 0.1, end = 0.9, labels = c("North", "South")) +
  scale_x_date(date_labels = "%Y")+
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Region", fill = "Region") # Edit legend titles

# Vaccine preventable admissions #

# Create measures
trends_region2$dsr_acs_vaccine_admitted <- (trends_region2$dexp_admitted_acs_vaccine / trends_region2$std_pop) * 100000 # Direct rate
trends_region2$dsr_acs_vaccine_low <- trends_region2$dsr_acs_vaccine_admitted - (1.96 * (sqrt(trends_region2$dsr_acs_vaccine_admitted))) # Lower bound 
trends_region2$dsr_acs_vaccine_upp <- trends_region2$dsr_acs_vaccine_admitted + (1.96 * (sqrt(trends_region2$dsr_acs_vaccine_admitted))) # Upper bound 

# Plot by sex
nor5_f <- ggplot(trends_region2[!is.na(trends_region2$north_south) & trends_region2$sex == "F"],) +
  # Plot lockdown periods
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Plot main data
  geom_ribbon(aes(x = date, ymin = dsr_acs_vaccine_low, ymax = dsr_acs_vaccine_upp, fill = factor(north_south)), alpha = 0.2) +
  geom_line(aes(x = date, y = dsr_acs_vaccine_admitted, group = factor(north_south), color = factor(north_south))) +
  # facet_wrap(~sex, labeller = labeller(sex = labels)) + 
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9, labels = c("North", "South")) +
  scale_fill_viridis_d(option = "turbo", begin = 0.1, end = 0.9, labels = c("North", "South")) +
  scale_x_date(date_labels = "%Y")+
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Region", fill = "Region") # Edit legend titles

nor5_m <- ggplot(trends_region2[!is.na(trends_region2$north_south) & trends_region2$sex == "M"],) +
  # Plot lockdown periods
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Plot main data
  geom_ribbon(aes(x = date, ymin = dsr_acs_vaccine_low, ymax = dsr_acs_vaccine_upp, fill = factor(north_south)), alpha = 0.2) +
  geom_line(aes(x = date, y = dsr_acs_vaccine_admitted, group = factor(north_south), color = factor(north_south))) +
  # facet_wrap(~sex, labeller = labeller(sex = labels)) + 
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9, labels = c("North", "South")) +
  scale_fill_viridis_d(option = "turbo", begin = 0.1, end = 0.9, labels = c("North", "South")) +
  scale_x_date(date_labels = "%Y")+
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Region", fill = "Region") # Edit legend titles

# Emergency Urgent Sensitive conditions #

# Create measures
trends_region2$dsr_eucs_admitted <- (trends_region2$dexp_admitted_eucs / trends_region2$std_pop) * 100000 # Direct rate
trends_region2$dsr_eucs_low <- trends_region2$dsr_eucs_admitted - (1.96 * (sqrt(trends_region2$dsr_eucs_admitted))) # Lower bound 
trends_region2$dsr_eucs_upp <- trends_region2$dsr_eucs_admitted + (1.96 * (sqrt(trends_region2$dsr_eucs_admitted))) # Upper bound 

# Plot by sex
nor6_f <- ggplot(trends_region2[!is.na(trends_region2$north_south) & trends_region2$sex == "F"],) +
  # Plot lockdown periods
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Plot main data
  geom_ribbon(aes(x = date, ymin = dsr_eucs_low, ymax = dsr_eucs_upp, fill = factor(north_south)), alpha = 0.2) +
  geom_line(aes(x = date, y = dsr_eucs_admitted, group = factor(north_south), color = factor(north_south))) +
  # facet_wrap(~sex, labeller = labeller(sex = labels)) + 
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9, labels = c("North", "South")) +
  scale_fill_viridis_d(option = "turbo", begin = 0.1, end = 0.9, labels = c("North", "South")) +
  scale_x_date(date_labels = "%Y")+
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Region", fill = "Region") # Edit legend titles

nor6_m <- ggplot(trends_region2[!is.na(trends_region2$north_south) & trends_region2$sex == "M"],) +
  # Plot lockdown periods
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Plot main data
  geom_ribbon(aes(x = date, ymin = dsr_eucs_low, ymax = dsr_eucs_upp, fill = factor(north_south)), alpha = 0.2) +
  geom_line(aes(x = date, y = dsr_eucs_admitted, group = factor(north_south), color = factor(north_south))) +
  # facet_wrap(~sex, labeller = labeller(sex = labels)) + 
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9, labels = c("North", "South")) +
  scale_fill_viridis_d(option = "turbo", begin = 0.1, end = 0.9, labels = c("North", "South")) +
  scale_x_date(date_labels = "%Y")+
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Region", fill = "Region") # Edit legend titles

# Combine into a single plot (seperate by sex)
overall_nor_plot_f <- nor1_f + nor2_f + nor3_f + nor4_f + nor5_f + nor6_f + # Combine these plots
  plot_annotation(tag_levels = 'A') + # Give plot labels
  plot_annotation(title = "Trends in female hospital admissions by region",
                  subtitle = "Directly age-standardised admission rates per 100,000 population",
                  caption = "A = Total emergency hospital admissions, B = All ambulatory care admissions, \nC = Acute ambulatory care admissions, D = Chronic ambulatory care admissions, \nE = Vaccine-preventable ambulatory care admissions, F = Emergency urgent care sensitive conditions.") + # Add title and descriptions
  plot_layout(guides = "collect") & theme(legend.position = "bottom") # Use same legend and place at bottom
overall_nor_plot_f
ggsave(overall_nor_plot_f, filename = "../output/plots/north_trends_f.jpeg")

overall_nor_plot_m <- nor1_m + nor2_m + nor3_m + nor4_m + nor5_m + nor6_m + # Combine these plots
  plot_annotation(tag_levels = 'A') + # Give plot labels
  plot_annotation(title = "Trends in male hospital admissions by region",
                  subtitle = "Directly age-standardised admission rates per 100,000 population",
                  caption = "A = Total emergency hospital admissions, B = All ambulatory care admissions, \nC = Acute ambulatory care admissions, D = Chronic ambulatory care admissions, \nE = Vaccine-preventable ambulatory care admissions, F = Emergency urgent care sensitive conditions.") + # Add title and descriptions
  plot_layout(guides = "collect") & theme(legend.position = "bottom") # Use same legend and place at bottom
overall_nor_plot_m
ggsave(overall_nor_plot_m, filename = "../output/plots/north_trends_m.jpeg")


## 2d. Differences between North-South regions ##

# Overall hospital admissions #

# Estimate absolute inequalities
test <- dcast(trends_region2, date ~ sex + north_south, value.var = "dsr_admitted")
test$Female <- test$F_North - test$F_South
test$Male <- test$M_North - test$M_South
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
nor_abs_1 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Rates per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Estimate relative inequalities
test$Female <- test$F_North / test$F_South
test$Male <- test$M_North / test$M_South
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))


# Plot
nor_rel_1 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Ratio") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles


# All avoidable admissions #

# Estimate absolute inequalities
test <- dcast(trends_region2, date ~ sex + north_south, value.var = "dsr_acs_all_admitted")
test$Female <- test$F_North - test$F_South
test$Male <- test$M_North - test$M_South
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
nor_abs_2 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Rates per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Estimate relative inequalities
test$Female <- test$F_North / test$F_South
test$Male <- test$M_North / test$M_South
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
nor_rel_2 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Ratio") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Acute avoidable admissions #

# Estimate absolute inequalities
test <- dcast(trends_region2, date ~ sex + north_south, value.var = "dsr_acs_acute_admitted")
test$Female <- test$F_North - test$F_South
test$Male <- test$M_North - test$M_South
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
nor_abs_3 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Rates per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Estimate relative inequalities
test$Female <- test$F_North / test$F_South
test$Male <- test$M_North / test$M_South
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
nor_rel_3 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Ratio") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Chronic avoidable admissions #

# Estimate absolute inequalities
test <- dcast(trends_region2, date ~ sex + north_south, value.var = "dsr_acs_chronic_admitted")
test$Female <- test$F_North - test$F_South
test$Male <- test$M_North - test$M_South
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
nor_abs_4 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Rates per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Estimate relative inequalities
test$Female <- test$F_North / test$F_South
test$Male <- test$M_North / test$M_South
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
nor_rel_4 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Ratio") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Vaccine preventable avoidable admissions #

# Estimate absolute inequalities
test <- dcast(trends_region2, date ~ sex + north_south, value.var = "dsr_acs_vaccine_admitted")
test$Female <- test$F_North - test$F_South
test$Male <- test$M_North - test$M_South
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
nor_abs_5 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Rates per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Estimate relative inequalities
test$Female <- test$F_North / test$F_South
test$Male <- test$M_North / test$M_South
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
nor_rel_5 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Ratio") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Emergency Urgent Sensitive #

# Estimate absolute inequalities
test <- dcast(trends_region2, date ~ sex + north_south, value.var = "dsr_eucs_admitted")
test$Female <- test$F_North - test$F_South
test$Male <- test$M_North - test$M_South
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
nor_abs_6 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Rates per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Estimate relative inequalities
test$Female <- test$F_North / test$F_South
test$Male <- test$M_North / test$M_South
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
nor_rel_6 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Ratio") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Combine into a single plot 
overall_abs_plot_nor <- nor_abs_1 + nor_abs_2 + nor_abs_3 + nor_abs_4 + nor_abs_5 + nor_abs_6 + # Combine these plots
  plot_annotation(tag_levels = 'A') + # Give plot labels
  plot_annotation(title = "Trends in hospital admissions by region",
                  subtitle = "Absolute difference in max/min rates (3 month moving average)",
                  caption = "A = Total emergency hospital admissions, B = All ambulatory care admissions, \nC = Acute ambulatory care admissions, D = Chronic ambulatory care admissions, \nE = Vaccine-preventable ambulatory care admissions, F = Emergency urgent care sensitive conditions.") + # Add title and descriptions
  plot_layout(guides = "collect") & theme(legend.position = "bottom") # Use same legend and place at bottom
overall_abs_plot_nor
ggsave(overall_abs_plot_nor, filename = "../output/plots/north_abs.jpeg")

overall_rel_plot_nor <- nor_rel_1 + nor_rel_2 + nor_rel_3 + nor_rel_4 + nor_rel_5 + nor_rel_6 + # Combine these plots
  plot_annotation(tag_levels = 'A') + # Give plot labels
  plot_annotation(title = "Trends in hospital admissions by region",
                  subtitle = "Relative difference in max/min rates (3 month moving average)",
                  caption = "A = Total emergency hospital admissions, B = All ambulatory care admissions, \nC = Acute ambulatory care admissions, D = Chronic ambulatory care admissions, \nE = Vaccine-preventable ambulatory care admissions, F = Emergency urgent care sensitive conditions.") + # Add title and descriptions
  plot_layout(guides = "collect") & theme(legend.position = "bottom") # Use same legend and place at bottom
overall_rel_plot_nor
ggsave(overall_rel_plot_nor, filename = "../output/plots/north_rel.jpeg")







