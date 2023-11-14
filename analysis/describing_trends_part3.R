############################################
### Trends in avoidable hospitalisations ###
####### Part 3: Ethnic inequalities ########
############################################

# Purpose: To create a series of descriptive time series plots to visualise and describe how each of our outcome measures have changed between January 2019 and April 2022. The analyses here focus on overall trends by ethnic group.

# Libraries
library(ggplot2)
library(viridis)
library(dplyr)
library(data.table)
library(patchwork)
library(tidyquant)

## 1. Load and tidy data ##

# Load data
trends_eth <- fread("../output/measures/standardised_ethnicity_trends.csv") # Load
trends_eth <- trends_eth[trends_eth$date < "2022-04-01"] # Drop last period

# Convert to date format
trends_eth$date <- as.Date(trends_eth$date)

# Shift dates to mid-point of month since 1st of month refers to whole month (akes plots nicer to look at)
trends_eth$date <- trends_eth$date + 14

## 2. Plot trends ##

# Define labels for sex (used in plots)
labels <- c("Females", "Males")
names(labels) <- c("F", "M")

## 2a. Overall trends by ethnic group ##

# Note:
# White = 1
# Mixed = 2
# Asian or Asian British = 3
# Black or Black British = 4
# Other ethnic groups = 5

# Overall hospitalisations #

# Calculate directly standardised rate for plotting
# trends_eth$smr <- trends_eth$admitted / trends_eth$iexp_admitted # SMR (indirect standarised rate if need)
trends_eth$dsr_admitted <- (trends_eth$dexp_admitted / trends_eth$std_pop) * 100000  # Rate (direct rate)
trends_eth$dsr_admitted_low <- trends_eth$dsr_admitted - (1.96 * (sqrt(trends_eth$dsr_admitted))) # Lower bound 
trends_eth$dsr_admitted_upp <- trends_eth$dsr_admitted + (1.96 * (sqrt(trends_eth$dsr_admitted))) # Upper bound 

# Plot (do seperately for each sex)
eth1_f <- ggplot(trends_eth[trends_eth$ethnicity != 0 & trends_eth$ethnicity != 5 & trends_eth$sex == "F",]) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_ribbon(aes(x = date, ymin = dsr_admitted_low, ymax = dsr_admitted_upp, fill = factor(ethnicity)), alpha = 0.2) + # Plot confidence intervals
  # geom_errorbar(aes(x = date, ymin = dsr_admitted_low, ymax = dsr_admitted_upp, color = factor(ethnicity)), width = 0.1) + # As error bars (lines)
  geom_line(aes(x = date, y = dsr_admitted, group = ethnicity, color = factor(ethnicity)), size = 1) + # Plot line for the rate
  #facet_wrap(~sex, labeller = labeller(sex = labels)) + # Stratify plot by sex
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  scale_colour_viridis_d(option = "plasma", begin = 0.1, end = 0.9, labels = c("White", "Mixed", "Asian", "Black")) + # Use colour blind friendly colours for lines
  scale_fill_viridis_d(option = "plasma", begin = 0.1, end = 0.9, labels = c("White", "Mixed", "Asian", "Black")) + # Use colour blind friendly colours for confidence intervals
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Ethnicity", fill = "Ethnicity") # Edit legend titles

eth1_m <- ggplot(trends_eth[trends_eth$ethnicity != 0 & trends_eth$ethnicity != 5 &trends_eth$sex == "M",]) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_ribbon(aes(x = date, ymin = dsr_admitted_low, ymax = dsr_admitted_upp, fill = factor(ethnicity)), alpha = 0.2) + # Plot confidence intervals
  # geom_errorbar(aes(x = date, ymin = dsr_admitted_low, ymax = dsr_admitted_upp, color = factor(ethnicity)), width = 0.1) + # As error bars (lines)
  geom_line(aes(x = date, y = dsr_admitted, group = ethnicity, color = factor(ethnicity)), size = 1) + # Plot line for the rate
  #facet_wrap(~sex, labeller = labeller(sex = labels)) + # Stratify plot by sex
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  scale_colour_viridis_d(option = "plasma", begin = 0.1, end = 0.9, labels = c("White", "Mixed", "Asian", "Black")) + # Use colour blind friendly colours for lines
  scale_fill_viridis_d(option = "plasma", begin = 0.1, end = 0.9, labels = c("White", "Mixed", "Asian", "Black")) + # Use colour blind friendly colours for confidence intervals
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Ethnicity", fill = "Ethnicity") # Edit legend titles


# # Rate of change over time
# trends_eth <- data.table(trends_eth)
# test <- trends_eth[order(date), list(date = date, dsr_admitted = dsr_admitted, prev_dsr = shift(x = dsr_admitted, n = 1, type = "lag", fill = NA)), by = c("ethnicity", "sex")]
# test$rate_admitted <- test$dsr_admitted / test$prev_dsr 
# ggplot(test[test$ethnicity != 0,], aes(x = date, y = rate_admitted, group = ethnicity, color = factor(ethnicity))) +
#   geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0.7, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
#   geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0.7, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
#   geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0.7, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
#   geom_line(size = 1) +
#   facet_wrap(~sex, labeller = labeller(sex = labels)) +
#   scale_x_date(date_labels = "%Y") +
#   scale_colour_viridis_d(option = "plasma", begin = 0.1, end = 0.9, labels = c("White", "Mixed", "Asian", "Black")) +
#   ylab("Change in rate") +
#   xlab("Date") +
#   labs(color = "Deprivation quintile")

# Re-do by measure: all avoidable hospitalisations #
# Create measures
trends_eth$dsr_acs_all <- (trends_eth$dexp_admitted_acs_all / trends_eth$std_pop) * 100000  # Rate (direct)
trends_eth$dsr_acs_low <- trends_eth$dsr_acs_all - (1.96 * (sqrt(trends_eth$dsr_acs_all))) # Lower bound 
trends_eth$dsr_acs_upp <- trends_eth$dsr_acs_all + (1.96 * (sqrt(trends_eth$dsr_acs_all))) # Upper bound 

# Plot (do seperately for each sex)
eth2_f <- ggplot(trends_eth[trends_eth$ethnicity != 0 & trends_eth$ethnicity != 5 &trends_eth$sex == "F",]) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_ribbon(aes(x = date, ymin = dsr_acs_low, ymax = dsr_acs_upp, fill = factor(ethnicity)), alpha = 0.2) + # Plot confidence intervals
  geom_line(aes(x = date, y = dsr_acs_all, group = ethnicity, color = factor(ethnicity)), size = 1) + # Plot line for the rate
  #facet_wrap(~sex, labeller = labeller(sex = labels)) + # Stratify plot by sex
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  scale_colour_viridis_d(option = "plasma", begin = 0.1, end = 0.9, labels = c("White", "Mixed", "Asian", "Black")) + # Use colour blind friendly colours for lines
  scale_fill_viridis_d(option = "plasma", begin = 0.1, end = 0.9, labels = c("White", "Mixed", "Asian", "Black")) + # Use colour blind friendly colours for confidence intervals
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Ethnicity", fill = "Ethnicity") # Edit legend titles

eth2_m <- ggplot(trends_eth[trends_eth$ethnicity != 0 & trends_eth$ethnicity != 5 &trends_eth$sex == "M",]) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_ribbon(aes(x = date, ymin = dsr_acs_low, ymax = dsr_acs_upp, fill = factor(ethnicity)), alpha = 0.2) + # Plot confidence intervals
  geom_line(aes(x = date, y = dsr_acs_all, group = ethnicity, color = factor(ethnicity)), size = 1) + # Plot line for the rate
  #facet_wrap(~sex, labeller = labeller(sex = labels)) + # Stratify plot by sex
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  scale_colour_viridis_d(option = "plasma", begin = 0.1, end = 0.9, labels = c("White", "Mixed", "Asian", "Black")) + # Use colour blind friendly colours for lines
  scale_fill_viridis_d(option = "plasma", begin = 0.1, end = 0.9, labels = c("White", "Mixed", "Asian", "Black")) + # Use colour blind friendly colours for confidence intervals
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Ethnicity", fill = "Ethnicity") # Edit legend titles

# Acute avoidable hospitalisations #

# Create measures
trends_eth$dsr_acs_acute <- (trends_eth$dexp_admitted_acs_acute / trends_eth$std_pop) * 100000  # Rate (direct)
trends_eth$dsr_acs_acute_low <- trends_eth$dsr_acs_acute - (1.96 * (sqrt(trends_eth$dsr_acs_acute))) # Lower bound 
trends_eth$dsr_acs_acute_upp <- trends_eth$dsr_acs_acute + (1.96 * (sqrt(trends_eth$dsr_acs_acute))) # Upper bound 

# Plot (do seperately for each sex)
eth3_f <- ggplot(trends_eth[trends_eth$ethnicity != 0 & trends_eth$ethnicity != 5 &trends_eth$ethnicity != 5 & trends_eth$sex == "F",]) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_ribbon(aes(x = date, ymin = dsr_acs_acute_low, ymax = dsr_acs_acute_upp, fill = factor(ethnicity)), alpha = 0.2) + # Plot confidence intervals
  geom_line(aes(x = date, y = dsr_acs_acute, group = ethnicity, color = factor(ethnicity)), size = 1) + # Plot line for the rate
  #facet_wrap(~sex, labeller = labeller(sex = labels)) + # Stratify plot by sex
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  scale_colour_viridis_d(option = "plasma", begin = 0.1, end = 0.9, labels = c("White", "Mixed", "Asian", "Black")) + # Use colour blind friendly colours for lines
  scale_fill_viridis_d(option = "plasma", begin = 0.1, end = 0.9, labels = c("White", "Mixed", "Asian", "Black")) + # Use colour blind friendly colours for confidence intervals
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Ethnicity", fill = "Ethnicity") # Edit legend titles

eth3_m <- ggplot(trends_eth[trends_eth$ethnicity != 0 & trends_eth$ethnicity != 5 &trends_eth$ethnicity != 5 & trends_eth$sex == "M",]) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_ribbon(aes(x = date, ymin = dsr_acs_acute_low, ymax = dsr_acs_acute_upp, fill = factor(ethnicity)), alpha = 0.2) + # Plot confidence intervals
  geom_line(aes(x = date, y = dsr_acs_acute, group = ethnicity, color = factor(ethnicity)), size = 1) + # Plot line for the rate
  #facet_wrap(~sex, labeller = labeller(sex = labels)) + # Stratify plot by sex
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  scale_colour_viridis_d(option = "plasma", begin = 0.1, end = 0.9, labels = c("White", "Mixed", "Asian", "Black")) + # Use colour blind friendly colours for lines
  scale_fill_viridis_d(option = "plasma", begin = 0.1, end = 0.9, labels = c("White", "Mixed", "Asian", "Black")) + # Use colour blind friendly colours for confidence intervals
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Ethnicity", fill = "Ethnicity") # Edit legend titles

# Chronic avoidable hospitalisations #

# Create measures
trends_eth$dsr_acs_chronic <- (trends_eth$dexp_admitted_acs_chronic / trends_eth$std_pop) * 100000  # Rate (direct)
trends_eth$dsr_acs_chronic_low <- trends_eth$dsr_acs_chronic - (1.96 * (sqrt(trends_eth$dsr_acs_chronic))) # Lower bound 
trends_eth$dsr_acs_chronic_upp <- trends_eth$dsr_acs_chronic + (1.96 * (sqrt(trends_eth$dsr_acs_chronic))) # Upper bound 

# Plot (do seperately for each sex)
eth4_f <- ggplot(trends_eth[trends_eth$ethnicity != 0 & trends_eth$ethnicity != 5 &trends_eth$ethnicity != 5 & trends_eth$sex == "F",]) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_ribbon(aes(x = date, ymin = dsr_acs_chronic_low, ymax = dsr_acs_chronic_upp, fill = factor(ethnicity)), alpha = 0.2) + # Plot confidence intervals
  geom_line(aes(x = date, y = dsr_acs_chronic, group = ethnicity, color = factor(ethnicity)), size = 1) + # Plot line for the rate
  #facet_wrap(~sex, labeller = labeller(sex = labels)) + # Stratify plot by sex
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  scale_colour_viridis_d(option = "plasma", begin = 0.1, end = 0.9, labels = c("White", "Mixed", "Asian", "Black")) + # Use colour blind friendly colours for lines
  scale_fill_viridis_d(option = "plasma", begin = 0.1, end = 0.9, labels = c("White", "Mixed", "Asian", "Black")) + # Use colour blind friendly colours for confidence intervals
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Ethnicity", fill = "Ethnicity") # Edit legend titles

eth4_m <- ggplot(trends_eth[trends_eth$ethnicity != 0 & trends_eth$ethnicity != 5 &trends_eth$ethnicity != 5 & trends_eth$sex == "M",]) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_ribbon(aes(x = date, ymin = dsr_acs_chronic_low, ymax = dsr_acs_chronic_upp, fill = factor(ethnicity)), alpha = 0.2) + # Plot confidence intervals
  geom_line(aes(x = date, y = dsr_acs_chronic, group = ethnicity, color = factor(ethnicity)), size = 1) + # Plot line for the rate
  #facet_wrap(~sex, labeller = labeller(sex = labels)) + # Stratify plot by sex
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  scale_colour_viridis_d(option = "plasma", begin = 0.1, end = 0.9, labels = c("White", "Mixed", "Asian", "Black")) + # Use colour blind friendly colours for lines
  scale_fill_viridis_d(option = "plasma", begin = 0.1, end = 0.9, labels = c("White", "Mixed", "Asian", "Black")) + # Use colour blind friendly colours for confidence intervals
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Ethnicity", fill = "Ethnicity") # Edit legend titles


# Vaccine preventable avoidable hospitalisations #

# Create measures
trends_eth$dsr_acs_vaccine <- (trends_eth$dexp_admitted_acs_vaccine / trends_eth$std_pop) * 100000  # Rate (direct)
trends_eth$dsr_acs_vaccine_low <- trends_eth$dsr_acs_vaccine - (1.96 * (sqrt(trends_eth$dsr_acs_vaccine))) # Lower bound 
trends_eth$dsr_acs_vaccine_upp <- trends_eth$dsr_acs_vaccine + (1.96 * (sqrt(trends_eth$dsr_acs_vaccine))) # Upper bound 

# Plot (do seperately for each sex)
eth5_f <- ggplot(trends_eth[trends_eth$ethnicity != 0 & trends_eth$ethnicity != 5 & trends_eth$sex == "F",]) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_ribbon(aes(x = date, ymin = dsr_acs_vaccine_low, ymax = dsr_acs_vaccine_upp, fill = factor(ethnicity)), alpha = 0.2) + # Plot confidence intervals
  geom_line(aes(x = date, y = dsr_acs_vaccine, group = ethnicity, color = factor(ethnicity)), size = 1) + # Plot line for the rate
  #facet_wrap(~sex, labeller = labeller(sex = labels)) + # Stratify plot by sex
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  scale_colour_viridis_d(option = "plasma", begin = 0.1, end = 0.9, labels = c("White", "Mixed", "Asian", "Black")) + # Use colour blind friendly colours for lines
  scale_fill_viridis_d(option = "plasma", begin = 0.1, end = 0.9, labels = c("White", "Mixed", "Asian", "Black")) + # Use colour blind friendly colours for confidence intervals
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Ethnicity", fill = "Ethnicity") # Edit legend titles


eth5_m <- ggplot(trends_eth[trends_eth$ethnicity != 0 & trends_eth$ethnicity != 5 & trends_eth$sex == "M",]) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_ribbon(aes(x = date, ymin = dsr_acs_vaccine_low, ymax = dsr_acs_vaccine_upp, fill = factor(ethnicity)), alpha = 0.2) + # Plot confidence intervals
  geom_line(aes(x = date, y = dsr_acs_vaccine, group = ethnicity, color = factor(ethnicity)), size = 1) + # Plot line for the rate
  #facet_wrap(~sex, labeller = labeller(sex = labels)) + # Stratify plot by sex
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  scale_colour_viridis_d(option = "plasma", begin = 0.1, end = 0.9, labels = c("White", "Mixed", "Asian", "Black")) + # Use colour blind friendly colours for lines
  scale_fill_viridis_d(option = "plasma", begin = 0.1, end = 0.9, labels = c("White", "Mixed", "Asian", "Black")) + # Use colour blind friendly colours for confidence intervals
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Ethnicity", fill = "Ethnicity") # Edit legend titles

# Emergency urgent sensitive conditions #

# Create measures
trends_eth$dsr_acs_eucs <- (trends_eth$dexp_admitted_eucs / trends_eth$std_pop) * 100000  # Rate (direct)
trends_eth$dsr_acs_eucs_low <- trends_eth$dsr_acs_eucs - (1.96 * (sqrt(trends_eth$dsr_acs_eucs))) # Lower bound 
trends_eth$dsr_acs_eucs_upp <- trends_eth$dsr_acs_eucs + (1.96 * (sqrt(trends_eth$dsr_acs_eucs))) # Upper bound 

# Plot (do seperately for each sex)
eth6_f <- ggplot(trends_eth[trends_eth$ethnicity != 0 & trends_eth$ethnicity != 5 & trends_eth$sex == "M",]) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_ribbon(aes(x = date, ymin = dsr_acs_eucs_low, ymax = dsr_acs_eucs_upp, fill = factor(ethnicity)), alpha = 0.2) + # Plot confidence intervals
  geom_line(aes(x = date, y = dsr_acs_eucs, group = ethnicity, color = factor(ethnicity)), size = 1) + # Plot line for the rate
  #facet_wrap(~sex, labeller = labeller(sex = labels)) + # Stratify plot by sex
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  scale_colour_viridis_d(option = "plasma", begin = 0.1, end = 0.9, labels = c("White", "Mixed", "Asian", "Black")) + # Use colour blind friendly colours for lines
  scale_fill_viridis_d(option = "plasma", begin = 0.1, end = 0.9, labels = c("White", "Mixed", "Asian", "Black")) + # Use colour blind friendly colours for confidence intervals
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Ethnicity", fill = "Ethnicity") # Edit legend titles

eth6_m <- ggplot(trends_eth[trends_eth$ethnicity != 0 & trends_eth$ethnicity != 5 & trends_eth$sex == "M",]) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_ribbon(aes(x = date, ymin = dsr_acs_eucs_low, ymax = dsr_acs_eucs_upp, fill = factor(ethnicity)), alpha = 0.2) + # Plot confidence intervals
  geom_line(aes(x = date, y = dsr_acs_eucs, group = ethnicity, color = factor(ethnicity)), size = 1) + # Plot line for the rate
  #facet_wrap(~sex, labeller = labeller(sex = labels)) + # Stratify plot by sex
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  scale_colour_viridis_d(option = "plasma", begin = 0.1, end = 0.9, labels = c("White", "Mixed", "Asian", "Black")) + # Use colour blind friendly colours for lines
  scale_fill_viridis_d(option = "plasma", begin = 0.1, end = 0.9, labels = c("White", "Mixed", "Asian", "Black")) + # Use colour blind friendly colours for confidence intervals
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Ethnicity", fill = "Ethnicity") # Edit legend titles

# Combine into a single plot (seperate by sex)
overall_eth_plot_f <- eth1_f + eth2_f + eth3_f + eth4_f + eth5_f + eth6_f + # Combine these plots
  plot_annotation(tag_levels = 'A') + # Give plot labels
  plot_annotation(title = "Trends in female hospital admissions by ethnicity",
                  subtitle = "Directly age-standardised admission rates per 100,000 population",
                  caption = "A = Total emergency hospital admissions, B = All ambulatory care sensitive admissions, \nC = Acute ambulatory care sensitive admissions, D = Chronic ambulatory care sensitive admissions, \nE = Vaccine-preventable ambulatory care sensitive admissions, F = Emergency urgent care sensitive conditions.") + # Add title and descriptions
  plot_layout(guides = "collect") & theme(legend.position = "bottom") # Use same legend and place at bottom
overall_eth_plot_f
ggsave(overall_eth_plot_f, filename = "../output/plots/eth_trends_f.jpeg")

overall_eth_plot_m <- eth1_m + eth2_m + eth3_m + eth4_m + eth5_m + eth6_m + # Combine these plots
  plot_annotation(tag_levels = 'A') + # Give plot labels
  plot_annotation(title = "Trends in male hospital admissions by ethnicity",
                  subtitle = "Directly age-standardised admission rates per 100,000 population",
                  caption = "A = Total emergency hospital admissions, B = All ambulatory care sensitive admissions, \nC = Acute ambulatory care sensitive admissions, D = Chronic ambulatory care sensitive admissions, \nE = Vaccine-preventable ambulatory care sensitive admissions, F = Emergency urgent care sensitive conditions.") + # Add title and descriptions
  plot_layout(guides = "collect") & theme(legend.position = "bottom") # Use same legend and place at bottom
overall_eth_plot_m
ggsave(overall_eth_plot_m, filename = "../output/plots/eth_trends_m.jpeg")


## 2b. Differences by ethnic group ##

# Overall hospital admissions #

# Estimate absolute inequalities
test <- dcast(trends_eth, date ~ sex + ethnicity, value.var = "dsr_admitted")
test$Female <- pmax(test$F_1, test$F_2, test$F_3, test$F_4, na.rm = T) - pmin(test$F_1, test$F_2, test$F_3, test$F_4, na.rm = T)
test$Male <- pmax(test$M_1, test$M_2, test$M_3, test$M_4, na.rm = T) - pmin(test$M_1, test$M_2, test$M_3, test$M_4, na.rm = T)
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
eth_abs_1 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "plasma", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Rates per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Estimate relative inequalities
test$Female <- pmax(test$F_1, test$F_2, test$F_3, test$F_4, na.rm = T) / pmin(test$F_1, test$F_2, test$F_3, test$F_4, na.rm = T)
test$Male <- pmax(test$M_1, test$M_2, test$M_3, test$M_4, na.rm = T) / pmin(test$M_1, test$M_2, test$M_3, test$M_4, na.rm = T)
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
eth_rel_1 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "plasma", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Ratio") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles


# All avoidable admissions #

# Estimate absolute inequalities
test <- dcast(trends_eth, date ~ sex + ethnicity, value.var = "dsr_acs_all_admitted")
test$Female <- pmax(test$F_1, test$F_2, test$F_3, test$F_4, na.rm = T) - pmin(test$F_1, test$F_2, test$F_3, test$F_4, na.rm = T)
test$Male <- pmax(test$M_1, test$M_2, test$M_3, test$M_4, na.rm = T) - pmin(test$M_1, test$M_2, test$M_3, test$M_4, na.rm = T)
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
eth_abs_2 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "plasma", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Rates per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Estimate relative inequalities
test$Female <- pmax(test$F_1, test$F_2, test$F_3, test$F_4, na.rm = T) / pmin(test$F_1, test$F_2, test$F_3, test$F_4, na.rm = T)
test$Male <- pmax(test$M_1, test$M_2, test$M_3, test$M_4, na.rm = T) / pmin(test$M_1, test$M_2, test$M_3, test$M_4, na.rm = T)
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
eth_rel_2 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "plasma", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Ratio") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Acute avoidable admissions #

# Estimate absolute inequalities
test <- dcast(trends_eth, date ~ sex + ethnicity, value.var = "dsr_acs_acute")
test$Female <- pmax(test$F_1, test$F_2, test$F_3, test$F_4, na.rm = T) - pmin(test$F_1, test$F_2, test$F_3, test$F_4, na.rm = T)
test$Male <- pmax(test$M_1, test$M_2, test$M_3, test$M_4, na.rm = T) - pmin(test$M_1, test$M_2, test$M_3, test$M_4, na.rm = T)
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
eth_abs_3 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "plasma", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Rates per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Estimate relative inequalities
test$Female <- pmax(test$F_1, test$F_2, test$F_3, test$F_4, na.rm = T) / pmin(test$F_1, test$F_2, test$F_3, test$F_4, na.rm = T)
test$Male <- pmax(test$M_1, test$M_2, test$M_3, test$M_4, na.rm = T) / pmin(test$M_1, test$M_2, test$M_3, test$M_4, na.rm = T)
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
eth_rel_3 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "plasma", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Ratio") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Chronic avoidable admissions #

# Estimate absolute inequalities
test <- dcast(trends_eth, date ~ sex + ethnicity, value.var = "dsr_acs_chronic")
test$Female <- pmax(test$F_1, test$F_2, test$F_3, test$F_4, na.rm = T) - pmin(test$F_1, test$F_2, test$F_3, test$F_4, na.rm = T)
test$Male <- pmax(test$M_1, test$M_2, test$M_3, test$M_4, na.rm = T) - pmin(test$M_1, test$M_2, test$M_3, test$M_4, na.rm = T)
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
eth_abs_4 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "plasma", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Rates per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Estimate relative inequalities
test$Female <- pmax(test$F_1, test$F_2, test$F_3, test$F_4, na.rm = T) / pmin(test$F_1, test$F_2, test$F_3, test$F_4, na.rm = T)
test$Male <- pmax(test$M_1, test$M_2, test$M_3, test$M_4, na.rm = T) / pmin(test$M_1, test$M_2, test$M_3, test$M_4, na.rm = T)
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
eth_rel_4 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "plasma", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Ratio") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Vaccine preventable avoidable admissions #

# Estimate absolute inequalities
test <- dcast(trends_eth, date ~ sex + ethnicity, value.var = "dsr_acs_vaccine")
test$Female <- pmax(test$F_1, test$F_2, test$F_3, test$F_4, na.rm = T) - pmin(test$F_1, test$F_2, test$F_3, test$F_4, na.rm = T)
test$Male <- pmax(test$M_1, test$M_2, test$M_3, test$M_4, na.rm = T) - pmin(test$M_1, test$M_2, test$M_3, test$M_4, na.rm = T)
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
eth_abs_5 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "plasma", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Rates per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Estimate relative inequalities
test$Female <- pmax(test$F_1, test$F_2, test$F_3, test$F_4, na.rm = T) / pmin(test$F_1, test$F_2, test$F_3, test$F_4, na.rm = T)
test$Male <- pmax(test$M_1, test$M_2, test$M_3, test$M_4, na.rm = T) / pmin(test$M_1, test$M_2, test$M_3, test$M_4, na.rm = T)
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
eth_rel_5 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "plasma", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Ratio") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Emergency Urgent Sensitive #

# Estimate absolute inequalities
test <- dcast(trends_eth, date ~ sex + ethnicity, value.var = "dsr_acs_eucs")
test$Female <- pmax(test$F_1, test$F_2, test$F_3, test$F_4, na.rm = T) - pmin(test$F_1, test$F_2, test$F_3, test$F_4, na.rm = T)
test$Male <- pmax(test$M_1, test$M_2, test$M_3, test$M_4, na.rm = T) - pmin(test$M_1, test$M_2, test$M_3, test$M_4, na.rm = T)
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
eth_abs_6 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "plasma", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Rates per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Estimate relative inequalities
test$Female <- pmax(test$F_1, test$F_2, test$F_3, test$F_4, na.rm = T) / pmin(test$F_1, test$F_2, test$F_3, test$F_4, na.rm = T)
test$Male <- pmax(test$M_1, test$M_2, test$M_3, test$M_4, na.rm = T) / pmin(test$M_1, test$M_2, test$M_3, test$M_4, na.rm = T)
long <- melt(test, id.vars = "date", measure.vars = c("Female", "Male"))

# Plot
eth_rel_6 <- ggplot(long) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 1, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = value, group = variable, color = variable), size = 1, alpha = 0.2) + # Plot line for the rate
  geom_ma(aes(x = date, y = value, group = variable, color = variable), ma_fun = SMA, n = 3, , size = 1, linetype = 1) + # 3 month moving average
  scale_colour_viridis_d(option = "plasma", begin = 0.1, end = 0.9) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Ratio") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles

# Combine into a single plot 
overall_abs_plot <- eth_abs_1 + eth_abs_2 + eth_abs_3 + eth_abs_4 + eth_abs_5 + eth_abs_6 + # Combine these plots
  plot_annotation(tag_levels = 'A') + # Give plot labels
  plot_annotation(title = "Trends in hospital admissions by ethnicity",
                  subtitle = "Absolute difference in max/min rates (3 month moving average)",
                  caption = "A = Total emergency hospital admissions, B = All ambulatory care admissions, \nC = Acute ambulatory care admissions, D = Chronic ambulatory care admissions, \nE = Vaccine-preventable ambulatory care admissions, F = Emergency urgent care sensitive conditions.") + # Add title and descriptions
  plot_layout(guides = "collect") & theme(legend.position = "bottom") # Use same legend and place at bottom
overall_abs_plot
ggsave(overall_abs_plot, filename = "../output/plots/eth_abs.jpeg")
ggsave(overall_abs_plot, filename = "../output/plots/figure3.jpeg", dpi = 300)

overall_rel_plot <- eth_rel_1 + eth_rel_2 + eth_rel_3 + eth_rel_4 + eth_rel_5 + eth_rel_6 + # Combine these plots
  plot_annotation(tag_levels = 'A') + # Give plot labels
  plot_annotation(title = "Trends in hospital admissions by ethnicity",
                  subtitle = "Relative difference in max/min rates (3 month moving average)",
                  caption = "A = Total emergency hospital admissions, B = All ambulatory care admissions, \nC = Acute ambulatory care admissions, D = Chronic ambulatory care admissions, \nE = Vaccine-preventable ambulatory care admissions, F = Emergency urgent care sensitive conditions.") + # Add title and descriptions
  plot_layout(guides = "collect") & theme(legend.position = "bottom") # Use same legend and place at bottom
overall_rel_plot
ggsave(overall_rel_plot, filename = "../output/plots/eth_rel.jpeg")


