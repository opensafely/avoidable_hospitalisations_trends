############################################
### Trends in avoidable hospitalisations ###
##### Part 1: Overall and deprivation ######
############################################

# Purpose: To create a series of descriptive time series plots to visualise and describe how each of our outcome measures have changed between January 2019 and April 2022. The analyses here focus on overall trends in outcomes and then stratified by neighbourhood socioeconomic deprivation. I have split up the R scripts so that they are not too long (they can be run in any order).

# Libraries
library(ggplot2)
library(viridis)
library(dplyr)
library(PHEindicatormethods)
library(data.table)
library(patchwork)

## 1. Load and tidy data ##

# Load data
trends_imd <- fread("../output/measures/standardised_imd_trends.csv") # Load
trends_imd <- trends_imd[trends_imd$date < "2022-04-01"] # Drop last period

# Convert to date format
trends_imd$date <- as.Date(trends_imd$date) 

# Shift dates to mid-point of month since 1st of month refers to whole month (akes plots nicer to look at)
trends_imd$date <- trends_imd$date + 14

## 2. Plot trends ##

# Define labels for sex (used in plots)
labels <- c("Females", "Males")
names(labels) <- c("F", "M")


## 2a. Overall trends by sex ##

# Sum values to totals
all_trends <- trends_imd[, list(admitted = sum(admitted), admitted_acs_all = sum(admitted_acs_all), admitted_acs_acute = sum(admitted_acs_acute), admitted_acs_chronic = sum(admitted_acs_chronic), admitted_acs_vaccine = sum(admitted_acs_vaccine), admitted_eucs = sum(admitted_eucs), pop = sum(pop)), by =c("date", "sex")]

# Overall trends #

# Create overall rates for each outcome 
all_trends$adm_rate <- all_trends$admitted / all_trends$pop * 100000
all_trends$acs_rate <- all_trends$admitted_acs_all / all_trends$pop * 100000
all_trends$acsa_rate <- all_trends$admitted_acs_acute / all_trends$pop * 100000
all_trends$acsc_rate <- all_trends$admitted_acs_chronic / all_trends$pop * 100000
all_trends$acsv_rate <- all_trends$admitted_acs_vaccine / all_trends$pop * 100000
all_trends$eucs_rate <- all_trends$admitted_eucs / all_trends$pop * 100000

# Plot each variable
# Overall hospitalisations
plot1 <- ggplot(all_trends) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = adm_rate, group = sex, color = sex), size = 1) + # Plot line for the rate
  scale_colour_viridis_d(option = "mako", begin = 0.1, end = 0.9, labels = c("Female", "Male")) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles
  # ggtitle("Total hospital admissions") # Title for plot

# All avoidable hospitalisations
plot2 <- ggplot(all_trends) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = acs_rate, group = sex, color = sex), size = 1) + # Plot line for the rate
  scale_colour_viridis_d(option = "mako", begin = 0.1, end = 0.9, labels = c("Female", "Male")) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles
  # ggtitle("Avoidable hospital admissions - all conditions") # Title for plot

# Acute avoidable hospitalisations
plot3 <- ggplot(all_trends) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = acsa_rate, group = sex, color = sex), size = 1) + # Plot line for the rate
  scale_colour_viridis_d(option = "mako", begin = 0.1, end = 0.9, labels = c("Female", "Male")) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles
  # ggtitle("Avoidable hospital admissions - acute conditions") # Title for plot

# Chronic avoidable hospitalisations
plot4 <- ggplot(all_trends) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = acsc_rate, group = sex, color = sex), size = 1) + # Plot line for the rate
  scale_colour_viridis_d(option = "mako", begin = 0.1, end = 0.9, labels = c("Female", "Male")) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles
  # ggtitle("Avoidable hospital admissions - chronic conditions") # Title for plot

# Vaccine-preventable avoidable hospitalisations
plot5 <- ggplot(all_trends) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = acsv_rate, group = sex, color = sex), size = 1) + # Plot line for the rate
  scale_colour_viridis_d(option = "mako", begin = 0.1, end = 0.9, labels = c("Female", "Male")) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles
  # ggtitle("Avoidable hospital admissions - vaccine-preventable conditions") # Title for plot

# Emergency urgent sensitive conditions
plot6 <- ggplot(all_trends) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_line(aes(x = date, y = eucs_rate, group = sex, color = sex), size = 1) + # Plot line for the rate
  scale_colour_viridis_d(option = "mako", begin = 0.1, end = 0.9, labels = c("Female", "Male")) + # Use colour blind friendly colours for lines
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Sex") # Edit legend titles
  # ggtitle("Emergency Urgent Sensitive Conditions") # Title for plot

# Combine plots together
overall_trends_plot <- plot1 + plot2 + plot3 + plot4 + plot5 + plot6 + # Which plots to use
  plot_annotation(tag_levels = 'A') + # Add letter descriptions/labels
  plot_annotation(title = "Overall trends in hospital admissions",
                  caption = "A = Total emergency hospital admissions, B = All ambulatory care sensitive admissions, \nC = Acute ambulatory care sensitive admissions, D = Chronic ambulatory care sensitive admissions, \nE = Vaccine-preventable ambulatory care sensitive admissions, F = Emergency urgent care sensitive conditions.") + # Add title and descriptions
  plot_layout(guides = 'collect') # Use same legend
overall_trends_plot
ggsave(overall_trends_plot, filename = "../output/plots/overall_trends.jpeg")
ggsave(overall_trends_plot, filename = "../output/plots/figure1.jpeg", dpi = 300)

# Relative change in trends #

# Calculate lagged values for each variable
test <- all_trends[order(date), list(date = date, adm_rate = adm_rate, prev_adm = shift(x = adm_rate, n = 1, type = "lag", fill = NA), acs_rate = acs_rate, prev_acs = shift(x = acs_rate, n = 1, type = "lag", fill = NA), acsa_rate = acsa_rate, prev_acsa = shift(x = acsa_rate, n = 1, type = "lag", fill = NA), acsc_rate = acsc_rate, prev_acsc = shift(x = acsc_rate, n = 1, type = "lag", fill = NA), acsv_rate = acsv_rate, prev_acsv = shift(x = acsv_rate, n = 1, type = "lag", fill = NA), eucs_rate = eucs_rate, prev_eucs = shift(x = eucs_rate, n = 1, type = "lag", fill = NA)), by = c("sex")]

# Calculate change in rates (relative)
test$chg_adm <- test$adm_rate / test$prev_adm 
test$chg_acs <- test$acs_rate / test$prev_acs
test$chg_acsa <- test$acsa_rate / test$prev_acsa 
test$chg_acsc <- test$acsc_rate / test$prev_acsc 
test$chg_acsv <- test$acsv_rate / test$prev_acsv 
test$chg_eucs <- test$eucs_rate / test$prev_eucs 

# Change to long format for plotting purposes
for_plotting <- melt(test, id = c("date", "sex"), measure.vars = c("chg_adm", "chg_acs", "chg_acsa", "chg_acsc", "chg_acsv", "chg_eucs"))

# Plot
rel_plot <- ggplot(for_plotting, aes(x = date, y = value, group = variable, color = factor(variable))) +
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0.5, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0.5, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0.5, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_line(size = 1) +
  facet_wrap(~sex, labeller = labeller(sex = labels)) +
  scale_x_date(date_labels = "%Y") +
  scale_colour_viridis_d(option = "mako", labels = c("All admissions", "All ambulatory", "Acute ambulatory", "Chronic ambulatory", "Vaccine ambulatory", "Emergency urgent"), begin = 0.1, end = 0.9) +
  ylab("Change in rate") +
  xlab("Date") +
  labs(color = "Measure")
ggsave(rel_plot, filename = "../output/plots/relative_change_overall.jpeg")

# Percent of emergency admissions

# Create overall rates for each outcome 
all_trends$acs_pc <- (all_trends$admitted_acs_all / all_trends$admitted) * 100
all_trends$acsa_pc <- (all_trends$admitted_acs_acute / all_trends$admitted) * 100
all_trends$acsc_pc <- (all_trends$admitted_acs_chronic / all_trends$admitted) * 100
all_trends$acsv_pc <- (all_trends$admitted_acs_vaccine / all_trends$admitted) * 100
all_trends$eucs_pc <- (all_trends$admitted_eucs / all_trends$admitted) * 100

# Change to long format for plotting purposes
for_plotting2 <- melt(all_trends, id = c("date", "sex"), measure.vars = c("acs_pc", "acsa_pc", "acsc_pc", "acsv_pc", "eucs_pc"))

# Plot
pc_plot <- ggplot(for_plotting2, aes(x = date, y = value, group = variable, color = factor(variable))) +
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0.5, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0.5, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0.5, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_line(size = 1) +
  facet_wrap(~sex, labeller = labeller(sex = labels)) +
  scale_x_date(date_labels = "%Y") +
  scale_colour_viridis_d(option = "mako", labels = c("All ambulatory", "Acute ambulatory", "Chronic ambulatory", "Vaccine ambulatory", "Emergency urgent"), begin = 0.1, end = 0.9) +
  ylab("Percentage (%)") +
  xlab("Date") +
  labs(color = "Measure")
ggsave(pc_plot, filename = "../output/plots/percent_overall.jpeg")


## 2b. Trends by deprivation ##

# Overall hospitalisations #

# Calculate directly standardised rate for plotting
# trends_imd$smr <- trends_imd$admitted / trends_imd$iexp_admitted # SMR (indirect standarised rate if need)
trends_imd$dsr_admitted <- (trends_imd$dexp_admitted / trends_imd$std_pop) * 100000  # Rate (direct rate)
trends_imd$dsr_admitted_low <- trends_imd$dsr_admitted - (1.96 * (sqrt(trends_imd$dsr_admitted))) # Lower bound 
trends_imd$dsr_admitted_upp <- trends_imd$dsr_admitted + (1.96 * (sqrt(trends_imd$dsr_admitted))) # Upper bound 

# Plot (do seperately for each sex)
imd1_f <- ggplot(trends_imd[trends_imd$imd_quintile != 0 & trends_imd$sex == "F",]) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_ribbon(aes(x = date, ymin = dsr_admitted_low, ymax = dsr_admitted_upp, fill = factor(imd_quintile)), alpha = 0.2) + # Plot confidence intervals
  # geom_errorbar(aes(x = date, ymin = dsr_admitted_low, ymax = dsr_admitted_upp, color = factor(imd_quintile)), width = 0.1) + # As error bars (lines)
  geom_line(aes(x = date, y = dsr_admitted, group = imd_quintile, color = factor(imd_quintile)), size = 1) + # Plot line for the rate
  #facet_wrap(~sex, labeller = labeller(sex = labels)) + # Stratify plot by sex
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  scale_colour_viridis_d(option = "viridis", begin = 0.1, end = 0.9, labels = c("1: Most deprived", "2", "3", "4", "5: Least Deprived")) + # Use colour blind friendly colours for lines
  scale_fill_viridis_d(option = "viridis", begin = 0.1, end = 0.9, labels = c("1: Most deprived", "2", "3", "4", "5: Least Deprived")) + # Use colour blind friendly colours for confidence intervals
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Deprivation quintile", fill = "Deprivation quintile") # Edit legend titles

imd1_m <- ggplot(trends_imd[trends_imd$imd_quintile != 0 & trends_imd$sex == "M",]) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_ribbon(aes(x = date, ymin = dsr_admitted_low, ymax = dsr_admitted_upp, fill = factor(imd_quintile)), alpha = 0.2) + # Plot confidence intervals
  # geom_errorbar(aes(x = date, ymin = dsr_admitted_low, ymax = dsr_admitted_upp, color = factor(imd_quintile)), width = 0.1) + # As error bars (lines)
  geom_line(aes(x = date, y = dsr_admitted, group = imd_quintile, color = factor(imd_quintile)), size = 1) + # Plot line for the rate
  #facet_wrap(~sex, labeller = labeller(sex = labels)) + # Stratify plot by sex
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  scale_colour_viridis_d(option = "viridis", begin = 0.1, end = 0.9, labels = c("1: Most deprived", "2", "3", "4", "5: Least Deprived")) + # Use colour blind friendly colours for lines
  scale_fill_viridis_d(option = "viridis", begin = 0.1, end = 0.9, labels = c("1: Most deprived", "2", "3", "4", "5: Least Deprived")) + # Use colour blind friendly colours for confidence intervals
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Deprivation quintile", fill = "Deprivation quintile") # Edit legend titles


# # Rate of change over time
# trends_imd <- data.table(trends_imd)
# test <- trends_imd[order(date), list(date = date, dsr_admitted = dsr_admitted, prev_dsr = shift(x = dsr_admitted, n = 1, type = "lag", fill = NA)), by = c("imd_quintile", "sex")]
# test$rate_admitted <- test$dsr_admitted / test$prev_dsr 
# ggplot(test[test$imd_quintile != 0,], aes(x = date, y = rate_admitted, group = imd_quintile, color = factor(imd_quintile))) +
#   geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0.7, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
#   geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0.7, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
#   geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0.7, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
#   geom_line(size = 1) +
#   facet_wrap(~sex, labeller = labeller(sex = labels)) +
#   scale_x_date(date_labels = "%Y") +
#   scale_colour_viridis_d(option = "viridis", begin = 0.1, end = 0.9, labels = c("1: Most deprived", "2", "3", "4", "5: Least Deprived")) +
#   ylab("Change in rate") +
#   xlab("Date") +
#   labs(color = "Deprivation quintile")

# Re-do by measure: all avoidable hospitalisations #
# Create measures
trends_imd$dsr_acs_all <- (trends_imd$dexp_admitted_acs_all / trends_imd$std_pop) * 100000  # Rate (direct)
trends_imd$dsr_acs_low <- trends_imd$dsr_acs_all - (1.96 * (sqrt(trends_imd$dsr_acs_all))) # Lower bound 
trends_imd$dsr_acs_upp <- trends_imd$dsr_acs_all + (1.96 * (sqrt(trends_imd$dsr_acs_all))) # Upper bound 

# Plot (do seperately for each sex)
imd2_f <- ggplot(trends_imd[trends_imd$imd_quintile != 0 & trends_imd$sex == "F",]) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_ribbon(aes(x = date, ymin = dsr_acs_low, ymax = dsr_acs_upp, fill = factor(imd_quintile)), alpha = 0.2) + # Plot confidence intervals
  geom_line(aes(x = date, y = dsr_acs_all, group = imd_quintile, color = factor(imd_quintile)), size = 1) + # Plot line for the rate
  #facet_wrap(~sex, labeller = labeller(sex = labels)) + # Stratify plot by sex
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  scale_colour_viridis_d(option = "viridis", begin = 0.1, end = 0.9, labels = c("1: Most deprived", "2", "3", "4", "5: Least Deprived")) + # Use colour blind friendly colours for lines
  scale_fill_viridis_d(option = "viridis", begin = 0.1, end = 0.9, labels = c("1: Most deprived", "2", "3", "4", "5: Least Deprived")) + # Use colour blind friendly colours for confidence intervals
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Deprivation quintile", fill = "Deprivation quintile") # Edit legend titles

imd2_m <- ggplot(trends_imd[trends_imd$imd_quintile != 0 & trends_imd$sex == "M",]) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_ribbon(aes(x = date, ymin = dsr_acs_low, ymax = dsr_acs_upp, fill = factor(imd_quintile)), alpha = 0.2) + # Plot confidence intervals
  geom_line(aes(x = date, y = dsr_acs_all, group = imd_quintile, color = factor(imd_quintile)), size = 1) + # Plot line for the rate
  #facet_wrap(~sex, labeller = labeller(sex = labels)) + # Stratify plot by sex
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  scale_colour_viridis_d(option = "viridis", begin = 0.1, end = 0.9, labels = c("1: Most deprived", "2", "3", "4", "5: Least Deprived")) + # Use colour blind friendly colours for lines
  scale_fill_viridis_d(option = "viridis", begin = 0.1, end = 0.9, labels = c("1: Most deprived", "2", "3", "4", "5: Least Deprived")) + # Use colour blind friendly colours for confidence intervals
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Deprivation quintile", fill = "Deprivation quintile") # Edit legend titles


# trends_imd$pc_acs_all <- (trends_imd$dexp_admitted_acs_all / trends_imd$dexp_admitted) * 100  # Rate (direct)
# ggplot(trends_imd[trends_imd$imd_quintile != 0,], aes(x = date, y = pc_acs_all, group = imd_quintile, color = factor(imd_quintile))) +
#   geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
#   geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
#   geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
#   geom_line(size = 1) +
#   facet_wrap(~sex, labeller = labeller(sex = labels)) +
#   scale_x_date(date_labels = "%Y") +
#   scale_colour_viridis_d(option = "viridis", begin = 0.1, end = 0.9, labels = c("1: Most deprived", "2", "3", "4", "5: Least Deprived")) +
#   ylab("Directly standardised rate (per 100,000)") +
#   xlab("Date") +
#   labs(color = "Deprivation quintile")

# Acute avoidable hospitalisations #

# Create measures
trends_imd$dsr_acs_acute <- (trends_imd$dexp_admitted_acs_acute / trends_imd$std_pop) * 100000  # Rate (direct)
trends_imd$dsr_acs_acute_low <- trends_imd$dsr_acs_acute - (1.96 * (sqrt(trends_imd$dsr_acs_acute))) # Lower bound 
trends_imd$dsr_acs_acute_upp <- trends_imd$dsr_acs_acute + (1.96 * (sqrt(trends_imd$dsr_acs_acute))) # Upper bound 

# Plot (do seperately for each sex)
imd3_f <- ggplot(trends_imd[trends_imd$imd_quintile != 0 & trends_imd$sex == "F",]) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_ribbon(aes(x = date, ymin = dsr_acs_acute_low, ymax = dsr_acs_acute_upp, fill = factor(imd_quintile)), alpha = 0.2) + # Plot confidence intervals
  geom_line(aes(x = date, y = dsr_acs_acute, group = imd_quintile, color = factor(imd_quintile)), size = 1) + # Plot line for the rate
  #facet_wrap(~sex, labeller = labeller(sex = labels)) + # Stratify plot by sex
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  scale_colour_viridis_d(option = "viridis", begin = 0.1, end = 0.9, labels = c("1: Most deprived", "2", "3", "4", "5: Least Deprived")) + # Use colour blind friendly colours for lines
  scale_fill_viridis_d(option = "viridis", begin = 0.1, end = 0.9, labels = c("1: Most deprived", "2", "3", "4", "5: Least Deprived")) + # Use colour blind friendly colours for confidence intervals
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Deprivation quintile", fill = "Deprivation quintile") # Edit legend titles

imd3_m <- ggplot(trends_imd[trends_imd$imd_quintile != 0 & trends_imd$sex == "M",]) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_ribbon(aes(x = date, ymin = dsr_acs_acute_low, ymax = dsr_acs_acute_upp, fill = factor(imd_quintile)), alpha = 0.2) + # Plot confidence intervals
  geom_line(aes(x = date, y = dsr_acs_acute, group = imd_quintile, color = factor(imd_quintile)), size = 1) + # Plot line for the rate
  #facet_wrap(~sex, labeller = labeller(sex = labels)) + # Stratify plot by sex
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  scale_colour_viridis_d(option = "viridis", begin = 0.1, end = 0.9, labels = c("1: Most deprived", "2", "3", "4", "5: Least Deprived")) + # Use colour blind friendly colours for lines
  scale_fill_viridis_d(option = "viridis", begin = 0.1, end = 0.9, labels = c("1: Most deprived", "2", "3", "4", "5: Least Deprived")) + # Use colour blind friendly colours for confidence intervals
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Deprivation quintile", fill = "Deprivation quintile") # Edit legend titles

# Chronic avoidable hospitalisations #

# Create measures
trends_imd$dsr_acs_chronic <- (trends_imd$dexp_admitted_acs_chronic / trends_imd$std_pop) * 100000  # Rate (direct)
trends_imd$dsr_acs_chronic_low <- trends_imd$dsr_acs_chronic - (1.96 * (sqrt(trends_imd$dsr_acs_chronic))) # Lower bound 
trends_imd$dsr_acs_chronic_upp <- trends_imd$dsr_acs_chronic + (1.96 * (sqrt(trends_imd$dsr_acs_chronic))) # Upper bound 

# Plot (do seperately for each sex)
imd4_f <- ggplot(trends_imd[trends_imd$imd_quintile != 0 & trends_imd$sex == "F",]) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_ribbon(aes(x = date, ymin = dsr_acs_chronic_low, ymax = dsr_acs_chronic_upp, fill = factor(imd_quintile)), alpha = 0.2) + # Plot confidence intervals
  geom_line(aes(x = date, y = dsr_acs_chronic, group = imd_quintile, color = factor(imd_quintile)), size = 1) + # Plot line for the rate
  #facet_wrap(~sex, labeller = labeller(sex = labels)) + # Stratify plot by sex
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  scale_colour_viridis_d(option = "viridis", begin = 0.1, end = 0.9, labels = c("1: Most deprived", "2", "3", "4", "5: Least Deprived")) + # Use colour blind friendly colours for lines
  scale_fill_viridis_d(option = "viridis", begin = 0.1, end = 0.9, labels = c("1: Most deprived", "2", "3", "4", "5: Least Deprived")) + # Use colour blind friendly colours for confidence intervals
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Deprivation quintile", fill = "Deprivation quintile") # Edit legend titles

imd4_m <- ggplot(trends_imd[trends_imd$imd_quintile != 0 & trends_imd$sex == "M",]) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_ribbon(aes(x = date, ymin = dsr_acs_chronic_low, ymax = dsr_acs_chronic_upp, fill = factor(imd_quintile)), alpha = 0.2) + # Plot confidence intervals
  geom_line(aes(x = date, y = dsr_acs_chronic, group = imd_quintile, color = factor(imd_quintile)), size = 1) + # Plot line for the rate
  #facet_wrap(~sex, labeller = labeller(sex = labels)) + # Stratify plot by sex
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  scale_colour_viridis_d(option = "viridis", begin = 0.1, end = 0.9, labels = c("1: Most deprived", "2", "3", "4", "5: Least Deprived")) + # Use colour blind friendly colours for lines
  scale_fill_viridis_d(option = "viridis", begin = 0.1, end = 0.9, labels = c("1: Most deprived", "2", "3", "4", "5: Least Deprived")) + # Use colour blind friendly colours for confidence intervals
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Deprivation quintile", fill = "Deprivation quintile") # Edit legend titles


# Vaccine preventable avoidable hospitalisations #

# Create measures
trends_imd$dsr_acs_vaccine <- (trends_imd$dexp_admitted_acs_vaccine / trends_imd$std_pop) * 100000  # Rate (direct)
trends_imd$dsr_acs_vaccine_low <- trends_imd$dsr_acs_vaccine - (1.96 * (sqrt(trends_imd$dsr_acs_vaccine))) # Lower bound 
trends_imd$dsr_acs_vaccine_upp <- trends_imd$dsr_acs_vaccine + (1.96 * (sqrt(trends_imd$dsr_acs_vaccine))) # Upper bound 

# Plot (do seperately for each sex)
imd5_f <- ggplot(trends_imd[trends_imd$imd_quintile != 0 & trends_imd$sex == "F",]) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_ribbon(aes(x = date, ymin = dsr_acs_vaccine_low, ymax = dsr_acs_vaccine_upp, fill = factor(imd_quintile)), alpha = 0.2) + # Plot confidence intervals
  geom_line(aes(x = date, y = dsr_acs_vaccine, group = imd_quintile, color = factor(imd_quintile)), size = 1) + # Plot line for the rate
  #facet_wrap(~sex, labeller = labeller(sex = labels)) + # Stratify plot by sex
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  scale_colour_viridis_d(option = "viridis", begin = 0.1, end = 0.9, labels = c("1: Most deprived", "2", "3", "4", "5: Least Deprived")) + # Use colour blind friendly colours for lines
  scale_fill_viridis_d(option = "viridis", begin = 0.1, end = 0.9, labels = c("1: Most deprived", "2", "3", "4", "5: Least Deprived")) + # Use colour blind friendly colours for confidence intervals
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Deprivation quintile", fill = "Deprivation quintile") # Edit legend titles


imd5_m <- ggplot(trends_imd[trends_imd$imd_quintile != 0 & trends_imd$sex == "M",]) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_ribbon(aes(x = date, ymin = dsr_acs_vaccine_low, ymax = dsr_acs_vaccine_upp, fill = factor(imd_quintile)), alpha = 0.2) + # Plot confidence intervals
  geom_line(aes(x = date, y = dsr_acs_vaccine, group = imd_quintile, color = factor(imd_quintile)), size = 1) + # Plot line for the rate
  #facet_wrap(~sex, labeller = labeller(sex = labels)) + # Stratify plot by sex
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  scale_colour_viridis_d(option = "viridis", begin = 0.1, end = 0.9, labels = c("1: Most deprived", "2", "3", "4", "5: Least Deprived")) + # Use colour blind friendly colours for lines
  scale_fill_viridis_d(option = "viridis", begin = 0.1, end = 0.9, labels = c("1: Most deprived", "2", "3", "4", "5: Least Deprived")) + # Use colour blind friendly colours for confidence intervals
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Deprivation quintile", fill = "Deprivation quintile") # Edit legend titles

# Emergency urgent sensitive conditions #

# Create measures
trends_imd$dsr_acs_eucs <- (trends_imd$dexp_admitted_eucs / trends_imd$std_pop) * 100000  # Rate (direct)
trends_imd$dsr_acs_eucs_low <- trends_imd$dsr_acs_eucs - (1.96 * (sqrt(trends_imd$dsr_acs_eucs))) # Lower bound 
trends_imd$dsr_acs_eucs_upp <- trends_imd$dsr_acs_eucs + (1.96 * (sqrt(trends_imd$dsr_acs_eucs))) # Upper bound 

# Plot (do seperately for each sex)
imd6_f <- ggplot(trends_imd[trends_imd$imd_quintile != 0 & trends_imd$sex == "M",]) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_ribbon(aes(x = date, ymin = dsr_acs_eucs_low, ymax = dsr_acs_eucs_upp, fill = factor(imd_quintile)), alpha = 0.2) + # Plot confidence intervals
  geom_line(aes(x = date, y = dsr_acs_eucs, group = imd_quintile, color = factor(imd_quintile)), size = 1) + # Plot line for the rate
  #facet_wrap(~sex, labeller = labeller(sex = labels)) + # Stratify plot by sex
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  scale_colour_viridis_d(option = "viridis", begin = 0.1, end = 0.9, labels = c("1: Most deprived", "2", "3", "4", "5: Least Deprived")) + # Use colour blind friendly colours for lines
  scale_fill_viridis_d(option = "viridis", begin = 0.1, end = 0.9, labels = c("1: Most deprived", "2", "3", "4", "5: Least Deprived")) + # Use colour blind friendly colours for confidence intervals
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Deprivation quintile", fill = "Deprivation quintile") # Edit legend titles

imd6_m <- ggplot(trends_imd[trends_imd$imd_quintile != 0 & trends_imd$sex == "M",]) + # Define data source for plotting
  # Plot each lockdown period as shaded areas
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = Inf), color = NA, fill = "grey", alpha = 0.1) +
  # Main data to be plotted onwards
  geom_ribbon(aes(x = date, ymin = dsr_acs_eucs_low, ymax = dsr_acs_eucs_upp, fill = factor(imd_quintile)), alpha = 0.2) + # Plot confidence intervals
  geom_line(aes(x = date, y = dsr_acs_eucs, group = imd_quintile, color = factor(imd_quintile)), size = 1) + # Plot line for the rate
  #facet_wrap(~sex, labeller = labeller(sex = labels)) + # Stratify plot by sex
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  scale_colour_viridis_d(option = "viridis", begin = 0.1, end = 0.9, labels = c("1: Most deprived", "2", "3", "4", "5: Least Deprived")) + # Use colour blind friendly colours for lines
  scale_fill_viridis_d(option = "viridis", begin = 0.1, end = 0.9, labels = c("1: Most deprived", "2", "3", "4", "5: Least Deprived")) + # Use colour blind friendly colours for confidence intervals
  ylab("Rate per 100,000") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Deprivation quintile", fill = "Deprivation quintile") # Edit legend titles

# Combine into a single plot (seperate by sex)
overall_imd_plot_f <- imd1_f + imd2_f + imd3_f + imd4_f + imd5_f + imd6_f + # Combine these plots
  plot_annotation(tag_levels = 'A') + # Give plot labels
  plot_annotation(title = "Trends in female hospital admissions by neighbourhood deprivation",
                  subtitle = "Directly age-standardised admission rates per 100,000 population",
                  caption = "A = Total emergency hospital admissions, B = All ambulatory care admissions, \nC = Acute ambulatory care admissions, D = Chronic ambulatory care admissions, \nE = Vaccine-preventable ambulatory care admissions, F = Emergency urgent care sensitive conditions.") + # Add title and descriptions
  plot_layout(guides = "collect") & theme(legend.position = "bottom") # Use same legend and place at bottom
overall_imd_plot_f
ggsave(overall_imd_plot_f, filename = "../output/plots/imd_trends_f.jpeg")

overall_imd_plot_m <- imd1_m + imd2_m + imd3_m + imd4_m + imd5_m + imd6_m + # Combine these plots
  plot_annotation(tag_levels = 'A') + # Give plot labels
  plot_annotation(title = "Trends in male hospital admissions by neighbourhood deprivation",
                  subtitle = "Directly age-standardised admission rates per 100,000 population",
                  caption = "A = Total emergency hospital admissions, B = All ambulatory care admissions, \nC = Acute ambulatory care admissions, D = Chronic ambulatory care admissions, \nE = Vaccine-preventable ambulatory care admissions, F = Emergency urgent care sensitive conditions.") + # Add title and descriptions
  plot_layout(guides = "collect") & theme(legend.position = "bottom") # Use same legend and place at bottom
overall_imd_plot_m
ggsave(overall_imd_plot_m, filename = "../output/plots/imd_trends_m.jpeg")


# Estimate Slope Index of Inequality (SII) and Relative Index of Inequality (RII) #

# Overall hospital admissions
males <- phe_sii(data = group_by(trends_imd[trends_imd$imd_quintile != 0 & !is.na(trends_imd$dsr_admitted) & trends_imd$sex == "M",], date), quantile = imd_quintile, population = pop, value = dsr_admitted , value_type = 0, lower_cl = dsr_admitted_low , upper_cl = dsr_admitted_upp, confidence = 0.95, rii = TRUE, type = "standard") # Estimate SII for males 
males$sex <- "Males" # Add sex variable
females <- phe_sii(data = group_by(trends_imd[trends_imd$imd_quintile != 0 & !is.na(trends_imd$dsr_admitted) & trends_imd$sex == "F",], date), quantile = imd_quintile, population = pop, value = dsr_admitted , value_type = 0, lower_cl = dsr_admitted_low , upper_cl = dsr_admitted_upp, confidence = 0.95, rii = TRUE, type = "standard") # Repeat for females
females$sex <- "Females" # Add sex again
rii <- rbind(males, females) # Join two datasets together
rii$measure <- "All admissions" # Describe measure

# All avoidable hospitalisations
males <- phe_sii(data = group_by(trends_imd[trends_imd$imd_quintile != 0 & !is.na(trends_imd$dsr_acs_all) & trends_imd$sex == "M",], date), quantile = imd_quintile, population = pop, value = dsr_acs_all, value_type = 0, lower_cl = dsr_acs_low , upper_cl = dsr_acs_upp, confidence = 0.95, rii = TRUE, type = "standard") # Estimate SII for males 
males$sex <- "Males" # Add sex variable
females <- phe_sii(data = group_by(trends_imd[trends_imd$imd_quintile != 0 & !is.na(trends_imd$dsr_acs_all) & trends_imd$sex == "F",], date), quantile = imd_quintile, population = pop, value = dsr_acs_all, value_type = 0, lower_cl = dsr_acs_low , upper_cl = dsr_acs_upp, confidence = 0.95, rii = TRUE, type = "standard") # Repeat for females
females$sex <- "Females" # Add sex again
hold <- rbind(males, females) # Join two datasets together
hold$measure <- "All avoidable" # Describe measure
rii <- rbind(rii, hold) # Join together data

# Acute avoidable hospitalisations
males <- phe_sii(data = group_by(trends_imd[trends_imd$imd_quintile != 0 & !is.na(trends_imd$dsr_admitted) & trends_imd$sex == "M",], date), quantile = imd_quintile, population = pop, value = dsr_acs_acute , value_type = 0, lower_cl = dsr_acs_acute_low , upper_cl = dsr_acs_acute_upp, confidence = 0.95, rii = TRUE, type = "standard") # Estimate SII for males for overall hospital admissions
males$sex <- "Males" # Add sex variable
females <- phe_sii(data = group_by(trends_imd[trends_imd$imd_quintile != 0 & !is.na(trends_imd$dsr_admitted) & trends_imd$sex == "F",], date), quantile = imd_quintile, population = pop, value = dsr_acs_acute , value_type = 0, lower_cl = dsr_acs_acute_low , upper_cl = dsr_acs_acute_upp, confidence = 0.95, rii = TRUE, type = "standard") # Repeat for females
females$sex <- "Females" # Add sex again
hold <- rbind(males, females) # Join two datasets together
hold$measure <- "Acute avoidable" # Describe measure
rii <- rbind(rii, hold) # Join together data

# Chronic avoidable hospitalisations
males <- phe_sii(data = group_by(trends_imd[trends_imd$imd_quintile != 0 & !is.na(trends_imd$dsr_admitted) & trends_imd$sex == "M",], date), quantile = imd_quintile, population = pop, value = dsr_acs_chronic , value_type = 0, lower_cl = dsr_acs_chronic_low , upper_cl = dsr_acs_chronic_upp, confidence = 0.95, rii = TRUE, type = "standard") # Estimate SII for males 
males$sex <- "Males" # Add sex variable
females <- phe_sii(data = group_by(trends_imd[trends_imd$imd_quintile != 0 & !is.na(trends_imd$dsr_admitted) & trends_imd$sex == "F",], date), quantile = imd_quintile, population = pop, value = dsr_acs_chronic , value_type = 0, lower_cl = dsr_acs_chronic_low , upper_cl = dsr_acs_chronic_upp, confidence = 0.95, rii = TRUE, type = "standard") # Repeat for females
females$sex <- "Females" # Add sex again
hold <- rbind(males, females) # Join two datasets together
hold$measure <- "Chronic avoidable" # Describe measure
rii <- rbind(rii, hold) # Join together data

# Vaccine preventable avoidable hospitalisations
males <- phe_sii(data = group_by(trends_imd[trends_imd$imd_quintile != 0 & !is.na(trends_imd$dsr_admitted) & trends_imd$sex == "M",], date), quantile = imd_quintile, population = pop, value = dsr_acs_vaccine , value_type = 0, lower_cl = dsr_acs_vaccine_low , upper_cl = dsr_acs_vaccine_upp, confidence = 0.95, rii = TRUE, type = "standard") # Estimate SII for males 
males$sex <- "Males" # Add sex variable
females <- phe_sii(data = group_by(trends_imd[trends_imd$imd_quintile != 0 & !is.na(trends_imd$dsr_admitted) & trends_imd$sex == "F",], date), quantile = imd_quintile, population = pop, value = dsr_acs_vaccine , value_type = 0, lower_cl = dsr_acs_vaccine_low , upper_cl = dsr_acs_vaccine_upp, confidence = 0.95, rii = TRUE, type = "standard") # Repeat for females
females$sex <- "Females" # Add sex again
hold <- rbind(males, females) # Join two datasets together
hold$measure <- "Vaccine avoidable" # Describe measure
rii <- rbind(rii, hold) # Join together data

# Emergency urgent sensitive conditions
males <- phe_sii(data = group_by(trends_imd[trends_imd$imd_quintile != 0 & !is.na(trends_imd$dsr_admitted) & trends_imd$sex == "M",], date), quantile = imd_quintile, population = pop, value = dsr_acs_eucs , value_type = 0, lower_cl = dsr_acs_eucs_low , upper_cl = dsr_acs_eucs_upp, confidence = 0.95, rii = TRUE, type = "standard") # Estimate SII for males 
males$sex <- "Males" # Add sex variable
females <- phe_sii(data = group_by(trends_imd[trends_imd$imd_quintile != 0 & !is.na(trends_imd$dsr_admitted) & trends_imd$sex == "F",], date), quantile = imd_quintile, population = pop, value = dsr_acs_eucs , value_type = 0, lower_cl = dsr_acs_eucs_low , upper_cl = dsr_acs_eucs_upp, confidence = 0.95, rii = TRUE, type = "standard") # Repeat for females
females$sex <- "Females" # Add sex again
hold <- rbind(males, females) # Join two datasets together
hold$measure <- "Emergency Urgent" # Describe measure
rii <- rbind(rii, hold) # Join together data

# Plot - slope index of inequality
sii_plot <- ggplot(rii) +
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = -Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = -Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = -Inf), color = NA, fill = "grey", alpha = 0.1) +
  geom_ribbon(aes(x = date, ymin = sii_lower95_0cl, ymax = sii_upper95_0cl, group = measure, fill = measure), alpha = 0.1) +
  geom_line(aes(x = date, y = sii, group = measure, color = measure), size = 1) +
  facet_wrap(~sex) +
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  scale_colour_viridis_d(option = "viridis", begin = 0.1, end = 0.9, labels = c("Acute ambulatory", "All admissions", "All ambulatory", "Chronic ambulatory", "Emergency urgent", "Vaccine ambulatory")) +
  scale_fill_viridis_d(option = "viridis", begin = 0.1, end = 0.9, labels = c("Acute ambulatory", "All admissions", "All ambulatory", "Chronic ambulatory", "Emergency urgent", "Vaccine ambulatory")) +
  ylab("Slope Index of Inequality (SII)") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Measure", fill = "Measure") + # Edit legend titles
  coord_cartesian(ylim = c(-650,0)) # Zoom in to plot for ease of visualisation
sii_plot
ggsave(sii_plot, filename = "../output/plots/imd_sii.jpeg")
ggsave(sii_plot, filename = "../output/plots/figure2.jpeg", dpi = 300)

# Plot Relative Index of Inequality
rii_plot <- ggplot(rii) +
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2020-06-01"), ymin = 0, ymax = 1), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"), ymin = 0, ymax = 1), color = NA, fill = "grey", alpha = 0.1) +
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=as.Date("2021-03-08"), ymin = 0, ymax = 1), color = NA, fill = "grey", alpha = 0.1) +
  geom_ribbon(aes(x = date, ymin = rii_lower95_0cl, ymax = rii_upper95_0cl, group = measure, fill = measure), alpha = 0.1) +
  geom_line(aes(x = date, y = rii, group = measure, color = measure), size = 1) +
  facet_wrap(~sex) +
  scale_x_date(date_labels = "%Y") + # Only show the start of years for x-axis
  scale_colour_viridis_d(option = "viridis", begin = 0.1, end = 0.9, labels = c("Acute ambulatory", "All admissions", "All ambulatory", "Chronic ambulatory", "Emergency urgent", "Vaccine ambulatory")) +
  scale_fill_viridis_d(option = "viridis", begin = 0.1, end = 0.9, labels = c("Acute ambulatory", "All admissions", "All ambulatory", "Chronic ambulatory", "Emergency urgent", "Vaccine ambulatory")) +
  ylab("Relative Index of Inequality (RII)") + # Y-axis label
  xlab("Date") + # Define x-axis label
  labs(color = "Measure", fill = "Measure") + # Edit legend titles
  coord_cartesian(ylim = c(0.2,0.7)) # Zoom in to plot for ease of visualisation
rii_plot
ggsave(rii_plot, filename = "../output/plots/imd_rii.jpeg")

