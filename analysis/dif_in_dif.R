############################################
### Trends in avoidable hospitalisations ###
######## Part 6: Dif-in-dif model ##########
############################################

# Purpose: To examine running a difference-in-differences regression to see if a better way of presenting trends in inequalities.

# Libraries
library(data.table)
library(ggplot2)
library(viridis)
library(scales)
library(ggplot2)

# Define functions to allow negative values to be plot on square root axis
S_sqrt <- function(x){sign(x)*sqrt(abs(x))}
IS_sqrt <- function(x){x^2*sign(x)}
S_sqrt_trans <- function() trans_new("S_sqrt",S_sqrt,IS_sqrt)



## 1. Clean data ##

# Load data
trends_imd <- fread("../output/measures/standardised_imd_trends.csv") # Load deprivation data
trends_imd <- trends_imd[trends_imd$date < "2022-04-01"] # Drop last period

trends_eth <- fread("../output/measures/standardised_ethnicity_trends.csv") # Load ethnicity data
trends_eth <- trends_eth[trends_eth$date < "2022-04-01"] # Drop last period

trends_region <- fread("../output/measures/standardised_region_trends.csv") # Load regional data
trends_region <- trends_region[trends_region$date < "2022-04-01"] # Drop last period

# Convert to date format
trends_imd$date <- as.Date(trends_imd$date) # Start with deprivation data
trends_eth$date <- as.Date(trends_eth$date) # Repeat for ethnicity data
trends_region$date <- as.Date(trends_region$date) # Then for region data

# Shift dates to mid-point of month since 1st of month refers to whole month (akes plots nicer to look at)
trends_imd$date <- trends_imd$date + 14 # Do for each dataset one-by-one
trends_eth$date <- trends_eth$date + 14
trends_region$date <- trends_region$date + 14

# Create variable to denote pre-post-pandemic
trends_imd$time <- 0 # Create blank variable in deprivation data
trends_imd$time[trends_imd$date > "2020-02-15"] <- 1 # All values after start of pandemic effect

trends_eth$time <- 0 # Create blank variable in ethnicity data
trends_eth$time[trends_eth$date > "2020-02-15"] <- 1 # All values after start of pandemic effect

trends_region$time <- 0 # Create blank variable in region data
trends_region$time[trends_region$date > "2020-02-15"] <- 1 # All values after start of pandemic effect

# Revise exposure variables as factors
trends_imd$imd_quintile <- as.factor(trends_imd$imd_quintile) # Convert to factor
trends_imd$imd_quintile <- relevel(trends_imd$imd_quintile, ref = "5") # Set least deprived quintile as reference groups
trends_eth$ethnicity <- as.factor(trends_eth$ethnicity) # Repeat process for each dataset
trends_region$region <- as.factor(trends_region$region)
trends_region$region <- relevel(trends_region$region, ref = "South East") # Set South East to reference group

# Create outcome variables as directly standardised rates (per 100,000)
# Start with deprivation data
trends_imd$dsr_admitted <- (trends_imd$dexp_admitted / trends_imd$std_pop) * 100000 # All admissions
trends_imd$dsr_acs_all <- (trends_imd$dexp_admitted_acs_all / trends_imd$std_pop) * 100000 # All ambulatory care admissions
trends_imd$dsr_acs_acute <- (trends_imd$dexp_admitted_acs_acute / trends_imd$std_pop) * 100000 # All acute ambulatory care
trends_imd$dsr_acs_chronic <- (trends_imd$dexp_admitted_acs_chronic / trends_imd$std_pop) * 100000 # All chronic ambulatory care
trends_imd$dsr_acs_vaccine <- (trends_imd$dexp_admitted_acs_vaccine / trends_imd$std_pop) * 100000 # All vaccine preventable ambulatory care
trends_imd$dsr_acs_eucs <- (trends_imd$dexp_admitted_eucs / trends_imd$std_pop) * 100000 # All emergency urgent care

trends_eth$dsr_admitted <- (trends_eth$dexp_admitted / trends_eth$std_pop) * 100000 # Repeat for ethnicity data
trends_eth$dsr_acs_all <- (trends_eth$dexp_admitted_acs_all / trends_eth$std_pop) * 100000
trends_eth$dsr_acs_acute <- (trends_eth$dexp_admitted_acs_acute / trends_eth$std_pop) * 100000 
trends_eth$dsr_acs_chronic <- (trends_eth$dexp_admitted_acs_chronic / trends_eth$std_pop) * 100000 
trends_eth$dsr_acs_vaccine <- (trends_eth$dexp_admitted_acs_vaccine / trends_eth$std_pop) * 100000 
trends_eth$dsr_acs_eucs <- (trends_eth$dexp_admitted_eucs / trends_eth$std_pop) * 100000

trends_region$dsr_admitted <- (trends_region$dexp_admitted / trends_region$std_pop) * 100000 # Repeat for region data
trends_region$dsr_acs_all <- (trends_region$dexp_admitted_acs_all / trends_region$std_pop) * 100000
trends_region$dsr_acs_acute <- (trends_region$dexp_admitted_acs_acute / trends_region$std_pop) * 100000 
trends_region$dsr_acs_chronic <- (trends_region$dexp_admitted_acs_chronic / trends_region$std_pop) * 100000 
trends_region$dsr_acs_vaccine <- (trends_region$dexp_admitted_acs_vaccine / trends_region$std_pop) * 100000 
trends_region$dsr_acs_eucs <- (trends_region$dexp_admitted_eucs / trends_region$std_pop) * 100000

## 2. Regression models ##

# Deprivation #

# Run regression model for each outcome one-by-one
# Males
model1m <- lm(dsr_admitted ~ time + imd_quintile + time*imd_quintile, data = trends_imd[trends_imd$sex == "M" & trends_imd$imd_quintile != "0"]) # All admissions
model2m <- lm(dsr_acs_all ~ time + imd_quintile + time*imd_quintile, data = trends_imd[trends_imd$sex == "M" & trends_imd$imd_quintile != "0"]) # All ambulatory care admissions
model3m <- lm(dsr_acs_acute ~ time + imd_quintile + time*imd_quintile, data = trends_imd[trends_imd$sex == "M" & trends_imd$imd_quintile != "0"]) # All acute ambulatory care
model4m <- lm(dsr_acs_chronic ~ time + imd_quintile + time*imd_quintile, data = trends_imd[trends_imd$sex == "M" & trends_imd$imd_quintile != "0"]) # All chronic ambulatory care
model5m <- lm(dsr_acs_vaccine ~ time + imd_quintile + time*imd_quintile, data = trends_imd[trends_imd$sex == "M" & trends_imd$imd_quintile != "0"]) # All vaccine preventable ambulatory care
model6m <- lm(dsr_acs_eucs ~ time + imd_quintile + time*imd_quintile, data = trends_imd[trends_imd$sex == "M" & trends_imd$imd_quintile != "0"]) # All emergency urgent care

# Females
model1f <- lm(dsr_admitted ~ time + imd_quintile + time*imd_quintile, data = trends_imd[trends_imd$sex == "F" & trends_imd$imd_quintile != "0"]) # All admissions
model2f <- lm(dsr_acs_all ~ time + imd_quintile + time*imd_quintile, data = trends_imd[trends_imd$sex == "F" & trends_imd$imd_quintile != "0"]) # All ambulatory care admissions
model3f <- lm(dsr_acs_acute ~ time + imd_quintile + time*imd_quintile, data = trends_imd[trends_imd$sex == "F" & trends_imd$imd_quintile != "0"]) # All acute ambulatory care
model4f <- lm(dsr_acs_chronic ~ time + imd_quintile + time*imd_quintile, data = trends_imd[trends_imd$sex == "F" & trends_imd$imd_quintile != "0"]) # All chronic ambulatory care
model5f <- lm(dsr_acs_vaccine ~ time + imd_quintile + time*imd_quintile, data = trends_imd[trends_imd$sex == "F" & trends_imd$imd_quintile != "0"]) # All vaccine preventable ambulatory care
model6f <- lm(dsr_acs_eucs ~ time + imd_quintile + time*imd_quintile, data = trends_imd[trends_imd$sex == "F" & trends_imd$imd_quintile != "0"]) # All emergency urgent care


# Extract key results - model-by-model
# Males
table <- cbind(summary(model1m)$coefficients[7:10,], confint(model1m)[7:10,], data.frame(Outcome = "All admissions", quintile = c(1,2,3,4))) # Save regression model summary statistics
table[nrow(table) + 1,] <- 0 # Add on bottom row to represent values for the reference group
table$Outcome[nrow(table)] <- "All admissions" # Add outcome variable
table$quintile[nrow(table)] <- 5 # Add reference group category
table$Outcome <- "All emergency admissions" # rename
dep_table_m <- table # Save table as unique object

table <- cbind(summary(model2m)$coefficients[7:10,], confint(model2m)[7:10,], data.frame(Outcome = "Any ambulatory", quintile = c(1,2,3,4))) # Repeat
table[nrow(table) + 1,] <- 0 
table$Outcome[nrow(table)] <- "Any ambulatory" 
table$quintile[nrow(table)] <- 5 
dep_table_m <- rbind(dep_table_m, table) # Join onto main table

table <- cbind(summary(model3m)$coefficients[7:10,], confint(model3m)[7:10,], data.frame(Outcome = "Acute ambulatory", quintile = c(1,2,3,4))) # Repeat
table[nrow(table) + 1,] <- 0 
table$Outcome[nrow(table)] <- "Acute ambulatory" 
table$quintile[nrow(table)] <- 5 
dep_table_m <- rbind(dep_table_m, table)

table <- cbind(summary(model4m)$coefficients[7:10,], confint(model4m)[7:10,], data.frame(Outcome = "Chronic ambulatory", quintile = c(1,2,3,4))) # Repeat
table[nrow(table) + 1,] <- 0 
table$Outcome[nrow(table)] <- "Chronic ambulatory" 
table$quintile[nrow(table)] <- 5 
dep_table_m <- rbind(dep_table_m, table)

table <- cbind(summary(model5m)$coefficients[7:10,], confint(model5m)[7:10,], data.frame(Outcome = "Vaccine-preventable ambulatory", quintile = c(1,2,3,4))) # Repeat
table[nrow(table) + 1,] <- 0 
table$Outcome[nrow(table)] <- "Vaccine-preventable ambulatory" 
table$quintile[nrow(table)] <- 5 
dep_table_m <- rbind(dep_table_m, table)

table <- cbind(summary(model6m)$coefficients[7:10,], confint(model6m)[7:10,], data.frame(Outcome = "EUCS", quintile = c(1,2,3,4))) # Repeat
table[nrow(table) + 1,] <- 0 
table$Outcome[nrow(table)] <- "EUCS" 
table$quintile[nrow(table)] <- 5 
dep_table_m <- rbind(dep_table_m, table)

rm(model1m, model2m, model3m, model4m, model5m, model6m) # Tidy

# Females
table <- cbind(summary(model1f)$coefficients[7:10,], confint(model1f)[7:10,], data.frame(Outcome = "All admissions", quintile = c(1,2,3,4))) # Save regression model summary statistics
table[nrow(table) + 1,] <- 0 # Add on bottom row to represent values for the reference group
table$Outcome[nrow(table)] <- "All emergency admissions" # Add outcome variable
table$quintile[nrow(table)] <- 5 # Add reference group category
table$Outcome <- "All emergency admissions" # rename
dep_table_f <- table # Save table as unique object

table <- cbind(summary(model2f)$coefficients[7:10,], confint(model2f)[7:10,], data.frame(Outcome = "Any ambulatory", quintile = c(1,2,3,4))) # Repeat
table[nrow(table) + 1,] <- 0 
table$Outcome[nrow(table)] <- "Any ambulatory" 
table$quintile[nrow(table)] <- 5 
dep_table_f <- rbind(dep_table_f, table) # Join onto main table

table <- cbind(summary(model3f)$coefficients[7:10,], confint(model3f)[7:10,], data.frame(Outcome = "Acute ambulatory", quintile = c(1,2,3,4))) # Repeat
table[nrow(table) + 1,] <- 0 
table$Outcome[nrow(table)] <- "Acute ambulatory" 
table$quintile[nrow(table)] <- 5 
dep_table_f <- rbind(dep_table_f, table)

table <- cbind(summary(model4f)$coefficients[7:10,], confint(model4f)[7:10,], data.frame(Outcome = "Chronic ambulatory", quintile = c(1,2,3,4))) # Repeat
table[nrow(table) + 1,] <- 0 
table$Outcome[nrow(table)] <- "Chronic ambulatory" 
table$quintile[nrow(table)] <- 5 
dep_table_f <- rbind(dep_table_f, table)

table <- cbind(summary(model5f)$coefficients[7:10,], confint(model5f)[7:10,], data.frame(Outcome = "Vaccine-preventable ambulatory", quintile = c(1,2,3,4))) # Repeat
table[nrow(table) + 1,] <- 0 
table$Outcome[nrow(table)] <- "Vaccine-preventable ambulatory" 
table$quintile[nrow(table)] <- 5 
dep_table_f <- rbind(dep_table_f, table)

table <- cbind(summary(model6f)$coefficients[7:10,], confint(model6f)[7:10,], data.frame(Outcome = "EUCS", quintile = c(1,2,3,4))) # Repeat
table[nrow(table) + 1,] <- 0 
table$Outcome[nrow(table)] <- "EUCS" 
table$quintile[nrow(table)] <- 5 
dep_table_f <- rbind(dep_table_f, table)

rm(model1f, model2f, model3f, model4f, model5f, model6f) # Tidy

# Plot results 
dep_table_f$sex <- "Female" # Add sex
dep_table_m$sex <- "Male"
dep_table <- rbind(dep_table_f, dep_table_m) # Join together
dep_table$Outcome <- factor(dep_table$Outcome, levels = c("EUCS", "Vaccine-preventable ambulatory", "Chronic ambulatory", "Acute ambulatory", "Any ambulatory" , "All emergency admissions")) # Change plotting order following reviewer suggestion

dep_plot <- ggplot(dep_table) +
  geom_point(aes(x = Outcome, y = Estimate, group = factor(quintile), color = factor(quintile)), position=position_dodge(width = 0.5), size = 1) +
  geom_linerange(aes(x = Outcome, y = Estimate, ymin = `2.5 %`, ymax = `97.5 %`, group = factor(quintile), color = factor(quintile)), lwd = 0.5, position=position_dodge(width = 0.5)) +
  scale_y_continuous(trans="S_sqrt", limits=c(-200,100)) +  # Plot using square root axis 
  facet_wrap(vars(sex)) +
  labs(color = "Quintile") +
  scale_colour_viridis_d(option = "mako", begin = 0.1, end = 0.9, labels = c("1 Most deprived", "2", "3", "4", "5 Least Deprived")) +
  #ylim(-150,50) +
  coord_flip()

rm(dep_table_f, dep_table_m) # Tidy

ggsave(plot = dep_plot, filename = "./figure2_lowres.jpeg")
ggsave(plot = dep_plot, filename = "./figure2.jpeg", dpi = 300)


# Ethnicity #
# Males
# Note we remove 0 as missing value and 5 is 'other' ethnicity which we don't report as less helpful
model1m <- lm(dsr_admitted ~ time + ethnicity + time*ethnicity, data = trends_eth[trends_eth$sex == "M" & trends_eth$ethnicity != "0" & trends_eth$ethnicity != "5"]) # All admissions
model2m <- lm(dsr_acs_all ~ time + ethnicity + time*ethnicity, data = trends_eth[trends_eth$sex == "M" & trends_eth$ethnicity != "0" & trends_eth$ethnicity != "5"]) # All ambulatory care admissions
model3m <- lm(dsr_acs_acute ~ time + ethnicity + time*ethnicity, data = trends_eth[trends_eth$sex == "M" & trends_eth$ethnicity != "0" & trends_eth$ethnicity != "5"]) # All acute ambulatory care
model4m <- lm(dsr_acs_chronic ~ time + ethnicity + time*ethnicity, data = trends_eth[trends_eth$sex == "M" & trends_eth$ethnicity != "0" & trends_eth$ethnicity != "5"]) # All chronic ambulatory care
model5m <- lm(dsr_acs_vaccine ~ time + ethnicity + time*ethnicity, data = trends_eth[trends_eth$sex == "M" & trends_eth$ethnicity != "0" & trends_eth$ethnicity != "5"]) # All vaccine preventable ambulatory care
model6m <- lm(dsr_acs_eucs ~ time + ethnicity + time*ethnicity, data = trends_eth[trends_eth$sex == "M" & trends_eth$ethnicity != "0" & trends_eth$ethnicity != "5"]) # All emergency urgent care

# Females
model1f <- lm(dsr_admitted ~ time + ethnicity + time*ethnicity, data = trends_eth[trends_eth$sex == "F" & trends_eth$ethnicity != "0" & trends_eth$ethnicity != "5"]) # All admissions
model2f <- lm(dsr_acs_all ~ time + ethnicity + time*ethnicity, data = trends_eth[trends_eth$sex == "F" & trends_eth$ethnicity != "0" & trends_eth$ethnicity != "5"]) # All ambulatory care admissions
model3f <- lm(dsr_acs_acute ~ time + ethnicity + time*ethnicity, data = trends_eth[trends_eth$sex == "F" & trends_eth$ethnicity != "0" & trends_eth$ethnicity != "5"]) # All acute ambulatory care
model4f <- lm(dsr_acs_chronic ~ time + ethnicity + time*ethnicity, data = trends_eth[trends_eth$sex == "F" & trends_eth$ethnicity != "0" & trends_eth$ethnicity != "5"]) # All chronic ambulatory care
model5f <- lm(dsr_acs_vaccine ~ time + ethnicity + time*ethnicity, data = trends_eth[trends_eth$sex == "F" & trends_eth$ethnicity != "0" & trends_eth$ethnicity != "5"]) # All vaccine preventable ambulatory care
model6f <- lm(dsr_acs_eucs ~ time + ethnicity + time*ethnicity, data = trends_eth[trends_eth$sex == "F" & trends_eth$ethnicity != "0" & trends_eth$ethnicity != "5"]) # All emergency urgent care

# Extract key results - model-by-model
# Males
table <- cbind(summary(model1m)$coefficients[6:8,], confint(model1m)[6:8,], data.frame(Outcome = "All admissions", ethnicity = c(2,3,4))) # Save regression model summary statistics
table[nrow(table) + 1,] <- 0 # Add on bottom row to represent values for the reference group
table$Outcome[nrow(table)] <- "All admissions" # Add outcome variable
table$quintile[nrow(table)] <- 1 # Add reference group category
table$Outcome <- "All emergency admissions" # rename
eth_table_m <- table # Save table as unique object

table <- cbind(summary(model2m)$coefficients[6:8,], confint(model2m)[6:8,], data.frame(Outcome = "Any ambulatory", ethnicity = c(2,3,4))) # Repeat
table[nrow(table) + 1,] <- 0 
table$Outcome[nrow(table)] <- "Any ambulatory" 
table$quintile[nrow(table)] <- 1 
eth_table_m <- rbind(eth_table_m, table) # Join onto main table

table <- cbind(summary(model3m)$coefficients[6:8,], confint(model3m)[6:8,], data.frame(Outcome = "Acute ambulatory", ethnicity = c(2,3,4))) # Repeat
table[nrow(table) + 1,] <- 0 
table$Outcome[nrow(table)] <- "Acute ambulatory" 
table$quintile[nrow(table)] <- 1 
eth_table_m <- rbind(eth_table_m, table)

table <- cbind(summary(model4m)$coefficients[6:8,], confint(model4m)[6:8,], data.frame(Outcome = "Chronic ambulatory", ethnicity = c(2,3,4))) # Repeat
table[nrow(table) + 1,] <- 0 
table$Outcome[nrow(table)] <- "Chronic ambulatory" 
table$quintile[nrow(table)] <- 1 
eth_table_m <- rbind(eth_table_m, table)

table <- cbind(summary(model5m)$coefficients[6:8,], confint(model5m)[6:8,], data.frame(Outcome = "Vaccine-preventable ambulatory", ethnicity = c(2,3,4))) # Repeat
table[nrow(table) + 1,] <- 0 
table$Outcome[nrow(table)] <- "Vaccine-preventable ambulatory" 
table$quintile[nrow(table)] <- 1 
eth_table_m <- rbind(eth_table_m, table)

table <- cbind(summary(model6m)$coefficients[6:8,], confint(model6m)[6:8,], data.frame(Outcome = "EUCS", ethnicity = c(2,3,4))) # Repeat
table[nrow(table) + 1,] <- 0 
table$Outcome[nrow(table)] <- "EUCS" 
table$quintile[nrow(table)] <- 1 
eth_table_m <- rbind(eth_table_m, table)

rm(model1m, model2m, model3m, model4m, model5m, model6m) # Tidy

# Females
table <- cbind(summary(model1f)$coefficients[6:8,], confint(model1f)[6:8,], data.frame(Outcome = "All admissions", ethnicity = c(2,3,4))) # Save regression model summary statistics
table[nrow(table) + 1,] <- 0 # Add on bottom row to represent values for the reference group
table$Outcome[nrow(table)] <- "All admissions" # Add outcome variable
table$quintile[nrow(table)] <- 1 # Add reference group category
table$Outcome <- "All emergency admissions" # rename
eth_table_f <- table # Save table as unique object

table <- cbind(summary(model2f)$coefficients[6:8,], confint(model2f)[6:8,], data.frame(Outcome = "Any ambulatory", ethnicity = c(2,3,4))) # Repeat
table[nrow(table) + 1,] <- 0 
table$Outcome[nrow(table)] <- "Any ambulatory" 
table$quintile[nrow(table)] <- 1 
eth_table_f <- rbind(eth_table_f, table) # Join onto main table

table <- cbind(summary(model3f)$coefficients[6:8,], confint(model3f)[6:8,], data.frame(Outcome = "Acute ambulatory", ethnicity = c(2,3,4))) # Repeat
table[nrow(table) + 1,] <- 0 
table$Outcome[nrow(table)] <- "Acute ambulatory" 
table$quintile[nrow(table)] <- 1 
eth_table_f <- rbind(eth_table_f, table)

table <- cbind(summary(model4f)$coefficients[6:8,], confint(model4f)[6:8,], data.frame(Outcome = "Chronic ambulatory", ethnicity = c(2,3,4))) # Repeat
table[nrow(table) + 1,] <- 0 
table$Outcome[nrow(table)] <- "Chronic ambulatory" 
table$quintile[nrow(table)] <- 1 
eth_table_f <- rbind(eth_table_f, table)

table <- cbind(summary(model5f)$coefficients[6:8,], confint(model5f)[6:8,], data.frame(Outcome = "Vaccine-preventable ambulatory", ethnicity = c(2,3,4))) # Repeat
table[nrow(table) + 1,] <- 0 
table$Outcome[nrow(table)] <- "Vaccine-preventable ambulatory" 
table$quintile[nrow(table)] <- 1 
eth_table_f <- rbind(eth_table_f, table)

table <- cbind(summary(model6f)$coefficients[6:8,], confint(model6f)[6:8,], data.frame(Outcome = "EUCS", ethnicity = c(2,3,4))) # Repeat
table[nrow(table) + 1,] <- 0 
table$Outcome[nrow(table)] <- "EUCS" 
table$quintile[nrow(table)] <- 1 
eth_table_f <- rbind(eth_table_f, table)

rm(model1f, model2f, model3f, model4f, model5f, model6f) # Tidy

# Plot results 
eth_table_f$sex <- "Female" # Add sex
eth_table_m$sex <- "Male"
eth_table <- rbind(eth_table_f, eth_table_m) # Join together
eth_table$Outcome <- factor(eth_table$Outcome, levels = c("EUCS", "Vaccine-preventable ambulatory", "Chronic ambulatory", "Acute ambulatory", "Any ambulatory" , "All emergency admissions")) # Change plotting order following reviewer suggestion

eth_plot <- ggplot(eth_table) +
  geom_point(aes(x = Outcome, y = Estimate, group = factor(ethnicity), color = factor(ethnicity)), position=position_dodge(width = 0.5), size = 1) +
  geom_linerange(aes(x = Outcome, y = Estimate, ymin = `2.5 %`, ymax = `97.5 %`, group = factor(ethnicity), color = factor(ethnicity)), lwd = 0.5, position=position_dodge(width = 0.5)) +
  scale_y_continuous(trans="S_sqrt", limits=c(-200,100)) +  # Plot using square root axis
  facet_wrap(vars(sex)) +
  labs(color = "Ethnic group") +
  scale_colour_viridis_d(option = "plasma", begin = 0.1, end = 0.9, labels = c("White", "Mixed", "Asian", "Black")) +
  #ylim(-150,80) +
  coord_flip()

rm(eth_table_f, eth_table_m) # Tidy

ggsave(plot = eth_plot, filename = "./figure3_lowres.jpeg") # Save
ggsave(plot = eth_plot, filename = "./figure3.jpeg", dpi = 300)


# Region #

# Regression models
# Males
model1m <- lm(dsr_admitted ~ time + region + time*region, data = trends_region[trends_region$sex == "M"]) # All admissions
model2m <- lm(dsr_acs_all ~ time + region + time*region, data = trends_region[trends_region$sex == "M"]) # All ambulatory care admissions
model3m <- lm(dsr_acs_acute ~ time + region + time*region, data = trends_region[trends_region$sex == "M"]) # All acute ambulatory care
model4m <- lm(dsr_acs_chronic ~ time + region + time*region, data = trends_region[trends_region$sex == "M"]) # All chronic ambulatory care
model5m <- lm(dsr_acs_vaccine ~ time + region + time*region, data = trends_region[trends_region$sex == "M"]) # All vaccine preventable ambulatory care
model6m <- lm(dsr_acs_eucs ~ time + region + time*region, data = trends_region[trends_region$sex == "M"]) # All emergency urgent care

# Females
model1f <- lm(dsr_admitted ~ time + region + time*region, data = trends_region[trends_region$sex == "F"]) # All admissions
model2f <- lm(dsr_acs_all ~ time + region + time*region, data = trends_region[trends_region$sex == "F"]) # All ambulatory care admissions
model3f <- lm(dsr_acs_acute ~ time + region + time*region, data = trends_region[trends_region$sex == "F"]) # All acute ambulatory care
model4f <- lm(dsr_acs_chronic ~ time + region + time*region, data = trends_region[trends_region$sex == "F"]) # All chronic ambulatory care
model5f <- lm(dsr_acs_vaccine ~ time + region + time*region, data = trends_region[trends_region$sex == "F"]) # All vaccine preventable ambulatory care
model6f <- lm(dsr_acs_eucs ~ time + region + time*region, data = trends_region[trends_region$sex == "F"]) # All emergency urgent care

# Extract key results - model-by-model
# Males
table <- cbind(summary(model1m)$coefficients[11:18,], confint(model1m)[11:18,], data.frame(Outcome = "All admissions", region = c("East", "East Midlands", "London", "North East", "North West", "South West", "West Midlands", "Yorkshire"))) # Save regression model summary statistics
table[nrow(table) + 1,] <- 0 # Add on bottom row to represent values for the reference group
table$Outcome[nrow(table)] <- "All admissions" # Add outcome variable
table$region[nrow(table)] <- "South East"# Add reference group category
table$Outcome <- "All emergency admissions" # rename
reg_table_m <- table # Save table as unique object

table <- cbind(summary(model2m)$coefficients[11:18,], confint(model2m)[11:18,], data.frame(Outcome = "Any ambulatory", region = c("East", "East Midlands", "London", "North East", "North West", "South West", "West Midlands", "Yorkshire"))) # Repeat
table[nrow(table) + 1,] <- 0 
table$Outcome[nrow(table)] <- "Any ambulatory" 
table$region[nrow(table)] <- "South East"
reg_table_m <- rbind(reg_table_m, table) # Join onto main table

table <- cbind(summary(model3m)$coefficients[11:18,], confint(model3m)[11:18,], data.frame(Outcome = "Acute ambulatory", region = c("East", "East Midlands", "London", "North East", "North West", "South West", "West Midlands", "Yorkshire"))) # Repeat
table[nrow(table) + 1,] <- 0 
table$Outcome[nrow(table)] <- "Acute ambulatory" 
table$region[nrow(table)] <- "South East"
reg_table_m <- rbind(reg_table_m, table)

table <- cbind(summary(model4m)$coefficients[11:18,], confint(model4m)[11:18,], data.frame(Outcome = "Chronic ambulatory", region = c("East", "East Midlands", "London", "North East", "North West", "South West", "West Midlands", "Yorkshire"))) # Repeat
table[nrow(table) + 1,] <- 0 
table$Outcome[nrow(table)] <- "Chronic ambulatory" 
table$region[nrow(table)] <- "South East"
reg_table_m <- rbind(reg_table_m, table)

table <- cbind(summary(model5m)$coefficients[11:18,], confint(model5m)[11:18,], data.frame(Outcome = "Vaccine-preventable ambulatory", region = c("East", "East Midlands", "London", "North East", "North West", "South West", "West Midlands", "Yorkshire"))) # Repeat
table[nrow(table) + 1,] <- 0 
table$Outcome[nrow(table)] <- "Vaccine-preventable ambulatory" 
table$region[nrow(table)] <- "South East"
reg_table_m <- rbind(reg_table_m, table)

table <- cbind(summary(model6m)$coefficients[11:18,], confint(model6m)[11:18,], data.frame(Outcome = "EUCS", region = c("East", "East Midlands", "London", "North East", "North West", "South West", "West Midlands", "Yorkshire"))) # Repeat
table[nrow(table) + 1,] <- 0 
table$Outcome[nrow(table)] <- "EUCS" 
table$region[nrow(table)] <- "South East"
reg_table_m <- rbind(reg_table_m, table)

rm(model1m, model2m, model3m, model4m, model5m, model6m) # Tidy

# Females
table <- cbind(summary(model1f)$coefficients[11:18,], confint(model1f)[11:18,], data.frame(Outcome = "All admissions", region = c("East", "East Midlands", "London", "North East", "North West", "South West", "West Midlands", "Yorkshire"))) # Save regression model summary statistics
table[nrow(table) + 1,] <- 0 # Add on bottom row to represent values for the reference group
table$Outcome[nrow(table)] <- "All admissions" # Add outcome variable
table$region[nrow(table)] <- "South East"# Add reference group category
table$Outcome <- "All emergency admissions" # rename
reg_table_f <- table # Save table as unique object

table <- cbind(summary(model2f)$coefficients[11:18,], confint(model2f)[11:18,], data.frame(Outcome = "Any ambulatory", region = c("East", "East Midlands", "London", "North East", "North West", "South West", "West Midlands", "Yorkshire"))) # Repeat
table[nrow(table) + 1,] <- 0 
table$Outcome[nrow(table)] <- "Any ambulatory" 
table$region[nrow(table)] <- "South East"
reg_table_f <- rbind(reg_table_f, table) # Join onto main table

table <- cbind(summary(model3f)$coefficients[11:18,], confint(model3f)[11:18,], data.frame(Outcome = "Acute ambulatory", region = c("East", "East Midlands", "London", "North East", "North West", "South West", "West Midlands", "Yorkshire"))) # Repeat
table[nrow(table) + 1,] <- 0 
table$Outcome[nrow(table)] <- "Acute ambulatory" 
table$region[nrow(table)] <- "South East"
reg_table_f <- rbind(reg_table_f, table)

table <- cbind(summary(model4f)$coefficients[11:18,], confint(model4f)[11:18,], data.frame(Outcome = "Chronic ambulatory", region = c("East", "East Midlands", "London", "North East", "North West", "South West", "West Midlands", "Yorkshire"))) # Repeat
table[nrow(table) + 1,] <- 0 
table$Outcome[nrow(table)] <- "Chronic ambulatory" 
table$region[nrow(table)] <- "South East"
reg_table_f <- rbind(reg_table_f, table)

table <- cbind(summary(model5f)$coefficients[11:18,], confint(model5f)[11:18,], data.frame(Outcome = "Vaccine-preventable ambulatory", region = c("East", "East Midlands", "London", "North East", "North West", "South West", "West Midlands", "Yorkshire"))) # Repeat
table[nrow(table) + 1,] <- 0 
table$Outcome[nrow(table)] <- "Vaccine-preventable ambulatory" 
table$region[nrow(table)] <- "South East"
reg_table_f <- rbind(reg_table_f, table)

table <- cbind(summary(model6f)$coefficients[11:18,], confint(model6f)[11:18,], data.frame(Outcome = "EUCS", region = c("East", "East Midlands", "London", "North East", "North West", "South West", "West Midlands", "Yorkshire"))) # Repeat
table[nrow(table) + 1,] <- 0 
table$Outcome[nrow(table)] <- "EUCS" 
table$region[nrow(table)] <- "South East" 
reg_table_f <- rbind(reg_table_f, table)

rm(model1f, model2f, model3f, model4f, model5f, model6f) # Tidy

# Plot results 
reg_table_f$sex <- "Female" # Add sex
reg_table_m$sex <- "Male"
reg_table <- rbind(reg_table_f, reg_table_m) # Join together
reg_table$Outcome <- factor(reg_table$Outcome, levels = c("EUCS", "Vaccine-preventable ambulatory", "Chronic ambulatory", "Acute ambulatory", "Any ambulatory" , "All emergency admissions")) # Change plotting order following reviewer suggestion
#reg_table$region2 <- factor(ifelse(is.na(reg_table$region), "South East", paste(reg_table$region)), levels = c(levels(reg_table$region), "South East")) # If missing then it should be South East so replace

reg_table$region <- factor(reg_table$region, levels=c("South East", "South West", "London", "East", "West Midlands", "East Midlands", "Yorkshire", "North West", "North East")) # Change order for plotting purpose (order by North to South)

reg_plot <- ggplot(reg_table) +
  geom_point(aes(x = Outcome, y = Estimate, group = factor(region), color = factor(region)), position=position_dodge(width = 0.5), size = 1) +
  geom_linerange(aes(x = Outcome, y = Estimate, ymin = `2.5 %`, ymax = `97.5 %`, group = factor(region), color = factor(region)), lwd = 0.5, position=position_dodge(width = 0.5)) +
  scale_y_continuous(trans="S_sqrt", breaks=seq(-200,100,100)) + # Plot using square root axis
  facet_wrap(vars(sex)) +
  labs(color = "Region") +
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9) +
  #ylim(-200,100) +
  coord_flip()

ggsave(plot = reg_plot, filename = "./figure4_lowres.jpeg")
ggsave(plot = reg_plot, filename = "./figure4.jpeg", dpi = 300)

rm(reg_table_f, reg_table_m) # Tidy




