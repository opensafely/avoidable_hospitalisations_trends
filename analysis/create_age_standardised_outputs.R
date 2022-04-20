######################################
### Create age-standardised trends ###
######################################


# Purpose: To age- and sex-standardise all monthly data and create output files.


# Load libraries
library(data.table)
library(readr)
library(here)



## 1. Load all data into R and tidy ##


# The code does this individually one month at a time - suggestions for more efficient code welcome!)

# January 2019
input <- read_csv(gsub("analysis", "", here("output/measures","input_2019-01-01.csv.gz"))) # Load
input$date <- "2019-01-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- input_imd # Resave object in what we will output (do for each)
output_region <- input_region
output_urbrur <- input_urbrur
rm(input_imd, input_region, input_urbrur) # Tidy

# February 2019
input <- read_csv(gsub("analysis", "", here("output/measures","input_2019-02-01.csv.gz"))) # Load
input$date <- "2019-02-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- rbind(output_imd, input_imd) # Add month records to output file (for each)
output_region <- rbind(output_region, input_region)
output_urbrur <- rbind(output_urbrur, input_urbrur)
rm(input_imd, input_region, input_urbrur) # Tidy

# March 2019
input <- read_csv(gsub("analysis", "", here("output/measures","input_2019-03-01.csv.gz"))) # Load
input$date <- "2019-03-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- rbind(output_imd, input_imd) # Add month records to output file (for each)
output_region <- rbind(output_region, input_region)
output_urbrur <- rbind(output_urbrur, input_urbrur)
rm(input_imd, input_region, input_urbrur) # Tidy

# April 2019
input <- read_csv(gsub("analysis", "", here("output/measures","input_2019-04-01.csv.gz"))) # Load
input$date <- "2019-04-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- rbind(output_imd, input_imd) # Add month records to output file (for each)
output_region <- rbind(output_region, input_region)
output_urbrur <- rbind(output_urbrur, input_urbrur)
rm(input_imd, input_region, input_urbrur) # Tidy

# May 2019
input <- read_csv(gsub("analysis", "", here("output/measures","input_2019-05-01.csv.gz"))) # Load
input$date <- "2019-05-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- rbind(output_imd, input_imd) # Add month records to output file (for each)
output_region <- rbind(output_region, input_region)
output_urbrur <- rbind(output_urbrur, input_urbrur)
rm(input_imd, input_region, input_urbrur) # Tidy

# June 2019
input <- read_csv(gsub("analysis", "", here("output/measures","input_2019-06-01.csv.gz"))) # Load
input$date <- "2019-06-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- rbind(output_imd, input_imd) # Add month records to output file (for each)
output_region <- rbind(output_region, input_region)
output_urbrur <- rbind(output_urbrur, input_urbrur)
rm(input_imd, input_region, input_urbrur) # Tidy

# July 2019
input <- read_csv(gsub("analysis", "", here("output/measures","input_2019-07-01.csv.gz"))) # Load
input$date <- "2019-07-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- rbind(output_imd, input_imd) # Add month records to output file (for each)
output_region <- rbind(output_region, input_region)
output_urbrur <- rbind(output_urbrur, input_urbrur)
rm(input_imd, input_region, input_urbrur) # Tidy

# August 2019
input <- read_csv(gsub("analysis", "", here("output/measures","input_2019-08-01.csv.gz"))) # Load
input$date <- "2019-08-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- rbind(output_imd, input_imd) # Add month records to output file (for each)
output_region <- rbind(output_region, input_region)
output_urbrur <- rbind(output_urbrur, input_urbrur)
rm(input_imd, input_region, input_urbrur) # Tidy

# September 2019
input <- read_csv(gsub("analysis", "", here("output/measures","input_2019-09-01.csv.gz"))) # Load
input$date <- "2019-09-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- rbind(output_imd, input_imd) # Add month records to output file (for each)
output_region <- rbind(output_region, input_region)
output_urbrur <- rbind(output_urbrur, input_urbrur)
rm(input_imd, input_region, input_urbrur) # Tidy

# October 2019
input <- read_csv(gsub("analysis", "", here("output/measures","input_2019-10-01.csv.gz"))) # Load
input$date <- "2019-10-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- rbind(output_imd, input_imd) # Add month records to output file (for each)
output_region <- rbind(output_region, input_region)
output_urbrur <- rbind(output_urbrur, input_urbrur)
rm(input_imd, input_region, input_urbrur) # Tidy

# November 2019
input <- read_csv(gsub("analysis", "", here("output/measures","input_2019-11-01.csv.gz"))) # Load
input$date <- "2019-11-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- rbind(output_imd, input_imd) # Add month records to output file (for each)
output_region <- rbind(output_region, input_region)
output_urbrur <- rbind(output_urbrur, input_urbrur)
rm(input_imd, input_region, input_urbrur) # Tidy

# December 2019
input <- read_csv(gsub("analysis", "", here("output/measures","input_2019-12-01.csv.gz"))) # Load
input$date <- "2019-12-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- rbind(output_imd, input_imd) # Add month records to output file (for each)
output_region <- rbind(output_region, input_region)
output_urbrur <- rbind(output_urbrur, input_urbrur)
rm(input_imd, input_region, input_urbrur) # Tidy

# January 2020
input <- read_csv(gsub("analysis", "", here("output/measures","input_2020-01-01.csv.gz"))) # Load
input$date <- "2020-01-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- rbind(output_imd, input_imd) # Add month records to output file (for each)
output_region <- rbind(output_region, input_region)
output_urbrur <- rbind(output_urbrur, input_urbrur)
rm(input_imd, input_region, input_urbrur) # Tidy

# February 2020
input <- read_csv(gsub("analysis", "", here("output/measures","input_2020-02-01.csv.gz"))) # Load
input$date <- "2020-02-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- rbind(output_imd, input_imd) # Add month records to output file (for each)
output_region <- rbind(output_region, input_region)
output_urbrur <- rbind(output_urbrur, input_urbrur)
rm(input_imd, input_region, input_urbrur) # Tidy

# March 2020
input <- read_csv(gsub("analysis", "", here("output/measures","input_2020-03-01.csv.gz"))) # Load
input$date <- "2020-03-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- rbind(output_imd, input_imd) # Add month records to output file (for each)
output_region <- rbind(output_region, input_region)
output_urbrur <- rbind(output_urbrur, input_urbrur)
rm(input_imd, input_region, input_urbrur) # Tidy

# April 2020
input <- read_csv(gsub("analysis", "", here("output/measures","input_2020-04-01.csv.gz"))) # Load
input$date <- "2020-04-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- rbind(output_imd, input_imd) # Add month records to output file (for each)
output_region <- rbind(output_region, input_region)
output_urbrur <- rbind(output_urbrur, input_urbrur)
rm(input_imd, input_region, input_urbrur) # Tidy

# May 2020
input <- read_csv(gsub("analysis", "", here("output/measures","input_2020-05-01.csv.gz"))) # Load
input$date <- "2020-05-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- rbind(output_imd, input_imd) # Add month records to output file (for each)
output_region <- rbind(output_region, input_region)
output_urbrur <- rbind(output_urbrur, input_urbrur)
rm(input_imd, input_region, input_urbrur) # Tidy

# June 2020
input <- read_csv(gsub("analysis", "", here("output/measures","input_2020-06-01.csv.gz"))) # Load
input$date <- "2020-06-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- rbind(output_imd, input_imd) # Add month records to output file (for each)
output_region <- rbind(output_region, input_region)
output_urbrur <- rbind(output_urbrur, input_urbrur)
rm(input_imd, input_region, input_urbrur) # Tidy

# July 2020
input <- read_csv(gsub("analysis", "", here("output/measures","input_2020-07-01.csv.gz"))) # Load
input$date <- "2020-07-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- rbind(output_imd, input_imd) # Add month records to output file (for each)
output_region <- rbind(output_region, input_region)
output_urbrur <- rbind(output_urbrur, input_urbrur)
rm(input_imd, input_region, input_urbrur) # Tidy

# August 2020
input <- read_csv(gsub("analysis", "", here("output/measures","input_2020-08-01.csv.gz"))) # Load
input$date <- "2020-08-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- rbind(output_imd, input_imd) # Add month records to output file (for each)
output_region <- rbind(output_region, input_region)
output_urbrur <- rbind(output_urbrur, input_urbrur)
rm(input_imd, input_region, input_urbrur) # Tidy

# September 2020
input <- read_csv(gsub("analysis", "", here("output/measures","input_2020-09-01.csv.gz"))) # Load
input$date <- "2020-09-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- rbind(output_imd, input_imd) # Add month records to output file (for each)
output_region <- rbind(output_region, input_region)
output_urbrur <- rbind(output_urbrur, input_urbrur)
rm(input_imd, input_region, input_urbrur) # Tidy

# October 2020
input <- read_csv(gsub("analysis", "", here("output/measures","input_2020-10-01.csv.gz"))) # Load
input$date <- "2020-10-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- rbind(output_imd, input_imd) # Add month records to output file (for each)
output_region <- rbind(output_region, input_region)
output_urbrur <- rbind(output_urbrur, input_urbrur)
rm(input_imd, input_region, input_urbrur) # Tidy

# November 2020
input <- read_csv(gsub("analysis", "", here("output/measures","input_2020-11-01.csv.gz"))) # Load
input$date <- "2020-11-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- rbind(output_imd, input_imd) # Add month records to output file (for each)
output_region <- rbind(output_region, input_region)
output_urbrur <- rbind(output_urbrur, input_urbrur)
rm(input_imd, input_region, input_urbrur) # Tidy

# December 2020
input <- read_csv(gsub("analysis", "", here("output/measures","input_2020-12-01.csv.gz"))) # Load
input$date <- "2020-12-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- rbind(output_imd, input_imd) # Add month records to output file (for each)
output_region <- rbind(output_region, input_region)
output_urbrur <- rbind(output_urbrur, input_urbrur)
rm(input_imd, input_region, input_urbrur) # Tidy

# January 2021
input <- read_csv(gsub("analysis", "", here("output/measures","input_2021-01-01.csv.gz"))) # Load
input$date <- "2021-01-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- rbind(output_imd, input_imd) # Add month records to output file (for each)
output_region <- rbind(output_region, input_region)
output_urbrur <- rbind(output_urbrur, input_urbrur)
rm(input_imd, input_region, input_urbrur) # Tidy

# February 2021
input <- read_csv(gsub("analysis", "", here("output/measures","input_2021-02-01.csv.gz"))) # Load
input$date <- "2021-02-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- rbind(output_imd, input_imd) # Add month records to output file (for each)
output_region <- rbind(output_region, input_region)
output_urbrur <- rbind(output_urbrur, input_urbrur)
rm(input_imd, input_region, input_urbrur) # Tidy

# March 2021
input <- read_csv(gsub("analysis", "", here("output/measures","input_2021-03-01.csv.gz"))) # Load
input$date <- "2021-03-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- rbind(output_imd, input_imd) # Add month records to output file (for each)
output_region <- rbind(output_region, input_region)
output_urbrur <- rbind(output_urbrur, input_urbrur)
rm(input_imd, input_region, input_urbrur) # Tidy

# April 2021
input <- read_csv(gsub("analysis", "", here("output/measures","input_2021-04-01.csv.gz"))) # Load
input$date <- "2021-04-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- rbind(output_imd, input_imd) # Add month records to output file (for each)
output_region <- rbind(output_region, input_region)
output_urbrur <- rbind(output_urbrur, input_urbrur)
rm(input_imd, input_region, input_urbrur) # Tidy

# May 2021
input <- read_csv(gsub("analysis", "", here("output/measures","input_2021-05-01.csv.gz"))) # Load
input$date <- "2021-05-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- rbind(output_imd, input_imd) # Add month records to output file (for each)
output_region <- rbind(output_region, input_region)
output_urbrur <- rbind(output_urbrur, input_urbrur)
rm(input_imd, input_region, input_urbrur) # Tidy

# June 2021
input <- read_csv(gsub("analysis", "", here("output/measures","input_2021-06-01.csv.gz"))) # Load
input$date <- "2021-06-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- rbind(output_imd, input_imd) # Add month records to output file (for each)
output_region <- rbind(output_region, input_region)
output_urbrur <- rbind(output_urbrur, input_urbrur)
rm(input_imd, input_region, input_urbrur) # Tidy

# July 2021
input <- read_csv(gsub("analysis", "", here("output/measures","input_2021-07-01.csv.gz"))) # Load
input$date <- "2021-07-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- rbind(output_imd, input_imd) # Add month records to output file (for each)
output_region <- rbind(output_region, input_region)
output_urbrur <- rbind(output_urbrur, input_urbrur)
rm(input_imd, input_region, input_urbrur) # Tidy

# August 2021
input <- read_csv(gsub("analysis", "", here("output/measures","input_2021-08-01.csv.gz"))) # Load
input$date <- "2021-08-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- rbind(output_imd, input_imd) # Add month records to output file (for each)
output_region <- rbind(output_region, input_region)
output_urbrur <- rbind(output_urbrur, input_urbrur)
rm(input_imd, input_region, input_urbrur) # Tidy

# September 2021
input <- read_csv(gsub("analysis", "", here("output/measures","input_2021-09-01.csv.gz"))) # Load
input$date <- "2021-09-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- rbind(output_imd, input_imd) # Add month records to output file (for each)
output_region <- rbind(output_region, input_region)
output_urbrur <- rbind(output_urbrur, input_urbrur)
rm(input_imd, input_region, input_urbrur) # Tidy

# October 2021
input <- read_csv(gsub("analysis", "", here("output/measures","input_2021-10-01.csv.gz"))) # Load
input$date <- "2021-10-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- rbind(output_imd, input_imd) # Add month records to output file (for each)
output_region <- rbind(output_region, input_region)
output_urbrur <- rbind(output_urbrur, input_urbrur)
rm(input_imd, input_region, input_urbrur) # Tidy

# November 2021
input <- read_csv(gsub("analysis", "", here("output/measures","input_2021-11-01.csv.gz"))) # Load
input$date <- "2021-11-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- rbind(output_imd, input_imd) # Add month records to output file (for each)
output_region <- rbind(output_region, input_region)
output_urbrur <- rbind(output_urbrur, input_urbrur)
rm(input_imd, input_region, input_urbrur) # Tidy

# December 2021
input <- read_csv(gsub("analysis", "", here("output/measures","input_2021-12-01.csv.gz"))) # Load
input$date <- "2021-12-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- rbind(output_imd, input_imd) # Add month records to output file (for each)
output_region <- rbind(output_region, input_region)
output_urbrur <- rbind(output_urbrur, input_urbrur)
rm(input_imd, input_region, input_urbrur) # Tidy

# January 2022
input <- read_csv(gsub("analysis", "", here("output/measures","input_2022-01-01.csv.gz"))) # Load
input$date <- "2022-01-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- rbind(output_imd, input_imd) # Add month records to output file (for each)
output_region <- rbind(output_region, input_region)
output_urbrur <- rbind(output_urbrur, input_urbrur)
rm(input_imd, input_region, input_urbrur) # Tidy

# February 2022
input <- read_csv(gsub("analysis", "", here("output/measures","input_2022-02-01.csv.gz"))) # Load
input$date <- "2022-02-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- rbind(output_imd, input_imd) # Add month records to output file (for each)
output_region <- rbind(output_region, input_region)
output_urbrur <- rbind(output_urbrur, input_urbrur)
rm(input_imd, input_region, input_urbrur) # Tidy

# March 2022
input <- read_csv(gsub("analysis", "", here("output/measures","input_2022-03-01.csv.gz"))) # Load
input$date <- "2022-03-01" # Add date
source(here("analysis", "age_standardise_month.R")) # Call age-standardisation script
output_imd <- rbind(output_imd, input_imd) # Add month records to output file (for each)
output_region <- rbind(output_region, input_region)
output_urbrur <- rbind(output_urbrur, input_urbrur)
rm(input_imd, input_region, input_urbrur) # Tidy



## 2. Save output ##

write.csv(output_imd, file = gsub("analysis", "", here("output/measures","standardised_imd_trends.csv")))
write.csv(output_region, file = gsub("analysis", "", here("output/measures","standardised_region_trends.csv")))
write.csv(output_urbrur, file = gsub("analysis", "", here("output/measures","standardised_urbrur_trends.csv")))

