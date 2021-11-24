###########################
### Create example plot ###
###########################

# Load libraries
library(ggplot2)

df_input <- read.csv(here::here("output", "input.csv"))

plot_age <- ggplot(data=df_input, aes(age)) + 
  geom_histogram() +
  ylab("Frequency") +
  xlab("Age")

ggsave(plot = plot_age, filename = "descriptive.png", path = here::here("output"))
