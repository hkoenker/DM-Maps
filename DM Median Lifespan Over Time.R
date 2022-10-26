################################################################ 
#### DM median lifespan over time
##
## Author: Hannah Koenker
## Created: October 26, 2022
## Data: estimated median survival in serviceable condition ("lifespan") of ITNs from durability monitoring studies
##        gathered from study reports (pmi.gov), from durabilitymonitoring.org, published literature.
##        List of studies is not exhaustive as durabilitymonitoring.org does not track any DM funded through Global Fund.
##        Not all study reports/publications report the outcome of interest; not all completed activities have public reports.
## Notes: some ongoing durability monitoring sites/brands are listed but don't have lifespan estimates yet
## GPS: GPS points in the excel file are copy/pasted from Google Maps near the study site. 
##      They are indicative only and do not represent any GPS points from study households.
################################################################

################
# Load libraries

library(tidyverse)
library(janitor)
library(readxl)

################
# Read in data

ls2 <- read_excel("Median Lifespan DM Multisite.xlsx") %>%
  clean_names() %>% 
  rename(year=end,
         lifespan=medianlifespan) %>% 
  separate(x95_percent_ci, into = c("lb", "ub"),
           sep = "\\s*(–|-)\\s*", convert = TRUE)

################
# Plot graph by country and year

ls2 %>% 
  ggplot(aes(
    x=year,
    y=lifespan,
    color=country,
    ymin=lb, ymax=ub)) +
  geom_point(position = position_jitter(seed = 123, width =0.2)) +
  geom_linerange(position = position_jitter(seed = 123, width =0.2), alpha=.25) +
  scale_x_continuous(breaks = seq(2010,2022, by=1)) +
  theme_classic() +
  theme(legend.position="none") + 
  labs(x = "",
       y = "Estimated median lifespan after 36 months",
       color="")

################
# Save plot

ggsave("DM_lifespans_by_year.png")
