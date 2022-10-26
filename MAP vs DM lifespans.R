#### MAP retention vs PMI DM median lifespan

library(tidyverse)
library(janitor)
library(readxl)


ls <- read_excel("/Users/hannahkoenker/Dropbox/A DHS MIS Datasets/Analysis/Durability Monitoring/data/Copy of MAP ITN retention data.xlsx", sheet="Sheet1") %>%
  clean_names() %>%
  drop_na(country) %>%
  rename(lifespan=median_retention_years) %>%
  separate(x95_percent_ci, into = c("lb", "ub"),
           sep = "\\s*(–|-)\\s*", convert = TRUE)


levels <- ls$country[ls$type=='MAP'][rev(order(ls$lifespan[ls$type=='MAP']))]
ls$country_ord <- factor(ls$country, levels = levels)


ls %>%
  ggplot() +
  geom_point(aes(
    y = lifespan,
    x = country_ord,
    color = type,
    alpha = both,
    shape = type
  )) +
  geom_linerange(aes(
    x = country_ord,
    ymin = lb,
    ymax = ub,
    color = type,
    alpha = both
  )) +
  theme_classic() +
  scale_alpha_continuous(range = c(0.25, 1), guide = "none") +
  scale_color_hue(name = "Study Type", labels = c("DM", "MAP")) +
  scale_shape_manual(
    name = "Study Type",
    labels = c("DM", "MAP"),
    values = c(19, 17)
  ) + # if guide=="none" here, shape won't appear correctly in legend
  theme(axis.text.x = element_text(
    angle = 45,
    vjust = 0.9,
    hjust = 1,
    size = 7
  )) +
  labs(x = "",
       y = "Retention / median lifespan")
ggsave("MAP_vs_DM_lifespans.png")




ls %>% 
  filter(type=="DM") %>%
  ggplot(aes(
    x = year,
    y = lifespan,
    color = country,
    ymin=lb, ymax=ub)) +
  geom_point(position = position_jitter(seed = 123, width =0.2)) +
  geom_linerange(position = position_jitter(seed = 123, width =0.2)) +
  theme_classic() +
  # scale_color_brewer(palette="Set3") +
  labs(x = "",
       y = "Estimated median lifespan after 36 months",
       color="")



