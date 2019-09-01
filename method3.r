library(tidyverse)
library(lubridate)

source("./utils.r")
source("./common_graphs.r")

data = get_all_data()

quartile_splits = c(0, 0.25, 0.5, 0.75, 1.1)
conso_labels = c("base (Q1)", "offpeak1 (Q2)", "offpeak2 (Q3)", "peak (Q4)")
wind_labels = c("still\n(Q1)", "windy\n(Q2)", "very windy\n(Q3)", "stormy\n(Q4)")
prod_type_color = c("chocolate4", "gray48", "dodgerblue3", "green4",
                    "firebrick", "darkorange", "darkorchid3", "hotpink1",
                    "gray48", "gray22", "firebrick4"
                    )
names(prod_type_color) = c("biomass", "fossil", "hydro", "nuclear",
                           "pumped_storage", "solar", "trade", "wind",
                           "oil", "coal", "gas")


data2 = data %>%
  group_by(season) %>%
  mutate(conso_rank = percent_rank(conso)) %>%
  mutate(conso_bin = cut(conso_rank, breaks = quartile_splits, right = FALSE, labels = conso_labels)) %>%
  ungroup() %>%
  group_by(season, conso_bin) %>%
  mutate(wind_rank = percent_rank(wind)) %>%
  mutate(wind_category = cut(wind_rank, breaks = quartile_splits, right = FALSE, labels = wind_labels)) %>%
  ungroup() %>%
  gather("solar", "wind", "hydro", "nuclear", "biomass", "trade", "fossil", "pumped_storage",
         key = "type", value = "type_prod")

prod_mean = data2 %>%
  group_by(conso_bin, type, wind_category, season) %>%
  summarise(type_prod = mean(type_prod), conso = mean(conso), co2_kg = mean(co2_kg), count = n()) %>%
  ungroup()

wind_prod_ref = prod_mean %>%
  filter(wind_category == wind_labels[1]) %>%
  select(conso_bin, type, season, type_prod, co2_kg) %>%
  rename(ref_prod = type_prod) %>%
  rename(ref_co2_kg = co2_kg)

mean_delta = prod_mean %>%
  filter(wind_category != wind_labels[1], type != "solar") %>%
  left_join(wind_prod_ref) %>%
  mutate(delta = type_prod - ref_prod)

ggplot(mean_delta, aes(x = wind_category , fill = type)) +
  geom_col(aes(y = delta), position = position_dodge()) +
  scale_fill_manual(values = prod_type_color) +
  facet_grid(cols= vars(conso_bin), rows = vars(season)) +
  labs(
    x = "Wind quartiles",
    y = "Change compared to wind Q1 (MW)",
    fill = "Types",
    caption = "Data: RTE 2012 - 2018\nSummer: May-Sept, Winter: Oct-April"
  )
ggsave("./figures/wind_prod_impact.png", width = unit(9, "cm"))

mean_delta_co2 = prod_mean %>%
  filter(wind_category != wind_labels[1], type == "wind") %>%
  left_join(wind_prod_ref) %>%
  mutate(avoided = -(co2_kg - ref_co2_kg) / (type_prod - ref_prod) / 500)

ggplot(mean_delta_co2, aes(x = wind_category, y = avoided)) +
  geom_col() +
  facet_grid(cols= vars(conso_bin), rows = vars(season)) +
  labs(
    x = "Wind quartiles",
    y = "Avoided emissions per wind production (kg CO2eq/kWh)",
    caption = "Data: RTE 2012 - 2018\nEmission factors (life-cycle analysis) from ADEME\nSummer: May-Sept, Winter: Oct-April"
  )
ggsave("./figures/wind_emissions_impact.png", width=unit(9, "cm"))

weighted.mean(mean_delta_co2$avoided, mean_delta_co2$count)

ggplot(data, aes(x = solar, y = conso)) +
  geom_point(shape=".") + 
  geom_smooth(method = "lm") +
  facet_wrap(~season)
  
  
  
  
