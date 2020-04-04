stats_by_month = data %>%
  filter(year >= start_year & year <= end_year) %>%
  group_by(month) %>%
  summarise(
    biomass = sum(biomass),
    nuclear = sum(nuclear),
    gas = sum(gas),
    coal = sum(coal),
    oil = sum(oil),
    wind = sum(wind),
    solar = sum(solar),
    hydro = sum(hydro),
    import = sum(import),
    export = sum(export),
    prod = sum(prod),
    conso = sum(conso),
  ) %>%
  gather(-month, key = "type", value = "type_prod") %>%
  mutate(type_prod = type_prod * 0.5 * 10^(-6) / year_count) %>%
  mutate(month_label = month(month, TRUE, TRUE, locale="en_US"))

y = stats_by_month %>%
  filter(type != "conso" & type != "prod" & type != "export")

ggplot(y, aes(x = month_label, y = type_prod, fill = type)) +
  geom_col() +
  scale_fill_manual(values = prod_type_color) +
  labs(
    title = paste("Mean month production (", start_year, " - ", end_year, ")", sep=""),
    x = "",
    y = "Production (TWh)",
    fill = "Types",
    caption = paste("Data: RTE ", start_year, " - ", end_year, sep="")
  ) +
  ggsave('./figures/prod_by_month.png', width = unit(10, "cm"))

base = stats_by_month %>%
  filter(type != "conso" & type != "prod" & type != "export") %>%
  mutate(type_prod = if_else(type_prod < 0, -type_prod, type_prod)) %>%
  group_by(type) %>%
  summarise(type_base = min(type_prod))

base_month_prod = base %>%
  summarise(prod = sum(type_base)) %>%
  pull()

base_month_conso = stats_by_month %>%
  filter(type == "conso") %>%
  summarise(conso = min(type_prod)) %>%
  pull()

month_prod_t = tibble(month = seq(1, 12)) %>%
  mutate(type = "z_base") %>%
  mutate(month_label =  month(month, TRUE, TRUE, locale="en_US")) %>%
  mutate(prod = base_month_prod)

monthly_prod = stats_by_month %>%
  filter(type != "conso" & type != "prod" & type != "export") %>%
  left_join(base) %>%
  mutate(prod = type_prod - type_base) %>%
  select(-type_prod, -type_base) %>%
  bind_rows(month_prod_t)

aa = base %>%
  rename(prod = type_base) %>%
  mutate(month = 0) %>%
  mutate(month_label = "base")
graph_data = monthly_prod %>%
  bind_rows(aa)

ggplot(graph_data, aes(x = reorder(month_label, month), y = prod, fill = type)) +
  geom_col() +
  scale_fill_manual(values = prod_type_color) +
  labs(
    title = paste("Base & monthly production (", start_year, " - ", end_year, ")", sep=""),
    x = "",
    y = "Production (TWh)",
    fill = "Types",
    caption = paste("Data: RTE ", start_year, " - ", end_year, sep="")
  ) +
  ggsave('./figures/monthly_prod.png', width = unit(10, "cm"), height = unit(10, "cm"))

base_co2 = base %>%
  spread(key = "type", value = "type_base") %>%
  mutate(co2_mt =
    coal * 1.06 +
    gas * 0.418 +
    hydro * 0.006 +
    nuclear * 0.006 +
    oil * 0.73 +
    solar * 0.055 +
    wind * 0.013 +
    biomass * 0.494
  ) %>%
  mutate(base_mix = co2_mt / base_month_prod)

base_co2_mt =  base_co2 %>% pull(co2_mt)
base_mix = base_co2 %>% pull(base_mix)

month_co2 = data %>%
  filter(year >= start_year & year <= end_year) %>%
  group_by(month) %>%
  summarise(
    import = sum(import),
    co2_kg = sum(co2_kg),
    prod = sum(prod),
    conso = sum(conso),
  ) %>%
  gather(-month, -co2_kg, key = "type", value = "type_prod") %>%
  mutate(type_prod = type_prod * 0.5 * 10^(-6) / year_count) %>%
  spread(key = type, value = "type_prod") %>%
  mutate(co2_mt = co2_kg / 10^9 / year_count) %>%
  select(-co2_kg) %>%
  mutate(co2_conso_mt = (co2_mt / prod) * conso + import * 0.42) %>%
  mutate(month_label = month(month, TRUE, TRUE, locale="en_US")) %>%
  mutate(mean_mix = co2_mt / prod) %>%
  mutate(mean_conso_mix = co2_conso_mt / conso) %>%
  mutate(monthly_prod = prod - base_month_prod) %>%
  mutate(monthly_conso = conso - base_month_conso) %>%
  mutate(monthly_co2_mt = co2_mt - base_co2_mt) %>%
  mutate(monthly_mix = monthly_co2_mt / monthly_prod)

ggplot(month_co2, aes(x = month_label, y = monthly_mix)) +
  geom_col() +
  labs(
    title = paste("Monthly mix (", start_year, " - ", end_year, ")", sep=""),
    x = "",
    y = "GHG emissions (kg CO2eq/kWh)",
    caption = paste("Data: RTE ", start_year, " - ", end_year, sep="")
  ) +
  ggsave('./figures/monthly_mix.png', width = unit(10, "cm"), height = unit(10, "cm"))


ggplot(month_co2, aes(x = month_label, y = co2_conso_mt)) +
  geom_col() +
  labs(
    title = paste("Mean monthly GHG emissions (", start_year, " - ", end_year, ")", sep=""),
    x = "",
    y = "GHG emissions (Mt CO2eq)",
    caption = paste("Data: RTE ", start_year, " - ", end_year, sep="")
  ) +
  ggsave('./figures/month_conso_emissions.png', width = unit(10, "cm"), height = unit(10, "cm"))

ggplot(month_co2, aes(x = month_label, y = mean_mix)) +
  geom_col() +
  labs(
    title = paste("Mean month mix GHG emissions (", start_year, " - ", end_year, ")", sep=""),
    x = "",
    y = "GHG emissions (kg CO2eq/kWh)",
    caption = paste("Data: RTE ", start_year, " - ", end_year, sep="")
  ) +
  ggsave('./figures/month_emissions.png', width = unit(10, "cm"), height = unit(10, "cm"))

heating_factors = tibble(
  month = seq(1, 12),
  factor = c(0.23, 0.17, 0.13, 0.05, 0, 0, 0, 0, 0, 0.05, 0.16, 0.21)
)

mixs = month_co2 %>% pull(monthly_mix)
factors = heating_factors %>% pull(factor)
res2 = sum(mixs * factors)
print(paste('Method 1, heating impact:', res2, 'kg CO2eq/kWh'))


