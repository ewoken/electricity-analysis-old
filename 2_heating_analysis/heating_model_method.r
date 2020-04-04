
data2 = data %>%
    filter(year >= start_year & year <= end_year) %>%
    mutate(co2_kg = co2_kg * conso / prod  + 500 * import * 0.42)
start_temp = 17

day_conso_temperature = data2 %>%
    group_by(day_date) %>%
    summarise(
        conso = sum(conso) * 0.5, # MWh
        mean_temperature = first(mean_temperature),
        reference_temperature = first(reference_temperature),
        week_day = first(week_day)
    )

e = day_conso_temperature %>%
    filter(mean_temperature <= start_temp & mean_temperature - reference_temperature < -4)
t = e %>% pull(mean_temperature)
c = e %>% pull(conso)
res = lm(c ~ t)
coeff = res$coefficients[2] / 24
intercept = res$coefficients[1]

ggplot(day_conso_temperature, aes(x = mean_temperature, y = conso / 10^6)) +
    geom_point(shape=".") +
    geom_abline(intercept = intercept / 10^6, slope = coeff * 24 / 10^6 , color = "blue") +
    labs(
        title = paste('Day consumption by temperature', " (", start_year, " - ", end_year, ")", sep=""),
        subtitle = paste('Temperature gradient:', format(coeff, digits = 3), 'MW/°C'),
        x = "Temperature (°C)",
        y = "Consumption (TWh)",
        caption = paste("Data: RTE ", start_year, " - ", end_year, sep="")
    ) +
    ggsave('./figures/day_consumption_by_temp.png', width = unit(10, "cm"))

data3 = data2 %>%
    mutate(real_conso = conso) %>%
    mutate(conso = if_else(mean_temperature < start_temp, conso - coeff * (mean_temperature - start_temp), conso))

day_conso_temperature2 = data3 %>%
    group_by(day_date) %>%
    summarise(
        conso = sum(conso) * 0.5, # MWh
        mean_temperature = first(mean_temperature)
    )
ggplot(day_conso_temperature2, aes(x = mean_temperature, y = conso / 10^6)) +
    geom_point(shape=".") +
    geom_abline(intercept = intercept / 10^6, slope=0, color = "blue") +
    labs(
        title = paste('Day consumption less heating model by temperature ', " (", start_year, " - ", end_year, ")", sep=""),
        x = "Temperature (°C)",
        y = "Consumption (TWh)",
        caption = paste("Data: RTE ", start_year, " - ", end_year, sep="")
    ) +
    ggsave('./figures/day_consumption_by_temp_less_heating.png', width = unit(10, "cm"))

cur_year = 2018
co2_heating = data2 %>%
    filter(year >= cur_year - 3 & year <= cur_year) %>%
    group_by(day_date) %>%
    summarise(
        co2_kg = sum(co2_kg),
        mean_temperature = first(mean_temperature)
    ) %>%
    mutate(heating_conso_kwh = if_else(mean_temperature <= start_temp, 1000 * 24 * coeff * (mean_temperature - start_temp), 0))

e = co2_heating %>%
    filter(mean_temperature <= start_temp)
h = e %>% pull(heating_conso_kwh)
c = e %>% pull(co2_kg)
res = lm(c ~ h)

ggplot(co2_heating, aes(x = heating_conso_kwh / 10^6, y = co2_kg / 10^6)) +
    geom_point(shape=".") +
    geom_abline(intercept = res$coefficients[1] / 10^6, slope=res$coefficients[2], color = "blue") +
    labs(
        title = paste('GHG emissions by heating consumption', " (", start_year, " - ", end_year, ")", sep=""),
        subtitle = "each point = 1 day",
        x = "Heating consumption (GWh)",
        y = "GHG emissions (kt CO2eq)",
        caption = paste("Data: RTE ", start_year, " - ", end_year, sep="")
    ) +
    ggsave('./figures/emissions_by_heating.png', width = unit(10, "cm"))

# divided by 0.9 due to loss in transport 
print(paste('Method 2, heating impact:', res$coefficients[2] / 0.9 , ' kg CO2eq/kWh'))
