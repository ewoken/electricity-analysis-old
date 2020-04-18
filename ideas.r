source('./utils.r')
data = get_country_data('france')

data %>%
    filter(year > 2013) %>%
    add_marginal_export_conso_co2_kg(ADEME_GHG_FACTORS) %>%
    select(year, conso_co2_kg) %>%
    mutate(conso_co2_kg = conso_co2_kg / 10^6) %>%
    arrange(year, desc(conso_co2_kg)) %>%
    group_by(year) %>%
    group_modify(~ {
        .x %>%
            mutate(i = (row_number() - 1) / 2) %>%
            mutate(cumsum = cumsum(conso_co2_kg))
    }) %>%
    ggplot(aes(x = i, y = cumsum, color = paste(year))) +
    geom_line()

LOSS_RATE = 0.1
week_emission_factors = data %>%
    filter(year > 2013) %>%
    mutate(week = week(date)) %>%
    add_marginal_export_conso_co2_kg(ADEME_GHG_FACTORS) %>%
    group_by(week) %>%
    summarise(
        conso = sum(conso) * MW_TO_KWH / 10^9,
        conso_co2_mt = sum(conso_co2_kg) / 10^9,
    ) %>%
    mutate(co2_kg_kwh = conso_co2_mt / conso * (1 + LOSS_RATE)) %>%
    mutate(date = ymd('2019-01-01') + weeks(week - 1))

week_emission_factors %>%
    ggplot(aes(x = date, y = co2_kg_kwh)) +
    geom_line() +
    scale_x_date(date_breaks = '1 month', date_labels = "%b") +
    labs(
        title = "Mean emissions factor of french electricity consumption per week",
        x = "",
        y = "Emissions factor (kg CO2eq/kWh)",
        caption = "Source: RTE 2014 - 2019"
    )



