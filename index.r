source('./utils.r')
data = get_country_data('france')

data = data %>%
    add_co2_kg()

ggplot(data, aes(x = paste(year), y = co2_kg_kwh)) +
    geom_boxplot(outlier.shape = NA) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(
        title = 'Carbon intensity of french electricity',
        x = 'Year',
        y = 'Carbon intensity (kg CO2eq/kWh)',
        caption = 'Source: RTE 2012 - 2019'
    ) +
    ggsave('./figures/carbon_intensity_by_year.png', width = unit(10, 'cm'))

