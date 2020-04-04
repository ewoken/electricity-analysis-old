t = data %>%
  mutate(hour = hour(date))

ggplot(t, aes( x= mean_temperature, y = hour)) +
  stat_summary_2d(aes(z = co2_kg_kwh), binwidth = c(0.6, 1)) +
  scale_fill_gradientn(
    colours = colorRampPalette(c("#5bb55e", "yellow", "brown"), bias = 5)(4)
  ) +
  labs(
    title = "GHG emissions of french electricity production by hour & temperature",
    x = "Mean temperature (°C)",
    y = "Day hour",
    fill = "GHG Emissions\n(kg CO2eq/kWh)",
    caption = "Data: RTE 2012 - 2019, Météo France\nWinter: Oct. to May"
  ) +
  ggsave('./figures/emission_by_temp_hour.png', width = unit(10, "cm"))
