
plot_correlation_consump_emissions = function(data) {
  plot = ggplot(filter(data, year == 2018), aes(x = conso / 1000, y = co2_t_h)) +
    geom_point(shape=".") +
    geom_smooth(method = "lm") +
    facet_wrap(~month_label) +
    labs(
      title = "Correlation between consumption and GHG emissions (2018)",
      x = "Consumption (GW)",
      y = "GHG emissions (t CO2eq/h)",
      caption = "Data: RTE 2018\nEmission factors (life-cycle analysis) from ADEME"
    )

  ggsave("./figures/correlation_consumption_emissions.png", width = unit(9, "cm"))

  return(plot)
}


