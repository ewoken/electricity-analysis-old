
plot_correlation_wind_export = function(data) {
  d = filter(data, year <= 2015)
  plot = ggplot(d, aes(x = wind, y = trade)) +
    geom_point(shape=".") +
    geom_smooth(method = "lm") +
    labs(
      title = "Correlation between french wind production and exportation (2012-2015)",
      subtitle = "In blue, regression line: y = -0.288x - 5927",
      x = "Wind production (MW)",
      y = "Physical exchanges (MW)",
      caption = "Data: RTE"
    )
  ggsave("./figures/correlation_wind_export.png", width = unit(9, "cm"))

  return(plot)
}

