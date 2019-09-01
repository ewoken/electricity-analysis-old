plot_results_all_months = function(results, folder) {
  d = results %>%
    filter(year != ALL_YEARS, month < ALL_MONTHS, yname == "co2_kg")

  plot = ggplot(d, aes(x = date, y = coeff)) +
    geom_path(group = 1, alpha = 0.2) +
    geom_point(mapping = aes(color = r2)) +
    scale_color_gradient(low = "red", high = "green", limits = c(0, 1)) +
    scale_x_date(
      date_labels = "%m/%y",
      breaks = "6 months",
      minor_breaks = "1 month",
      limits = as.Date(c("2012-01-01", "2018-12-01"))
    ) +
    facet_grid(rows = vars(xname)) +
    labs(
      y = "Avoided emissions (kg CO2eq/kWh)",
      color = "R²"
    ) +
    theme(axis.title.x=element_blank())

  ggsave(
    paste("./figures/", folder, "/impact_by_month.png", sep=""),
    width = unit(10, "cm")
  )

  return (plot)
}

plot_results_group_by_month = function(results, folder, cur_xname = "", title) {
  d = results %>%
    filter(year == ALL_YEARS, month < ALL_MONTHS, yname == "co2_kg")

  if (cur_xname != "") {
    d = filter(d, xname == cur_xname)
  }

  plot = ggplot(d, aes(x = month_label, y = -coeff)) +
    geom_path(group=1, alpha = 0.2) +
    geom_point(mapping = aes(color = r2)) +
    scale_color_gradient(low = "red", high = "green", limits=c(0,1)) +
    labs(
      y = "Avoided emissions (kg CO2eq/kWh)",
      x = "",
      color = "R²",
      caption = "Data: RTE 2012 - 2018"
    ) +
    theme(axis.title.x=element_blank())

  if (cur_xname == "") {
    plot = plot + facet_grid(rows = vars(xname))
  }

  if (cur_xname == "wind") {
    plot = plot + scale_y_continuous(limits = c(0, NA))
  }

  ggsave(
    paste("./figures/", folder, "/impact_group_by_month.png", sep=""),
    width = unit(8, "cm")
  )

  return(plot)
}

plot_results_by_season = function(results, folder, cur_xname = "") {
  d = results %>%
    filter(year != ALL_YEARS, month >= SUMMER, yname == "co2_kg") %>%
    mutate(date = if_else(month==SUMMER, paste((year - 2000), "S"), paste((year - 2000), "W")))
  
  if (cur_xname != "") {
    d = d %>% filter(xname == cur_xname)
  }

  plot = ggplot(d, aes(x = date, y = coeff)) +
    geom_path(group=1, alpha = 0.2) +
    geom_point(mapping = aes(color = r2)) +
    scale_color_gradient(low = "red", high = "green", limits= c(0, 1)) +
    labs(
      y = "Avoided emissions (kg CO2eq/kWh)",
      x = " ",
      color = "R²",
      caption = "Data: RTE 2012 - 2018\nEmission factors (life-cycle analysis) from ADEME"
    ) +
    theme(axis.title.x=element_blank())

  if (cur_xname == "") {
    plot = plot + facet_grid(rows = vars(xname))
  }
  
  ggsave(
    paste("./figures/", folder, "/impact_by_season.png", sep=""),
    width=unit(8, "cm")
  )

  return(plot)
}
