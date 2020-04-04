t = data %>%
    filter(year == 2012, day_date != "2013-01-01") %>%
    select(day_date, mean_temperature, reference_temperature) %>%
    group_by(day_date) %>%
    summarise(
        mean_temperature = first(mean_temperature),
        reference_temperature = first(reference_temperature)
    ) %>%
    mutate(base = pmin(mean_temperature, reference_temperature)) %>%
    mutate(top = pmax(mean_temperature, reference_temperature))

Sys.setlocale(locale = "en_US")
ggplot(t, aes(x = day_date, y = reference_temperature)) +
    geom_ribbon(aes(ymin=base, ymax = reference_temperature), fill = "blue") +
    geom_ribbon(aes(ymin=reference_temperature, ymax = top), fill = "orange") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    geom_line(size = 1.5) +
    geom_line(y = 15, color = 'red') +
    labs(
        title = "Mean temperature in France - 2012",
        x = "",
        y = "Temperature (°C)",
        caption = "Data: Météo France"
    ) +
        ggsave('./figures/temp_var.png', width = unit(10, "cm"))
