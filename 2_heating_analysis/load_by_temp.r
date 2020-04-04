temp_breaks = c(-20, -4, 0, 4, 8, 12, 16)
temp_labels = paste(temp_breaks)[2:7]
data2 = data %>%
  filter(season == "winter") %>%
  mutate(temp_bin = cut(
    mean_temperature, breaks = temp_breaks, right = FALSE, labels = temp_labels)
  ) %>%
  filter(!is.na(temp_bin)) %>%
  mutate(hour = hour(date) + 0.) %>%
  mutate(hour = if_else(minute(date) <  30, hour, hour + 0.5)) %>%
  group_by(temp_bin, hour) %>%
  summarise(conso = mean(conso))

ggplot(data2, aes( x= hour, y = conso / 1000, color = temp_bin)) +
  geom_line(size=1.5) +
  scale_color_brewer(
    palette = "Spectral", direction = -1
  ) +
  labs(
    title = "Mean load on french grid in winter by temperature",
    x = "Day",
    y = "Load (GW)",
    color = "Mean temperature (°C)",
    caption = "Data: RTE 2012 - 2019, Météo France\nWinter: Oct. to May"
  ) +
  ggsave('./figures/load_by_temperature.png', width = unit(10, "cm"))
