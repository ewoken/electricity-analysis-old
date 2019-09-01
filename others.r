plotSeasonProd = function(varname, yName, cur_title, myColor) {
    summup = data %>%
        select(year, varname, month) %>%
        filter(year==2018) %>%
        group_by(month, ) %>%
        summarise(prod = sum(get(varname)) * 0.0005 )

    ggplot(summup, mapping = aes(x=month, y = prod )) +
        geom_col(fill = myColor) +
        scale_y_continuous(limits=c(0, NA)) +
        labs(
            x=NULL,
            y=paste(yName, " (GWh)", sep=""),
            title=cur_title,
            caption = "Source: RTE"
        )

    ggsave(paste("./figures/seasonProd/", varname, ".png", sep=""))
}

plotSeasonProd("solar", "Solar production", "Solar production in 2018", "orange")
plotSeasonProd("wind", "Wind production", "Wind production in 2018", "dodgerblue4")
plotSeasonProd("fossil", "Fosiil production", "Fossil production in 2018", "gray20")
plotSeasonProd("hydro", "Fosiil production", "Fossil production in 2018", "dodgerblue2")

# =============================================================================


ggplot(data, aes(x=wind_load_factor, y = stat(count) / (2 * length(years)) )) +
    geom_freqpoly(binwidth = 0.02) +
    labs(
        x="Wind load factor",
        y="Hours count",
        title="Distribution of wind load factor (2012 - 2018)",
        caption = "Source: RTE"
    )

ggsave('./figures/wind_distrib.png')

# =============================================================================
d = data %>%
        filter(year == 2018) %>%
        select(conso, trade) %>%
        mutate(total = conso - trade) %>%
        arrange(desc(conso)) %>%
        filter((row_number() %% 2 ) == 1) %>%
        rownames_to_column(var = "index") %>%
        mutate_at(vars(index), as.numeric) %>%
        arrange(desc(total)) %>%
        rownames_to_column(var = "index2") %>%
        mutate_at(vars(index2), as.numeric)

ggplot(d) +
    # geom_line(mapping = aes(x= index2, y = total, group=1), color = "green") +
    geom_line(mapping = aes(x= index, y = conso, group=1), color = "blue") +
    scale_y_continuous(limits=c(0, NA)) +
    labs(
      x="Heures",
      y="Consommation (MW)",
      title="Monotone de charge 2018",
      caption = "Source: RTE"
    )
ggsave('./figures/monotone_conso.png')

#   scale_x_date(
#     date_labels = "%m/%y",
#     breaks = "6 months",
#     minor_breaks = "1 month",
#     limits = as.Date(c("2012-01-01", "2018-12-01"))
#   ) +

ggplot(d, aes(x = month, y = coeff_solar, ymin = coeff_solar_low, ymax = coeff_solar_high)) +
  geom_pointrange(shape=20) + #position= position_dodge(width=0.9)
  geom_path(group=1)
  # geom_path(group=1) +
  # geom_vline(xintercept=seq(1.5, 11.5), colour='black') +
  # scale_color_brewer(palette = "Set1") +
  #facet_wrap(~yName)


data2 = filter(data, year == 2013, monthIndex == 1, hour(date) >= 8, hour(date) <= 10)
x = unlist(data2["conso"])
x2 = x * x
y = unlist(data2["co2_kg"])
reg = lm(y ~ x2 + x)

t = seq(min(x), max(x), 1000)
u = reg$coefficients[2]*t*t + reg$coefficients[3]*t + reg$coefficients[1]
predi = tibble(c = t, pred = u)

ggplot(data2, aes(x=conso, y = co2_kg)) +
  geom_point(aes(color = hour(date))) +
  geom_line(data = predi, mapping = aes(x = c, y = pred), color = "red")
  #geom_smooth(aes(color = as.character(year), group = as.character(year)))


e = data %>%
  group_by(year, season) %>%
  summarise(co2_kg = sum(co2_kg, na.rm = TRUE))
f = results %>%
    filter(month >= SUMMER, year != ALL_YEARS, xname == "wind") %>%
    mutate(season = if_else(month == SUMMER, "summer", "winter")) %>%
    left_join(e) %>%
    mutate(date = if_else(month==SUMMER, paste((year - 2000), "S"), paste((year - 2000), "W")))

ggplot() +
  geom_path(data = f, mapping = aes(x = date, y = coeff), group=1, alpha = 0.2) +
  geom_path(data = f, mapping = aes(x = date, y = co2_kg), group=1, alpha = 0.2) +
  scale_color_gradient(low = "red", high = "green", limits= c(0, 1)) +
  labs(
    y = "Avoided emissions (kg CO2eq/kWh)",
    color = "R²"
  )

ggplot(data3, aes(x = wind_category , fill = type)) +
  geom_col(aes(y = prod)) +
  geom_point(aes(y = conso), show.legend = FALSE) +
  scale_fill_manual(values = prod_type_color) +
  facet_grid(cols= vars(conso_bin), rows = vars(season))

 #mutate(wind_category = case_when(
  #  wind_rank < 1/3 ~ "low_wind",
  #  wind_rank > 2/3 ~ "high_wind",
  #  TRUE ~ "mean_wind"
  #)) %>%

delta_data = data2 %>%
  select(season, prod_bin, wind_category, type, type_prod) %>%
  filter(wind_category != "Q1") %>%
  left_join(wind_prod_ref) %>%
  mutate(delta = type_prod - ref_prod)

ggplot(delta_data, aes(x = wind_category , fill = type)) +
  geom_boxplot(aes(y = delta), position = position_dodge(), outlier.shape = NA) +
  scale_fill_manual(values = prod_type_color) +
  facet_grid(cols= vars(prod_bin), rows = vars(season))
