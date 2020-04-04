source("../utils.r")

data = get_all_data()

data2 = data %>%
    mutate(
        conso_co2_kg = (
            coal * 0.820 +
                gas * 0.490 +
                hydro * 0.024 +
                nuclear * 0.012 +
                oil * 0.65 +
                solar * 0.045 +
                wind * 0.011 +
                biomass * 0.230 +
                import * 0.420
        ) * 500,
    ) %>%
    group_by(day_date) %>%
    summarise(
        prod = sum(prod),
        import = sum(import),
        conso_co2_kg = sum(conso_co2_kg),
        week_day_label = first(week_day_label),
        year = last(year),
        month_label = last(month_label),
    ) %>%
    mutate(month_week = mapply(get_month_week, day_date)) %>%
    mutate(conso_co2_kg_kwh = conso_co2_kg / ((prod + import) * 500))

ggplot(data2, aes(x = week_day_label, y = month_week, fill = conso_co2_kg_kwh)) +
    geom_tile(color = "white") +
    facet_grid(year~month_label) +
    scale_y_reverse() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
    ) +
    #scale_fill_gradientn(
    # colours = colorRampPalette(c("#5bb55e", "yellow", "brown"), bias = 2)(4)
    #    colours = colorRampPalette(c("#5bb55e", "yellow"), bias = 1)(5)
    #) +
    scale_fill_gradientn(
        colours = c('#2AA364', '#F5EB4D', '#9E4229', '#381d02'),
        values = c(0, 150, 600, 750) / 750,
        limits = c(0, 0.750)
    ) +
    labs(
        title = "GHG emissions of french electricity consumption by day",
        x = "",
        y = "",
        fill = "GHG Emissions\n(kg CO2eq/kWh)",
        caption = "Data: RTE 2012 - 2019, Emission factors: IPCC 2014"
    ) +
    ggsave("./figures/france.png", width = unit(12, "cm"), height = unit(6, "cm"))

years_germany = seq(2010, 2019)
get_all_data_germany = function() {
    data = tibble()

    for (year in years_germany) {
        year_data = read_delim(paste("../0_germany_data/", year, ".csv", sep=""), delim = ",")
        
        if (year == 2019) {
            year_data = year_data %>%
                mutate(day_hour = paste(date(startDate), hour(startDate))) %>%
                group_by(day_hour) %>%
                summarise(
                    startDate = first(startDate),
                    endDate = last(endDate),
                    trade = mean(trade),
                    hydro = mean(hydro),
                    biomass = mean(biomass),
                    nuclear = mean(nuclear),
                    brown_coal = mean(brown_coal),
                    hard_coal = mean(hard_coal),
                    gas = mean(gas),
                    oil = mean(oil),
                    wind = mean(wind),
                    solar = mean(solar),
                    pumped_storage = mean(pumped_storage),
                    seasonal_storage = mean(seasonal_storage),
                    others = mean(others)
                ) %>%
                select(-day_hour)
        }
        
        data = bind_rows(data, year_data)
    }

    data = data %>%
        select(-endDate) %>%
        rename(date = startDate) %>%
        mutate(day_date = date(date)) %>%
        mutate(import = if_else(trade > 0, trade, 0)) %>%
        mutate(export = if_else(trade < 0, -trade, 0)) %>%
        mutate(prod = hydro + biomass + nuclear + brown_coal + hard_coal + gas + oil + solar + wind + pumped_storage + seasonal_storage + others) %>%
        mutate(year = year(date)) %>%
        mutate(month = month(date)) %>%
        mutate(month_label = month(date, TRUE, TRUE, locale="en_US")) %>%
        mutate(season = mapply(get_season, date, year)) %>%
        mutate(season_year = mapply(get_season_year, date, year)) %>%
        mutate(week_day = wday(day_date)) %>%
        mutate(week_day_label = wday(day_date, TRUE, TRUE, locale="en_US")) %>%
        mutate(day_type = if_else(week_day == 1 | week_day == 7, "weekend", "weekday")) %>%
        mutate(
            co2_kg = (
                brown_coal * 0.820 +
                hard_coal * 0.820 +
                gas * 0.490 +
                (hydro + pumped_storage) * 0.024 +
                nuclear * 0.012 +
                oil * 0.65 +
                solar * 0.045 +
                wind * 0.011 +
                biomass * 0.230
            ) * 500,
        ) %>%
        mutate(conso_co2_kg = co2_kg + import * 500 * 0.420) %>%
        mutate(co2_kg_kwh = co2_kg / (500 * prod)) %>%
        mutate(conso_co2_kg_kwh = conso_co2_kg / (500 * (prod + import)))

    return(data)
}
data_germany = get_all_data_germany()

germany_calendar_data = data_germany %>%
    group_by(day_date) %>%
    summarise(
        prod = sum(prod),
        import = sum(import),
        conso_co2_kg = sum(conso_co2_kg),
        week_day_label = first(week_day_label),
        year = last(year),
        month_label = last(month_label),
    ) %>%
    mutate(month_week = mapply(get_month_week, day_date)) %>%
    mutate(conso_co2_kg_kwh = conso_co2_kg / ((prod + import) * 500))

ggplot(germany_calendar_data, aes(x = week_day_label, y = month_week, fill = conso_co2_kg_kwh)) +
    geom_tile(color = "white") +
    facet_grid(year~month_label) +
    scale_y_reverse() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
    ) +
    scale_fill_gradientn(
     colours = c('#2AA364', '#F5EB4D', '#9E4229', '#381d02'),
     values = c(0, 150, 600, 750) / 750,
     limits = c(0, 0.750)
    ) +
    labs(
        title = "GHG emissions of german electricity consumption by day",
        x = "",
        y = "",
        fill = "GHG Emissions\n(kg CO2eq/kWh)",
        caption = "Data: energy-charts.de, Emission factors: IPCC 2014"
    ) +
    ggsave("./figures/germany.png", width = unit(12, "cm"), height = unit(6, "cm"))

to_box_plot_values <- function(x) {
    r <- quantile(x, probs = c(0.00, 0.25, 0.5, 0.75, 1))
    names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
    r
}

ggplot(data_germany %>% filter(nuclear > 0), aes(x = paste(year), y = conso_co2_kg_kwh)) +
    stat_summary(fun.data=to_box_plot_values, geom="boxplot") +
    scale_y_continuous(limits = c(0, 0.6), breaks = seq(0, 6) * 0.1)  + 
    labs(
        title = "Hourly GHG emission distributions of german electric production",
        x = "",
        y = "GHG emissions\n(kgCO2eq/kWh)",
        caption = "Data: energy-charts.de, Emission factors: IPCC 2014"
    ) +
    ggsave("./figures/germany2.png", width = unit(10, "cm"))


e = data_germany %>%
    filter(day_date == "2019-01-01") %>%
    select(date, year, wind, solar) %>%
    arrange(date) %>%
    mutate(wind_prev = lag(wind)) %>%
    mutate(wind_var = wind - wind_prev)

renew_variations = data_germany %>%
    select(date, year, wind, solar) %>%
    arrange(date) %>%
    mutate(wind_prev = lag(wind)) %>%
    # mutate(solar_prev = lag(solar)) %>%
    mutate(wind_var = wind - wind_prev)
    # mutate(solar = solar - solar_prev) %>%
    # select(-wind_prev, -solar_prev) %>%
    #mutate('renew' = wind + solar)

    gather(wind, 'renew', solar, key = var_type, value = value)
    
a = renew_variations %>% filter(!is.na(wind_var)) %>% summarise(t = max(wind_var), u = min(wind_var))
b = renew_variations %>% filter(wind_var== 29501)

ggplot(renew_variations, aes(x = var_type, y = value)) +
    geom_boxplot(outlier.shape = NA) +
    scale_y_continuous(limits = c(-1, 1) * 5000, breaks = seq(-1, 1, by = 0.2) * 5000) +
    # stat_summary(fun.data=to_box_plot_values, geom="boxplot") +
    facet_grid(cols = vars(year))
    


