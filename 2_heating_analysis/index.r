source("../utils.r")

data = get_all_data()

start_year = 2012
end_year = 2019
year_count = end_year - start_year + 1

source('./temp_variations.r')
source('./emissions_by_temp_hour.r')
source('./load_by_temp.r')
source('./monthly_method.r')
source('./heating_model_method.r')


datai = data %>%
    filter(year > 2012) %>%
    mutate(conso_coal = if_else(coal + export < 0, 0, coal + export)) %>%
    mutate(conso_ccg = if_else(coal + gaz_ccg + export < 0, 0, coal + gaz_ccg + export)) %>%
    mutate(remaining_trade = if_else(coal + gaz_ccg + export < 0, coal + gaz_ccg + export, 0)) %>%
    mutate(
        conso_co2_kg = (
            conso_coal * 1.06 +
            (conso_ccg + gas_cogen + gaz_tac + gaz_autres) * 0.418 +
            hydro * 0.006 +
            nuclear * 0.006 +
            oil * 0.73 +
            solar * 0.055 +
            wind * 0.013 +
            biomass * 0.494 +
            import * 1.06
        ) * 500 * conso / (conso - remaining_trade)
    ) %>%
    mutate(conso_co2_kg_kwh = conso_co2_kg / conso / 500)

sum = datai %>%
    group_by(year) %>%
    summarise(
        co2_mt = sum(co2_kg) / 10^9,
        conso_co2_mt = sum(conso_co2_kg) / 10^9,
        conso = 500 * sum(conso) / 10 ^ 9,
        prod = 500 * sum(prod) / 10^9,
        export = -500 * sum(export) / 10^9,
        import = 500 * sum(import) / 10^9
    ) %>%
    mutate(co2_kg_kwh = co2_mt  / prod) %>%
    mutate(conso_co2_kg_kwh = conso_co2_mt  / conso) %>%
    mutate(export_co2_mt = co2_mt - conso_co2_mt + import) %>%
    mutate(export_co2_kg_kwh = export_co2_mt / export)
    


ggplot(datai, aes(x = paste(year), y = t)) +
    geom_boxplot()

ggplot(datai, aes(x = coal, y = trade, color = paste(year))) +
    geom_point(shape=".")

ggplot(datai, aes(x = coal, y = gaz_ccg, color = paste(year))) +
    geom_point(shape=".")

ggplot(datai, aes(gaz_ccg, group = paste(year))) +
    geom_freqpoly(aes(color = paste(year)))

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
    scale_fill_gradient(low="#5bb55e", high="yellow") +
    labs(
        title = "GHG emissions of french electricity consumption by day",
        x = "",
        y = "",
        fill = "GHG Emissions\n(kg CO2eq/kWh)",
        caption = "Data: RTE 2012 - 2019, IPCC"
    ) +
    ggsave("./figures/test.png", width = unit(12, "cm"), height = unit(6, "cm"))

n = data %>% summarise(i = n()) %>% pull(i)

data3 = data %>%
    select(conso_co2_kg_kwh, year) %>%
    arrange(-conso_co2_kg_kwh) %>%
    group_by(year) %>%
    mutate(i = row_number())

ggplot(data3, aes(x = i/(2*365*24), y = conso_co2_kg_kwh, group = paste(year), color = paste(year))) + 
    geom_line()



