library(tidyverse)
library(lubridate)

years = seq(2012, 2018)
data = tibble()

getSummerData = function(data, year) {
    summer_start = make_datetime(year, 04, 30, 23)
    summer_end = make_datetime(year, 09, 30, 23)
    return(filter(data,
        summer_start <= date,
        date <= summer_end
    ))
}

getWinterData = function(data, year) {
    winter_start = make_datetime(year, 09, 30, 23)
    winter_end = make_datetime(year + 1, 04, 30, 23)
    return(filter(data,
                  winter_start <= date,
                  date <= winter_end
    ))
}

getPeriod = function(date, year) {
    summer_start = make_datetime(year, 04, 30, 23)
    summer_end = make_datetime(year, 09, 30, 23)

    if (summer_start <= date & date <= summer_end) {
        return("summer")
    }
    return("winter")
}

for (year in years) {
    year_data = read_delim(paste("data/", year, ".csv", sep=""),
        delim = ";",
        col_types = cols(
            perimetre = col_skip(),
            nature = col_skip(),
            date = col_skip(),
            heure = col_skip(),
            date_heure = col_datetime(format = "%Y-%m-%dT%H:%M:%S%z"),
            prevision_j1 = col_skip(),
            prevision_j = col_skip(),
            taux_co2 = col_skip(),
            ech_comm_angleterre = col_integer(),
            ech_comm_espagne = col_integer(),
            ech_comm_italie = col_integer(),
            ech_comm_suisse = col_integer(),
            ech_comm_allemagne_belgique = col_integer(),
            fioul_tac = col_skip(),
            fioul_cogen = col_skip(),
            fioul_autres = col_skip(),
            gaz_tac = col_skip(),
            gaz_cogen = col_skip(),
            gaz_ccg = col_skip(),
            gaz_autres = col_skip(),
            hydraulique_fil_eau_eclusee = col_skip(),
            hydraulique_lacs = col_skip(),
            hydraulique_step_turbinage = col_skip(),
            bioenergies_dechets = col_skip(),
            bioenergies_biomasse = col_skip(),
            bioenergies_biogaz = col_skip()
        )
    )
    year_data = filter(year_data, !is.na(consommation))
    data = bind_rows(data, year_data)
    # year_data = year_data[!is.na(year_data$consommation),]
    # data = rbind(data, year_data)
}
rm(year_data, year)
data = rename(data,
    "date"="date_heure",
    "conso"="consommation",
    "oil"="fioul",
    "coal"="charbon",
    "gas"="gaz",
    "nuclear"="nucleaire",
    "wind"="eolien",
    "solar"="solaire",
    "hydro"="hydraulique",
    "pumped_storage"="pompage",
    "biomass"="bioenergies",
    "trade"="ech_physiques",
    "trade_UK"="ech_comm_angleterre",
    "trade_SP"="ech_comm_espagne",
    "trade_IT"="ech_comm_italie",
    "trade_CH"="ech_comm_suisse",
    "trade_DE_BE"="ech_comm_allemagne_belgique"
)

data = mutate(data, co2_kg_kwh = (
    coal * 1.06 +
    gas * 0.418 +
    hydro * 0.006 +
    nuclear * 0.006 +
    oil * 0.73 +
    solar * 0.055 +
    wind * 0.013
) / (coal + gas + hydro + nuclear + oil + solar + wind))
data = mutate(data, fossil = coal + oil + gas)
data = mutate_at(data, vars(solar), ~ pmax(., 0))
data = mutate(data, new_renewables = wind + solar)
data = mutate(data, year = year(date))
data = mutate(data, month = month(date, TRUE, TRUE, locale="en_US"))
data = mutate(data, period = mapply(getPeriod, date, year))

installed_capacities = read_delim("./data/installed_capacities.csv",
    delim=";",
    col_types=cols(
        "annee"=col_integer()
    )
)
installed_capacities = rename(installed_capacities,
    "year"="annee",
    "fossils_capacities"="parc_thermique_fossile",
    "oil_capacities"="parc_fioul",
    "coal_capacities"="parc_charbon",
    "gas_capacities"="parc_gaz",
    "hydro_capacities"="parc_hydraulique",
    "nuclear_capacities"="parc_nucleaire",
    "solar_capacities"="parc_solaire",
    "wind_capacities"="parc_eolien",
    "biomass_capacities"="parc_bioenergie"
)

data = left_join(data, select(installed_capacities, year, solar_capacities, wind_capacities))

data$wind_load_factor = data$wind / data$wind_capacities
data$solar_load_factor = data$solar / data$solar_capacities

analyse <- function(data, yName, xName, cur_period) {
    res = c();

    for (year in years) {
        period_data = filter(data, period = cur_period)
        if (xName == "solar") {
            period_data = filter(period_data, solar > 0)
        }

        x = unlist(period_data[xName])
        y = unlist(period_data[yName])
        reg = lm(y~x)
        res = c(res, reg$coefficients[2])
    }
    names(res) = c()
    return(res)
}
results = data.frame(
    "fossil_wind_summer"=analyse(data, "fossil", "wind", "summer"),
    "nuclear_wind_summer"=analyse(data, "nuclear", "wind", "summer"),
    "hydro_wind_summer"=analyse(data, "hydro", "wind", "summer"),
    "storage_wind_summer"=analyse(data, "pumped_storage", "wind", "summer"),
    "wind_conso_summer"=analyse(data, "wind", "conso", "summer"),

    "fossil_solar_summer"=analyse(data, "fossil", "solar", "summer"),
    "nuclear_solar_summer"=analyse(data, "nuclear", "solar", "summer"),
    "hydro_solar_summer"=analyse(data, "hydro", "solar", "summer"),
    "storage_solar_summer"=analyse(data, "pumped_storage", "solar", "summer"),
    "solar_conso_summer"=analyse(data, "solar", "conso", "summer"),

    "fossil_nuclear_summer"=analyse(data, "fossil", "nuclear", "summer"),
    "fossil_nuclear_summer"=analyse(data, "fossil", "hydro", "summer")
);

row.names(results) = years
write.csv(results, "test.txt")

plotSeasonDistrib = function(varname, yName, cur_title) {
    ggplot(data) +
        geom_boxplot(mapping = aes(
            x = month,
            y = get(varname),
            ymin = min(get(varname)),
            ymax = max(get(varname))
        ),
        outlier.shape = NA,
        coef=10
        ) +
        scale_y_continuous(limits=c(0, NA)) +
        labs(
            x=NULL,
            y=yName,
            title=cur_title,
            subtitle = "Source: RTE"
        )

    ggsave(paste("./figures/", varname, ".png", sep=""))
}

plotSeasonProd = function(varname, yName, cur_title, myColor) {
    summup = data %>%
        select(year, varname, month) %>%
        filter(year==2018) %>%
        group_by(month, ) %>%
        summarise(prod = sum(get(varname)) * 0.0005 )
    
    ggplot(summup) +
        geom_col(mapping = aes(x=month, y = prod ), fill = myColor) +
        scale_y_continuous(limits=c(0, NA)) +
        labs(
            x=NULL,
            y=paste(yName, " (GWh)", sep=""),
            title=cur_title,
            subtitle = "Source: RTE"
        )
    
    ggsave(paste("./figures/", varname, ".png", sep=""))
}

plotSeasonDistrib("solar_load_factor", "Solar load factor", "Distribution of solar load factor in France (2012 - 2018)")
plotSeasonDistrib("wind_load_factor", "Wind load factor", "Distribution of wind load factor in France (2012 - 2018)")
plotSeasonDistrib("fossil", "Fossil load (MW)", "Distribution of fossil load in France (2012 - 2018)")
plotSeasonDistrib("nuclear", "Nuclear load (MW)", "Distribution of nuclear load in France (2012 - 2018)")
plotSeasonDistrib("conso", "Consommation (MW)", "Distribution of consommation in France (2012 - 2018)")

plotSeasonProd("solar", "Solar production", "Solar production in 2018", "orange")
plotSeasonProd("wind", "Wind production", "Wind production in 2018", "blue")
plotSeasonProd("fossil", "Fosiil production", "Fossil production in 2018", "grey")

ggplot(data, aes(x=solar_load_factor, color = period)) +
    geom_density()
