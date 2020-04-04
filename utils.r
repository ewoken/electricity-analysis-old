source("../check_dependencies.r")
library(tidyverse)
library(lubridate)
library(latex2exp)

# here all constants
years = seq(2012, 2019)
ALL_YEARS = 1
ALL_MONTHS = 13
SUMMER = 14
WINTER = 15
prod_type_color = c("chocolate4", "gray48", "dodgerblue3", "green4",
                    "firebrick", "darkorange", "darkorchid3", "hotpink1",
                    "gray48", "gray22", "firebrick4", "tan3", "slategray4"
)
names(prod_type_color) = c("biomass", "fossil", "hydro", "nuclear",
                           "pumped_storage", "solar", "export", "wind",
                           "oil", "coal", "gas", "import", "z_base")

# theme configuration for plots
plotTheme = theme_set(theme_bw())
plotTheme = plotTheme +
  theme(axis.title.x = element_text(vjust = -2)) +
    theme(axis.title.y = element_text(angle = 90, vjust = 2)) +
    theme(plot.title = element_text(size = 15, vjust = 3)) +
    theme(plot.margin = unit(c(1, 0.5, 0.5, 1), "cm"))
theme_set(plotTheme)

# compute to which season a date belongs to
get_season = function(date, year) {
  summer_start = make_datetime(year, 04, 30, 23)
  summer_end = make_datetime(year, 09, 30, 23)
  if (summer_start <= date & date <= summer_end) {
    return("summer")
  }
  return("winter")
}

get_period_label = function(year, season) {
  small_season = ifelse(season == "summer", "S", "W")
  return(paste(year - 2000, small_season, sep=" "))
}

season_from_month = function(month) {
  return(ifelse(month == SUMMER, "summer", "winter"))
}

get_month_week = function(d) {
  month_day = day(d)
  d2 = date(d)
  day(d2) <- 1
  offset = wday(d2) - 1
  return(ceiling((offset + month_day) / 7))
}

# compute to which season year a date belongs to
# ex: 01/12/2015 is in winter 2015
# ex: 01/03/2016 is in winter 2015
get_season_year = function(date, year) {
  summer_start = make_datetime(year, 04, 30, 23)
  if (date <= summer_start) {
    return(year - 1)
  }

  return(year)
}

# gather all data and put it in a tibble frame
get_all_data = function() {
  data = tibble()

  for (year in years) {
    year_data = read_delim(paste("../0_data/", year, ".csv", sep=""),
      delim = ";",
      col_types = cols(
        perimetre = col_skip(),
        nature = col_skip(),
        #date = col_skip(),
        heure = col_skip(),
        date_heure = col_datetime(format = "%Y-%m-%dT%H:%M:%S%z"),
        prevision_j1 = col_skip(),
        prevision_j = col_skip(),
        taux_co2 = col_skip(),
        ech_comm_angleterre = col_skip(),
        ech_comm_espagne = col_skip(),
        ech_comm_italie = col_skip(),
        ech_comm_suisse = col_skip(),
        ech_comm_allemagne_belgique = col_skip(),
        fioul_tac = col_skip(),
        fioul_cogen = col_skip(),
        fioul_autres = col_skip(),
        gaz_tac = col_number(),
        gaz_cogen = col_number(),
        gaz_ccg = col_number(),
        gaz_autres = col_number(),
        # hydraulique_fil_eau_eclusee = col_skip(),
        # hydraulique_lacs = col_skip(),
        # hydraulique_step_turbinage = col_skip(),
        bioenergies_dechets = col_skip(),
        bioenergies_biomasse = col_skip(),
        bioenergies_biogaz = col_skip()
      )
    )
    year_data = filter(year_data, !is.na(consommation), minute(date_heure) != 15, minute(date_heure) != 45)
    data = bind_rows(data, year_data)
  }

  data = rename(data,
    "day_date"="date",
    "date"="date_heure",
    "conso"="consommation",
    "oil"="fioul",
    "coal"="charbon",
    "gas"="gaz",
    "gas_cogen"="gaz_cogen",
    "nuclear"="nucleaire",
    "wind"="eolien",
    "solar"="solaire",
    "hydro"="hydraulique",
    "pumped_storage"="pompage",
    "biomass"="bioenergies",
    "trade"="ech_physiques"
    # "hydro_run_of_river"="hydraulique_fil_eau_eclusee",
    # "hydro_lake"="hydraulique_lacs",
    # "hydro_pumped_storage"="hydraulique_step_turbinage"
  )

  data = mutate(data, import = if_else(trade > 0, trade, 0))
  data = mutate(data, export = if_else(trade < 0, trade, 0))
  data = mutate(data, prod = (coal + gas + hydro + nuclear + oil + solar + wind + biomass))

  # emissions for the given half-hour slot in kg
  # all emissions factor are in kg CO2eq per kWh
  data = mutate(data, co2_kg = (
      coal * 1.06 +
      gas * 0.418 +
      hydro * 0.006 +
      nuclear * 0.006 +
      oil * 0.73 +
      solar * 0.055 +
      wind * 0.013 +
      biomass * 0.494
    ) * 500
  )
  data = mutate(data, conso_co2_kg = co2_kg + 500 * import * 0.420)

  # emissions rate in tonnes of CO2eq per hour
  data = mutate(data, co2_t_h = co2_kg / 500)

  # emission factor of production for the given slot
  data = mutate(data, co2_kg_kwh = co2_kg / (prod * 500))
  data = mutate(data, conso_co2_kg_kwh = conso_co2_kg / ((prod + import) * 500))
  data = mutate(data, fossil = coal + oil + gas)
  data = mutate_at(data, vars(solar), ~ pmax(., 0))
  data = mutate(data, new_renewables = wind + solar)
  data = mutate(data, year = year(date))
  data = mutate(data, month = month(date))
  data = mutate(data, month_label = month(date, TRUE, TRUE, locale="en_US"))
  data = mutate(data, season = mapply(get_season, date, year))
  data = mutate(data, season_year = mapply(get_season_year, date, year))
  data = mutate(data, week_day = wday(day_date))
  data = mutate(data, week_day_label = wday(day_date, TRUE, TRUE, locale="en_US"))
  data = mutate(data, day_type = if_else(week_day == 1 | week_day == 7, "weekend", "weekday"))

  installed_capacities = read_delim("../0_data/installed_capacities.csv",
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

  data = mutate(data, wind_load_factor = wind / wind_capacities)
  data = mutate(data, solar_load_factor = solar / solar_capacities)

  temperatures = read_delim("../0_data/temperatures.csv",
    delim=";"
  )

  temperatures = rename(temperatures,
    "day_date"="date",
    "day_peak"="pic_journalier_consommation",
    "mean_temperature"="temperature_moyenne",
    "reference_temperature"="temperature_reference"
  )

  data = left_join(data, temperatures)

  return(data)
}
