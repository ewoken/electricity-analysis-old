library(tidyverse)
library(lubridate)

# here all constants
years = seq(2012, 2018)
ALL_YEARS = 1
ALL_MONTHS = 13
SUMMER = 14
WINTER = 15

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
    year_data = read_delim(paste("./data/", year, ".csv", sep=""),
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
        ech_comm_angleterre = col_skip(),
        ech_comm_espagne = col_skip(),
        ech_comm_italie = col_skip(),
        ech_comm_suisse = col_skip(),
        ech_comm_allemagne_belgique = col_skip(),
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
  }

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
    "trade"="ech_physiques"
  )

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

  # emissions rate in tonnes of CO2eq per hour
  data = mutate(data, co2_t_h = co2_kg / 500)

  # emission factor of production for the given slot
  data = mutate(data, co2_kg_kwh = co2_kg / (prod * 500))
  data = mutate(data, fossil = coal + oil + gas)
  data = mutate_at(data, vars(solar), ~ pmax(., 0))
  data = mutate(data, new_renewables = wind + solar)
  data = mutate(data, year = year(date))
  data = mutate(data, month = month(date))
  data = mutate(data, month_label = month(date, TRUE, TRUE, locale="en_US"))
  data = mutate(data, season = mapply(get_season, date, year))
  data = mutate(data, season_year = mapply(get_season_year, date, year))

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

  data = mutate(data, wind_load_factor = wind / wind_capacities)
  data = mutate(data, solar_load_factor = solar / solar_capacities)

  return(data)
}

compute_avoided_emissions_by_year = function(data, results) {
  coeffs = results %>%
    filter(month < SUMMER, year != ALL_YEARS, yname == "co2_kg") %>%
    select(year, month, coeff, xname)

  res = data %>%
    gather("solar", "wind", key = "xname", value="prod") %>%
    left_join(coeffs) %>%
    group_by(year, xname) %>%
    summarise(co2_kg = sum(-500 * prod * coeff))

  return(res)
}
