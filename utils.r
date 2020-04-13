suppressMessages(library(tidyverse))
suppressMessages(library(lubridate))

YEARS = seq(2012, 2019)
MW_TO_KWH = 500

ADEME_GHG_FACTORS = c(0.006, 0.006, 0.013, 0.055, 0.418, 0.494, 0.73, 1.06)
names(ADEME_GHG_FACTORS) = c('hydro', 'nuclear', 'wind', 'solar', 'gas', 'bioenergies', 'oil', 'coal')

IPCC_GHG_FACTORS = c(0.024, 0.012, 0.011, 0.045, 0.490, 0.230, 0.65, 0.82)
names(IPCC_GHG_FACTORS) = c('hydro', 'nuclear', 'wind', 'solar', 'gas', 'bioenergies', 'oil', 'coal')

IMPORT_GHG_FACTOR = 0.420
IMPORT_MARG_GHG_FACTOR = 1.06

plotTheme = theme_set(theme_bw())
plotTheme = plotTheme +
    theme(axis.title.x = element_text(vjust = -2)) +
    theme(axis.title.y = element_text(angle = 90, vjust = 2)) +
    theme(plot.title = element_text(size = 15, vjust = 3)) +
    theme(plot.margin = unit(c(1, 0.5, 0.5, 1), 'cm'))
theme_set(plotTheme)

annual_use = read_csv(
    'data/annual_use.csv',
    col_types = cols(
        week_index = col_double(),
        industry_energy = col_double(),
        heating = col_double(),
        air_conditioning = col_double(),
        others = col_double(),
        lighting = col_double(),
        hot_water = col_double(),
        cooking = col_double(),
        grid_loss = col_double(),
        electric_vehicules = col_double()
    )
)
january_typical_use = read_csv(
    'data/january_typical_use.csv',
    col_types = cols(
        weekday = col_double(),
        hour = col_double(),
        grid_loss = col_double(),
        industry_energy = col_double(),
        heating = col_double(),
        air_conditioning = col_double(),
        others = col_double(),
        lighting = col_double(),
        hot_water = col_double(),
        cooking = col_double(),
        electric_vehicules = col_double()
    )
)

get_year_data = function(country, year) {
  suppressWarnings((year_data = read_delim(paste('./_data/', country, '/', year, '.csv', sep=''),
    delim = ';',
    col_types = cols(
        perimetre = col_skip(),
        nature = col_skip(),
        heure = col_skip(),
        date_heure = col_datetime(format = '%Y-%m-%dT%H:%M:%S%z'),
        prevision_j1 = col_skip(),
        prevision_j = col_skip(),
        taux_co2 = col_skip(),
        ech_comm_angleterre = col_skip(),
        ech_comm_espagne = col_skip(),
        ech_comm_italie = col_skip(),
        ech_comm_suisse = col_skip(),
        ech_comm_allemagne_belgique = col_skip(),
        fioul_tac = col_number(),
        fioul_cogen = col_number(),
        fioul_autres = col_number(),
        gaz_tac = col_number(),
        gaz_cogen = col_number(),
        gaz_ccg = col_number(),
        gaz_autres = col_number(),
        hydraulique_fil_eau_eclusee = col_number(),
        hydraulique_lacs = col_number(),
        hydraulique_step_turbinage = col_number(),
        bioenergies_dechets = col_number(),
        bioenergies_biomasse = col_number(),
        bioenergies_biogaz = col_number()
    ),
    )))
    year_data = rename(year_data,
        datetime = date_heure,
        conso = consommation,
        physical_flow = ech_physiques,
        coal = charbon,

        oil = fioul,
        oil_turbine = fioul_tac,
        oil_cogen = fioul_cogen,
        oil_others = fioul_autres,

        gas = gaz,
        gas_cogen = gaz_cogen,
        gas_ccg = gaz_ccg,
        gas_turbine = gaz_tac,
        gas_others = gaz_autres,

        nuclear = nucleaire,

        wind = eolien,
        solar = solaire,

        hydro = hydraulique,
        hydro_pumped_storage = pompage,
        hydro_run_of_river = hydraulique_fil_eau_eclusee,
        hydro_lake = hydraulique_lacs,
        hydro_pumped_discharge = hydraulique_step_turbinage,

        waste = bioenergies_dechets,
        biomass = bioenergies_biomasse,
        biogas = bioenergies_biogaz
    )
    year_data = filter(year_data, !is.na(conso), minute(datetime) != 15, minute(datetime) != 45)
    year_data = year_data %>%
        mutate(solar = if_else(solar > 0, solar, 0)) %>%
        mutate(coal = if_else(coal > 0, coal, 0)) %>%
        mutate(gas_ccg = if_else(gas_ccg > 0, gas_ccg, 0)) %>%
        mutate(gas_turbine = if_else(gas_turbine > 0, gas_turbine, 0)) %>%
        mutate(oil_turbine = if_else(oil_turbine > 0, oil_turbine, 0))

    year_data
}

add_installed_capacities = function(data, country) {
    installed_capacities = read_delim(
        paste('./_data/', country, '/installed_capacities.csv', sep=''),
        delim=';',
        col_types=cols(
            'annee'=col_integer()
        )
    )
    installed_capacities = rename(installed_capacities,
        year = annee,
        fossils_capacities = parc_thermique_fossile,
        oil_capacities = parc_fioul,
        coal_capacities = parc_charbon,
        gas_capacities = parc_gaz,
        hydro_capacities = parc_hydraulique,
        nuclear_capacities = parc_nucleaire,
        solar_capacities = parc_solaire,
        wind_capacities = parc_eolien,
        biomass_capacities = parc_bioenergie
    )
    data = left_join(data, select(installed_capacities, year, solar_capacities, wind_capacities), by = 'year')
    data
}

add_temperatures = function(data, country) {
    temperatures = read_delim(
        paste('./_data/', country, '/temperatures.csv', sep=''),
        delim=';',
        col_types = cols(
            date = col_date(format = ''),
            pic_journalier_consommation = col_double(),
            temperature_moyenne = col_double(),
            temperature_reference = col_double()
        )
    )
    temperatures = rename(temperatures,
        day_peak = pic_journalier_consommation,
        mean_temperature = temperature_moyenne,
        reference_temperature = temperature_reference
    )
    data = left_join(data, temperatures, by = 'date')
    data
}

get_country_data = function(country) {
    data = tibble()

    for (year in YEARS) {
        year_data = get_year_data(country, year)
        data = bind_rows(data, year_data)
    }
    data = data %>%
        mutate(year = year(date)) %>% # TODO deal with timeslot
        add_installed_capacities(country) %>%
        add_temperatures(country) %>%
        filter(year < 2020) # TODO
    data
}

add_import_export = function(data) {
    data = mutate(data, import = if_else(physical_flow > 0, physical_flow, 0))
    data = mutate(data, export = if_else(physical_flow < 0, -physical_flow, 0))
    data
}

add_prod = function(data) {
    data = mutate(data, prod = (coal + gas + hydro + nuclear + oil + solar + wind + bioenergies))
    data
}

add_co2_kg = function(data, ghg_factors, energy_factor = MW_TO_KWH) {
    data = data %>%
        mutate(co2_kg = 0) %>%
        add_prod()

    # we sum here emissions for each type of production
    for (prod_type in names(ghg_factors)) {
        data[['co2_kg']] = data[['co2_kg']] +
            data[[prod_type]] * energy_factor * ghg_factors[prod_type]
    }

    # then we compute carbon intensity
    data = data %>%
        mutate(co2_kg_kwh = co2_kg / (prod * energy_factor))
    data
}

add_month = function(data) {
    data = data %>%
        mutate(month = month(date)) %>%
        mutate(month_label = month(date, TRUE, TRUE, locale='en_US'))
    data
}

add_season = function(data) {
    data = data %>%
        add_month() %>%
        mutate(season = if_else(month >= 5 && month < 10, 'summmer', 'winter'))
    data
}

add_mixin_conso_co2_kg = function(data, ghg_factors, import_ghg_factor = IMPORT_GHG_FACTOR) {
    data = data %>%
        add_co2_kg(ghg_factors) %>%
        add_import_export() %>%
        add_prod() %>%
        mutate(
            export_co2_kg = (co2_kg / prod) * export,
            import_co2_kg = import * MW_TO_KWH * import_ghg_factor,
            conso_co2_kg = co2_kg - export_co2_kg + import_co2_kg,

            conso_co2_kg_kwh = conso_co2_kg / ((conso - hydro_pumped_storage) * MW_TO_KWH),
            export_co2_kg_kwh = export_co2_kg / (export * MW_TO_KWH)
        )
    data
}

add_marginal_export_dispatch = function(data) {
    data = data %>%
        mutate(
            remaining_export = export,
            export_ccg = if_else(remaining_export - gas_ccg < 0, remaining_export, gas_ccg),
            remaining_export = if_else(remaining_export - gas_ccg < 0, 0, remaining_export - gas_ccg),

            export_coal = if_else(remaining_export - coal < 0, remaining_export, coal),
            remaining_export = if_else(remaining_export - coal < 0, 0, remaining_export - coal),

            conso_coal = coal - export_coal,
            conso_ccg = gas_ccg - export_ccg,
        )
    data
}

add_marginal_export_conso_co2_kg = function(data, ghg_factors, import_ghg_factor = IMPORT_MARG_GHG_FACTOR) {
    data = data %>%
        add_co2_kg(ghg_factors) %>%
        add_import_export() %>%
        add_marginal_export_dispatch() %>%
        mutate(
            remaining_co2_kg = (
                conso_coal * ghg_factors['coal'] +
                (conso_ccg + gas_cogen + gas_turbine + gas_others) * ghg_factors['gas'] +
                hydro * ghg_factors['hydro'] +
                nuclear * ghg_factors['nuclear'] +
                oil * ghg_factors['oil'] +
                solar * ghg_factors['solar'] +
                wind * ghg_factors['wind'] +
                bioenergies * MW_TO_KWH * ghg_factors['bioenergies']
            ),
            export_co2_kg =
                export_coal * MW_TO_KWH * ghg_factors['coal'] +
                export_ccg * MW_TO_KWH * ghg_factors['gas'] +
                remaining_co2_kg / ((prod - export_ccg - export_coal)) * remaining_export,
            import_co2_kg = import * MW_TO_KWH * import_ghg_factor,
            conso_co2_kg = co2_kg - export_co2_kg + import_co2_kg,

            export_co2_kg_kwh = export_co2_kg / (export * MW_TO_KWH),
            conso_co2_kg_kwh = conso_co2_kg / ((conso - hydro_pumped_storage) * MW_TO_KWH)
        )

    data
}




