get_heating_impact_by_monthly_analysis = function(
  data,
  ghg_factors = ADEME_GHG_FACTORS,
  import_ghg_factor = IMPORT_GHG_FACTOR,
  loss_rate = 0.1
) {
  data = data %>%
    add_month() %>%
    add_import_export() %>%
    add_prod()

  year_count = data %>%
    summarise(n = n_distinct(year)) %>%
    pull()

  stats_by_month = data %>%
    group_by(month) %>%
    summarise(
      bioenergies = sum(bioenergies),
      nuclear = sum(nuclear),
      gas = sum(gas),
      coal = sum(coal),
      oil = sum(oil),
      wind = sum(wind),
      solar = sum(solar),
      hydro = sum(hydro),
      import = sum(import)
    ) %>%
    gather(-month, key = 'type', value = 'value') %>%
    mutate(value = value * 0.5 * 10^(-6) / year_count)

  base_type_prod = stats_by_month %>%
    group_by(type) %>%
    summarise(base_prod = min(value))

  base_prod = base_type_prod %>%
    summarise(prod = sum(base_prod)) %>%
    pull()

  base_co2 = base_type_prod %>%
    spread(key = 'type', value = 'base_prod') %>%
    add_co2_kg(ghg_factors, energy_factor = 1) %>%
    rename(co2_mt = co2_kg) %>%
    mutate(base_mix = co2_mt / base_prod)

  base_co2_mt =  base_co2 %>% pull(co2_mt)
  base_mix = base_co2 %>% pull(base_mix)

  month_co2 = data %>%
    add_co2_kg(ghg_factors) %>%
    group_by(month) %>%
    summarise(
      import = sum(import) * 0.5 * 10^(-6) / year_count,
      co2_mt = sum(co2_kg) / 10^9 / year_count,
      prod = sum(prod) * 0.5 * 10^(-6) / year_count,
    ) %>%
    mutate(
      monthly_prod = prod - base_prod,
      monthly_co2_mt = co2_mt + import * import_ghg_factor - base_co2_mt,
      monthly_mix = monthly_co2_mt / monthly_prod
    )

  heating_factors = tibble(
    month = seq(1, 12),
    factor = c(0.23, 0.17, 0.13, 0.05, 0, 0, 0, 0, 0, 0.05, 0.16, 0.21)
  )

  mixs = month_co2 %>% pull(monthly_mix)
  factors = heating_factors %>% pull(factor)
  heating_kg_kwh = sum(mixs * factors) * (1 + loss_rate)


  monthly_type_prod = stats_by_month %>%
    filter(type != 'export') %>%
    left_join(base_type_prod, by = 'type') %>%
    mutate(prod = value - base_prod) %>%
    select(-value, -base_prod)
  plot = monthly_graph(base_type_prod, monthly_type_prod, title = "Base & monthly consumption")

  list(heating_kg_kwh = heating_kg_kwh, monthly_graph = plot)
}
