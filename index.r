library(tidyverse)
library(lubridate)
library(rgl)

source("./utils.r")
source("./common_graphs.r")

data = get_all_data()

get_multilinear_reg = function(data, cur_year, cur_month, cur_xname, cur_yname) {
  cur_data = data

  if (cur_year != ALL_YEARS) {
    if (cur_month >= SUMMER) {
      cur_data = filter(cur_data, season_year == cur_year)
    } else {
      cur_data = filter(cur_data, year == cur_year)
    }
  }

  if (cur_month < ALL_MONTHS) {
    cur_data = filter(cur_data, month == cur_month)
  } else if (cur_month >= SUMMER) {
    cur_data = filter(cur_data, season == season_from_month(cur_month))
  }

  x = unlist(cur_data[cur_xname])
  conso = pull(cur_data, conso)

  if (cur_yname == "co2_kg") {
    # convert load from MW to kWh
    # 1 MW produces 500 kWh during 1/2 hour
    x = x * 500
    conso = conso * 500
  }

  y = unlist(cur_data[cur_yname])
  reg = lm(y ~ x + conso)

  return(reg)
}

get_multilinear_results = function(data) {
    results = tibble(year = -1, month = 0, yname = "", coeff_conso = 0,
                    xname = "", coeff = 0,
                    intercept = 0, r2 = 0)
    months = seq(1, 12)
    xnames = c("wind", "solar")
    ynames = c("co2_kg") # "fossil", "nuclear",
    years = c(years, ALL_YEARS)
    months = c(months, ALL_MONTHS, SUMMER, WINTER)

    for(cur_year in years) {
      for (cur_month in months) {
        if (cur_year == ALL_YEARS & cur_month >= ALL_MONTHS) {
            next
        }
        for (cur_xname in xnames) {
          for (cur_yname in ynames) {
            reg = get_multilinear_reg(data, cur_year, cur_month, cur_xname, cur_yname)

            cur_r2 = summary(reg)$r.squared

            # add two rows in order to ease later computation
            results = add_row(results, year=cur_year, month=cur_month,
                              xname = cur_xname,
                              yname=cur_yname,
                              intercept=reg$coefficients[1],
                              coeff = reg$coefficients[2],
                              coeff_conso=reg$coefficients[3],
                              r2 = cur_r2)
          }
        }
      }
    }

    results = results %>%
      filter(year > 0) %>% # remove first empty line
      arrange(year, month) %>%
      mutate(month_label = month(month, TRUE, TRUE, locale="en_US")) %>%
      mutate(date = as.Date(make_datetime(year, month)))

    return(results)
}

plot_results_group_by_year = function(results) {
  d = results %>%
    filter(month == ALL_MONTHS, year != ALL_YEARS, yname == "co2_kg")

  plot = ggplot(d, aes(x = year, y = coeff)) +
    geom_path(group=1, alpha = 0.2) +
    geom_point(mapping = aes(color = r2)) +
    scale_color_gradient(low = "red", high = "green", limits=c(0,1)) +
    scale_y_continuous(limits = c(NA, 0)) +
    facet_grid(rows = vars(xname)) +
    labs(
      x = NULL,
      y = "Avoided emissions (kg CO2eq/kWh)",
      color = "R²"
    )
  ggsave("figures/multilinear_method/impact_by_year.png")

  return(plot)
}

plot_multilinear_reg_group_by_season = function(data, results, cur_xname, withISO = FALSE) {
  max = 14 * 10^3 # t CO2eq/h
  min_value = (data %>% select(co2_t_h) %>% min()) / max
  my_values = c(0, 0.1, 0.2, 0.3, 0.5, 0.7, 1)
  my_breaks = max * my_values
  my_levels = my_breaks
  levels = tibble(level = my_levels)
  lim = c(0, max)
  xtitle = ifelse(cur_xname == "solar", "Solar (GW)", "Wind (GW)")

  a = results %>%
    filter(year != ALL_YEARS, month >= SUMMER, yname == "co2_kg", xname ==  cur_xname) %>%
    mutate(season = if_else(month == SUMMER, "summer", "winter")) %>%
    mutate(season_year = year) %>%
    crossing(levels) %>%
    mutate(level2slope = (-coeff)/(coeff_conso)) %>%
    mutate(level2intercept = ((level- intercept/500)/(1000*coeff_conso)))

  d = filter(data, season_year >= 2012)
  plot = ggplot(d, aes(x = pull(d, cur_xname) / 1000, y = conso / 1000)) +
    stat_summary_2d(aes(z = co2_t_h), bins = 40, alpha = 0.9) +
    scale_fill_gradientn(colours = rainbow(n = 6),
                         limits = lim,
                         breaks = my_breaks,
                         values = my_values,
    ) +
    scale_x_continuous(labels = (function(v) { return(as.integer(v)) })) +
    facet_grid(cols = vars(season_year), rows = vars(season)) +
    labs(
      x = xtitle,
      y = "Consumption (GW)",
      fill = "GHG Emissions\n(t CO2eq/h)",
      caption = "Data: RTE 2012 - 2018\nEmission factors (life-cycle analysis) from ADEME\nSummer: May-Sept, Winter: Oct-April"
    )

  if (withISO) {
    plot = plot +
      geom_abline(data = a, na.rm = T, aes(
        intercept = level2intercept,
        slope=level2slope,
        colour=level
      )) +
      scale_colour_gradientn(colours = rainbow(n = 6),
                             limits = lim,
                             values = my_values,
                             guide = "none")
  }

  ggsave(paste("./figures/multilinear_method/multilinear_reg_", cur_xname, "_by_season.png", sep=""),
         width = unit(11, "cm"), height = unit(5, "cm"))

  return(plot)
}

# correlation between consumption and emissions
ggplot(filter(data, year == 2018), aes(x = conso / 1000, y = co2_t_h)) +
  geom_point(shape=".") +
  geom_smooth(method = "lm") +
  facet_wrap(~month_label) +
  labs(
    title = "Correlation between consumption and GHG emissions (2018)",
    x = "Consumption (GW)",
    y = "GHG emissions (t CO2eq/h)",
    caption = "Data: RTE 2018\nEmission factors (life-cycle analysis) from ADEME"
  )
ggsave("./figures/correlation_consumption_emissions.png", width = unit(9, "cm"))

results = get_multilinear_results(data)

plot_multilinear_reg_group_by_season(data, results, "wind", T)
plot_multilinear_reg_group_by_season(data, results, "solar", T)

plot_results_by_season(results, "multilinear_method", "wind")
plot_results_by_season(results, "multilinear_method", "solar")

coeffs = results %>%
    filter(month >= SUMMER, year != ALL_YEARS, yname == "co2_kg") %>%
    mutate(season = ifelse(month == SUMMER, "summer", "winter")) %>%
    select(year, season, xname, coeff) %>%
    spread(key = xname, value = coeff) %>%
    rename(coeff_wind=wind, coeff_solar=solar, season_year = year)


dataWithCoeff = data %>%
  filter(season_year >= 2012) %>%
  left_join(coeffs) %>%
  mutate(wind_avoided = (-coeff_wind) * wind * 500) %>%
  mutate(solar_avoided = (-coeff_solar) * solar * 500)

suma = aggregate(dataWithCoeff$solar_avoided, by=list(Category=dataWithCoeff$year), FUN=sum)
sum(suma$x)/141.7/10^9

map2color <- function(x, pal, limits = range(x)){
  pal[findInterval(x, seq(limits[1], limits[2], length.out = length(pal) + 1),
                   all.inside=TRUE)]
}

d = filter(data, season_year == 2012, season == "winter")
x = pull(d, wind) / 1000
y = pull(d, conso) / 1000
z = pull(d, co2_t_h)
reg = filter(results, year == 2012, month == WINTER, xname == "wind")
a = reg$coeff * 1000
b = reg$coeff_conso * 1000
c = -1
d = reg$intercept / 500

plot3d(x, y, z, col = map2color(z, rainbow(n = 6)), xlab = "Wind", ylab = "Consump.", zlab = "GHG")
planes3d(a, b, c, d, alpha = 0.5)
movie3d(spin3d(axis = c(0, 0, 1)), duration = 6,
        dir = getwd())

