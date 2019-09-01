library(tidyverse)
library(lubridate)
library(car)
library(lmtest)

source("./utils.r")
source("./common_graphs.r")

data = get_all_data()

get_linear_reg = function(data, cur_year, cur_month, cur_xname, cur_yname) {
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
    if (cur_yname == "co2_kg") {
        x = x * 500 # kWh/MW producted in 1/2 hour
    }

    y = unlist(cur_data[cur_yname])
    reg = lm(y ~ x)

    return(reg)
}

get_linear_results = function(data) {
  results = tibble(year = -1, month = 0, yname = "",
                  coeff = 0, xname = "",
                  intercept = 0, r2 = 0) # init first line
  months = seq(1, 12)
  months = c(months, SUMMER, WINTER)
  years = c(years, ALL_YEARS)
  xnames = c("solar", "wind")
  ynames = c("co2_kg")

  for(cur_year in years) {
    for (cur_month in months) {
        for(cur_xname in xnames) {
            for (cur_yname in ynames) {
                reg = get_linear_reg(data, cur_year, cur_month, cur_xname, cur_yname)

                cur_r2 = summary(reg)$r.squared
                results = add_row(results,
                    year=cur_year,
                    month=cur_month,
                    xname=cur_xname,
                    yname=cur_yname,
                    intercept=reg$coefficients[1],
                    coeff=reg$coefficients[2],
                    r2 = cur_r2
                )
            }
        }
    }
  }

  results = results %>%
    filter(year > 0) %>% # remove first line
    arrange(year, month) %>%
    mutate(month_label = month(month, TRUE, TRUE, locale="en_US")) %>%
    mutate(date = as.Date(make_datetime(year, month)))

  return(results)
}

plot_reg_by_year = function(data) {
  plot = ggplot(data, aes(x = wind / 1000, y = co2_t_h)) +
    geom_point(shape=".", alpha = 0.2) +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    scale_x_continuous(labels = (function(v) { return(as.integer(v)) })) +
    facet_wrap(~year) +
    labs(
      x = "Wind (GW)",
      y = "GHG emissions (t CO2eq/h)"
    )

  ggsave('./figures/linear_method/linear_reg_by_year.png')
  return(plot)
}

plot_linear_reg_group_by_month = function(data, results, cur_xname) {
  d = results %>%
    filter(xname == cur_xname, yname == "co2_kg", year == ALL_YEARS)

  plot = ggplot(data, aes(x = wind / 1000, y = co2_t_h)) +
    geom_point(shape=".") +
    geom_smooth(method = "lm") +
    scale_x_continuous(labels = (function(v) { return(as.integer(v)) })) +
    facet_wrap(~month_label) +
    labs(
      x = "Wind (GW)",
      y = "GHG emissions (t CO2eq/h)"
    )

  ggsave("./figures/linear_method/regression_group_by_month.png", width = unit(9, "cm"))
  return(plot)
}

results = get_linear_results(data)

plot_results_all_months(results, "linear_method")
plot_results_group_by_month(results, folder = "linear_method")
plot_linear_reg_group_by_month(data, results, "wind")
plot_results_by_season(results, "linear_method")

wind_impact_by_year = compute_avoided_emissions_by_year(data, results) %>% filter(xname == "wind")

wind_impact = wind_impact_by_year %>%
  group_by(xname) %>%
  summarise(co2_kg = sum(co2_kg)) %>%
  select(co2_kg) %>%
  dplyr::first()

d = filter(data, year <= 2015)
ggplot(d, aes(x = wind, y = trade)) +
  geom_point(shape=".") +
  geom_smooth(method = "lm") +
  labs(
    title = "Correlation between french wind production and exportation (2012-2015)",
    subtitle = "In blue, regression line: y = -0.288x - 5927",
    x = "Wind production (MW)",
    y = "Physical exchanges (MW)",
    caption = "Data: RTE"
  )
ggsave("regression.png", width = unit(9, "cm"))
x = pull(d, wind)
y = pull(d, trade)
reg = lm(y ~ x)

