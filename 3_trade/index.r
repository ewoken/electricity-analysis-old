source("../utils.r")

data = get_all_data()

ggplot(data, aes(x = conso, y = trade)) +
    geom_point(shape=".") +
    facet_grid(cols = vars(year), rows = vars(season))

ggplot(data, aes(x = trade / 1000)) +
    geom_histogram() +
    facet_grid(cols = vars(year), rows = vars(season))

d = data %>%
    filter(trade > 0) %>%
    group_by(year) %>%
    summarise(s = sum(trade) * 0.0005)

ggplot(d, aes(x = paste(year), y = s, label = s)) +
    geom_col() +
    labs(
        x = "",
        y = "GWh",
        title = "Net electricity imports of France",
        subtitle = "Source: RTE"
    ) +
    ggsave("./figures/imports.png")

d = data %>%
    filter(trade < 0) %>%
    group_by(year, season) %>%
    summarise(s = sum(-trade) * 0.0005)

ggplot(d, aes(x = paste(year), y = s, label = s)) +
    geom_col(aes(fill = season)) +
    labs(
        x = "",
        y = "GWh",
        title = "Net electricity exports of France",
        subtitle = "Source: RTE"
    ) +
    ggsave("./figures/exports_season.png")





