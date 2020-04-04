source("../utils.r")

data = get_all_data()

data2 = data %>%
    filter(year > 2012) %>%
    group_by(year) %>%
    summarise(ccg_prod = sum(gaz_ccg) * 0.5 * 10^(-6))

ggplot(data2, aes(x = paste(year), y = ccg_prod)) +
    geom_col() +
    labs(
        title = "CCG production in France",
        x = "",
        y = "Production (TWh)",
        caption = "Data: RTE 2013 - 2019"
    ) +
    ggsave('./figures/ccg_prod.png', width = unit(10, "cm"))



ccg_mw = append(rep(5598, 3), rep(6173, 4))
installed_ccg = tibble(year, ccg_mw)

data3 = data2 %>%
    left_join(installed_ccg) %>%
    mutate(load_factor = ccg_prod * 10^6 / (ccg_mw * 365 * 24) * 100)

ggplot(data3, aes(x = paste(year), y = load_factor)) +
    geom_col() +
    labs(
        title = "CCG load factor in France",
        x = "",
        y = "Load factor (%)",
        caption = "Data: RTE 2013 - 2019"
    ) +
    ggsave('./figures/ccg_load_factor.png', width = unit(10, "cm"))



