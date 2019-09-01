source("./utils.r")

data = get_all_data()

plot_season_distrib = function(data, varname, yname, cur_title) {
    ggplot(data) +
        geom_boxplot(
            mapping = aes(
                x = month_label,
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
            y=yname,
            title=cur_title,
            caption = "Source: RTE"
        )

    ggsave(paste("./figures/season_distrib/", varname, ".png", sep=""))
}

plot_season_distrib(data, "solar_load_factor", "Solar load factor", "Distribution of solar load factor in France (2012 - 2018)")
plot_season_distrib(data, "wind_load_factor", "Wind load factor", "Distribution of wind load factor in France (2012 - 2018)")
plot_season_distrib(data, "fossil", "Fossil load (MW)", "Distribution of fossil load in France (2012 - 2018)")
plot_season_distrib(data, "nuclear", "Nuclear load (MW)", "Distribution of nuclear load in France (2012 - 2018)")
plot_season_distrib(data, "conso", "Consumption (MW)", "Distribution of consumption in France (2012 - 2018)")
plot_season_distrib(data, "hydro", "Hydro (MW)", "Distribution of hydro load in France (2012 - 2018)")
plot_season_distrib(data, "trade", "Trade (MW)", "Distribution of trade in France (2012 - 2018)")
