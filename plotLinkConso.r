source("./utils.r")

data = getAllData()

plotLinkConso = function(data, varname, xTitle, yTitle, myTitle) {
    cur_data = filter(data, year == 2018)

    ggplot(cur_data, mapping = aes(x = conso, y = get(varname))) +
        stat_smooth(method = "lm") +
        geom_point(color="dodgerblue4", shape="." ) +
        geom_smooth(method="lm", color="red") +
        facet_wrap(~month) +
        labs(
            x=xTitle,
            y=yTitle,
            title=myTitle,
            caption = "Source: RTE"
        )
    ggsave(paste('./figures/linkConso/', varname, '.png', sep = ""))
}

plotLinkConso(data, "wind", "Consumption (MW)", "Wind production (MW)", "Link between consumption and wind production (2018)")
plotLinkConso(data, "nuclear", "Consumption (MW)", "Nuclear production (MW)", "Link between consumption and nuclear production (2018)")
plotLinkConso(data, "fossil", "Consumption (MW)", "Fossil production (MW)", "Link between consumption and fossil production (2018)")
plotLinkConso(data, "solar", "Consumption (MW)", "Solar production (MW)", "Link between consumption and solar production (2018)")
plotLinkConso(data, "hydro", "Consumption (MW)", "Hydro production (MW)", "Link between consumption and hydro production (2018)")
