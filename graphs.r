

PROD_TYPES_COLORS = c('#2772b2', '#f5b300', '#74cdb9', '#f27406', '#f30a0a', '#166a57', '#8356a2', '#ac8c35', '#505050', '#969696', '#114774')
names(PROD_TYPES_COLORS) = c('hydro', 'nuclear', 'wind', 'solar', 'gas', 'bioenergies', 'oil', 'coal', 'import', 'export', 'hydro_pumped_storage')

PROD_TYPES_ORDER = c('hydro_pumped_storage', 'export', 'bioenergies', 'nuclear', 'hydro', 'gas', 'oil', 'coal',  'wind', 'solar', 'import')
PROD_TYPES_ORDER_2 = c('hydro_pumped_storage', 'export', 'bioenergies', 'wind', 'solar', 'nuclear', 'hydro', 'gas', 'oil', 'coal', 'import')

date_labels = function(breaks) {
    res = sapply(breaks, function(the_break) {

        if (is.na(the_break)) {
            return('')
        }

        if (hour(the_break) == 0 && minute(the_break) == 0) {
            return(format(the_break, format = '%a\n%Y-%m-%d\n00:00'))
        }

        return(format(the_break, format = '%H:00'))
    })

    return(res)
}

prod_by_types_graph = function(data, start_date, end_date, prod_type_order = PROD_TYPES_ORDER) {
    start_date = ymd(start_date)
    end_date = ymd(end_date)
    data = data %>%
        filter(start_date <= datetime, datetime <= end_date) %>%
        add_import_export() %>%
        mutate(export = -export) %>%
        select(datetime, conso, hydro, nuclear, bioenergies, gas, coal, oil, wind, solar, import, export, hydro_pumped_storage) %>%
        gather(-conso, -datetime, key = 'prod_type', value = 'value') %>%
        arrange(datetime)

    data$prod_type = factor(data$prod_type, levels = rev(prod_type_order))

    plot = ggplot(data, aes(x = datetime)) +
        geom_area(aes(y = value, fill = prod_type), alpha = 0.8) +
        geom_line(aes(y = conso), color = 'black') +
        scale_x_datetime(date_minor_breaks = '1 hour', labels = date_labels ) +
        scale_fill_manual(values = PROD_TYPES_COLORS) +
        labs(
            subtitle = 'The black line is the consumption',
            x = '',
            y = 'Power (MW)',
            fill = 'Types',
            caption = 'Source: RTE'
        )

    plot
}

monthly_graph = function(base_type_prod, monthly_type_prod, title = 'Base & monthly production') {
    options(warn=-1)

    # compute base prod in TWh
    base_prod = base_type_prod %>%
        summarise(prod = sum(base_prod)) %>%
        pull()

    # make it a 'type' tibble
    base_prod_t = tibble(month = seq(1, 12)) %>%
        mutate(
            type = 'base',
            prod = base_prod
        )

    # add 'base' type
    monthly_all_prod = monthly_type_prod %>%
        bind_rows(base_prod_t) %>%
        mutate(month_label = month(month, TRUE, TRUE, locale="en_US"))

    # make a 'base' month
    base_month = base_type_prod %>%
        rename(prod = base_prod) %>%
        mutate(month = 0) %>%
        mutate(month_label = 'base')

    # bind all together
    graph_data = monthly_all_prod %>%
        bind_rows(base_month)

    prod_types_colors = c(PROD_TYPES_COLORS, 'grey')
    names(prod_types_colors) = c(names(PROD_TYPES_COLORS), 'base')

    order = c(rev(PROD_TYPES_ORDER_2), 'base')
    graph_data$type = factor(graph_data$type, levels = order)

    options(warn=-1)

    ggplot(graph_data, aes(x = reorder(month_label, month), y = prod, fill = type)) +
        geom_col() +
        scale_fill_manual(values = prod_types_colors) +
        labs(
            title = paste(title, ' (', start_year, ' - ', end_year, ')', sep=''),
            x = '',
            y = 'Production (TWh)',
            fill = 'Types',
            caption = paste('Data: RTE ', start_year, ' - ', end_year, sep='')
        )
}
