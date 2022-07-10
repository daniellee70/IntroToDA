plot_LQ <-
function(gdp_LQ_tbl,
                    is_LQ_dollars = TRUE,
                    include_all_year = TRUE,
                    industry_trunc_numb = 20,
                    panel_spacing_dist = 1,
                    title_text = "") {

    # Decide which variable is mapped to which axis
    if (include_all_year) {

        # Decide type of LQ
        if (is_LQ_dollars) {

            g <- gdp_LQ_tbl %>%

                filter(place %in% c("New Hampshire", "Massachusetts")) %>%

                # Plot
                ggplot(aes(year %>% as.factor(),
                           NAICS_desc %>%
                               str_trunc(industry_trunc_numb, side = "right") %>%
                               reorder_within(-LQ_dollar_rank, place),
                           text = label_LQ_dollar)) +
                geom_tile(aes(fill = -LQ_dollar_rank)) +
                facet_wrap(~place, scales = "free_y")

        } else {

            g <- gdp_LQ_tbl %>%

                filter(place %in% c("New Hampshire", "Massachusetts")) %>%

                # Plot
                ggplot(aes(year %>% as.factor(),
                           NAICS_desc %>%
                               str_trunc(industry_trunc_numb, side = "right") %>%
                               reorder_within(-LQ_rank, place),
                           text = label_lq)) +
                geom_tile(aes(fill = -LQ_rank)) +
                facet_wrap(~place, scales = "free_y")

        }

    } else {

        if (is_LQ_dollars) {

            g <- gdp_LQ_tbl %>%

                filter(year %in% c(min(year), max(year))) %>%

                ggplot(aes(x = NAICS_desc %>% str_trunc(industry_trunc_numb, side = c("right")),
                           y = place,
                           text = label_LQ_dollar)) +
                geom_tile(aes(fill = LQ_dollar)) +

                facet_wrap(~year, scales = "free")

        } else {

            g <- g <- gdp_LQ_tbl %>%

                filter(year %in% c(min(year), max(year))) %>%

                ggplot(aes(x = NAICS_desc %>% str_trunc(industry_trunc_numb, side = c("right")),
                           y = place,
                           text = label_lq)) +
                geom_tile(aes(fill = log(LQ))) +

                facet_wrap(~year, scales = "free")

        }

    }

    # Determine value for midpoint
    if (include_all_year) midpoint_value = -10
    else midpoint_value = 0


    g_formatted <- g  +

        # Formatting
        scale_fill_gradient2(low = palette_light()[1],
                             mid = "white",
                             high = palette_light()[2],
                             midpoint = midpoint_value) +
        scale_y_reordered() +
        theme_tq() +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none",
            plot.title = element_text(face = "bold"),
            plot.caption = element_text(face = "bold.italic")
        ) +

        labs(
            title = title_text,
            subtitle = "measured in location quotient",
            caption = "New Hampshire's top industry is Retail while...",
            x     = "",
            y     = ""
        ) +

        theme(panel.spacing=unit(panel_spacing_dist, "cm"))


    g_formatted %>% ggplotly(tooltip = "text")

}
