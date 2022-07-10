# IMPORT CLEAN DATA ----

# Goal
# Import data to build economic dashboard of Russell Investment
# https://russellinvestments.com/us/resources/financial-professionals/economic-indicators-dashboard

# Set up

# Core
library(tidyverse)
library(tidyquant)

# Time series
library(timetk)

# Interactive visualization
library(plotly)
library(tidytext)


# Import data ----

gdp <- read_rds("00_data/realGDP_states_a_NAICS_tbl.rds")


# Clean data ----

# Note: state total is the sum of the private industry, excluding NAICS 92: Public Administration

gdp_us_tbl <- gdp %>%

    # filter(year %in% c(min(year), max(year))) %>%
    filter(place == "the United States") %>%

    # Get all industry total for us
    add_tally(value, name = "total") %>%
    mutate(pct_of_total_us = value / total)

gdp_us_tbl

gdp_state_tbl <- gdp %>%

    # filter(year %in% c(min(year), max(year))) %>%
    filter(place != "the United States") %>%

    select(year, value, NAICS_code, NAICS_desc, place) %>%

    # Calculate state total
    add_count(place, wt = value, name = "state_total") %>%

    # Calculate industry pct of state total
    group_by(place) %>%
    mutate(pct_of_total_state = value / state_total) %>%
    ungroup()

gdp_state_tbl

# Calculate LQ ----

gdp_LQ_tbl <- gdp_state_tbl %>%

    select(year, NAICS_code, NAICS_desc, place, value, state_total, pct_of_total_state) %>%

    left_join(gdp_us_tbl %>% select(year, NAICS_code, pct_of_total_us)) %>%

    # Calculate LQ
    group_by(place) %>%
    mutate(LQ = pct_of_total_state / pct_of_total_us) %>%
    ungroup() %>%

    # Calculate LQ in $
    mutate(value_wouldbe = state_total * pct_of_total_us) %>%
    mutate(LQ_dollar = value - value_wouldbe) %>%

    # Create rank within year and place
    group_by(year, place) %>%
    arrange(desc(LQ)) %>%
    mutate(LQ_rank = row_number()) %>%
    ungroup() %>%

    # Create rank within year and place
    group_by(year, place) %>%
    arrange(desc(LQ_dollar)) %>%
    mutate(LQ_dollar_rank = row_number()) %>%
    ungroup() %>%

    mutate(label_lq = str_glue("State: {place}
                                Year: {year}
                                NAICS: {NAICS_desc}
                                real GDP: {value %>% scales::dollar(suffix = 'M')}
                                Location Quotient: {LQ %>% round(digits = 1)}
                                It is the {LQ_rank}th most important industry in {place},
                                measured by location quotients.")) %>%

    mutate(label_LQ_dollar = str_glue("State: {place}
                                      Year: {year}
                                      NAICS: {NAICS_desc}
                                      real GDP: {value %>% scales::dollar(suffix = 'M')}
                                      Location Quotient in $: {LQ_dollar %>% scales::dollar(suffix = 'M')}
                                      It is the {LQ_dollar_rank}th most important industry in {place},
                                      measured by location quotients in dollars."))

gdp_LQ_tbl

write_rds(gdp_LQ_tbl, "00_data/gdp_LQ_tbl.rds")

# Plot LQ ----



## * Heatmap: all years ----
# Compare NH and MA

g <- gdp_LQ_tbl %>%

    filter(place %in% c("New Hampshire", "Massachusetts")) %>%

    # Plot
    ggplot(aes(year %>% as.factor(),
               NAICS_desc %>%
                   str_trunc(20, side = "right") %>%
                   reorder_within(-LQ_rank, place),
               text = label_lq)) +
    geom_tile(aes(fill = -LQ_rank)) +
    facet_wrap(~place, scales = "free_y") +

    # Formatting
    scale_fill_gradient2(low = palette_light()[1],
                         mid = "white",
                         high = palette_light()[2],
                         midpoint = -10) +
    scale_y_reordered() +
    theme_tq() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(face = "bold.italic")
    ) +

    labs(
        title = "Which industries are major drivers of the economy?",
        subtitle = "measured in location quotient",
        caption = "New Hampshire's top industry is Retail while...",
        x     = "",
        y     = ""
    )


g %>% ggplotly(tooltip = "text")




g <- gdp_LQ_tbl %>%

    filter(place %in% c("New Hampshire", "Massachusetts")) %>%

    # Plot
    ggplot(aes(year %>% as.factor(),
               NAICS_desc %>%
                   str_trunc(20, side = "right") %>%
                   reorder_within(-LQ_dollar_rank, place),
               text = label_LQ_dollar)) +
    geom_tile(aes(fill = -LQ_dollar_rank)) +
    facet_wrap(~place, scales = "free_y") +

    # Formatting
    scale_fill_gradient2(low = palette_light()[1],
                         mid = "white",
                         high = palette_light()[2],
                         midpoint = -10) +
    scale_y_reordered() +
    theme_tq() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(face = "bold.italic")
    ) +

    labs(
        title = "Which industries are major drivers of the economy?",
        subtitle = "measured in location quotient $",
        caption = "New Hampshire's top industry is Retail while...",
        x     = "",
        y     = ""
    )


g %>% ggplotly(tooltip = "text")

## * Heatmap: all 50 states ----
# Compare 1997 and 2021

# LQ in decimal numbers

g <- gdp_LQ_tbl %>%

    filter(year %in% c(min(year), max(year))) %>%

    ggplot(aes(x = NAICS_desc %>% str_trunc(20, side = c("right")),
               y = place,
               text = label_lq)) +
    geom_tile(aes(fill = log(LQ))) +

    facet_wrap(~year, scales = "free") +

    scale_fill_gradient2(low = palette_light()[5],
                         mid = "white",
                         high = palette_light()[2],
                         midpoint = 0) +
    labs(
        title = "Location Quotient",
        y     = "",
        x     = "NAICS Sectors - 2 digits"
    ) +

    theme_tq() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(face = "bold.italic")
    )

g %>% ggplotly(tooltip = "text")

# LQ in dollars
g <- gdp_LQ_tbl %>%

    filter(year %in% c(min(year), max(year))) %>%

    ggplot(aes(x = NAICS_desc %>% str_trunc(20, side = c("right")),
               y = place,
               text = label_LQ_dollar)) +
    geom_tile(aes(fill = LQ_dollar)) +

    facet_wrap(~year, scales = "free") +

    scale_fill_gradient2(low = palette_light()[5],
                         mid = "white",
                         high = palette_light()[2],
                         midpoint = 0) +
    labs(
        title = "Location Quotient in Dollars",
        y     = "",
        x     = "NAICS Sectors - 2 digits"
    ) +

    theme_tq() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(face = "bold.italic")
    )

g %>% ggplotly(tooltip = "text")


## * Line plot ----

# LQ in decimal numbers

g <- gdp_LQ_tbl %>%

    filter(place == "New Hampshire") %>%

    ggplot(aes(year, LQ,
               color = NAICS_desc %>% str_trunc(20, side = "right"))) +
    geom_line() +
    geom_point(aes(text = label_lq),
               size = 0.1,
               alpha = 0.3) +

    theme_tq() +
    scale_color_tq() +

    labs(
        title = "New Hampshire's Location Qutient",
        color = "NAICS Sectors",
        x     = "",
        y     = "Location Quotient"
    )

g %>% ggplotly(tooltip = "text")

# LQ in dollars

g <- gdp_LQ_tbl %>%

    filter(place == "New Hampshire") %>%

    ggplot(aes(year, LQ_dollar,
               color = NAICS_desc %>% str_trunc(20, side = "right"))) +
    geom_line() +
    geom_point(aes(text = label_LQ_dollar),
               size = 0.1,
               alpha = 0.3) +

    theme_tq() +
    scale_color_tq() +
    scale_y_continuous(labels = scales::dollar_format(suffix = "B", scale = 1e-3)) +

    labs(
        title = "New Hampshire's Location Qutient in Dollars",
        color = "NAICS Sectors",
        x     = "",
        y     = "Location Quotient in Dollars"
    )

g %>% ggplotly(tooltip = "text")



# Two types LQ: LQ in decimal and LQ in dollars
# LQ_rank_type: 1) LQ_rank 2) LQ_dollar_rank
# label_type: 1) label_lq 2) label_LQ_dollar

plot_LQ <- function(gdp_LQ_tbl,
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

gdp_LQ_tbl %>% plot_LQ(is_LQ_dollars = FALSE,
                       include_all_year = FALSE,
                       industry_trunc_numb = 20,
                       panel_spacing_dist = 1,
                       title_text = "Which industries are major drivers of the economy?")

dump(list = "plot_LQ",
     file = "00_scripts/plot_LQ.R")
