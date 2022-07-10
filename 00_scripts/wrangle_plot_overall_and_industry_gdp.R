wrangle_plot_overall_and_industry_gdp <-
function(realGDP_states_q_NAICS_tbl,
                                                  NAICS_text = "construction",
                                                  remove.postCOVID = FALSE) {

    data <- realGDP_states_q_NAICS_tbl %>%

        # Select place
        filter(place == "the United States") %>%

        # Select industry
        filter(str_detect(NAICS_desc %>% str_to_lower(), NAICS_text)) %>%

        # Calcuate year-over-year growth rate
        mutate(growth_industry = (value / lag(value, n = 4)) - 1) %>%
        na.omit() %>%

        # Select variables
        select(date = period, growth_industry) %>%

        # Convert date to numeric
        mutate(date = date %>% yq() %>% quarter(type = "date_last")) %>%

        # Add US gdp growth
        left_join(GDP_Growth_tbl %>%

                      # Convert date to quarter
                      # mutate(date = date %>% lubridate::quarter(type = "year.quarter")) %>%
                      mutate(value = value / 100) %>%
                      rename(growth_us = value),
                  by = "date")

    # Filter out COVID
    if (remove.postCOVID) {

        data <- data %>%

            filter(date < "2020-01-01")

    }

    g <- data %>%

        # Transform data to long form
        pivot_longer(cols = -date, names_to = "place", values_to = "growth") %>%

        # Plot
        ggplot(aes(date, growth, col = place)) +
        geom_line() +

        scale_x_date(breaks = scales::pretty_breaks(n = 7)) +
        scale_y_continuous(label = scales::percent) +

        labs(title = "Overall U.S. Economy versus Individual Industry",
             subtitle = "Annual Growth of Real GDP",
             y = "Year-Over-Year Percent Change")

    ggplotly(g)

}
