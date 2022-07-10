# VISUALIZE DATA ----

# Goal
# Experiment data visualization for economic dashboard
# Create data visualizing functions for fred time series with us recessions
# Use geom_vline() for recessions. geom_rec() doesn't work for plotly

# https://rpubs.com/jkang09/620791

# Set up

# Core
library(tidyverse)
library(tidyquant)

# Time series
library(timetk)
library(lubridate)

# Interactive visualization
library(plotly)

# Import Data
# Market Indicators
VIX_tbl                <- read_rds("00_data/wrangled_data//VIX_tbl.rds")
Treasury10_Yield_tbl   <- read_rds("00_data/wrangled_data//Treasury10_Yield_tbl.rds")
Yield_Spread_tbl       <- read_rds("00_data/wrangled_data//Yield_Spread_tbl.rds")
Home_Price_tbl         <- read_rds("00_data/wrangled_data//Home_Price_tbl.rds")

# Economic Indicators - US
Recession_Dates_tbl    <- read_rds("00_data/wrangled_data//Recession_Dates_tbl.rds")
Inflation_tbl          <- read_rds("00_data/wrangled_data//Inflation_tbl.rds")
Unemployment_Rate_tbl  <- read_rds("00_data/wrangled_data//Unemployment_Rate_tbl.rds")
GDP_Growth_tbl         <- read_rds("00_data/wrangled_data//GDP_Growth_tbl.rds")
Consumer_Sentiment_tbl <- read_rds("00_data/wrangled_data//Consumer_Sentiment_tbl.rds")

# Industry
realGDP_states_q_NAICS_tbl <- read_rds("00_data/wrangled_data//realGDP_states_q_NAICS_tbl.rds")

# scripts
source("00_scripts/get_realGDP_by_NAICS.R")


# 1 Market and Economic Indicators ----

## 1.1 Plot all in one ----

market_indicators_tbl <- list(Yield_Spread_tbl %>% rename(Yield_Spread = value),
                              VIX_tbl %>% rename(VIX = value),
                              Home_Price_tbl %>%

                                  # Calculate year-over-year change
                                  mutate(value = (value/lag(value, n = 12)-1)*100) %>%
                                  rename(Home_Price = value)) %>%

    # Aggregate
    reduce(left_join) %>%

    # Transform data to long form
    pivot_longer(cols = -date, names_to = "indicator", values_to = "value")


# Necessary to put variables into the facet labels
indicator_desc <- as_labeller(
    c(`Home_Price` = "S&P/Case-Shiller 20-City Composite Home Price Index (year-over-year percent change)",
      `VIX` = "The Chicago Board Options Exchange Volatility Index",
      `Yield_Spread` = "Treasury Yield Spread between the 10 Year Note and the 3 Month Bill"))


market_indicators_fig <- market_indicators_tbl %>%

    mutate(value = round(value, 1)) %>%

    ggplot(aes(date, value)) +
    geom_line(show.legend = FALSE) +

    facet_wrap(~indicator, scales = "free", ncol = 1, labeller = indicator_desc) +

    # geom_rect doesn't show up in plotly
    geom_vline(data = Recession_Dates_tbl %>% filter(USREC == 1),
               aes(xintercept = as.numeric(date)), color = "grey70", size = 0.7, alpha = 0.2) +
    geom_hline(yintercept = 0, color = "#18BC9C", size = 1, alpha = 0.5) +

    labs(y = NULL,
         x = NULL) +

    theme_tq() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank())

ggplotly(market_indicators_fig)

write_rds(market_indicators_fig, "00_data/fig/market_indicators_fig.rds")


economic_indicators_tbl <- list(Inflation_tbl %>% rename(Inflation = value),
                                Unemployment_Rate_tbl %>% rename(Unemployment_Rate = value),
                                Consumer_Sentiment_tbl %>%

                                    # Calculate year-over-year change
                                    mutate(value = (value/lag(value, n = 12)-1)*100) %>%
                                    rename(Consumer_Sentiment = value)) %>%

    # Aggregate
    reduce(left_join) %>%

    # Transform data to long form
    pivot_longer(cols = -date, names_to = "indicator", values_to = "value")


# Necessary to put variables into the facet labels
indicator_desc <- as_labeller(
    c(`Consumer_Sentiment` = "Consumer Sentiment Index (year-over-year percent change)",
      `Inflation` = "Consumer Price Index (year-over-year percent change)",
      `Unemployment_Rate` = "Unemployment Rate (percent)"))


economic_indicators_fig <- economic_indicators_tbl %>%

    mutate(value = round(value, 1)) %>%

    ggplot(aes(date, value)) +
    geom_line(show.legend = FALSE) +

    facet_wrap(~indicator, scales = "free", ncol = 1, labeller = indicator_desc) +

    # geom_rect doesn't show up in plotly
    geom_vline(data = Recession_Dates_tbl %>% filter(USREC == 1),
               aes(xintercept = as.numeric(date)), color = "grey70", size = 0.7, alpha = 0.2) +
    geom_hline(yintercept = 0, color = "#18BC9C", size = 1, alpha = 0.5) +

    labs(y = NULL,
         x = NULL,
         caption = "Source: FRED at St. Louis Fed") +

    theme_tq()  +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank() )


ggplotly(economic_indicators_fig)

write_rds(economic_indicators_fig, "00_data/fig/economic_indicators_fig.rds")


## 1.2 Plot each series separate ----

g <- Treasury10_Yield_tbl %>%

    ggplot(aes(date, value)) +
    geom_line() +

    # geom_rect doesn't show up in plotly
    geom_vline(data = Recession_Dates_tbl %>% filter(USREC == 1),
               aes(xintercept = as.numeric(date)), color = "#4876FF", alpha = 0.3) +

    labs(title = "Market Yield on U.S. Treasury Securities at 10-Year Constant Maturity",
         subtitle = "US Recessions in Blue Shade",
         y = "Percent",
         x = NULL)


ggplotly(g)


# 2 Industry Indicators ----

## 2.1 Plot all in one ----

industry_indicators_tbl <- realGDP_states_q_NAICS_tbl %>%

    # Select place
    filter(place == "the United States") %>%
    filter(NAICS_desc != "Other Services") %>%

    # Calcuate year-over-year growth rate
    group_by(NAICS_desc) %>%
    mutate(industry = (value / lag(value, n = 4)) - 1) %>%
    ungroup() %>%
    na.omit() %>%

    unite(col = "NAICS", NAICS_code, NAICS_desc, sep = " ") %>%

    # Select variables
    select(date, NAICS, industry) %>%

    # Add US gdp growth
    left_join(GDP_Growth_tbl %>%

                  # Convert date to quarter
                  # mutate(date = date %>% lubridate::quarter(type = "year.quarter")) %>%
                  mutate(value = value / 100) %>%
                  rename(overall = value),
              by = "date") %>%

    # Filter out Covid
    filter(date < "2020-01-01") %>%

    # Transform data to long form
    pivot_longer(cols = -c(date, NAICS), names_to = "type", values_to = "growth")

industry_indicators_fig <- industry_indicators_tbl %>%

    mutate(growth = growth %>% round(3)) %>%

    ggplot(aes(date, growth, col = type)) +
    geom_line() +

    geom_hline(yintercept = 0, color = "#18BC9C", size = 0.5, alpha = 0.7) +

    facet_wrap(~NAICS %>% str_trunc(width = 54, side = "right"),
               scales = "free", ncol = 2) +

    scale_x_date(breaks = scales::pretty_breaks(n = 8)) +
    scale_y_continuous(label = scales::percent_format(accuracy = 1)) +
    scale_color_tq() +
    theme_tq() +
    theme(legend.position = "bottom",
          strip.text = element_text(size = 8),
          panel.grid.major.y = element_blank()) +

    labs(subtitle = "Annual Growth of Real GDP",
         y = NULL,
         x = NULL,
         color = NULL)

ggplotly(industry_indicators_fig)%>%
    layout(legend = list(
        orientation = "h",
        x = 0.25))

write_rds(industry_indicators_fig, "00_data/fig/industry_indicators_fig.rds")

## 2.2 Plot each industry separate ----

wrangle_plot_overall_and_industry_gdp <- function(realGDP_states_q_NAICS_tbl,
                                                  remove.postCOVID = FALSE) {

    data <- realGDP_states_q_NAICS_tbl %>%

        # Select place
        filter(place == "the United States") %>%

        # Calcuate year-over-year growth rate
        mutate(growth_industry = (value / lag(value, n = 4)) - 1) %>%
        na.omit() %>%

        # Select variables
        select(date, growth_industry) %>%

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


get_realGDP_by_NAICS(NAICS_code = "NAICS 31-33", freq_txt = "Q") %>%

    wrangle_plot_overall_and_industry_gdp()

dump(list = c("wrangle_plot_overall_and_industry_gdp"),
     file = "00_scripts/wrangle_plot_overall_and_industry_gdp.R")











# *********************************** -----
# Old code ----
# ************************************ ----

# Add us recession shading to plotly
# refer to https://plotly.com/r/shapes/#rectangles

# Create a function to modify Recession_Dates_tbl to individual time series
Tseries <- Home_Price_tbl

customize_recessions_dates <- function(Tseries) {

    data <- Recession_Dates_tbl %>%

        # Match date to time series
        semi_join(Tseries) %>%

        mutate(recession = case_when(
            USREC == 0 ~ "Peak",
            TRUE       ~ "Trough"
        )) %>%
        mutate(change = USREC - lag(USREC)) %>%
        filter(change != 0) %>%
        select(recession, date)

    if(data[1, "recession"] == "Peak") {

        data <- data %>%
            bind_rows(tibble(recession = "Trough",
                             date      = min(Tseries$date, na.rm = T))) %>%
            arrange(date)

    }

    data %>%
        pivot_wider(names_from = recession, values_from = date, values_fn = list) %>%
        unnest(c(Trough, Peak))

}

recession.dates <- Yield_Spread_tbl %>% customize_recessions_dates()


# Plot time series ----

## long code ----
fig <- plot_ly(VIX_tbl, x = ~date, y = ~value, type = "scatter", mode = "lines")

# add shapes to the layout
fig <- fig %>% layout(title = 'Highlighting with US Recessions',
                      xaxis = list(title = ""),
                      yaxis = list(title = ""),
                      shapes = list(
                          list(type = "rect",
                               fillcolor = "pink", line = list(color = "pink"), opacity = 0.3,
                               x0 = "1990-08-01", x1 = "1991-04-01", xref = "x",
                               y0 = 0, y1 = 70, yref = "y"),
                          list(type = "rect",
                               fillcolor = "pink", line = list(color = "pink"), opacity = 0.3,
                               x0 = "2001-04-01", x1 = "2001-12-01", xref = "x",
                               y0 = 0, y1 = 70, yref = "y"),
                          list(type = "rect",
                               fillcolor = "pink", line = list(color = "pink"), opacity = 0.3,
                               x0 = "2008-01-01", x1 = "2009-07-01", xref = "x",
                               y0 = 0, y1 = 70, yref = "y"),
                          list(type = "rect",
                               fillcolor = "pink", line = list(color = "pink"), opacity = 0.3,
                               x0 = "2020-03-01", x1 = "2020-05-01", xref = "x",
                               y0 = 0, y1 = 70, yref = "y")
                      )
)

fig

# Add comment
Tseries <- Treasury10_Yield_tbl

generate_comment <- function(Tseries, title = "") {

    diff_between_date <- Tseries %>% head(2) %>% pull(date) %>% diff() %>% as.numeric()

    if (diff_between_date < 60) {
        k_num = 12
        k_txt = "12-month"
    } else {
        k_num = 4
        k_txt = "4-quarter"
    }

    Tseries <- Tseries %>% mutate(mavg = rollmean(value, k = k_num, na.pad = TRUE, align = "right", na.rm = TRUE))
    latest_value <- Tseries %>% tail(1) %>% pull(mavg) %>% round(digits = 1)
    year_prior_date  <- Tseries %>% mutate(date = date %-time% "1 year") %>% tail(1) %>% pull(date)
    year_prior_value <- Tseries %>% filter(date == year_prior_date) %>% pull(mavg) %>% round(digits = 1)

    if (latest_value > year_prior_value) {

        str_glue("The latest value of the {k_txt} moving average is {latest_value},
        higher than {year_prior_value} a year ago, indicating an improving economy.")

    } else {

        str_glue("The latest value of the {k_txt} moving average is {latest_value},
        lower than {year_prior_value} a year ago, indicating a worsening economy.")

    }

}

Treasury10_Yield_tbl %>% generate_comment()


## short code ----
# Create a plotting function with us recessions
Tseries <- Treasury10_Yield

plot_Tseries_with_us_recessions <- function(Tseries, title_text = "",
                                            yaxis_title = "") {

    # Fit us recessions for time series
    recession.dates <- Tseries %>% customize_recessions_dates()

    # Create layout for us recessions
    rec_list <- NULL

    for (i in 1:nrow(recession.dates)) {

        rec <- list(type = "rect", fillcolor = "gray", line = list(color = "gray"), opacity = 0.3,
                    x0 = recession.dates[[i, 1]], x1 = recession.dates[[i, 2]], xref = "x",
                    y0 = Tseries$value %>% min(na.rm = TRUE) %>% floor(),
                    y1 = Tseries$value %>% max(na.rm = TRUE) %>% ceiling(),
                    yref = "y")

        rec_list[i] <- list(rec)

    }

    rec_list



    fig <- plot_ly(Tseries, x = ~date, y = ~value, type = "scatter", mode = "lines")

    fig %>% layout(title = title_text,
                   xaxis = list(title = generate_comment(Tseries, title = title_text), showgrid = FALSE),
                   yaxis = list(title = yaxis_title),
                   shapes = rec_list
    )

}

Treasury10_Yield_tbl %>% plot_Tseries_with_us_recessions(title_text = "10-YR Treasury Yield", yaxis_title = "Percent")
VIX_tbl %>% plot_Tseries_with_us_recessions()
Home_Price_tbl %>% plot_Tseries_with_us_recessions()

dump(list = c("customize_recessions_dates",
              "generate_comment",
              "plot_Tseries_with_us_recessions"),
     file = "00_scripts/plot_Tseries_with_us_recessions.R")

# To do list
# 1 Revise the last part of the comment in the function.
# An increase isn't always indicative of an improving economy.
# 2 NAs in time series: e.g., Consumer Sentiment






# Industry ----

# How sensitive is your industry to the national economy?
# Does your industry lead or lag the national economy?
# Is your industry more or less volatile than the national economy?

# Get the national GDP
GDP_Growth_tbl

# Get the inudstry GDP
realGDP_states_q_NAICS_tbl %>% glimpse()

realGDP_states_q_NAICS_tbl %>%

    # Select place
    filter(place == "the United States") %>%

    # Select industry
    filter(str_detect(NAICS_desc %>% str_to_lower(), "profess")) %>%

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
              by = "date") %>%

    # Filter out COVID
    filter(date < "2020-01-01") %>%

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

wrangle_plot_overall_and_industry_gdp <- function(realGDP_states_q_NAICS_tbl,
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

realGDP_states_q_NAICS_tbl %>% wrangle_plot_overall_and_industry_gdp()

dump(list = c("wrangle_plot_overall_and_industry_gdp"), file = "00_scripts/wrangle_plot_overall_and_industry_gdp.R")





