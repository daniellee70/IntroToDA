# IMPORT CLEAN DATA ----

# Goal
# Import data to build economic dashboard of Russell Investment
# https://russellinvestments.com/us/resources/financial-professionals/economic-indicators-dashboard

# Set up

# Core
library(tidyverse)
library(tidyquant)

# FRED data
library(eFRED)

# BEA data
library(bea.R)

# Time series
library(timetk)


api_key <- "11c92c9630cc2c5c83eb5da3b01a59f0"
set_fred_key(api_key)



# 1 Market Indicators ----

## * Market Volatility ----

download_and_clean_VIX <- function() {

    search_results <- fred_search(text = "CBOE VIX", key = api_key)

    VIX_tbl <- search_results %>%

        # Get id
        filter(title == "CBOE Volatility Index: VIX") %>%
        pull(id) %>%

        # Download data
        fred(key = api_key) %>%
        set_names(c("date", "value")) %>%

        # Convert to monthly
        summarise_by_time(.date_var = date, value = mean(value, na.rm = T), .by = "month") %>%

        # Move date to the last day of the month
        mutate(date = date %+time% "1 month" %-time% "1 day")

}

VIX_tbl <- download_and_clean_VIX()

write_rds(VIX_tbl, "00_data/VIX_tbl.rds")

## * 10 Year US Treasury Yield ----

download_and_clean_Treasury10_Yield <- function() {

    search_results <- fred_search(text = "Treasury", key = api_key)

    Treasury10_Yield_tbl <- search_results %>%

        # Get id (Inflation-Indexed measure is also available in FRED)
        filter(title == "Market Yield on U.S. Treasury Securities at 10-Year Constant Maturity, Quoted on an Investment Basis") %>%
        pull(id) %>%

        # Download
        fred(key = api_key) %>%
        select(date, DGS10) %>%
        set_names(c("date", "value")) %>%

        # Remove NAs before 1962
        filter(date >= ymd("1962-01-01")) %>%

        # Convert to monthly
        summarise_by_time(.date_var = date, value = last(value), .by = "month") %>%

        # Move date to the last day of the month
        mutate(date = date %+time% "1 month" %-time% "1 day")

}

Treasury10_Yield_tbl <- download_and_clean_Treasury10_Yield()

write_rds(Treasury10_Yield_tbl, "00_data/Treasury10_Yield_tbl.rds")

## * Yield Spread ----

download_and_clean_Yield_Spread <- function() {

    search_results <- fred_search(text = "Treasury", key = api_key)

    Yield_Spread_tbl <- search_results %>%

        # Get id
        filter(title == "10-Year Treasury Constant Maturity Minus 3-Month Treasury Constant Maturity") %>%
        filter(frequency == "Daily") %>%
        pull(id) %>%

        # Download
        fred(key = api_key) %>%
        set_names(c("date", "value")) %>%

        # Convert to monthly
        summarise_by_time(.date_var = date, value = last(value), .by = "month") %>%

        # Move date to the last day of the month
        mutate(date = date %+time% "1 month" %-time% "1 day")

}

Yield_Spread_tbl <- download_and_clean_Yield_Spread()

write_rds(Yield_Spread_tbl, "00_data/Yield_Spread_tbl.rds")


## * Home Prices ----

download_and_clean_Home_Price <- function() {

    search_results <- fred_search(text = "Home Price", key = api_key)

    Home_Price_tbl <- search_results %>%

        # Get id
        filter(title == "S&P/Case-Shiller 20-City Composite Home Price Index") %>%
        filter(seasonal_adjustment_short == "NSA") %>%
        pull(id) %>%

        # Download
        fred(key = api_key) %>%
        set_names(c("date", "value")) %>%

        # Move date to the last day of the month
        mutate(date = date %+time% "1 month" %-time% "1 day") %>%

        filter(!is.na(value))

}

Home_Price_tbl <- download_and_clean_Home_Price()

write_rds(Home_Price_tbl, "00_data/Home_Price_tbl.rds")



# 2 Economic Indicators: US ----

## * Recession Dates ----

download_and_clean_Recession_Dates <- function() {

    search_results <- fred_search(text = "recession", key = api_key)

    Recession_Dates_tbl <- search_results %>%
        filter(id == "USREC") %>%
        pull(id) %>%
        fred(key = api_key) %>%

        # Move date to the last day of the month
        mutate(date = date %+time% "1 month" %-time% "1 day")

}

Recession_Dates_tbl <- download_and_clean_Recession_Dates()

write_rds(Recession_Dates_tbl, "00_data/Recession_Dates_tbl.rds")

## * Inflation ----

download_and_clean_Inflation <- function() {

    search_results <- fred_search(text = "consumer price index", key = api_key)

    Inflation_tbl <- search_results %>%

        # Get id
        filter(title == "Consumer Price Index: Total All Items for the United States") %>%
        filter(frequency_short == "M") %>%
        filter(units == "Growth Rate Same Period Previous Year") %>%
        pull(id) %>%

        # Download
        fred(key = api_key) %>%
        set_names(c("date", "value")) %>%

        # Move date to the last day of the month
        mutate(date = date %+time% "1 month" %-time% "1 day") %>%

        filter(!is.na(value))

}

Inflation_tbl <- download_and_clean_Inflation()

write_rds(Inflation_tbl, "00_data/Inflation_tbl.rds")

## * Unemployment ----

download_and_clean_Unemployment_Rate <- function() {

    search_results <- fred_search(text = "unemployment", key = api_key)

    Unemployment_Rate_tbl <- search_results %>%

        # Get id
        filter(title == "Unemployment Rate") %>%
        filter(seasonal_adjustment_short == "SA") %>%
        pull(id) %>%

        # Download
        fred(key = api_key) %>%
        set_names(c("date", "value")) %>%

        # Move date to the last day of the month
        mutate(date = date %+time% "1 month" %-time% "1 day")

}

Unemployment_Rate_tbl <- download_and_clean_Unemployment_Rate()

write_rds(Unemployment_Rate_tbl, "00_data/Unemployment_Rate_tbl.rds")

## * Economic Expansion ----

download_and_clean_GDP_Growth <- function() {

    search_results <- fred_search(text = "gross domestic product", key = api_key)

    GDP_Growth_tbl <- search_results %>%

        # Get id
        filter(title == "Real Gross Domestic Product") %>%
        filter(seasonal_adjustment_short == "SAAR") %>%
        filter(units == "Percent Change from Preceding Period") %>%
        pull(id) %>%

        # Download
        fred(key = api_key) %>%
        set_names(c("date", "value")) %>%

        # Move date to the last day of the quarter
        mutate(date = date %+time% "3 months" %-time% "1 day")

}

GDP_Growth_tbl <- download_and_clean_GDP_Growth()

write_rds(GDP_Growth_tbl, "00_data/GDP_Growth_tbl.rds")

## * Consumer Sentiment ----

download_and_clean_Consumer_Sentiment <- function() {

    search_results <- fred_search(text = "Consumer Sentiment", key = api_key)

    Consumer_Sentiment_tbl <- search_results %>%

        # Get id
        filter(title == "University of Michigan: Consumer Sentiment") %>%
        pull(id) %>%

        # Download
        fred(key = api_key) %>%
        set_names(c("date", "value")) %>%

        # Move date to the last day of the month
        mutate(date = date %+time% "1 month" %-time% "1 day")

}

Consumer_Sentiment_tbl <- download_and_clean_Consumer_Sentiment()

write_rds(Consumer_Sentiment_tbl, "00_data/Consumer_Sentiment_tbl.rds")


# 3 Industry Indicators ----

# * GDP by state by industry ----
# Search using naics code instead of state small states like NH doesn't show up in top 1000 results

NAICS_code <- "NAICS 31-33"
# freq_txt: "A" or "Q"

get_realGDP_by_NAICS <- function(NAICS_code, freq_txt = "A") {

    search_results <- fred_search(text = NAICS_code, key = api_key)

    data <- search_results %>%

        # Get id
        filter(str_detect(title, str_glue("\\({NAICS_code}\\)"))) %>%
        filter(str_detect(title, "Real Gross Domestic Product")) %>%
        filter(!str_detect(title, "Region|District of Columbia")) %>%
        filter(frequency_short == freq_txt) %>%
        pull(id) %>%

        # Download
        fred(key = api_key) %>%

        # Transform it to long form
        pivot_longer(cols = -date, names_to = "id", values_to = "value") %>%

        # Add meta data
        left_join(search_results %>% select(id, title, units, seasonal_adjustment)) %>%

        # Clean title
        mutate(title = title %>% str_remove(" \\(Except Government and Government Enterprises\\)")) %>%
        separate(title, into = c("variable", "V2"), sep = " \\(") %>%
        separate(variable, into = c("variable", "NAICS_desc"), sep = ": ") %>%
        separate(V2, into = c("NAICS_code", "place"), sep = " in ") %>%
        mutate(NAICS_code = NAICS_code %>% str_remove_all("NAICS |\\)")) %>%

        # Move date to the last day of the year
        mutate(date = date %+time% "2 months" %>% lubridate::rollforward()) %>%
        select(-id)

    # Suspend before the search function
    Sys.sleep(30)

    return(data)

}

gdp <- get_realGDP_by_NAICS(NAICS_code = "NAICS 31-33", freq_txt = "Q")

dump(list = c("get_realGDP_by_NAICS"),
     file = "00_scripts/get_realGDP_by_NAICS.R")

# Get 2 digit NAICS labels
# Remove NAICS 92 Public Administration: No data in FRED
naics_numbers <- c("11", "21", "22", "23", "31-33", "42", "44-45", "48-49",
                   "51", "52", "53", "54", "55", "56", "61", "62", "71", "72", "81")

NAICS_code_vec <- str_glue("NAICS {naics_numbers}") %>% as.vector()

# Download data
realGDP_states_q_NAICS_tbl <- NAICS_code_vec %>%

    map(.x = ., .f = ~get_realGDP_by_NAICS(NAICS_code = .x, freq_txt = "Q")) %>%

    # Collapse list into a tibble
    reduce(bind_rows)


realGDP_states_q_NAICS_tbl

write_rds(realGDP_states_q_NAICS_tbl, "00_data/realGDP_states_q_NAICS_tbl.rds")














# *************************************** ----
# Old code ----




# Industry Indicators ----

# * GDP by state by industry ----
# Search using naics code instead of state small states like NH doesn't show up in top 1000 results

NAICS_code <- "NAICS 31-33"
# freq_txt: "A" or "Q"

get_realGDP_by_NAICS <- function(NAICS_code, freq_txt = "A") {

    search_results <- fred_search(text = NAICS_code, key = api_key)

    data <- search_results %>%

        # Get id
        filter(str_detect(title, str_glue("\\({NAICS_code}\\)"))) %>%
        filter(str_detect(title, "Real Gross Domestic Product")) %>%
        filter(!str_detect(title, "Region|District of Columbia")) %>%
        filter(frequency_short == freq_txt) %>%
        pull(id) %>%

        # Download
        fred(key = api_key) %>%

        # Transform it to long form
        pivot_longer(cols = -date, names_to = "id", values_to = "value") %>%

        # Add meta data
        left_join(search_results %>% select(id, title, units, seasonal_adjustment))

    # Suspend before the search function
    Sys.sleep(40)

    return(data)

}

gdp <- get_realGDP_by_NAICS(NAICS_code = "NAICS 31-33", freq_txt = "Q")

# Get 2 digit NAICS labels
# Remove NAICS 92 Public Administration: No data in FRED
naics_numbers <- c("11", "21", "22", "23", "31-33", "42", "44-45", "48-49",
                   "51", "52", "53", "54", "55", "56", "61", "62", "71", "72", "81")

NAICS_code_vec <- str_glue("NAICS {naics_numbers}") %>% as.vector()

# Download data
realGDP_states_q_NAICS_raw_tbl <- NAICS_code_vec %>%

    map(.x = ., .f = ~get_realGDP_by_NAICS(NAICS_code = .x, freq_txt = "Q")) %>%

    # Collapse list into a tibble
    reduce(bind_rows)


realGDP_states_q_NAICS_tbl <- realGDP_states_q_NAICS_raw_tbl %>%

    # Clean title
    mutate(title = title %>% str_remove(" \\(Except Government and Government Enterprises\\)")) %>%
    separate(title, into = c("variable", "V2"), sep = " \\(") %>%
    separate(variable, into = c("variable", "NAICS_desc"), sep = ": ") %>%
    separate(V2, into = c("NAICS_code", "place"), sep = " in ") %>%
    mutate(NAICS_code = NAICS_code %>% str_remove_all("NAICS |\\)")) %>%

    # Move date to the last day of the year
    mutate(period = str_glue("{year(date)}.{quarter(date)}")) %>%
    select(period, everything(), -date, -id)

realGDP_states_q_NAICS_tbl

write_rds(realGDP_states_q_NAICS_tbl, "00_data/realGDP_states_q_NAICS_tbl.rds")




