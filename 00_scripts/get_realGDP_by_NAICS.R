get_realGDP_by_NAICS <-
function(NAICS_code, freq_txt = "A") {

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
