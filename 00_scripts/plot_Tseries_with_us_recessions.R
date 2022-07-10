customize_recessions_dates <-
function(Tseries) {

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
generate_comment <-
function(Tseries, title = "") {

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
plot_Tseries_with_us_recessions <-
function(Tseries, title_text = "",
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
