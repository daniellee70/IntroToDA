# Nowcasting Recession Using kmeans ----
# Are we in recession? ----

# Base the experiment on the economic principles of four D: depth, duration, and ?
# Have an indicator to represent all four economic processes: production, employment, income, and sales


library(tidyverse)
library(broom)
library(umap)
library(ggrepel)
library(tidyquant)
library(timktk)

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
Consumer_Sentiment_tbl <- read_rds("00_data/wrangled_data//Consumer_Sentiment_tbl.rds")

Recession_Dates_tbl    <- read_rds("00_data/wrangled_data/Recession_Dates_tbl.rds")

# 1.0 Transform data ----
# 1.1 Combine ----
all_indicators_tbl <- list(Yield_Spread_tbl %>% rename(Yield_Spread = value),
                           Inflation_tbl %>% rename(Inflation = value),
                           Unemployment_Rate_tbl %>% rename(Unemployment_Rate = value),
                           Consumer_Sentiment_tbl %>%

                               # Calculate year-over-year change
                               mutate(value = (value/lag(value, n = 12)-1)*100) %>%
                               rename(Consumer_Sentiment = value)) %>%

    # Aggregate
    reduce(left_join)

all_indicators_tbl %>% glimpse()

# 1.2 Normalize ----
all_normal_tbl <- all_indicators_tbl %>%

    # Impute missing values
    mutate(across(-date, timetk::ts_impute_vec)) %>%

    # Normalize
    mutate(across(-date, timetk::standardize_vec))

# 1.3 Convert to user-item format ----
# It is already in user-item format

all_normal_tbl <- all_normal_tbl %>%

    mutate(date = as.character(date))



# 2.0 MODELING: K-MEANS CLUSTERING ----

# 2.1 Performing K-Means ----
?kmeans

kmeans_obj <- all_normal_tbl %>%
    select(-date) %>%
    kmeans(centers = 2, nstart = 100)

kmeans_obj$cluster

kmeans_obj$cluster %>% count(.cluster)


# 2.2 Tidying a K-Means Object ----

broom::tidy(kmeans_obj) %>% glimpse()

broom::glance(kmeans_obj)

broom::augment(kmeans_obj, all_normal_tbl) %>%
    select(date, .cluster)


forecast_recessions_tbl <- broom::augment(kmeans_obj, all_normal_tbl) %>%
    select(date, .cluster) %>%
    mutate(date = date %>% ymd()) %>%

    # Add us recession dates
    left_join(Recession_Dates_tbl) %>%
    na.omit() %>%

    # Count correct
    mutate(correct_forecast = if_else(.cluster == USREC, "correct", "incorrect"))


forecast_recessions_tbl %>%

    ggplot(aes(date, USREC)) +
    geom_line() +

    geom_vline(date = forecast_recessions_tbl %>%
                   filter(.cluster == 1),
               aes(xintercept = as.numeric(date)), color = "green", size = 0.001, alpha = 0.2)

# 2.3 How many centers (customer groups) to use? ----

# Functions that works on 1 element
center <- 3

kmeans_mapper <- function(centers = 3) {

    all_normal_tbl %>%
        select(-date) %>%
        kmeans(centers = centers, nstart = 100)
}

3 %>% kmeans_mapper() %>% glance()

# Mapping the function to many elements
kmeans_mapped_tbl <- tibble(centers = 1:15) %>%
    mutate(k_means = centers %>% map(kmeans_mapper)) %>%
    mutate(glance  = k_means %>% map(glance))

kmeans_mapped_tbl %>%
    unnest(glance) %>%
    select(centers, tot.withinss)

# 2.4 Skree Plot ----

kmeans_mapped_tbl %>%
    unnest(glance) %>%
    select(centers, tot.withinss) %>%

    # Visualization
    ggplot(aes(centers, tot.withinss)) +
    geom_point(color = "#2c3e50", size = 4) +
    geom_line(color = "#2c3e50", size = 1) +
    ggrepel::geom_label_repel(aes(label = centers), color = "#2c3e50") +

    # Formatting
    theme_tq() +
    labs(
        title = "Skree Plot",
        subtitle = "Measures the distance each of the customer are from the closes K-Means center",
        caption = "Conclusion: Based on the Scree Plot, we select 4 clusters to segment the customer base."
    )


# 3.0 VISUALIZATION: UMAP ----

# 3.1 Use UMAP to get 2-D Projection ----
?umap

umap_obj <- all_normal_tbl %>%
    select(-date) %>%
    umap()

umap_results_tbl <- umap_obj$layout %>%
    as_tibble() %>%
    set_names(c("x", "y")) %>%
    bind_cols(
        all_normal_tbl %>% select(date)
    )

umap_results_tbl %>%
    ggplot(aes(x, y)) +
    geom_point() +
    geom_label_repel(aes(label = date), size = 3)

# 3.2 Use K-Means to Add Cluster Assignments ----
umap_results_tbl

kmeans_4_obj <- kmeans_mapped_tbl %>%
    pull(k_means) %>%
    pluck(4)

kmeans_4_clusters_tbl <- kmeans_4_obj %>%
    augment(all_normal_tbl) %>%
    select(date, .cluster)

umap_kmeans_4_results_tbl <- umap_results_tbl %>%
    left_join(kmeans_4_clusters_tbl)

# 3.3 Visualize UMAP'ed Projections with Cluster Assignments ----

umap_kmeans_4_results_tbl %>%
    mutate(label_text = str_glue("Customer: {date}
                                 Cluster: {.cluster}")) %>%

    ggplot(aes(x, y, color = .cluster)) +

    # Geometries
    geom_point() +
    geom_label_repel(aes(label = label_text), size = 3) +

    # Formatting
    theme_tq() +
    scale_color_tq() +
    labs(
        title = "Customer Segmentation: 2D Projection",
        subtitle = "UMAP 2D Projection with K-Means Cluster Assignment",
        caption = "Conclusion: 4 Customer Segments identified using 2 algorithms"
    ) +
    theme(legend.position = "none")


# 4.0 ANALYZE PURCHASING TRENDS ----

customer_trends_tbl %>%
    pull(price) %>%
    quantile(probs = c(0, 0.33, 0.66, 1))

?quantile

cluster_trends_tbl <- customer_trends_tbl %>%

    # Join Cluster Assignment by Bikeshop Name
    left_join(umap_kmeans_4_results_tbl) %>%

    mutate(price_bin = case_when(
        price <= 2240 ~ "low",
        price <= 4260 ~ "medium",
        TRUE ~ "high"
    )) %>%

    select(.cluster, model, contains("price"),
           category_1:quantity_purchased) %>%

    # Aggregate quantity purchased by cluster and product attributes
    group_by_at(.vars = vars(.cluster:frame_material)) %>%
    summarise(total_quantity = sum(quantity_purchased)) %>%
    ungroup() %>%

    # Calculate Proportion of Total
    group_by(.cluster) %>%
    mutate(prop_of_total = total_quantity / sum(total_quantity)) %>%
    ungroup()

cluster_trends_tbl


# Cluster 1 - Low/Medium Price, Road Model Preference
cluster_trends_tbl %>%
    filter(.cluster == 1) %>%
    arrange(desc(prop_of_total)) %>%
    mutate(cum_prop = cumsum(prop_of_total)) %>%
    View()

get_cluster_trends <- function(cluster = 1) {

    cluster_trends_tbl %>%
        filter(.cluster == cluster) %>%
        arrange(desc(prop_of_total)) %>%
        mutate(cum_prop = cumsum(prop_of_total))

}

get_cluster_trends(cluster = 1)

# Cluster 2 - Low/Medium Price, Mountain Model Preference, Aluminum Frame
get_cluster_trends(cluster = 2) %>% View()

# Cluster 3 - High End Price, Mountain Preference, Carbon Frame
get_cluster_trends(cluster = 3)

# Cluster 4 - High End Price, Road Preference, Carbon Frame
get_cluster_trends(cluster = 4)


# Update Visualization

cluster_label_tbl <- tibble(
    .cluster = 1:4,
    .cluster_label = c(
        "Low/Medium Price, Road",
        "Low/Medium Price, Mountain, Aluminum Frame",
        "High End Price, Mountain, Carbon Frame",
        "High End Price, Road, Carbon Frame"
    )
) %>%
    mutate(.cluster = as_factor(as.character(.cluster)))
cluster_label_tbl


umap_kmeans_4_results_tbl %>%
    left_join(cluster_label_tbl)

umap_kmeans_4_results_tbl %>%
    left_join(cluster_label_tbl) %>%
    mutate(label_text = str_glue("Customer: {date}
                                 Cluster: {.cluster}
                                 {.cluster_label}
                                 ")) %>%

    ggplot(aes(x, y, color = .cluster)) +

    # Geometries
    geom_point() +
    geom_label_repel(aes(label = label_text), size = 3) +

    # Formatting
    theme_tq() +
    scale_color_tq() +
    labs(
        title = "Customer Segmentation: 2D Projection",
        subtitle = "UMAP 2D Projection with K-Means Cluster Assignment",
        caption = "Conclusion: 4 Customer Segments identified using 2 algorithms"
    ) +
    theme(legend.position = "none")
