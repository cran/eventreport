## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "##"
)

## ----include = FALSE----------------------------------------------------------
devtools::load_all()

df <- small_maverick_event_report %>% dplyr::arrange(event_id)

## ----message = FALSE, warning = FALSE-----------------------------------------
#install.packages("tidyverse")
#install.packages("tinytable")

library(tidyverse)
library(tinytable)

## ----echo = FALSE-------------------------------------------------------------
table <- df %>% 
  dplyr::select(event_id, city, location, actor1, deaths_best, source) %>% 
  dplyr::filter(event_id == "CIV-0003") %>% 
  head()

tt(table)

## -----------------------------------------------------------------------------
#install.packages("eventreport")

## -----------------------------------------------------------------------------
library(eventreport)

## -----------------------------------------------------------------------------
dscore(
  df,
  group_var = "event_id",
  variables = c("country", "actor1", "deaths_best")
  ) %>% 
  head(10)

## -----------------------------------------------------------------------------
mean_dscore(
  df,
  group_var = "event_id",
  variables = c("country", "actor1", "deaths_best", "injuries_best")
  )

## -----------------------------------------------------------------------------
mean_dscore(
  df,
  group_var = "event_id",
  variables = c("country", "actor1", "deaths_best", "injuries_best"),
  normalize = TRUE
  )

## ----fig.width=6, fig.height=4, dpi=150, out.width="70%"----------------------
mean_dscore(
  df,
  group_var = "event_id",
  variables = c("country", "actor1", "deaths_best"),
  normalize = TRUE,
  plot = TRUE
  )

## -----------------------------------------------------------------------------
diagnostics <- aggregation_diagnostics(
  df,
  group_var = "event_id",
  variables = c("city", "deaths_best", "actor1")
)

tt(diagnostics)

## -----------------------------------------------------------------------------
calc_mode(c("Sweden", "Sweden", "Denmark", "Sweden"))

## -----------------------------------------------------------------------------
calc_mode(
  c("Sweden", "Sweden", "Denmark", "Denmark"),
  tie_break = c(1, 1, 1, 1),
  second_tie_break = c(1, 4, 1, 1)
)

## -----------------------------------------------------------------------------
calc_mode(
  c("Sweden", "Sweden", "Denmark", "Denmark")
)

## -----------------------------------------------------------------------------
calc_mode(
  c("Sweden", "", "", "Denmark")
)

## -----------------------------------------------------------------------------
calc_mode_na_ignore(
  c("Sweden", "", "", "Denmark"),
  tie_break = c(1, 1, 1, 1),
  second_tie_break = c(4, 1, 1, 1)
)

## -----------------------------------------------------------------------------
calc_mode_binary(
  c(0, 1, 1, 1, 0, 0)
)

## -----------------------------------------------------------------------------
calc_mode_numeric(
  c(1, 1, 1, 2, 3, 5)
)

## -----------------------------------------------------------------------------
calc_mode_date(
  c("2024-01-01", "2024-01-01", "2024-01-02")
)

## -----------------------------------------------------------------------------
calc_max_precision(
  x = c("Tranas", "Smaland", "Sweden"),
  precision_var = c(3, 2, 1)
)

## -----------------------------------------------------------------------------
calc_min_precision(
  x = c("Tranas", "Smaland", "Sweden"),
  precision_var = c(3, 2, 1)
)

## -----------------------------------------------------------------------------
aggregate_strings(
  c("Sweden", "Sweden", "Denmark", "", "Finland")
)

## -----------------------------------------------------------------------------
df <- maverick_event_report %>% dplyr::arrange(event_id) %>% utils::head(n = 100)

## -----------------------------------------------------------------------------
df %>% 
  aggregateData(
    group_var = "event_id",
    find_mode = "city"
  ) %>% 
  utils::head(10)

## -----------------------------------------------------------------------------
df %>% 
  aggregateData(
    group_var = "event_id",
    find_mode = c("city", "location", "actor1")
  ) %>% 
  utils::head(10)

## -----------------------------------------------------------------------------
df %>% 
  aggregateData(
    group_var = "event_id",
    find_mode = c("city", "location"),
    find_mode_na_ignore = "actor1",
    find_max = "deaths_best",
    combine_strings = "source"
  ) %>% 
  dplyr::select(event_id:actor1, deaths_best:unit_of_analysis, source) %>% 
  dplyr::filter(event_id == "CIV-0002")

## -----------------------------------------------------------------------------
df %>% 
  aggregateData(
    group_var = "event_id",
    find_mode = c("city", "location"),
    find_mode_na_ignore = "actor1",
    find_max = "deaths_best",
    tie_break = "source_classification",
    second_tie_break = "certain"
  ) %>% 
  utils::head(10)

## -----------------------------------------------------------------------------
df %>% 
  aggregateData(
    group_var = "event_id",
    find_most_precise = list(
      list(var = "city", precision_var = "geo_precision"),
      list(var = "location", precision_var = "geo_precision")
    ),
    find_mode_na_ignore = "actor1",
    find_max = "deaths_best",
    tie_break = "source_classification",
    second_tie_break = "certain",
  ) %>% 
  utils::head(10)

## -----------------------------------------------------------------------------
conservative <- df %>% 
  aggregateData(
    group_var = "event_id",
    find_mode = c("city", "location"),
    find_min = c("deaths_best", "injuries_best"),
    tie_break = "source_classification",
    second_tie_break = "certain",
    aggregation_name = "Most-conservative"
  ) %>% 
  utils::head(10)

maximalist <- df %>% 
  aggregateData(
    group_var = "event_id",
    find_mode_na_ignore = c("city", "location"),
    find_max = c("deaths_best", "injuries_best"),
    tie_break = "source_classification",
    second_tie_break = "certain",
    aggregation_name = "Most-informative"
  ) %>% 
  utils::head(10)

rbind(conservative, maximalist) %>% 
  dplyr::arrange(event_id)

## -----------------------------------------------------------------------------
# Calculate the average divergence score

mean_dscore(
  maverick_event_report,
  group_var = "event_id",
  variables = c("date_start", "deaths_best")
)

## -----------------------------------------------------------------------------
# Create representative aggregation set

representative <- maverick_event_report %>%
  aggregateData(
    group_var = "event_id",
    find_mode = "country",
    find_mode_numeric = "deaths_best",
    find_mode_date = "date_start",
    tie_break = "source_classification",
    second_tie_break = "certain",
    aggregation_name = "Representative"
  )

# Create informative aggregation set

informative <- maverick_event_report %>%
  aggregateData(
    group_var = "event_id",
    find_mode = "country",
    find_max = c("deaths_best", "date_start"),
    tie_break = "source_classification",
    second_tie_break = "certain",
    aggregation_name = "Informative"
  )

# Combine dataframes

combined <- rbind(representative, informative)

## -----------------------------------------------------------------------------
# Subset and calculate deaths per week

maverick_time_series_week <- combined %>%
  dplyr::filter(number_of_sources > 1) %>%
  dplyr::mutate(date_start = as.Date(as.character(date_start), format = "%Y-%m-%d")) %>%
  dplyr::mutate(week_start = lubridate::floor_date(date_start, unit = "week")) %>%
  tidyr::complete(
    week_start = seq(ymd("1995-01-01"), ymd("2023-12-31"), by = "1 week"),
    country, aggregation, fill = list(deaths_best = 0)
  ) %>%
  dplyr::group_by(week_start, country, aggregation) %>%
  dplyr::summarize(deaths_best = sum(deaths_best, na.rm = TRUE), .groups = "drop")

## ----fig.width=7, fig.height=4, dpi=150, out.width="70%"----------------------

maverick_time_series_week %>%
  dplyr::filter(
    week_start > "2010-09-30"
    & week_start < "2011-06-01"
    & country == "Ivory Coast"
  ) %>%
  ggplot2::ggplot() +
  ggplot2::geom_line(aes(y = deaths_best, x = week_start, color = aggregation), linewidth = 1) +
  ggplot2::scale_x_date(
    breaks = seq(as.Date("2010-10-01"), as.Date("2011-06-01"), by = "1 month"),
    date_labels = "%b %Y"
  ) +
  ggplot2::labs(
    x = NULL,
    y = "Best estimated number of weekly deaths"
  ) +
  ggplot2::theme_bw()

