#' Load the most-representative aggregation of MAVERICK
#'
#' This convenience function aggregates the MAVERICK event report data to the event level using the most-representative aggregation model.
#'
#' @param data The MAVERICK event report level dataset. Already pre-loaded.
#'
#' @return Returns a dataframe of the most-representative aggregation of the MAVERICK dataset
#' @export
#'
#' @examples
#' \donttest{
#' maverick_representative <- aggregate_maverick_rep()
#' }

aggregate_maverick_rep <- function(data) {
  aggregateData(
    data = maverick_event_report,
    group_var = "event_id",
    find_mode = c(
      "country", "election", "city", "location",
      "actor1", "actor1_id", "actor1_type", "actor1_subtype", "actor1_party", "actor1_violence",
      "actor2", "actor2_id", "actor2_type", "actor2_subtype", "actor2_party", "actor2_violence",
      "actor3", "actor3_id", "actor3_type", "actor3_subtype", "actor3_party", "actor3_violence",
      "actor4", "actor4_id", "actor4_type", "actor4_subtype", "actor4_party", "actor4_violence",
      "actor5", "actor5_id", "actor5_type", "actor5_subtype", "actor5_party", "actor5_violence",
      "actor6", "actor6_id", "actor6_type", "actor6_subtype", "actor6_party", "actor6_violence",
      "event_context", "target"
    ),
    find_mode_date = c("date_start", "date_end"),
    find_mode_numeric = c(
      "latitude", "longitude", "geo_precision",
      "actor1_victim", "actor2_victim", "actor3_victim", "actor4_victim", "actor5_victim", "actor6_victim",
      "deaths_best", "deaths_low", "deaths_high", "injuries_best", "injuries_low", "injuries_high"
    ),
    find_mode_bin = c("displacement", "damage"),
    summarize_vars = c(
      "actor1_initiator", "actor1_perpetrator", "actor1_intervener", "actor1_bystander",
      "actor2_initiator", "actor2_perpetrator", "actor2_intervener", "actor2_bystander",
      "actor3_initiator", "actor3_perpetrator", "actor3_intervener", "actor3_bystander",
      "actor4_initiator", "actor4_perpetrator", "actor4_intervener", "actor4_bystander",
      "actor5_initiator", "actor5_perpetrator", "actor5_intervener", "actor5_bystander",
      "actor6_initiator", "actor6_perpetrator", "actor6_intervener", "actor6_bystander"
    ),
    combine_strings = "source",
    find_max = c("certain1", "certain2", "certain3", "certain4", "certain5", "certain6"),
    tie_break = "source_classification",
    second_tie_break = "certain",
    aggregation_name = "Most-representative"
  ) %>%
  mutate(certain = certain1 + certain2 + certain3 + certain4 + certain5 + certain6) %>%
  rowwise() %>%
  mutate(
    across(
      matches("^actor[1-6]_initiator$"),
      ~ {
        corresponding_actor <- get(gsub("_initiator", "", cur_column()))
        if (is.na(corresponding_actor)) {
          NA_integer_
        } else {
          max_value <- max(c_across(matches("^actor[1-6]_initiator$")), na.rm = TRUE)
          num_max <- sum(c_across(matches("^actor[1-6]_initiator$")) == max_value, na.rm = TRUE)
          ifelse(. == max_value & max_value > 0 & num_max == 1, 1, 0)
        }
      }
    ),
    across(
      matches("^actor[1-6]_perpetrator$"),
      ~ {
        corresponding_actor <- get(gsub("_perpetrator", "", cur_column()))
        if (is.na(corresponding_actor)) {
          NA_integer_
        } else {
          ifelse(. > 0 & get(gsub("_perpetrator", "_bystander", cur_column())) < 1, 1, 0)
        }
      }
    ),
    across(
      matches("^actor[1-6]_intervener$"),
      ~ {
        corresponding_actor <- get(gsub("_intervener", "", cur_column()))
        if (is.na(corresponding_actor)) {
          NA_integer_
        } else {
          ifelse(. > 0 & get(gsub("_intervener", "_initiator", cur_column())) == 0, 1, 0)
        }
      }
    ),
    across(
      matches("^actor[1-6]_bystander$"),
      ~ {
        corresponding_actor <- get(gsub("_bystander", "", cur_column()))
        if (is.na(corresponding_actor)) {
          NA_integer_
        } else {
          ifelse(
            . > 0 &
              get(gsub("_bystander", "_initiator", cur_column())) == 0 &
              get(gsub("_bystander", "_perpetrator", cur_column())) == 0 &
              get(gsub("_bystander", "_intervener", cur_column())) == 0, 1, 0
          )
        }
      }
    )
  ) %>%
  ungroup() %>%
  mutate(
    actor1_violence = case_when(actor1_violence == "NA" ~ NA_character_, TRUE ~ actor1_violence),
    actor2_violence = case_when(actor2_violence == "NA" ~ NA_character_, TRUE ~ actor2_violence),
    actor3_violence = case_when(actor3_violence == "NA" ~ NA_character_, TRUE ~ actor3_violence),
    actor4_violence = case_when(actor4_violence == "NA" ~ NA_character_, TRUE ~ actor4_violence),
    actor5_violence = case_when(actor5_violence == "NA" ~ NA_character_, TRUE ~ actor5_violence),
    actor6_violence = case_when(actor6_violence == "NA" ~ NA_character_, TRUE ~ actor6_violence)
  ) %>%
  select(
    event_id:election, date_start, date_end, city, location, latitude, longitude, geo_precision,
    actor1, actor1_id, actor1_type, actor1_subtype, actor1_party, actor1_violence, actor1_initiator, actor1_perpetrator, actor1_intervener, actor1_bystander, actor1_victim,
    actor2, actor2_id, actor2_type, actor2_subtype, actor2_party, actor2_violence, actor2_initiator, actor2_perpetrator, actor2_intervener, actor2_bystander, actor2_victim,
    actor3, actor3_id, actor3_type, actor3_subtype, actor3_party, actor3_violence, actor3_initiator, actor3_perpetrator, actor3_intervener, actor3_bystander, actor3_victim,
    actor4, actor4_id, actor4_type, actor4_subtype, actor4_party, actor4_violence, actor4_initiator, actor4_perpetrator, actor4_intervener, actor4_bystander, actor4_victim,
    actor5, actor5_id, actor5_type, actor5_subtype, actor5_party, actor5_violence, actor5_initiator, actor5_perpetrator, actor5_intervener, actor5_bystander, actor5_victim,
    actor6, actor6_id, actor6_type, actor6_subtype, actor6_party, actor6_violence, actor6_initiator, actor6_perpetrator, actor6_intervener, actor6_bystander, actor6_victim,
    event_context:damage, deaths_best:injuries_high,
    certain, certain1:certain6, source, number_of_sources:aggregation
  )
}
