.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Thank you for using the eventreport package!\n",
    "Please use citation(\"eventreport\") to see how to cite the package, ",
    "the associated methods paper, and the enclosed MAVERICK dataset."
  )
}

utils::globalVariables(c(
  "maverick_event_report", "total", "variable",
  "id", "event_id", "country", "election", "certain", "certain1", "certain2",
  "certain3", "certain4", "certain5", "certain6", "date_start", "date_end",
  "city", "location", "latitude", "longitude", "geo_precision", "actor1",
  "actor1_id", "actor1_type", "actor1_subtype", "actor1_party", "actor1_violence",
  "actor1_precision", "actor1_initiator", "actor1_perpetrator",
  "actor1_intervener", "actor1_bystander", "actor1_victim", "actor2",
  "actor2_id", "actor2_type", "actor2_subtype", "actor2_party",
  "actor2_violence", "actor2_precision", "actor2_initiator",
  "actor2_perpetrator", "actor2_intervener", "actor2_bystander",
  "actor2_victim", "actor3", "actor3_id", "actor3_type", "actor3_subtype",
  "actor3_party", "actor3_violence", "actor3_precision", "actor3_initiator",
  "actor3_perpetrator", "actor3_intervener", "actor3_bystander",
  "actor3_victim", "actor4", "actor4_id", "actor4_type", "actor4_subtype",
  "actor4_party", "actor4_violence", "actor4_precision", "actor4_initiator",
  "actor4_perpetrator", "actor4_intervener", "actor4_bystander",
  "actor4_victim", "actor5", "actor5_id", "actor5_type", "actor5_subtype",
  "actor5_party", "actor5_violence", "actor5_precision", "actor5_initiator",
  "actor5_perpetrator", "actor5_intervener", "actor5_bystander",
  "actor5_victim", "actor6", "actor6_id", "actor6_type", "actor6_subtype",
  "actor6_party", "actor6_violence", "actor6_precision", "actor6_initiator",
  "actor6_perpetrator", "actor6_intervener", "actor6_bystander",
  "actor6_victim", "event_context", "target", "deaths_best", "deaths_low",
  "deaths_high", "injuries_best", "injuries_low", "injuries_high",
  "displacement", "damage", "source", "number_of_sources", "source_author",
  "source_type", "source_classification", "sampling", "unit_of_analysis",
  "aggregation"
))
