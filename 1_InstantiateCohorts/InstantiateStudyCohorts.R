# instantiate outcome cohorts
info(logger, "- getting outcome definitions")

outcome_cohorts <- CDMConnector::readCohortSet(here(
  "1_InstantiateCohorts",
  "CancerOutcomeCohorts"
))

info(logger, "- getting outcomes")


cdm <- CDMConnector::generateCohortSet(cdm, outcome_cohorts,
                                       cohortTableName = outcome_table_name,
                                       overwrite = TRUE
)


info(logger, "- got outcomes")



