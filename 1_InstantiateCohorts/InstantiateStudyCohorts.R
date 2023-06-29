# ============================================================================ #
#                 INSTANTIATE COHORTS FOR CANCER/COVID STUDY                   #
#                                Nicola Barclay                                #
#                                 18-01-2023                                   #
# ============================================================================ #



# ============================================================================ #
#                       1.  CANCER DIAGNOSES AS OUTCOMES                       #
# ============================================================================ #


# instantiate cancer outcome cohorts
info(logger, "- getting cancer outcomes")

outcome_cohorts_1 <- readCohortSet(here::here("1_InstantiateCohorts","CancerOutcomeCohorts"))




cdm <- generateCohortSet(cdm = cdm, 
                         cohortSet = outcome_cohorts_1,
                         name = outcome_table_name_1,
                         overwrite = TRUE
)


cohortCount(cdm[[outcome_table_name_1]]) %>%  glimpse()

cohortAttrition(cdm[[outcome_table_name_1]]) %>%  glimpse()

cohortSet(cdm[[outcome_table_name_1]]) %>%  glimpse()

info(logger, "- got cancer outcomes")



# ============================================================================ #
#         2.  1st EVER CANCER DIAGNOSES AS OUTCOMES FOR PREVALENCE             #
# ============================================================================ #


# instantiate cancer outcome cohorts
info(logger, "- getting 1st ever cancer outcomes")

outcome_cohorts_2 <- readCohortSet(here::here("1_InstantiateCohorts","1stEverCancerOutcomeCohorts"))




cdm <- generateCohortSet(cdm = cdm, 
                         cohortSet = outcome_cohorts_2,
                         name = outcome_table_name_2,
                         overwrite = TRUE
)

cohortCount(cdm[[outcome_table_name_2]]) %>%  glimpse()

cohortAttrition(cdm[[outcome_table_name_2]]) %>%  glimpse()

cohortSet(cdm[[outcome_table_name_2]]) %>%  glimpse()

info(logger, "- got 1st ever cancer outcomes")



# ============================================================================ #
#                       3.  SCREENING TESTS AS OUTCOMES                        #
# ============================================================================ #


# instantiate cancer outcome cohorts
info(logger, "- getting screening test outcomes")


outcome_cohorts_3 <- readCohortSet(here::here("1_InstantiateCohorts","ScreeningOutcomeCohorts"))


cdm <- generateCohortSet(cdm = cdm, 
                         cohortSet = outcome_cohorts_3,
                         name = outcome_table_name_3,
                         overwrite = TRUE
)

cohortCount(cdm[[outcome_table_name_3]]) %>%  glimpse()

cohortAttrition(cdm[[outcome_table_name_3]]) %>%  glimpse()

cohortSet(cdm[[outcome_table_name_3]]) %>%  glimpse()

info(logger, "- got screening test outcomes")