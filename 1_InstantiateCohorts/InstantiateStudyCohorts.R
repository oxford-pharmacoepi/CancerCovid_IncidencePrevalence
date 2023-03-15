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
                         cohortTableName = outcome_table_name_1,
                         overwrite = TRUE
)

cdm[[outcome_table_name_1]] %>% group_by(cohort_definition_id) %>% tally() %>% collect() 

info(logger, "- got cancer outcomes")



# ============================================================================ #
#         2.  1st EVER CANCER DIAGNOSES AS OUTCOMES FOR PREVALENCE             #
# ============================================================================ #


# instantiate cancer outcome cohorts
info(logger, "- getting 1st ever cancer outcomes")

outcome_cohorts_2 <- readCohortSet(here::here("1_InstantiateCohorts","1stEverCancerOutcomeCohorts"))




cdm <- generateCohortSet(cdm = cdm, 
                         cohortSet = outcome_cohorts_2,
                         cohortTableName = outcome_table_name_2,
                         overwrite = TRUE
)

cdm[[outcome_table_name_2]] %>% group_by(cohort_definition_id) %>% tally() %>% collect() 

info(logger, "- got 1st ever cancer outcomes")


# ============================================================================ #
#                    3.  CANCER DIAGNOSES AS DENOMINATOR STRATA                #
# ============================================================================ #
info(logger, "- getting cancer strata")

strata_cohorts_1 <- readCohortSet(here("1_InstantiateCohorts", "CancerStrataCohorts"))

cdm <- generateCohortSet(cdm = cdm, 
                         cohortSet = strata_cohorts_1,
                         cohortTableName = strata_table_name_1,
                         overwrite = TRUE) 

cdm[[strata_table_name_1]] %>% group_by(cohort_definition_id) %>% tally() %>% collect() 

info(logger, "- got cancer strata")

# ============================================================================ #
#                    4.  ENDOCRINE TREATMENTS AS OUTCOMES                      #
# ============================================================================ #


info(logger, "- getting endocrine outcomes")

outcome_cohorts_3 <- readCohortSet(here("1_InstantiateCohorts", "EndocrineTxOutcomeCohorts"))

cdm <- generateCohortSet(cdm = cdm, 
                         cohortSet = outcome_cohorts_3,
                         cohortTableName = outcome_table_name_3,
                         overwrite = TRUE) 

cdm[[outcome_table_name_3]] %>% group_by(cohort_definition_id) %>% tally() %>% collect() 

info(logger, "- got endocrine outcomes")

# ============================================================================ #
#       5.  CANCER DIAGNOSES WITH ENDOCRINE TX AS DENOMINATOR STRATA           #
# ============================================================================ #

info(logger, "- getting cancer and endocrine treatment strata")

strata_cohorts_2 <- readCohortSet(here("1_InstantiateCohorts", "CancerTXStrataCohorts"))

cdm <- generateCohortSet(cdm = cdm, 
                         cohortSet = strata_cohorts_2,
                         cohortTableName = strata_table_name_2,
                         overwrite = TRUE) 

cdm[[strata_table_name_2]] %>% group_by(cohort_definition_id) %>% tally() %>% collect() 

info(logger, "- getting cancer and endocrine treatment strata")

# ============================================================================ #
#                 6.  ENDOCRINE TREATMENT-RELATED OUTCOMES                     #
# ============================================================================ #

info(logger, "- getting endocrine treatment related outcomes")

outcome_cohorts_4 <- readCohortSet(here("1_InstantiateCohorts", "OsteoDxOutcomeCohorts"))

cdm <- generateCohortSet(cdm = cdm, 
                         cohortSet = outcome_cohorts_4,
                         cohortTableName = outcome_table_name_4,
                         overwrite = TRUE) 

cdm[[outcome_table_name_4]] %>% group_by(cohort_definition_id) %>% tally() %>% collect() 

info(logger, "- got endocrine treatment related outcomes")



# ============================================================================ #
#                       7.  SCREENING TESTS AS OUTCOMES                        #
# ============================================================================ #


# instantiate cancer outcome cohorts
info(logger, "- getting screening test outcomes")

outcome_cohorts_5 <- readCohortSet(here::here("1_InstantiateCohorts","ScreeningOutcomeCohorts"))




cdm <- generateCohortSet(cdm = cdm, 
                         cohortSet = outcome_cohorts_5,
                         cohortTableName = outcome_table_name_5,
                         overwrite = TRUE
)

cdm[[outcome_table_name_5]] %>% group_by(cohort_definition_id) %>% tally() %>% collect() 

info(logger, "- got screening test outcomes")