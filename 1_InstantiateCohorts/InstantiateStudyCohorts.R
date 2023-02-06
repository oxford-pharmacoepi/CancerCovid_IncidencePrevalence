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

cdm$cancercovidcancers %>% group_by(cohort_definition_id) %>% tally() %>% collect() 

info(logger, "- got cancer outcomes")



# ============================================================================ #
#                    2.  CANCER DIAGNOSES AS DENOMINATOR STRATA                #
# ============================================================================ #
info(logger, "- getting cancer strata")

strata_cohorts_1 <- readCohortSet(here("1_InstantiateCohorts", "CancerStrataCohorts"))

cdm <- generateCohortSet(cdm = cdm, 
                         cohortSet = strata_cohorts_1,
                         cohortTableName = strata_table_name_1,
                         overwrite = TRUE) 

cdm$breast_prostate_strata %>% group_by(cohort_definition_id) %>% tally() %>% collect() 

info(logger, "- got cancer strata")

# ============================================================================ #
#                    3.  ENDOCRINE TREATMENTS AS OUTCOMES                      #
# ============================================================================ #


info(logger, "- getting endocrine outcomes")

outcome_cohorts_2 <- readCohortSet(here("1_InstantiateCohorts", "EndocrineTxOutcomeCohorts"))

cdm <- generateCohortSet(cdm = cdm, 
                         cohortSet = outcome_cohorts_2,
                         cohortTableName = outcome_table_name_2,
                         overwrite = TRUE) 

cdm$endocrine_tx_table %>% group_by(cohort_definition_id) %>% tally() %>% collect() 

info(logger, "- got endocrine outcomes")

# ============================================================================ #
#       4.  CANCER DIAGNOSES WITH ENDOCRINE TX AS DENOMINATOR STRATA           #
# ============================================================================ #

info(logger, "- getting cancer and endocrine treatment strata")

strata_cohorts_2 <- readCohortSet(here("1_InstantiateCohorts", "CancerTXStrataCohorts"))

cdm <- generateCohortSet(cdm = cdm, 
                         cohortSet = strata_cohorts_2,
                         cohortTableName = strata_table_name_2,
                         overwrite = TRUE) 

cdm$breast_prostate_endocrine_strata %>% group_by(cohort_definition_id) %>% tally() %>% collect() 

info(logger, "- getting cancer and endocrine treatment strata")

# ============================================================================ #
#                 5.  ENDOCRINE TREATMENT-RELATED OUTCOMES                     #
# ============================================================================ #

info(logger, "- getting endocrine treatment related outcomes")

outcome_cohorts_3 <- readCohortSet(here("1_InstantiateCohorts", "OsteoDxOutcomeCohorts"))

cdm <- generateCohortSet(cdm = cdm, 
                         cohortSet = outcome_cohorts_3,
                         cohortTableName = outcome_table_name_3,
                         overwrite = TRUE) 

cdm$osteo_dx_table %>% group_by(cohort_definition_id) %>% tally() %>% collect() 

info(logger, "- got endocrine treatment related outcomes")



# ============================================================================ #
#                       6.  SCREENING TESTS AS OUTCOMES                        #
# ============================================================================ #


# instantiate cancer outcome cohorts
info(logger, "- getting screening test outcomes")

outcome_cohorts_4 <- readCohortSet(here::here("1_InstantiateCohorts","ScreeningOutcomeCohorts"))




cdm <- generateCohortSet(cdm = cdm, 
                         cohortSet = outcome_cohorts_4,
                         cohortTableName = outcome_table_name_4,
                         overwrite = TRUE
)

cdm$screening_outcomes_table %>% group_by(cohort_definition_id) %>% tally() %>% collect() 

info(logger, "- got screening test outcomes")