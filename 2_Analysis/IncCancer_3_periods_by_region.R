# ============================================================================ #
#                         Incidence/Prevalence for                             #
#                 Breast, Colorectal, Lung and Prostate Cancer                 #
#                         Pre-OCIVD, lockdown, and Post-lockdown               #
#                              Nicola Barclay                                  #
#                                22-03-2023                                    #
# ============================================================================ #

print(paste0("- 1. Incidence and Prevalence of Cancers"))
info(logger, "- 1. Incidence and Prevalence of Cancers")


## ============================= PRE-COVID ================================== ##

## ======= Compute the denominator population pre-COVID  ==================== ##

# add selects etc then loop over all the locations
allRegions <- cdm$location %>% pull(location_source_value)
cdm$observation_period_original <- cdm$observation_period
cdm$observation_period <- cdm$observation_period_original %>% 
  left_join(cdm$person %>% select(person_id, care_site_id), by = "person_id") %>% 
  left_join(cdm$care_site %>% select(care_site_id, location_id), by = "care_site_id") %>%
  left_join(cdm$location %>% select(location_id, region = location_source_value), by = "location_id") %>%
  select(-c(care_site_id, location_id)) %>%
  filter(region == !!allRegions[1]) %>%
  select(-"region") %>%
  compute()

print(paste0("- Getting denominator: general population"))
info(logger, "- Getting denominator: general population")

cdm$denominator_pre <- generateDenominatorCohortSet(
  cdm = cdm,
  startDate = as.Date("2017-01-01"),
  endDate = as.Date("2020-03-22"),
  ageGroup = list(c(0,150), c(0,19), c(20,39), c(40,59), c(60,79), c(80,150)),
  sex = c("Both", "Male", "Female"),
  daysPriorHistory = 365,
  verbose = TRUE
)

cdm$denominator_pre %>% glimpse()

cdm$denominator_pre %>% tally()  # to check numbers in denominator population

print(paste0("- Got denominator: pre-COVID"))
info(logger, "- Got denominator: pre-COVID")



## ================= CALCULATE INCIDENCE - PRE-COVID ======================== ##

print(paste0("- Getting incidence: cancer populations pre-covid"))
info(logger, "- Getting incidence: cancer populations pre-covid")


inc_pre <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator_pre",
  outcomeTable = outcome_table_name_1, 
  outcomeCohortId = outcome_cohorts_1$cohortId,
  outcomeCohortName = outcome_cohorts_1$cohortName,
  interval = "overall",
  completeDatabaseIntervals = FALSE,
  outcomeWashout = c(0, NULL, 365),
  repeatedEvents = FALSE,
  minCellCount = 5,
  verbose = TRUE
)

inc_pre %>%
  glimpse()


save(inc_pre, file = here("Results", db.name, "1_Cancers", "inc_pre.RData"))


print(paste0("- Got incidence: cancer populations pre-covid"))
info(logger, "- Got incidence: cancer populations pre-covid")






## ============================= LOCKDOWN ================================== ##

## ======= Compute the denominator population LOCKDOWN  ==================== ##

print(paste0("- Getting denominator: general population lockdown"))
info(logger, "- Getting denominator: general population lockdown")

cdm$denominator_lock <- generateDenominatorCohortSet(
  cdm = cdm,
  startDate = as.Date("2020-03-23"),
  endDate = as.Date("2020-07-03"),
  ageGroup = list(c(0,150), c(0,19), c(20,39), c(40,59), c(60,79), c(80,150)),
  sex = c("Both", "Male", "Female"),
  daysPriorHistory = 365,
  verbose = TRUE
)

cdm$denominator_lock %>% glimpse()

cdm$denominator_lock %>% tally()  # to check numbers in denominator population

print(paste0("- Got denominator: lockdown"))
info(logger, "- Got denominator: lockdown")



## ================= CALCULATE INCIDENCE - LOCKDOWN ======================== ##

print(paste0("- Getting incidence: cancer populations lockdown"))
info(logger, "- Getting incidence: cancer populations lockdown")


inc_lock <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator_lock",
  outcomeTable = outcome_table_name_1, 
  outcomeCohortId = outcome_cohorts_1$cohortId,
  outcomeCohortName = outcome_cohorts_1$cohortName,
  interval = "overall",
  completeDatabaseIntervals = FALSE,
  outcomeWashout = c(0, NULL, 365),
  repeatedEvents = FALSE,
  minCellCount = 5,
  verbose = TRUE
)

inc_lock %>%
  glimpse()


save(inc_lock, file = here("Results", db.name, "1_Cancers", "inc_lock.RData"))


print(paste0("- Got incidence: cancer populations lockdown"))
info(logger, "- Got incidence: cancer populations lockdown")








## ============================= POST-LOCKDOWN ============================== ##

## ======= Compute the denominator population POST-LOCKDOWN  =================== ##

print(paste0("- Getting denominator: general population post-lockdown"))
info(logger, "- Getting denominator: general population post-lockdown")

cdm$denominator_post <- generateDenominatorCohortSet(
  cdm = cdm,
  startDate = as.Date("2020-07-04"),
  ageGroup = list(c(0,150), c(0,19), c(20,39), c(40,59), c(60,79), c(80,150)),
  sex = c("Both", "Male", "Female"),
  daysPriorHistory = 365,
  verbose = TRUE
)

cdm$denominator_post %>% glimpse()

cdm$denominator_post %>% tally()  # to check numbers in denominator population

print(paste0("- Got denominator: post-lockdown"))
info(logger, "- Got denominator: post-lockdown")



## ================= CALCULATE INCIDENCE - POST-LOCKDOWN ======================== ##

print(paste0("- Getting incidence: cancer populations post-lockdown"))
info(logger, "- Getting incidence: cancer populations post-lockdown")


inc_post <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator_post",
  outcomeTable = outcome_table_name_1, 
  outcomeCohortId = outcome_cohorts_1$cohortId,
  outcomeCohortName = outcome_cohorts_1$cohortName,
  interval = "overall",
  completeDatabaseIntervals = FALSE,
  outcomeWashout = c(0, NULL, 365),
  repeatedEvents = FALSE,
  minCellCount = 5,
  verbose = TRUE
)

inc_post %>%
  glimpse()


save(inc_post, file = here("Results", db.name, "1_Cancers", "inc_post.RData"))


print(paste0("- Got incidence: cancer populations post-lockdown"))
info(logger, "- Got incidence: cancer populations post-lockdown")


## ======== GATHER ALL INCIDENCE RESULTS ===================== ##

print(paste0("- Gathering incidence results: cancer populations"))
info(logger, "- Gathering incidence results: cancer populations")

study_results_3_periods <- gatherIncidencePrevalenceResults(cdm=cdm,
                                                  resultList=list(inc_pre, inc_lock, inc_post),
                                                  databaseName = db.name)


# save study results as a separate R.data file
save(study_results_3_periods, file = here("Results", db.name, "1_Cancers", "StudyResults_cancers_3_periods.RData"))

print(paste0("- Got incidence results: cancer populations - 3 periods"))
info(logger, "- Got incidence  results: cancer populations - 3 periods")


## ======== EXPORT ALL INCIDENCE  RESULTS ===================== ##

print(paste0("- Exporting incidence results: cancer populations - 3 periods"))
info(logger, "- Exporting incidence results: cancer populations - 3 periods")

exportIncidencePrevalenceResults(result=study_results_3_periods, 
                                 zipName=paste0(db.name, "IncCancerResults"),
                                 outputFolder=here("Results", db.name, "1_Cancers")) 

print(paste0("- Exported incidence results: cancer populations - 3 periods"))
info(logger, "- Exported incidence results: cancer populations - 3 periods")

