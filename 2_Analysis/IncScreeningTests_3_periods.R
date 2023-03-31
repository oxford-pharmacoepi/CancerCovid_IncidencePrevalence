# ============================================================================ #
#                         Incidence/Prevalence for                             #
#                 Breast, Colorectal, Lung and Prostate Cancer                 #
#                       Screening and diagnostic tests                         #
#                         Pre-COVID, lockdown, and Post-lockdown               #
#                              Nicola Barclay                                  #
#                                30-03-2023                                    #
# ============================================================================ #

print(paste0("- 1. Incidence of screening and diagnostic tests"))
info(logger, "- 1. Incidence screening and diagnostic tests")


## ============================= PRE-COVID ================================== ##

## ======= Compute the denominator population pre-COVID  ==================== ##

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

print(paste0("- Getting incidence: screening and diagnostic tests pre-covid"))
info(logger, "- Getting incidence: screening and diagnostic tests pre-covid")


inc_pre <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator_pre",
  outcomeTable = outcome_table_name_5, 
  outcomeCohortId = outcome_cohorts_5$cohortId,
  outcomeCohortName = outcome_cohorts_5$cohortName,
  interval = "overall",
  completeDatabaseIntervals = FALSE,
  outcomeWashout = c(0, NULL, 365),
  repeatedEvents = FALSE,
  minCellCount = 5,
  verbose = TRUE
)

inc_pre %>%
  glimpse()


save(inc_pre, file = here("Results", db.name, "4_ScreeningTests", "inc_pre.RData"))


print(paste0("- Got incidence: screening and diagnostic tests pre-covid"))
info(logger, "- Got incidence: screening and diagnostic tests pre-covid")






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

print(paste0("- Getting incidence: screening and diagnostic tests lockdown"))
info(logger, "- Getting incidence: screening and diagnostic tests lockdown")


inc_lock <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator_lock",
  outcomeTable = outcome_table_name_5, 
  outcomeCohortId = outcome_cohorts_5$cohortId,
  outcomeCohortName = outcome_cohorts_5$cohortName,
  interval = "overall",
  completeDatabaseIntervals = FALSE,
  outcomeWashout = c(0, NULL, 365),
  repeatedEvents = FALSE,
  minCellCount = 5,
  verbose = TRUE
)

inc_lock %>%
  glimpse()


save(inc_lock, file = here("Results", db.name, "4_ScreeningTests", "inc_lock.RData"))


print(paste0("- Got incidence: screening and diagnostic tests lockdown"))
info(logger, "- Got incidence: screening and diagnostic tests lockdown")








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

print(paste0("- Getting incidence: screening and diagnostic tests post-lockdown"))
info(logger, "- Getting incidence: screening and diagnostic tests post-lockdown")


inc_post <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator_post",
  outcomeTable = outcome_table_name_5, 
  outcomeCohortId = outcome_cohorts_5$cohortId,
  outcomeCohortName = outcome_cohorts_5$cohortName,
  interval = "overall",
  completeDatabaseIntervals = FALSE,
  outcomeWashout = c(0, NULL, 365),
  repeatedEvents = FALSE,
  minCellCount = 5,
  verbose = TRUE
)

inc_post %>%
  glimpse()


save(inc_post, file = here("Results", db.name, "4_ScreeningTests", "inc_post.RData"))


print(paste0("- Got incidence: screening and diagnostic tests post-lockdown"))
info(logger, "- Got incidence: screening and diagnostic tests post-lockdown")


## ======== GATHER ALL INCIDENCE RESULTS ===================== ##

print(paste0("- Gathering incidence results: screening and diagnostic tests"))
info(logger, "- Gathering incidence results: screening and diagnostic tests")

study_results_3_periods <- gatherIncidencePrevalenceResults(cdm=cdm,
                                                  resultList=list(inc_pre, inc_lock, inc_post),
                                                  databaseName = db.name)


# save study results as a separate R.data file
save(study_results_3_periods, file = here("Results", db.name, "4_ScreeningTests", "StudyResults_cancers_3_periods.RData"))

print(paste0("- Got incidence results: screening and diagnostic tests - 3 periods"))
info(logger, "- Got incidence  results: screening and diagnostic tests - 3 periods")


## ======== EXPORT ALL INCIDENCE  RESULTS ===================== ##

print(paste0("- Exporting incidence results: screening and diagnostic tests - 3 periods"))
info(logger, "- Exporting incidence results: screening and diagnostic tests - 3 periods")

exportIncidencePrevalenceResults(result=study_results_3_periods, 
                                 zipName=paste0(db.name, "IncScreeningTestsResults_3_time_periods"),
                                 outputFolder=here("Results", db.name, "4_ScreeningTests")) 

print(paste0("- Exported incidence results: screening and diagnostic tests - 3 periods"))
info(logger, "- Exported incidence results: screening and diagnostic tests - 3 periods")

