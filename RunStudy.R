# output files ----
if (!file.exists(output.folder)){
  dir.create(output.folder, recursive = TRUE)}

if (!file.exists(output.folder1)){
  dir.create(output.folder1, recursive = TRUE)}

if (!file.exists(output.folder2)){
  dir.create(output.folder2, recursive = TRUE)}

if (!file.exists(output.folder3)){
  dir.create(output.folder3, recursive = TRUE)}

if (!file.exists(output.folder4)){
  dir.create(output.folder4, recursive = TRUE)}


# table names----
outcome_table_name_1 <- paste0(outcome_table_stem,"_cancers") # this is the four cancers
outcome_table_name_2 <- paste0(outcome_table_stem,"_1stevercancers") # this is the four cancers
strata_table_name_1 <- paste0(outcome_table_stem,"_breast_prostate_strata") # this is the breast and prostate cohorts to be used as denominator strata
outcome_table_name_3 <- paste0(outcome_table_stem,"_endocrine_tx_table") # this is the table for the endocrine treatments
strata_table_name_2 <- paste0(outcome_table_stem,"_breast_prostate_endocrine_strata") # this is the table for the breast/prostate cancer diagnosis cohorts who are on endocrine treatments to be used as denominator strata
outcome_table_name_4 <- paste0(outcome_table_stem,"_osteo_dx_table") # this is the table for the endocrine-treatment related outcomes of osteoporosis, osteopenia, bon fracture, bisphosphonates and denosumab
outcome_table_name_5 <- paste0(outcome_table_stem,"_screening_outcomes_table") # this is the table for the screening tests as outcomes

start<-Sys.time()

# start log ----
log_file <- paste0(output.folder, "/log.txt")
logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"

# instantiate study cohorts ----
info(logger, 'INSTANTIATING STUDY COHORTS')
source(here("1_InstantiateCohorts","InstantiateStudyCohorts.R"))
info(logger, 'GOT STUDY COHORTS')

# Run incidence and prevalence analysis of cancers ----
info(logger, 'RUNNING INCIDENCE AND PREVALENCE ANALYSIS OF CANCERS')
source(here("2_Analysis","IncPrevCancer.R"))
info(logger, 'INCIDENCE AND PREVALENCE ANALYSIS OF CANCERS RAN')

# Run incidence and prevalence analysis of endocrine treatments ----
info(logger, 'RUNNING INCIDENCE AND PREVALENCE ANALYSIS OF ENDOCRINE TREATMENTS')
source(here("2_Analysis","IncPrevEndocrineTx.R"))
info(logger, 'INCIDENCE AND PREVALENCE ANALYSIS OF ENDOCRINE TREATMENTS RAN')

# Run incidence and prevalence analysis of endocrine-treatment related outcomes ----
info(logger, 'RUNNING INCIDENCE AND PREVALENCE ANALYSIS OF ENDOCRINE TREATMENT RELATED OUTCOMES')
source(here("2_Analysis","IncPrevOsteoDx.R"))
info(logger, 'INCIDENCE AND PREVALENCE ANALYSIS OF ENDOCRINE TREATMENT RELATED OUTCOMES RAN')

# Run incidence analysis of screening tests as outcomes ----
info(logger, 'RUNNING INCIDENCE ANALYSIS OF SCREENING TESTS OUTCOMES')
source(here("2_Analysis","IncScreening.R"))
info(logger, 'INCIDENCE ANALYSIS OF SCREENING TESTS OUTCOMES RAN')

# add code for combining and exporting results

print("Done!")
print("-- If all has worked, there should now be a zip folder with your results in the output folder to share")
print("-- Thank you for running the study!")
Sys.time()-start
readLines(log_file)