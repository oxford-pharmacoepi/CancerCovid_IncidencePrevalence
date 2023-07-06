# output files ----
if (!file.exists(output.folder)){
  dir.create(output.folder, recursive = TRUE)}

if (!file.exists(output.folder1)){
  dir.create(output.folder1, recursive = TRUE)}

if (!file.exists(output.folder2)){
  dir.create(output.folder2, recursive = TRUE)}

if (!file.exists(output.folder3)){
  dir.create(output.folder3, recursive = TRUE)}



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

# Run incidence analysis of screening tests as outcomes ----
info(logger, 'RUNNING INCIDENCE ANALYSIS OF SCREENING TESTS OUTCOMES')
source(here("2_Analysis","IncScreening.R"))
info(logger, 'INCIDENCE ANALYSIS OF SCREENING TESTS OUTCOMES RAN')


# Zip the files to export
files2zip <- dir(c(output.folder, output.folder1, output.folder2), full.names = TRUE)
zip(zipfile = paste0(db.name, "_full_inc_prev_results"), files = files2zip)



print("Done!")
print("-- If all has worked, there should now be a zip folder with your results in your home directory to share")
print("-- Thank you for running the study!")
Sys.time()-start
readLines(log_file)