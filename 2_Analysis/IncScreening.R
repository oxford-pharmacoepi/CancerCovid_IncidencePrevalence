# ============================================================================ #
#                               Incidence for                                  #
#                        Cancer related Screening Tests                        #
#                              Nicola Barclay                                  #
#                                30-03-2023                                    #
# ============================================================================ #

print(paste0("- 4. Incidence of Screening tests"))
info(logger, "- 4. Incidence of Screening tests")

## ======= Compute the denominator population (\~7 minutes in CPRD GOLD) ==== ##

print(paste0("- Getting denominator: general population"))
info(logger, "- Getting denominator: general population")

cdm$denominator <- generateDenominatorCohortSet(
  cdm = cdm,
  startDate = as.Date("2017-01-01"),
  ageGroup = list(c(0,150), c(0,19), c(20,39), c(40,59), c(60,79), c(80,150)),
  sex = c("Both", "Male", "Female"),
  daysPriorHistory = 365,
  verbose = TRUE
)

cdm$denominator %>% glimpse()

cdm$denominator %>% tally()  # to check numbers in denominator population

dpop <- cdm$denominator %>%
  collect() %>%
  left_join(settings(cdm$denominator))

dpop %>%
  group_by(cohort_definition_id, age_group, sex) %>%
  tally()

# View attrition table
attrition(cdm$denominator)

print(paste0("- Got denominator: general population"))
info(logger, "- Got denominator: general population")


## ======================== CALCULATE INCIDENCE ============================= ##

print(paste0("- Getting incidence: Screening tests"))
info(logger, "- Getting incidence: Screening tests")


inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = outcome_table_name_5, 
  outcomeCohortId = outcome_cohorts_5$cohortId,
  outcomeCohortName = outcome_cohorts_5$cohortName,
  interval = c("months", "years"),
  completeDatabaseIntervals = FALSE,
  outcomeWashout = c(0, NULL, 365),
  repeatedEvents = TRUE,
  minCellCount = 5,
  verbose = TRUE
)

inc %>%
  glimpse()



save(inc, file = here("Results", db.name, "4_ScreeningTests", "inc.RData"))


print(paste0("- Got incidence: Screening tests"))
info(logger, "- Got incidence: Screening tests")


## ======== GATHER ALL INCIDENCE  RESULTS ===================== ##

print(paste0("- Gathering incidence  results: Screening tests"))
info(logger, "- Gathering incidence  results: Screening tests")

study_results <- gatherIncidencePrevalenceResults(cdm=cdm,
                                                  resultList=list(inc),
                                                  databaseName = db.name)


# save study results as a separate R.data file
save(study_results, file = here("Results", db.name, "4_ScreeningTests", "StudyResults_ScreeningTests.RData"))

print(paste0("- Got incidence  results: Screening tests"))
info(logger, "- Got incidence  results: Screening tests")


## ======== EXPORT ALL INCIDENCE  RESULTS ===================== ##

print(paste0("- Exporting incidence  results: Screening tests"))
info(logger, "- Exporting incidence  results: Screening tests")

exportIncidencePrevalenceResults(result=study_results, 
                                 zipName=paste0(db.name, "IncScreeningTestsResults"),
                                 outputFolder=here("Results", db.name, "4_ScreeningTests")) 

print(paste0("- Exported incidence  results: Screening tests"))
info(logger, "- Exported incidence  results: Screening tests")


## ===================== PLOTS FOR DENOMINATOR POP == 1 ===================== ##
# These all need updating


print(paste0("- Plotting incidence Screening tests denominator 1"))
info(logger, "- Plotting incidence Screening tests denominator 1")


# INCIDENCE IN YEARS FOR ALL AGE AND SEX STRATA

inc_yrs_plot <- study_results$incidence_estimates %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_outcome_washout == 365) %>% 
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_name == "BreastCancer" ~ "Breast",
                             outcome_cohort_name == "ColorectalCancer" ~ "Colorectal",
                             outcome_cohort_name == "LungCancer" ~ "Lung",
                             outcome_cohort_name == "ProstateCancer" ~ "Prostate")) %>% 
  as.data.frame()

inc_yrs_plot <- 
  ggplot(inc_yrs_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                                ymin = incidence_100000_pys_95CI_lower,
                                ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, 150)) +
  ggtitle("Incidence Rates of Screening Tests in Years Before and After COVID-19 Lockdown") +
  labs(colour = "Cancer", x="Time" , y="Incidence per 100000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

inc_yrs_plot

# save the plot as pdf
analysis.name <- "Screening"
plotname <- paste0("inc_yrs",db.name, analysis.name, ".pdf")

pdf(here("Results", db.name,"4_ScreeningTests",plotname),
    width = 10, height = 8)
print(inc_yrs_plot, newpage = FALSE)
dev.off()




# INCIDENCE IN MONTHS FOR ALL AGE AND SEX STRATA 

inc_months_plot <- study_results$incidence_estimates %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_outcome_washout == 365) %>% 
  filter(analysis_interval == "months") %>%
  mutate(outcome = case_when(outcome_cohort_name == "BreastCancer" ~ "Breast",
                             outcome_cohort_name == "ColorectalCancer" ~ "Colorectal",
                             outcome_cohort_name == "LungCancer" ~ "Lung",
                             outcome_cohort_name == "ProstateCancer" ~ "Prostate")) %>% 
  as.data.frame()

inc_months_plot <- 
  ggplot(inc_months_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                           ymin = incidence_100000_pys_95CI_lower,
                           ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, 150)) +
  ggtitle("Incidence Rates of Screening Tests in Months Before and After COVID-19 Lockdown") +
  labs(colour = "Cancer", x="Time" , y="Incidence per 100000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

inc_months_plot

# save the plot as pdf
plotname <- paste0("inc_months", db.name, analysis.name, ".pdf")

pdf(here("Results", db.name,"4_ScreeningTests",plotname),
    width = 12, height = 8)
print(inc_months_plot, newpage = FALSE)
dev.off()

print(paste0("- Plots of incidence of screening tests denominator 1 done"))
info(logger, "- Plots of incidence of screening tests denominator 1 done")

# ========= PLOTS STRATIFIED BY AGE AND SEX ================================== #

print(paste0("- Plotting incidence of screening tests stratified by age and sex"))
info(logger, "- Plotting incidence of screening tests stratified by age and sex")


# INCIDENCE IN YEARS STRATIFIED BY AGE AND SEX

inc_yrs_plot_s <- study_results$incidence_estimates %>%  
  filter(analysis_outcome_washout == 365) %>% 
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_name == "BreastCancer" ~ "Breast",
                             outcome_cohort_name == "ColorectalCancer" ~ "Colorectal",
                             outcome_cohort_name == "LungCancer" ~ "Lung",
                             outcome_cohort_name == "ProstateCancer" ~ "Prostate")) %>% 
  as.data.frame()

inc_yrs_plot_s <- 
  ggplot(inc_yrs_plot_s, aes(x = incidence_start_date, y=incidence_100000_pys,
                           ymin = incidence_100000_pys_95CI_lower,
                           ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, 400)) +
  facet_grid(~denominator_age_group ~denominator_sex) +
    ggtitle("Incidence Rates of Cancer in Years before and after COVID-19 Lockdown Stratified by Age and Sex") +
  labs(colour = "Cancer", x="Time" , y="Incidence per 100000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

inc_yrs_plot_s

# save the plot as pdf
plotname <- paste0("inc_yrs_s",db.name, analysis.name, ".pdf")

pdf(here("Results", db.name,"4_ScreeningTests",plotname),
    width = 12, height = 12)
print(inc_yrs_plot_s, newpage = FALSE)
dev.off()




# INCIDENCE IN MONTHS STRATIFIED BY AGE AND SEX 

inc_months_plot_s <- study_results$incidence_estimates %>%  
  filter(analysis_interval == "months") %>%
  filter(analysis_outcome_washout == 365) %>% 
  mutate(outcome = case_when(outcome_cohort_name == "BreastCancer" ~ "Breast",
                             outcome_cohort_name == "ColorectalCancer" ~ "Colorectal",
                             outcome_cohort_name == "LungCancer" ~ "Lung",
                             outcome_cohort_name == "ProstateCancer" ~ "Prostate")) %>% 
  as.data.frame()

inc_months_plot_s <- 
  ggplot(inc_months_plot_s, aes(x = incidence_start_date, y=incidence_100000_pys,
                              ymin = incidence_100000_pys_95CI_lower,
                              ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, 400)) +
  facet_grid(~denominator_age_group ~denominator_sex) +
    ggtitle("Incidence Rates of Screening Tests in Months Before and After COVID-19 Lockdown Stratified by Age and Sex") +
  labs(colour = "Cancer", x="Time" , y="Incidence per 100000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

inc_months_plot_s

# save the plot as pdf
plotname <- paste0("inc_months_s", db.name, analysis.name, ".pdf")

pdf(here("Results", db.name,"4_ScreeningTests",plotname),
    width = 12, height = 12)
print(inc_months_plot_s, newpage = FALSE)
dev.off()


print(paste0("- Plots of incidence of screening tests stratified by age and sex done"))
info(logger, "- Plots of incidence of screening tests stratified by age and sex done")

print(paste0("- Analysis of screening tests done"))
info(logger, "- Analysis of screening tests done")
