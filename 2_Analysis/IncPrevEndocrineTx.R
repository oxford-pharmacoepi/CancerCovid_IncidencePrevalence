# ============================================================================ #
#                         Incidence/Prevalence for                             #
#          Endocrine Treatments in Breast and Prostate Cancer Cohorts          #
#                              Nicola Barclay                                  #
#                                18-1-2023                                     #
# ============================================================================ #


print(paste0("- Getting denominator: general population"))
info(logger, "- Getting denominator: general population")

## =================== SET THE STRATA DENOMINATOR COHORTS ==================== ##

strata_cohorts <- CDMConnector::readCohortSet(here("1_InstantiateCohorts", "CancerStrataCohorts"))
cdm <- CDMConnector::generateCohortSet(cdm, 
                                       strata_cohorts,
                                       cohortTableName = "breast_prostate_strata",
                                       overwrite = TRUE) 


cdm$breast_prostate_strata %>% group_by(cohort_definition_id) %>% tally() %>% collect() 


## ======= Compute the denominator population  ============================== ##

cdm$denominator <- generateDenominatorCohortSet(
  cdm = cdm,
  startDate = as.Date("2017-01-01"),
  strataTable = "breast_prostate_strata",
  strataCohortId = 1,
  strataCohortName = "strata_cohort_1_breast",
  ageGroup = list(c(0,150), c(0,19), c(20,39), c(40,59), c(60,79), c(80,150)),
  sex = c("Both", "Male", "Female"),
  daysPriorHistory = 180,
  verbose = TRUE

)


cdm$denominator %>% group_by(cohort_definition_id) %>% tally() %>% collect() # THIS SHOULD SHOW 9319 FOR THE BREAST CANCER COHORT

cdm$denominator %>% glimpse()

cdm$denominator %>%
  tally()

dpop <- cdm$denominator %>%
  collect() %>%
  left_join(settings(cdm$denominator))

dpop %>%
  glimpse()

dpop %>%
  group_by(cohort_definition_id, age_group, sex) %>%
  tally()

# View attrition table
attrition(cdm$denominator)

save(cdm, file = here("5_IncPrev", "Results", "breastdenominator.RData"))




## =================== SET THE OUTCOME COHORTS ============================== ##

outcome_cohorts <- CDMConnector::readCohortSet(here("5_IncPrev","endocrine_tx_cohorts"))
cdm <- CDMConnector::generateCohortSet(cdm, 
                                       outcome_cohorts,
                                       cohortTableName = "endocrine_tx_table",
                                       overwrite = TRUE)


cdm$endocrine_tx_table %>% group_by(cohort_definition_id) %>% tally() %>% collect() 

# these cohorts are instantiated as follows:

# cohort_definition_id       n
# <int64> <int64>
# 1                    1  575995         AndrogenDeprivationTherapy_GnRH
# 2                    2   32074         AndrogenDeprivationTherapy
# 3                    3   61737         AromataseInhibitors
# 4                    4  117034         EndocrineTxBreast
# 5                    5    2541         GnRH
# 6                    6  569665         GnRHAgonists
# 7                    7   77752         Tamoxifen



## =========================== BREAST CANCER ================================ ##


## =================== CALCULATE POINT PREVALENCE =========================== ##

# years
point_prev_yrs <- estimatePointPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "endocrine_tx_table", # this used to say "outcome". Change this to wherever your cohorts are instantiated
  outcomeCohortId = c(3,4,5,7), 
  interval = "Years",
  timePoint = "start",
  minCellCount = 5
)

point_prev_yrs %>%
  glimpse()


# months
point_prev_months <- estimatePointPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "endocrine_tx_table", # this used to say "outcome". Change this to wherever your cohorts are instantiated
  outcomeCohortId = c(3,4,5,7), 
  interval = "Months",
  timePoint = "start",
  minCellCount = 5
)

point_prev_months %>%
  glimpse()


## =================== CALCULATE PERIOD PREVALENCE =========================== ##

# years
per_prev_yrs <- estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "endocrine_tx_table",
  outcomeCohortId = c(3,4,5,7),
  interval = "Years",
  fullContribution= FALSE,
  minCellCount = 5
)

per_prev_yrs %>%
  glimpse()


per_prev_months <- estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "endocrine_tx_table", # this used to say "outcome". Change this to wherever your cohorts are instantiated
  outcomeCohortId = c(3,4,5,7), 
  interval = "Months",
  fullContribution= FALSE,
  minCellCount = 5
)

point_prev_months %>%
  glimpse()


save(point_prev_yrs, point_prev_months, per_prev_yrs, per_prev_months, file = here("5_IncPrev", "Results", "PrevTxBreast2.RData"))

## ======================== CALCULATE INCIDENCE ============================= ##

# years
inc_yrs <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable =  "endocrine_tx_table",
  outcomeCohortId = c(3,4,5,7), 
  interval = "years",
  outcomeWashout = 0,
  repeatedEvents = TRUE
)

inc_yrs %>%
  glimpse()


# months
inc_months <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable =  "endocrine_tx_table",
  outcomeCohortId = c(3,4,5,7), 
  interval = "months",
  outcomeWashout = 0,
  repeatedEvents = TRUE
)

inc_months %>%
  glimpse()


save(inc_yrs, inc_months, file = here("5_IncPrev", "Results", "IncTxBreast2.RData"))



## ======== GATHER ALL INCIDENCE AND PREVALENCE RESULTS FOR PLOTTING ======== ##

study_results <- gatherIncidencePrevalenceResults(cdm=cdm,
                                                  resultList=list(point_prev_yrs, point_prev_months, per_prev_yrs, per_prev_months, inc_yrs, inc_months),
                                                  databaseName = "cdmgold202007")

dplyr::glimpse(study_results$prevalence_estimates_cdmgold202007)

save(study_results, file = here("5_IncPrev", "Results", "StudyResults_breast2.RData"))

# After gathering results, we can export them as CSVs in a zip folder using the `exportIncidencePrevalenceResults()` function. 

exportIncidencePrevalenceResults(result=study_results, 
                                 zipName="IncPrev_BreastCancerTxResults2",
                                 outputFolder=here("5_IncPrev", "Results")) 



## ===================== PLOTS FOR DENOMINATOR POP == 1 ===================== ##

# POINT PREVALENCE IN YEARS FOR ALL AGE AND SEX STRATA

point_prev_yrs_plot_breast_tx <- study_results$prevalence_estimates %>% 
  filter(denominator_cohort_id == 1) %>% 
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_id == 3 ~ "Aromatase Inhibitors",
                             outcome_cohort_id == 4 ~ "Any Endocrine Treatments",
                             outcome_cohort_id == 5 ~ "GnRH",
                             outcome_cohort_id == 7 ~ "Tamoxifen"))

point_prev_yrs_plot_breast_tx <- as.data.frame(point_prev_yrs_plot_breast_tx )


point_prev_yrs_plot_breast_tx <- 
  ggplot(point_prev_yrs_plot_breast_tx , aes(x = prevalence_start_date, y=prevalence,
                                  ymin = prevalence_95CI_lower,
                                  ymax = prevalence_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  ggtitle("Point Prevalence of endocrine treatments in breast cancer patients, in years, before and after COVID-19 Lockdown") +
  labs(colour = "Endocrine Treatment") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  

print(point_prev_yrs_plot_breast_tx)

# save the plot as pdf
plotname <- paste0("point_prev_yrs_plot_breast_tx ",".pdf")

pdf(here("5_IncPrev", "Plots",plotname),
    width = 15, height = 5)
print(point_prev_yrs_plot_breast_tx, newpage = FALSE)
dev.off()


# POINT PREVALENCE IN MONTHS FOR ALL AGE AND SEX STRATA

point_prev_months_plot_breast_tx <- study_results$prevalence_estimates %>%  # need to amend this bit of code to select the estimates relating to point_prev_yrs
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "months") %>%
  mutate(outcome = case_when(outcome_cohort_id == 3 ~ "Aromatase Inhibitors",
                             outcome_cohort_id == 4 ~ "Any Endocrine Treatments",
                             outcome_cohort_id == 5 ~ "GnRH",
                             outcome_cohort_id == 7 ~ "Tamoxifen"))

point_prev_months_plot_breast_tx <- 
  ggplot(point_prev_months_plot_breast_tx, aes(x = prevalence_start_date, y=prevalence,
                                     ymin = prevalence_95CI_lower,
                                     ymax = prevalence_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  ggtitle("Point Prevalence of endocrine treatments in breast cancer patients, in months, before and after COVID-19 Lockdown") +
  labs(colour = "Endocrine Treatments") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-04-01"))),linetype=2, color="black")

point_prev_months_plot_breast_tx

# save the plot as pdf
plotname <- paste0("point_prev_months_breast_tx",".pdf")

pdf(here("5_IncPrev", "Plots",plotname),
    width = 15, height = 5)
print(point_prev_months_plot_breast_tx, newpage = FALSE)
dev.off()


# PERID PREVALENCE IN YEARS FOR ALL AGE AND SEX STRATA

per_prev_yrs_plot_breast_tx <- study_results$prevalence_estimates %>%  # need to amend this bit of code to select the estimates relating to per_prev_yrs
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_type == "period") %>% 
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_id == 3 ~ "Aromatase Inhibitors",
                             outcome_cohort_id == 4 ~ "Any Endocrine Treatments",
                             outcome_cohort_id == 5 ~ "GnRH",
                             outcome_cohort_id == 7 ~ "Tamoxifen"))

per_prev_yrs_plot_breast_tx <- 
  ggplot(per_prev_yrs_plot_breast_tx, aes(x = prevalence_start_date, y=prevalence,
                                ymin = prevalence_95CI_lower,
                                ymax = prevalence_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  ggtitle("Period Prevalence of endocrine treatments in breast cancer patients, in years, before and after COVID-19 Lockdown") +
  labs(colour = "Endocrine treatments") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-04-01"))),linetype=2, color="black")

per_prev_yrs_plot_breast_tx

# save the plot as pdf
plotname <- paste0("per_prev_yrs_breast_tx",".pdf")

pdf(here("5_IncPrev", "Plots",plotname),
    width = 11, height = 5)
print(per_prev_yrs_plot_breast_tx, newpage = FALSE)
dev.off()

# PERIOD PREVALENCE IN MONTHS FOR ALL AGE AND SEX STRATA

per_prev_months_plot_breast_tx <- study_results$prevalence_estimates %>%  # need to amend this bit of code to select the estimates relating to per_prev_yrs
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_type == "period") %>% 
  filter(analysis_interval == "months") %>%
  mutate(outcome = case_when(outcome_cohort_id == 3 ~ "Aromatase Inhibitors",
                             outcome_cohort_id == 4 ~ "Any Endocrine Treatments",
                             outcome_cohort_id == 5 ~ "GnRH",
                             outcome_cohort_id == 7 ~ "Tamoxifen"))

per_prev_months_plot_breast_tx <- 
  ggplot(per_prev_months_plot_breast_tx, aes(x = prevalence_start_date, y=prevalence,
                                   ymin = prevalence_95CI_lower,
                                   ymax = prevalence_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  ggtitle("Period Prevalence of endocrine treatments in breast cancer patients, in months, before and after COVID-19 Lockdown") +
  labs(colour = "Endocrine Treatments") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-04-01"))),linetype=2, color="black")

per_prev_months_plot_breast_tx


# save the plot as pdf
plotname <- paste0("per_prev_months_breast_tx",".pdf")

pdf(here("5_IncPrev", "Plots",plotname),
    width = 12, height = 5)
print(per_prev_months_plot_breast_tx, newpage = FALSE)
dev.off()

# INCIDENCE IN YEARS FOR ALL AGE AND SEX STRATA

inc_yrs_plot_breast_tx <- study_results$incidence_estimates %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_id == 3 ~ "Aromatase Inhibitors",
                             outcome_cohort_id == 4 ~ "Any Endocrine Treatments",
                             outcome_cohort_id == 5 ~ "GnRH",
                             outcome_cohort_id == 7 ~ "Tamoxifen"))

inc_yrs_plot_breast_tx <- as.data.frame(inc_yrs_plot_breast_tx)

inc_yrs_plot_breast_tx %>%
  ggplot(aes(x = incidence_start_date, y = incidence_100000_pys,
             ymin = incidence_100000_pys_95CI_lower,
             ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, NA)) +
  ggtitle("Incidence Rates of endocrine treatments in breast cancer patients, in years, before and after COVID-19 Lockdown") +
  labs(colour = "Endocrine Treatments") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-04-01"))),linetype=2, color="black")

inc_yrs_plot_breast_tx

# save the plot as pdf
plotname <- paste0("inc_yrs_plot_breast_tx",".pdf")

pdf(here("5_IncPrev", "Plots",plotname),
    width = 15, height = 5)
print(inc_yrs_plot_breast_tx, newpage = FALSE)
dev.off()

# INCIDENCE IN MONTHS FOR ALL AGE AND SEX STRATA

inc_months_plot_breast_tx <- study_results$incidence_estimates %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
  filter(denominator_cohort_id == 1) %>%
  mutate(outcome = case_when(outcome_cohort_id == 3 ~ "Aromatase Inhibitors",
                             outcome_cohort_id == 4 ~ "Any Endocrine Treatments",
                             outcome_cohort_id == 5 ~ "GnRH",
                             outcome_cohort_id == 7 ~ "Tamoxifen"))

inc_months_plot_breast_tx %>%
  ggplot(aes(x = incidence_start_date, y = incidence_100000_pys,
             ymin = incidence_100000_pys_95CI_lower,
             ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, NA)) +
  ggtitle("Incidence Rates of endocrine treatments in breast cancer patients, in months, before and after COVID-19 Lockdown") +
  labs(colour = "Endocrine Treatments") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-04-01"))),linetype=2, color="black")

inc_months_plot_breast_tx

# save the plot as pdf
plotname <- paste0("inc_months_breast_tx",".pdf")

pdf(here("5_IncPrev", "Plots",plotname),
    width = 15, height = 5)
print(inc_months_plot_breast_tx, newpage = FALSE)
dev.off()



# save all plot objects

save(point_prev_yrs_plot_breast_tx, point_prev_months_plot_breast_tx, per_prev_yrs_plot_breast_tx, per_prev_months_plot_breast_tx, inc_yrs_plot_breast_tx, inc_months_plot_breast_tx, file = here("5_IncPrev", "Plots", "BreastTxPlots2.RData"))


# ========= PLOTS STRATIFIED BY AGE AND SEX ================================== #

# POINT PREVALENCE IN YEARS STRATIFIED BY AGE AND SEX
point_prev_yrs_plot_breast_tx_s <- study_results$prevalence_estimates %>% 
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_id == 3 ~ "Aromatase Inhibitors",
                             outcome_cohort_id == 4 ~ "Any Endocrine Treatments",
                             outcome_cohort_id == 5 ~ "GnRH",
                             outcome_cohort_id == 7 ~ "Tamoxifen"))

point_prev_yrs_plot_breast_tx_s <- as.data.frame(point_prev_yrs_plot_breast_tx_s)

point_prev_yrs_plot_breast_tx_s <- 
  ggplot(point_prev_yrs_plot_breast_tx_s, aes(x = prevalence_start_date, y=prevalence,
                                    ymin = prevalence_95CI_lower,
                                    ymax = prevalence_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  facet_grid(~denominator_age_group ~denominator_sex) +
  ggtitle("Point Prevalence of Endocrine Treatments in Breast Cancer Patients in Years, Before and After COVID-19 Lockdown, Stratified by Age and Sex") +
  labs(colour = "Cancer") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-04-01"))),linetype=2, color="black")

point_prev_yrs_plot_breast_tx_s

# save the plot as pdf
plotname <- paste0("point_prev_yrs_s",".pdf")

pdf(here("4_IncPrev", "Plots",plotname),
    width = 15, height = 15)
print(point_prev_yrs_plot_breast_tx_s, newpage = FALSE)
dev.off()



# POINT PREVALENCE IN MONTHS STRATIFIED BY AGE AND SEX

point_prev_months_plot_s <- study_results$prevalence_estimates %>%  # need to amend this bit of code to select the estimates relating to point_prev_yrs
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "months") %>%
  mutate(outcome = case_when(outcome_cohort_id == 3 ~ "Aromatase Inhibitors",
                             outcome_cohort_id == 4 ~ "Any Endocrine Treatments",
                             outcome_cohort_id == 5 ~ "GnRH",
                             outcome_cohort_id == 7 ~ "Tamoxifen"))


point_prev_months_plot_s <- 
  ggplot(point_prev_months_plot_s, aes(x = prevalence_start_date, y=prevalence,
                                       ymin = prevalence_95CI_lower,
                                       ymax = prevalence_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  facet_grid(~denominator_age_group ~denominator_sex) +
  ggtitle("Point Prevalence of Endocrine Treatments in Breast Cancer Patients in Months, Before and After COVID-19 Lockdown Stratified by Age and Sex") +
  labs(colour = "Cancer") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-04-01"))),linetype=2, color="black")

point_prev_months_plot_s

# save the plot as pdf
plotname <- paste0("point_prev_months_plot_s",".pdf")

pdf(here("4_IncPrev", "Plots",plotname),
    width = 15, height = 15)
print(point_prev_months_plot_s, newpage = FALSE)
dev.off()


# PERIOD PREVALENCE IN YEARS STRATIFIED BY AGE AND SEX

per_prev_yrs_plot_s <- study_results$prevalence_estimates %>%  # need to amend this bit of code to select the estimates relating to per_prev_yrs
  filter(analysis_type == "period") %>% 
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_id == 3 ~ "Aromatase Inhibitors",
                             outcome_cohort_id == 4 ~ "Any Endocrine Treatments",
                             outcome_cohort_id == 5 ~ "GnRH",
                             outcome_cohort_id == 7 ~ "Tamoxifen"))


per_prev_yrs_plot_s <- 
  ggplot(per_prev_yrs_plot_s, aes(x = prevalence_start_date, y=prevalence,
                                  ymin = prevalence_95CI_lower,
                                  ymax = prevalence_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  facet_grid(~denominator_age_group ~denominator_sex) +
  ggtitle("Period Prevalence of Endocrine Treatments in Breast Cancer Patients in Years, Before and After COVID-19 Lockdown Stratified by Age and Sex") +
  labs(colour = "Cancer") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-04-01"))),linetype=2, color="black") 

per_prev_yrs_plot_s

# save the plot as pdf
plotname <- paste0("per_prev_yrs_plot_s",".pdf")

pdf(here("4_IncPrev", "Plots",plotname),
    width = 15, height = 15)
print(per_prev_yrs_plot_s, newpage = FALSE)
dev.off()

# PERIOD PREVALENCE IN MONTHS STRATIFIED BY AGE AND SEX

per_prev_months_plot_s <- study_results$prevalence_estimates %>%  # need to amend this bit of code to select the estimates relating to per_prev_yrs
  filter(analysis_type == "period") %>% 
  filter(analysis_interval == "months") %>%
  mutate(outcome = case_when(outcome_cohort_id == 3 ~ "Aromatase Inhibitors",
                             outcome_cohort_id == 4 ~ "Any Endocrine Treatments",
                             outcome_cohort_id == 5 ~ "GnRH",
                             outcome_cohort_id == 7 ~ "Tamoxifen"))


per_prev_months_plot_s <- 
  ggplot(per_prev_months_plot_s, aes(x = prevalence_start_date, y=prevalence,
                                     ymin = prevalence_95CI_lower,
                                     ymax = prevalence_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  facet_grid(~denominator_age_group ~denominator_sex) +
  ggtitle("Period Prevalence of Endocrine Treatments in Breast Cancer Patients in Months, Before and After COVID-19 Lockdown Stratified by Age and Sex") +
  labs(colour = "Cancer") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-04-01"))),linetype=2, color="black")

per_prev_months_plot_s


# save the plot as pdf
plotname <- paste0("per_prev_months_plot_s",".pdf")

pdf(here("4_IncPrev", "Plots",plotname),
    width = 15, height = 15)
print(per_prev_months_plot_s, newpage = FALSE)
dev.off()

# INCIDENCE IN YEARS STRATIFIED BY AGE AND SEX

inc_yrs_plot_s <- study_results$incidence_estimates %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_id == 3 ~ "Aromatase Inhibitors",
                             outcome_cohort_id == 4 ~ "Any Endocrine Treatments",
                             outcome_cohort_id == 5 ~ "GnRH",
                             outcome_cohort_id == 7 ~ "Tamoxifen"))

inc_yrs_plot_s <- as.data.frame(inc_yrs_plot_s)

inc_yrs_plot_s %>%
  ggplot(aes(x = incidence_start_date, y = incidence_100000_pys,
             ymin = incidence_100000_pys_95CI_lower,
             ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, NA)) +
  facet_grid(~denominator_age_group ~denominator_sex) +
  ggtitle("Incidence Rates of Endocrine Treatments in Breast Cancer Patients in Years, Before and After COVID-19 Lockdown Stratified by Age and Sex") +
  labs(colour = "Cancer") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-04-01"))),linetype=2, color="black")

print(inc_yrs_plot_s)

# save the plot as pdf
plotname <- paste0("inc_yrs_plot_s",".pdf")

pdf(here("4_IncPrev", "Plots",plotname),
    width = 15, height = 15)
print(inc_yrs_plot_s, newpage = FALSE)
dev.off()

# INCIDENCE IN MONTHS STRATIFIED BY AGE AND SEX

inc_months_plot_s <- study_results$incidence_estimates %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
  mutate(outcome = case_when(outcome_cohort_id == 3 ~ "Aromatase Inhibitors",
                             outcome_cohort_id == 4 ~ "Any Endocrine Treatments",
                             outcome_cohort_id == 5 ~ "GnRH",
                             outcome_cohort_id == 7 ~ "Tamoxifen"))


inc_months_plot_s %>%
  ggplot(aes(x = incidence_start_date, y = incidence_100000_pys,
             ymin = incidence_100000_pys_95CI_lower,
             ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, NA)) +
  facet_grid(~denominator_age_group ~denominator_sex) +
  ggtitle("Incidence Rates of Endocrine Treatments in Breast Cancer Patients in Months, Before and After COVID-19 Lockdown Stratified by Age and Sex") +
  labs(colour = "Cancer") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-04-01"))),linetype=2, color="black")

inc_months_plot_s

# save the plot as pdf
plotname <- paste0("inc_months_plot_s",".pdf")

pdf(here("4_IncPrev", "Plots",plotname),
    width = 15, height = 5)
print(inc_months_plot_s, newpage = FALSE)
dev.off()




# save all plot objects

save(point_prev_yrs_plot_breast_tx_s, point_prev_months_plot_breast_tx_s, per_prev_yrs_plot_breast_tx_s, per_prev_months_plot_breast_tx_s, inc_yrs_plot_breast_tx_s, inc_months_plot_breast_tx_s, file = here("5_IncPrev", "Plots", "BreastTxPlots_s2.RData"))




## =========================== PROSTATE CANCER ================================ ##




## ======= Compute the denominator population  ============================== ##

cdm$denominator <- generateDenominatorCohortSet(
  cdm = cdm,
  startDate = as.Date("2017-01-01"),
  strataTable = "breast_prostate_strata",
  strataCohortId = 2,
  strataCohortName = "strata_cohort_2_prostate",
  ageGroup = list(c(0,150), c(0,19), c(20,39), c(40,59), c(60,79), c(80,150)),
  sex = c("Both", "Male", "Female"),
  daysPriorHistory = 180, 
  verbose = TRUE
  
)


cdm$denominator %>% group_by(cohort_definition_id) %>% tally() %>% collect() # THIS SHOULD SHOW 8155 FOR THE PROSTATE CANCER COHORT

cdm$denominator %>% glimpse()

cdm$denominator %>%
  tally()

dpop <- cdm$denominator %>%
  collect() %>%
  left_join(settings(cdm$denominator))

dpop %>%
  glimpse()

dpop %>%
  group_by(cohort_definition_id, age_group, sex) %>%
  tally()

## =================== CALCULATE POINT PREVALENCE =========================== ##

# years
point_prev_yrs_PS <- estimatePointPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "endocrine_tx_table", # this used to say "outcome". Change this to wherever your cohorts are instantiated
  outcomeCohortId = c(1,2,6), 
  interval = "Years",
  timePoint = "middle",
  minCellCount = 5,outcomeLookbackDays = 
)

point_prev_yrs_PS %>%
  glimpse()


# months
point_prev_months_PS <- estimatePointPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "endocrine_tx_table", # this used to say "outcome". Change this to wherever your cohorts are instantiated
  outcomeCohortId = c(1,2,6), 
  interval = "Months",
  timePoint = "middle",
  minCellCount = 5
)

point_prev_months_PS %>%
  glimpse()


## =================== CALCULATE PERIOD PREVALENCE =========================== ##

# years
per_prev_yrs_PS <- estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "endocrine_tx_table",
  outcomeCohortId = c(1,2,6),
  interval = "Years",
  fullContribution= FALSE,
  minCellCount = 5
)

per_prev_yrs_PS %>%
  glimpse()


per_prev_months_PS <- estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "endocrine_tx_table", # this used to say "outcome". Change this to wherever your cohorts are instantiated
  outcomeCohortId = c(1,2,6), 
  interval = "Months",
  fullContribution= FALSE,
  minCellCount = 5
)

point_prev_months_PS %>%
  glimpse()


save(point_prev_yrs_PS, point_prev_months_PS, per_prev_yrs_PS, per_prev_months_PS, file = here("5_IncPrev", "Results", "PrevTxProstate.RData"))

## ======================== CALCULATE INCIDENCE ============================= ##

# years
inc_yrs_PS <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable =  "endocrine_tx_table",
  outcomeCohortId = c(1,2,6), 
  interval = "years",
  outcomeWashout = 0,
  repeatedEvents = TRUE
)

inc_yrs_PS %>%
  glimpse()


# months
inc_months_PS <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable =  "endocrine_tx_table",
  outcomeCohortId = c(1,2,6), 
  interval = "months",
  outcomeWashout = 0,
  repeatedEvents = TRUE
)

inc_months_PS %>%
  glimpse()


save(inc_yrs_PS, inc_months_PS, file = here("5_IncPrev", "Results", "IncTxProstate.RData"))



## ======== GATHER ALL INCIDENCE AND PREVALENCE RESULTS FOR PLOTTING ======== ##

study_results <- gatherIncidencePrevalenceResults(cdm=cdm,
                                                  resultList=list(point_prev_yrs_PS, point_prev_months_PS, per_prev_yrs_PS, per_prev_months_PS, inc_yrs_PS, inc_months_PS),
                                                  databaseName = "cdmgold202007")

dplyr::glimpse(study_results$prevalence_estimates_cdmgold202007)

save(study_results, file = here("5_IncPrev", "Results", "StudyResults_prostate.RData"))

# After gathering results, we can export them as CSVs in a zip folder using the `exportIncidencePrevalenceResults()` function. 

exportIncidencePrevalenceResults(result=study_results, 
                                 zipName="IncPrev_ProstateCancerTxResults2",
                                 outputFolder=here("5_IncPrev", "Results")) 



## ===================== PLOTS FOR DENOMINATOR POP == 1 ===================== ##

# POINT PREVALENCE IN YEARS FOR ALL AGE AND SEX STRATA

point_prev_yrs_plot_prostate_tx <- study_results$prevalence_estimates %>% 
  filter(denominator_cohort_id == 1) %>% 
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_id == 1 ~ "AndrogenDeprivationTherapy_GnRH",
                             outcome_cohort_id == 2 ~ "AndrogenDeprivationTherapy",
                             outcome_cohort_id == 6 ~ "GnRHAgonists"))

point_prev_yrs_plot_prostate_tx <- as.data.frame(point_prev_yrs_plot_prostate_tx)


point_prev_yrs_plot_prostate_tx <- 
  ggplot(point_prev_yrs_plot_prostate_tx , aes(x = prevalence_start_date, y=prevalence,
                                             ymin = prevalence_95CI_lower,
                                             ymax = prevalence_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  ggtitle("Point Prevalence of endocrine treatments in prostate cancer patients, in years, before and after COVID-19 Lockdown") +
  labs(colour = "Endocrine Treatment") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  

print(point_prev_yrs_plot_prostate_tx)

# save the plot as pdf
plotname <- paste0("point_prev_yrs_plot_prostate_tx ",".pdf")

pdf(here("5_IncPrev", "Plots",plotname),
    width = 10, height = 5)
print(point_prev_yrs_plot_prostate_tx, newpage = FALSE)
dev.off()


# POINT PREVALENCE IN MONTHS FOR ALL AGE AND SEX STRATA

point_prev_months_plot_prostate_tx <- study_results$prevalence_estimates %>%  # need to amend this bit of code to select the estimates relating to point_prev_yrs
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "months") %>%
  mutate(outcome = case_when(outcome_cohort_id == 1 ~ "AndrogenDeprivationTherapy_GnRH",
                             outcome_cohort_id == 2 ~ "AndrogenDeprivationTherapy",
                             outcome_cohort_id == 6 ~ "GnRHAgonists"))

point_prev_months_plot_prostate_tx <- 
  ggplot(point_prev_months_plot_prostate_tx, aes(x = prevalence_start_date, y=prevalence,
                                               ymin = prevalence_95CI_lower,
                                               ymax = prevalence_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  ggtitle("Point Prevalence of endocrine treatments in prostate cancer patients, in months, before and after COVID-19 Lockdown") +
  labs(colour = "Endocrine Treatments") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-04-01"))),linetype=2, color="black")

point_prev_months_plot_prostate_tx

# save the plot as pdf
plotname <- paste0("point_prev_months_prostate_tx",".pdf")

pdf(here("5_IncPrev", "Plots",plotname),
    width = 15, height = 5)
print(point_prev_months_plot_prostate_tx, newpage = FALSE)
dev.off()


# PERID PREVALENCE IN YEARS FOR ALL AGE AND SEX STRATA

per_prev_yrs_plot_prostate_tx <- study_results$prevalence_estimates %>%  # need to amend this bit of code to select the estimates relating to per_prev_yrs
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_type == "period") %>% 
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_id == 1 ~ "AndrogenDeprivationTherapy_GnRH",
                             outcome_cohort_id == 2 ~ "AndrogenDeprivationTherapy",
                             outcome_cohort_id == 6 ~ "GnRHAgonists"))

per_prev_yrs_plot_prostate_tx <- 
  ggplot(per_prev_yrs_plot_prostate_tx, aes(x = prevalence_start_date, y=prevalence,
                                          ymin = prevalence_95CI_lower,
                                          ymax = prevalence_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  ggtitle("Period Prevalence of endocrine treatments in prostate cancer patients, in years, before and after COVID-19 Lockdown") +
  labs(colour = "Endocrine treatments") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-04-01"))),linetype=2, color="black")

per_prev_yrs_plot_prostate_tx

# save the plot as pdf
plotname <- paste0("per_prev_yrs_prostate_tx",".pdf")

pdf(here("5_IncPrev", "Plots",plotname),
    width = 10.5, height = 5)
print(per_prev_yrs_plot_prostate_tx, newpage = FALSE)
dev.off()

# PERIOD PREVALENCE IN MONTHS FOR ALL AGE AND SEX STRATA

per_prev_months_plot_prostate_tx <- study_results$prevalence_estimates %>%  # need to amend this bit of code to select the estimates relating to per_prev_yrs
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_type == "period") %>% 
  filter(analysis_interval == "months") %>%
  mutate(outcome = case_when(outcome_cohort_id == 1 ~ "AndrogenDeprivationTherapy_GnRH",
                             outcome_cohort_id == 2 ~ "AndrogenDeprivationTherapy",
                             outcome_cohort_id == 6 ~ "GnRHAgonists"))

per_prev_months_plot_prostate_tx <- 
  ggplot(per_prev_months_plot_prostate_tx, aes(x = prevalence_start_date, y=prevalence,
                                             ymin = prevalence_95CI_lower,
                                             ymax = prevalence_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  ggtitle("Period Prevalence of endocrine treatments in prostate cancer patients, in months, before and after COVID-19 Lockdown") +
  labs(colour = "Endocrine Treatments") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-04-01"))),linetype=2, color="black")

per_prev_months_plot_prostate_tx


# save the plot as pdf
plotname <- paste0("per_prev_months_prostate_tx",".pdf")

pdf(here("5_IncPrev", "Plots",plotname),
    width = 15, height = 5)
print(per_prev_months_plot_prostate_tx, newpage = FALSE)
dev.off()

# INCIDENCE IN YEARS FOR ALL AGE AND SEX STRATA

inc_yrs_plot_prostate_tx <- study_results$incidence_estimates %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_id == 1 ~ "AndrogenDeprivationTherapy_GnRH",
                             outcome_cohort_id == 2 ~ "AndrogenDeprivationTherapy",
                             outcome_cohort_id == 6 ~ "GnRHAgonists"))

inc_yrs_plot_prostate_tx <- as.data.frame(inc_yrs_plot_prostate_tx)

inc_yrs_plot_prostate_tx %>%
  ggplot(aes(x = incidence_start_date, y = incidence_100000_pys,
             ymin = incidence_100000_pys_95CI_lower,
             ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, NA)) +
  ggtitle("Incidence Rates of endocrine treatments in prostate cancer patients, in years, before and after COVID-19 Lockdown") +
  labs(colour = "Endocrine Treatments") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-04-01"))),linetype=2, color="black")

inc_yrs_plot_prostate_tx

# save the plot as pdf
plotname <- paste0("inc_yrs_plot_prostate_tx",".pdf")

pdf(here("5_IncPrev", "Plots",plotname),
    width = 10.5, height = 5)
print(inc_yrs_plot_prostate_tx, newpage = FALSE)
dev.off()

# INCIDENCE IN MONTHS FOR ALL AGE AND SEX STRATA

inc_months_plot_prostate_tx <- study_results$incidence_estimates %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
  filter(denominator_cohort_id == 1) %>%
  mutate(outcome = case_when(outcome_cohort_id == 1 ~ "AndrogenDeprivationTherapy_GnRH",
                             outcome_cohort_id == 2 ~ "AndrogenDeprivationTherapy",
                             outcome_cohort_id == 6 ~ "GnRHAgonists"))

inc_months_plot_prostate_tx %>%
  ggplot(aes(x = incidence_start_date, y = incidence_100000_pys,
             ymin = incidence_100000_pys_95CI_lower,
             ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, NA)) +
  ggtitle("Incidence Rates of endocrine treatments in prostate cancer patients, in months, before and after COVID-19 Lockdown") +
  labs(colour = "Endocrine Treatments") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-04-01"))),linetype=2, color="black")

inc_months_plot_prostate_tx

# save the plot as pdf
plotname <- paste0("inc_months_prostate_tx",".pdf")

pdf(here("5_IncPrev", "Plots",plotname),
    width = 15, height = 5)
print(inc_months_plot_prostate_tx, newpage = FALSE)
dev.off()




# save all plot objects

save(point_prev_yrs_plot_prostate_tx, point_prev_months_plot_prostate_tx, per_prev_yrs_plot_prostate_tx, per_prev_months_plot_prostate_tx, inc_yrs_plot_prostate_tx, inc_months_plot_prostate_tx, file = here("5_IncPrev", "Plots", "prostateTxPlots2.RData"))




## ========== PLOTS FOR PROSTATE CANCER STRATIFIED BY AGE AND SEX =========== ##

# POINT PREVALENCE IN YEARS STRATIFIED BY AGE AND SEX

point_prev_yrs_plot_prostate_tx_s <- study_results$prevalence_estimates %>% 
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_id == 1 ~ "AndrogenDeprivationTherapy_GnRH",
                             outcome_cohort_id == 2 ~ "AndrogenDeprivationTherapy",
                             outcome_cohort_id == 6 ~ "GnRHAgonists"))

point_prev_yrs_plot_prostate_tx_s <- as.data.frame(point_prev_yrs_plot_prostate_tx_s)


point_prev_yrs_plot_prostate_tx_s <- 
  ggplot(point_prev_yrs_plot_prostate_tx_s , aes(x = prevalence_start_date, y=prevalence,
                                               ymin = prevalence_95CI_lower,
                                               ymax = prevalence_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  facet_grid(~denominator_age_group ~denominator_sex) +
  ggtitle("Point Prevalence of endocrine treatments in prostate cancer patients, in years, before and after COVID-19 Lockdown Stratified by Age and Sex") +
  labs(colour = "Endocrine Treatment") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  

print(point_prev_yrs_plot_prostate_tx_s)

# save the plot as pdf
plotname <- paste0("point_prev_yrs_plot_prostate_tx_s ",".pdf")

pdf(here("5_IncPrev", "Plots",plotname),
    width = 10, height = 5)
print(point_prev_yrs_plot_prostate_tx_s, newpage = FALSE)
dev.off()


# POINT PREVALENCE IN MONTHS STRATIFIED BY AGE AND SEX

point_prev_months_plot_prostate_tx_s <- study_results$prevalence_estimates %>%  # need to amend this bit of code to select the estimates relating to point_prev_yrs
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "months") %>%
  mutate(outcome = case_when(outcome_cohort_id == 1 ~ "AndrogenDeprivationTherapy_GnRH",
                             outcome_cohort_id == 2 ~ "AndrogenDeprivationTherapy",
                             outcome_cohort_id == 6 ~ "GnRHAgonists"))

point_prev_months_plot_prostate_tx_s <- 
  ggplot(point_prev_months_plot_prostate_tx_s, aes(x = prevalence_start_date, y=prevalence,
                                                 ymin = prevalence_95CI_lower,
                                                 ymax = prevalence_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  facet_grid(~denominator_age_group ~denominator_sex) +
  ggtitle("Point Prevalence of endocrine treatments in prostate cancer patients, in months, before and after COVID-19 lockdown stratified by age and sex") +
  labs(colour = "Endocrine Treatments") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-04-01"))),linetype=2, color="black") 

point_prev_months_plot_prostate_tx_s

# save the plot as pdf
plotname <- paste0("point_prev_months_prostate_tx_s",".pdf")

pdf(here("5_IncPrev", "Plots",plotname),
    width = 15, height = 5)
print(point_prev_months_plot_prostate_tx_s, newpage = FALSE)
dev.off()


# PERID PREVALENCE IN YEARS STRATIFIED BY AGE AND SEX

per_prev_yrs_plot_prostate_tx_s <- study_results$prevalence_estimates %>%  # need to amend this bit of code to select the estimates relating to per_prev_yrs
  filter(analysis_type == "period") %>% 
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_id == 1 ~ "AndrogenDeprivationTherapy_GnRH",
                             outcome_cohort_id == 2 ~ "AndrogenDeprivationTherapy",
                             outcome_cohort_id == 6 ~ "GnRHAgonists"))

per_prev_yrs_plot_prostate_tx_s <- 
  ggplot(per_prev_yrs_plot_prostate_tx_s, aes(x = prevalence_start_date, y=prevalence,
                                            ymin = prevalence_95CI_lower,
                                            ymax = prevalence_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  facet_grid(~denominator_age_group ~denominator_sex) +
  ggtitle("Period Prevalence of endocrine treatments in prostate cancer patients, in years, before and after COVID-19 lockdown stratified by age and sex") +
  labs(colour = "Endocrine treatments") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-04-01"))),linetype=2, color="black")

per_prev_yrs_plot_prostate_tx_s

# save the plot as pdf
plotname <- paste0("per_prev_yrs_prostate_tx_s",".pdf")

pdf(here("5_IncPrev", "Plots",plotname),
    width = 10.5, height = 5)
print(per_prev_yrs_plot_prostate_tx_s, newpage = FALSE)
dev.off()

# PERIOD PREVALENCE IN MONTHS STRATIFIED BY AGE AND SEX

per_prev_months_plot_prostate_tx_s <- study_results$prevalence_estimates %>%  # need to amend this bit of code to select the estimates relating to per_prev_yrs
  filter(analysis_type == "period") %>% 
  filter(analysis_interval == "months") %>%
  mutate(outcome = case_when(outcome_cohort_id == 1 ~ "AndrogenDeprivationTherapy_GnRH",
                             outcome_cohort_id == 2 ~ "AndrogenDeprivationTherapy",
                             outcome_cohort_id == 6 ~ "GnRHAgonists"))

per_prev_months_plot_prostate_tx_s <- 
  ggplot(per_prev_months_plot_prostate_tx_s, aes(x = prevalence_start_date, y=prevalence,
                                               ymin = prevalence_95CI_lower,
                                               ymax = prevalence_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  facet_grid(~denominator_age_group ~denominator_sex) +
  ggtitle("Period Prevalence of endocrine treatments in prostate cancer patients, in months, before and after COVID-19 lockdown startified by age and sex") +
  labs(colour = "Endocrine Treatments") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-04-01"))),linetype=2, color="black")

per_prev_months_plot_prostate_tx_s


# save the plot as pdf
plotname <- paste0("per_prev_months_prostate_tx_s",".pdf")

pdf(here("5_IncPrev", "Plots",plotname),
    width = 15, height = 5)
print(per_prev_months_plot_prostate_tx_s, newpage = FALSE)
dev.off()

# INCIDENCE IN YEARS STRATIFIED BY AGE AND SEX

inc_yrs_plot_prostate_tx_s <- study_results$incidence_estimates %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_id == 1 ~ "AndrogenDeprivationTherapy_GnRH",
                             outcome_cohort_id == 2 ~ "AndrogenDeprivationTherapy",
                             outcome_cohort_id == 6 ~ "GnRHAgonists"))

inc_yrs_plot_prostate_tx_s <- as.data.frame(inc_yrs_plot_prostate_tx_s)

inc_yrs_plot_prostate_tx_s %>%
  ggplot(aes(x = incidence_start_date, y = incidence_100000_pys,
             ymin = incidence_100000_pys_95CI_lower,
             ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, NA)) +
  facet_grid(~denominator_age_group ~denominator_sex) +
  ggtitle("Incidence Rates of endocrine treatments in prostate cancer patients, in years, before and after COVID-19 lockdown startified by age and sex") +
  labs(colour = "Endocrine Treatments") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-04-01"))),linetype=2, color="black")

inc_yrs_plot_prostate_tx_s

# save the plot as pdf
plotname <- paste0("inc_yrs_plot_prostate_tx_s",".pdf")

pdf(here("5_IncPrev", "Plots",plotname),
    width = 10.5, height = 5)
print(inc_yrs_plot_prostate_tx_s, newpage = FALSE)
dev.off()

# INCIDENCE IN MONTHS STRATIFIED BY AGE AND SEX

inc_months_plot_prostate_tx_s <- study_results$incidence_estimates %>% 
  filter(analysis_interval == "months") %>%# need to amend this bit of code to select the estimates relating to inc_yrs
  mutate(outcome = case_when(outcome_cohort_id == 1 ~ "AndrogenDeprivationTherapy_GnRH",
                             outcome_cohort_id == 2 ~ "AndrogenDeprivationTherapy",
                             outcome_cohort_id == 6 ~ "GnRHAgonists"))

inc_months_plot_prostate_tx_s %>%
  ggplot(aes(x = incidence_start_date, y = incidence_100000_pys,
             ymin = incidence_100000_pys_95CI_lower,
             ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, NA)) +
  facet_grid(~denominator_age_group ~denominator_sex) +
  ggtitle("Incidence Rates of endocrine treatments in prostate cancer patients, in months, before and after COVID-19 lockdown startified by age and sex") +
  labs(colour = "Endocrine Treatments") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-04-01"))),linetype=2, color="black")

inc_months_plot_prostate_tx_s

# save the plot as pdf
plotname <- paste0("inc_months_prostate_tx_s",".pdf")

pdf(here("5_IncPrev", "Plots",plotname),
    width = 15, height = 5)
print(inc_months_plot_prostate_tx_s, newpage = FALSE)
dev.off()




# save all plot objects

save(point_prev_yrs_plot_prostate_tx_s, point_prev_months_plot_prostate_tx_s, per_prev_yrs_plot_prostate_tx_s, per_prev_months_plot_prostate_tx_s, inc_yrs_plot_prostate_tx_s, inc_months_plot_prostate_tx_s, file = here("5_IncPrev", "Plots", "prostateTxPlots_s2.RData"))

