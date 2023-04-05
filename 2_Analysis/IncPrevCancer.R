# ============================================================================ #
#                         Incidence/Prevalence for                             #
#                 Breast, Colorectal, Lung and Prostate Cancer                 #
#                              Nicola Barclay                                  #
#                                18-01-2023                                    #
# ============================================================================ #

print(paste0("- 1. Incidence and Prevalence of Cancers"))
info(logger, "- 1. Incidence and Prevalence of Cancers")

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

## =================== CALCULATE POINT PREVALENCE FOR CANCERS ================ ##

print(paste0("- Getting point prevalence: cancer populations"))
info(logger, "- Getting point prevalence: cancer populations")

point_prev <- estimatePointPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = outcome_table_name_1, 
  outcomeCohortId = outcome_cohorts_1$cohortId,
  outcomeCohortName = outcome_cohorts_1$cohortName,
  outcomeLookbackDays = 365,
  interval = c("months", "quarters", "years"),
  timePoint = "start",
  minCellCount = 5,
  verbose = TRUE
)


point_prev %>%
  glimpse()


print(paste0("- Got point prevalence: cancer populations"))
info(logger, "- Got point prevalence: cancer populations")

save(point_prev, file = here("Results", db.name, "1_Cancers", "prev_cancers.RData"))


## ============= CALCULATE POINT PREVALENCE FOR 1st EVER CANCERS ============ ##

print(paste0("- Getting point prevalence: 1st ever cancer populations"))
info(logger, "- Getting point prevalence: 1st ever cancer populations")

point_prev_1st_ever <- estimatePointPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = outcome_table_name_2, 
  outcomeCohortId = outcome_cohorts_2$cohortId,
  outcomeCohortName = outcome_cohorts_2$cohortName,
  outcomeLookbackDays = 365,
  interval = c("months", "quarters", "years"),
  timePoint = "start",
  minCellCount = 5,
  verbose = TRUE
)

point_prev_1st_ever %>%
  glimpse()


print(paste0("- Got point prevalence: 1st ever cancer populations"))
info(logger, "- Got point prevalence: 1st ever cancer populations")

save(point_prev_1st_ever, file = here("Results", db.name, "1_Cancers", "prev_1st_ever_cancers.RData"))


## ======================== CALCULATE INCIDENCE ============================= ##

print(paste0("- Getting incidence: cancer populations"))
info(logger, "- Getting incidence: cancer populations")


inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = outcome_table_name_1, 
  outcomeCohortId = outcome_cohorts_1$cohortId,
  outcomeCohortName = outcome_cohorts_1$cohortName,
  interval = c("months", "quarters", "years"),
  completeDatabaseIntervals = FALSE,
  outcomeWashout = c(0, NULL, 365),
  repeatedEvents = FALSE,
  minCellCount = 5,
  verbose = TRUE
)

inc %>%
  glimpse()


save(inc, file = here("Results", db.name, "1_Cancers", "inc.RData"))


print(paste0("- Got incidence: cancer populations"))
info(logger, "- Got incidence: cancer populations")


## ======== GATHER ALL INCIDENCE AND PREVALENCE RESULTS ===================== ##

print(paste0("- Gathering incidence and prevalence results: cancer populations"))
info(logger, "- Gathering incidence and prevalence results: cancer populations")

study_results <- gatherIncidencePrevalenceResults(cdm=cdm,
                                                  resultList=list(point_prev, point_prev_1st_ever, inc),
                                                  databaseName = db.name)


# save study results as a separate R.data file
save(study_results, file = here("Results", db.name, "1_Cancers", "StudyResults_cancers.RData"))

print(paste0("- Got incidence and prevalence results: cancer populations"))
info(logger, "- Got incidence and prevalence results: cancer populations")


## ======== EXPORT ALL INCIDENCE AND PREVALENCE RESULTS ===================== ##

print(paste0("- Exporting incidence and prevalence results: cancer populations"))
info(logger, "- Exporting incidence and Prevalence results: cancer populations")

exportIncidencePrevalenceResults(result=study_results, 
                                 zipName=paste0(db.name, "IncPrevCancerResults"),
                                 outputFolder=here("Results", db.name, "1_Cancers")) 

print(paste0("- Exported incidence and prevalence results: cancer populations"))
info(logger, "- Exported incidence and prevalence results: cancer populations")


## ===================== PLOTS FOR DENOMINATOR POP == 1 ===================== ##


print(paste0("- Plotting incidence and prevalence results: cancer populations denominator 1"))
info(logger, "- Plotting incidence and prevalence results: cancer populations denominator 1")

# POINT PREVALENCE FOR CANCERS IN QUARTERS FOR ALL AGE AND SEX STRATA

point_prev_qrs_plot <- study_results$prevalence_estimates %>% 
  filter(denominator_cohort_id == 1) %>% 
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "quarters") %>%
  mutate(outcome = case_when(outcome_cohort_name == "BreastCancer" ~ "Breast",
                             outcome_cohort_name == "ColorectalCancer" ~ "Colorectal",
                             outcome_cohort_name == "LungCancer" ~ "Lung",
                             outcome_cohort_name == "ProstateCancer" ~ "Prostate")) %>% 
  filter(!is.na(outcome)) %>%
  as.data.frame()

point_prev_qrs_plot <- 
  ggplot(point_prev_qrs_plot, aes(x = prevalence_start_date, y=prevalence,
                                  ymin = prevalence_95CI_lower,
                                  ymax = prevalence_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  ggtitle("Point Prevalence of Cancer in Quarters Before and After COVID-19 Lockdown") +
  labs(colour = "Cancer", x="Time" , y="Prevalence") +
   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

print(point_prev_qrs_plot)

# save the plot as pdf
analysis.name <- "cancers"
plotname <- paste0(analysis.name, db.name, "_point_prev_qrs")

pdf(here("Results", db.name , "1_Cancers", paste0(plotname, ".pdf")),
    width = 10, height = 8)
print(point_prev_qrs_plot, newpage = FALSE)
dev.off()


# Save the plot as jpg
ggsave(here("Results", db.name , "1_Cancers", paste0(plotname, ".jpg")), point_prev_qrs_plot, dpi=300, scale = 1, width = 12, height = 9)


# POINT PREVALENCE FOR CANCERS IN YEARS FOR ALL AGE AND SEX STRATA

point_prev_yrs_plot <- study_results$prevalence_estimates %>% 
  filter(denominator_cohort_id == 1) %>% 
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_name == "BreastCancer" ~ "Breast",
                             outcome_cohort_name == "ColorectalCancer" ~ "Colorectal",
                             outcome_cohort_name == "LungCancer" ~ "Lung",
                             outcome_cohort_name == "ProstateCancer" ~ "Prostate")) %>% 
  filter(!is.na(outcome)) %>%
  as.data.frame()

point_prev_yrs_plot <- 
  ggplot(point_prev_yrs_plot, aes(x = prevalence_start_date, y=prevalence,
                                  ymin = prevalence_95CI_lower,
                                  ymax = prevalence_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  ggtitle("Point Prevalence of Cancer in Years Before and After COVID-19 Lockdown") +
  labs(colour = "Cancer", x="Time" , y="Prevalence") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

print(point_prev_yrs_plot)

# save the plot as pdf
analysis.name <- "cancers"
plotname <- paste0(analysis.name, db.name, "_point_prev_yrs")

pdf(here("Results", db.name , "1_Cancers", paste0(plotname, ".pdf")),
    width = 10, height = 8)
print(point_prev_yrs_plot, newpage = FALSE)
dev.off()


# Save the plot as jpg
ggsave(here("Results", db.name , "1_Cancers", paste0(plotname, ".jpg")), point_prev_yrs_plot, dpi=300, scale = 1, width = 12, height = 9)



# POINT PREVALENCE IN MONTHS FOR ALL AGE AND SEX STRATA

point_prev_months_plot <- study_results$prevalence_estimates %>%
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "months") %>%
  mutate(outcome = case_when(outcome_cohort_name == "BreastCancer" ~ "Breast",
                             outcome_cohort_name == "ColorectalCancer" ~ "Colorectal",
                             outcome_cohort_name == "LungCancer" ~ "Lung",
                             outcome_cohort_name == "ProstateCancer" ~ "Prostate")) %>% 
  filter(!is.na(outcome)) %>%
  as.data.frame()

point_prev_months_plot <- 
  ggplot(point_prev_months_plot, aes(x = prevalence_start_date, y=prevalence,
                                     ymin = prevalence_95CI_lower,
                                     ymax = prevalence_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
   ggtitle("Point Prevalence of Cancer in Months Before and After COVID-19 Lockdown") +
  labs(colour = "Cancer", x="Time" , y="Prevalence") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

point_prev_months_plot

# save the plot as pdf
plotname <- paste0(analysis.name, db.name, "_point_prev_months")

pdf(here("Results", db.name , "1_Cancers", paste0(plotname, ".pdf")),
    width = 10, height = 8)
print(point_prev_months_plot, newpage = FALSE)
dev.off()


# Save the plot as jpg
ggsave(here("Results", db.name , "1_Cancers", paste0(plotname, ".jpg")), point_prev_months_plot, dpi=300, scale = 1, width = 12, height = 9)



# POINT PREVALENCE FOR 1st EVER CANCERS (by type) IN QUARTERS FOR ALL AGE AND SEX STRATA

point_prev_1st_qrs_plot <- study_results$prevalence_estimates %>% 
  filter(denominator_cohort_id == 1) %>% 
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "quarters") %>%
  mutate(outcome = case_when(outcome_cohort_name == "1stEverBreastCancer" ~ "Breast",
                             outcome_cohort_name == "1stEverColorectalCancer" ~ "Colorectal",
                             outcome_cohort_name == "1stEverLungCancer" ~ "Lung",
                             outcome_cohort_name == "1stEverProstateCancer" ~ "Prostate")) %>% 
  filter(!is.na(outcome)) %>%
  as.data.frame()

point_prev_1st_qrs_plot <- 
  ggplot(point_prev_1st_qrs_plot, aes(x = prevalence_start_date, y=prevalence,
                                  ymin = prevalence_95CI_lower,
                                  ymax = prevalence_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  ggtitle("Point Prevalence of 1st Ever Cancer (by type) in Quarters Before and After COVID-19 Lockdown") +
  labs(colour = "Cancer", x="Time" , y="Prevalence") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))  +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

print(point_prev_1st_qrs_plot)

# save the plot as pdf
analysis.name <- "cancers"
plotname <- paste0(analysis.name, db.name, "_point_prev_1st_qrs")

pdf(here("Results", db.name , "1_Cancers", paste0(plotname, ".pdf")),
    width = 10, height = 8)
print(point_prev_1st_qrs_plot, newpage = FALSE)
dev.off()


# Save the plot as jpg
ggsave(here("Results", db.name , "1_Cancers", paste0(plotname, ".jpg")), point_prev_1st_qrs_plot, dpi=300, scale = 1, width = 12, height = 9)

# POINT PREVALENCE FOR 1st EVER CANCERS (by type) IN YEARS FOR ALL AGE AND SEX STRATA

point_prev_1st_yrs_plot <- study_results$prevalence_estimates %>% 
  filter(denominator_cohort_id == 1) %>% 
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_name == "1stEverBreastCancer" ~ "Breast",
                             outcome_cohort_name == "1stEverColorectalCancer" ~ "Colorectal",
                             outcome_cohort_name == "1stEverLungCancer" ~ "Lung",
                             outcome_cohort_name == "1stEverProstateCancer" ~ "Prostate")) %>% 
  filter(!is.na(outcome)) %>%
  as.data.frame()

point_prev_1st_yrs_plot <- 
  ggplot(point_prev_1st_yrs_plot, aes(x = prevalence_start_date, y=prevalence,
                                      ymin = prevalence_95CI_lower,
                                      ymax = prevalence_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  ggtitle("Point Prevalence of 1st Ever Cancer (by type) in Years Before and After COVID-19 Lockdown") +
  labs(colour = "Cancer", x="Time" , y="Prevalence") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))  +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

print(point_prev_1st_yrs_plot)

# save the plot as pdf
analysis.name <- "cancers"
plotname <- paste0(analysis.name, db.name, "_point_prev_1st_yrs")

pdf(here("Results", db.name , "1_Cancers", paste0(plotname, ".pdf")),
    width = 10, height = 8)
print(point_prev_1st_yrs_plot, newpage = FALSE)
dev.off()


# Save the plot as jpg
ggsave(here("Results", db.name , "1_Cancers", paste0(plotname, ".jpg")), point_prev_1st_yrs_plot, dpi=300, scale = 1, width = 12, height = 9)



# POINT PREVALENCE 1st EVER CANCERS (by type) IN MONTHS FOR ALL AGE AND SEX STRATA

point_prev_1st_months_plot <- study_results$prevalence_estimates %>%
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "months") %>%
  mutate(outcome = case_when(outcome_cohort_name == "1stEverBreastCancer" ~ "Breast",
                             outcome_cohort_name == "1stEverColorectalCancer" ~ "Colorectal",
                             outcome_cohort_name == "1stEverLungCancer" ~ "Lung",
                             outcome_cohort_name == "1stEverProstateCancer" ~ "Prostate")) %>% 
  filter(!is.na(outcome)) %>%
  as.data.frame()

point_prev_1st_months_plot <- 
  ggplot(point_prev_1st_months_plot, aes(x = prevalence_start_date, y=prevalence,
                                     ymin = prevalence_95CI_lower,
                                     ymax = prevalence_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  ggtitle("Point Prevalence of 1st Ever Cancers (by type) in Months Before and After COVID-19 Lockdown") +
  labs(colour = "Cancer", x="Time" , y="Prevalence") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

point_prev_1st_months_plot

# save the plot as pdf
plotname <- paste0(analysis.name, db.name, "_point_prev_1st_months")

pdf(here("Results", db.name , "1_Cancers", paste0(plotname, ".pdf")),
    width = 10, height = 8)
print(point_prev_1st_months_plot, newpage = FALSE)
dev.off()


# Save the plot as jpg
ggsave(here("Results", db.name , "1_Cancers", paste0(plotname, ".jpg")), point_prev_1st_months_plot, dpi=300, scale = 1, width = 12, height = 9)




# INCIDENCE IN QUARTERS FOR ALL AGE AND SEX STRATA

inc_qrs_plot <- study_results$incidence_estimates %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_outcome_washout == 365) %>% 
  filter(analysis_interval == "quarters") %>%
  mutate(outcome = case_when(outcome_cohort_name == "BreastCancer" ~ "Breast",
                             outcome_cohort_name == "ColorectalCancer" ~ "Colorectal",
                             outcome_cohort_name == "LungCancer" ~ "Lung",
                             outcome_cohort_name == "ProstateCancer" ~ "Prostate")) %>% 
  filter(!is.na(outcome)) %>%
  as.data.frame()

inc_qrs_plot <- 
  ggplot(inc_qrs_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                                ymin = incidence_100000_pys_95CI_lower,
                                ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, 150)) +
  ggtitle("Incidence Rates of Cancer in Quarters Before and After COVID-19 Lockdown") +
  labs(colour = "Cancer", x="Time" , y="Incidence per 100000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

inc_qrs_plot

# save the plot as pdf
plotname <- paste0(analysis.name, db.name, "_inc_qrs")

pdf(here("Results", db.name,"1_Cancers",paste0(plotname, ".pdf")),
    width = 10, height = 8)
print(inc_qrs_plot, newpage = FALSE)
dev.off()


# Save the plot as jpg
ggsave(here("Results", db.name , "1_Cancers", paste0(plotname, ".jpg")), inc_qrs_plot, dpi=300, scale = 1, width = 12, height = 9)


# INCIDENCE IN YEARS FOR ALL AGE AND SEX STRATA

inc_yrs_plot <- study_results$incidence_estimates %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_outcome_washout == 365) %>% 
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_name == "BreastCancer" ~ "Breast",
                             outcome_cohort_name == "ColorectalCancer" ~ "Colorectal",
                             outcome_cohort_name == "LungCancer" ~ "Lung",
                             outcome_cohort_name == "ProstateCancer" ~ "Prostate")) %>% 
  filter(!is.na(outcome)) %>%
  as.data.frame()

inc_yrs_plot <- 
  ggplot(inc_yrs_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                           ymin = incidence_100000_pys_95CI_lower,
                           ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, 150)) +
  ggtitle("Incidence Rates of Cancer in Years Before and After COVID-19 Lockdown") +
  labs(colour = "Cancer", x="Time" , y="Incidence per 100000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

inc_yrs_plot

# save the plot as pdf
plotname <- paste0(analysis.name, db.name, "_inc_yrs")

pdf(here("Results", db.name,"1_Cancers",paste0(plotname, ".pdf")),
    width = 10, height = 8)
print(inc_qrs_plot, newpage = FALSE)
dev.off()


# Save the plot as jpg
ggsave(here("Results", db.name , "1_Cancers", paste0(plotname, ".jpg")), inc_yrs_plot, dpi=300, scale = 1, width = 12, height = 9)


# INCIDENCE IN MONTHS FOR ALL AGE AND SEX STRATA 

inc_months_plot <- study_results$incidence_estimates %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_outcome_washout == 365) %>% 
  filter(analysis_interval == "months") %>%
  mutate(outcome = case_when(outcome_cohort_name == "BreastCancer" ~ "Breast",
                             outcome_cohort_name == "ColorectalCancer" ~ "Colorectal",
                             outcome_cohort_name == "LungCancer" ~ "Lung",
                             outcome_cohort_name == "ProstateCancer" ~ "Prostate")) %>% 
  filter(!is.na(outcome)) %>%
  as.data.frame()

inc_months_plot <- 
  ggplot(inc_months_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                           ymin = incidence_100000_pys_95CI_lower,
                           ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, 150)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  ggtitle("Incidence Rates of Cancer in Months Before and After COVID-19 Lockdown") +
  labs(colour = "Cancer", x="Time" , y="Incidence per 100000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

inc_months_plot

# save the plot as pdf
plotname <- paste0(analysis.name, db.name, "_inc_months")

pdf(here("Results", db.name,"1_Cancers",paste0(plotname, ".pdf")),
    width = 10, height = 8)
print(inc_months_plot, newpage = FALSE)
dev.off()


# Save the plot as jpg
ggsave(here("Results", db.name , "1_Cancers", paste0(plotname, ".jpg")), inc_months_plot, dpi=300, scale = 1, width = 12, height = 9)



print(paste0("- Plots of incidence and prevalence results: cancer populations denominator 1 done"))
info(logger, "- Plots of incidence and prevalence results: cancer populations denominator 1 done")

# ========= PLOTS STRATIFIED BY AGE AND SEX ================================== #

print(paste0("- Plotting incidence and prevalence results: cancer populations stratified by age and sex"))
info(logger, "- Plotting incidence and prevalence results: cancer populations stratified by age and sex")

# POINT PREVALENCE IN QUARTERS STRATIFIED BY AGE AND SEX
point_prev_qrs_plot_s <- study_results$prevalence_estimates %>% 
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "quarters") %>%
  mutate(outcome = case_when(outcome_cohort_name == "BreastCancer" ~ "Breast",
                             outcome_cohort_name == "ColorectalCancer" ~ "Colorectal",
                             outcome_cohort_name == "LungCancer" ~ "Lung",
                             outcome_cohort_name == "ProstateCancer" ~ "Prostate")) %>% 
  filter(!is.na(outcome)) %>%
  filter(!denominator_age_group %in% c("0;19")) %>%
  as.data.frame()

point_prev_qrs_plot_s <- 
  ggplot(point_prev_qrs_plot_s, aes(x = prevalence_start_date, y=prevalence,
                                    ymin = prevalence_95CI_lower,
                                    ymax = prevalence_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  facet_grid(~denominator_age_group ~denominator_sex) +
  ggtitle("Point Prevalence of Cancer in Quarters Before and After COVID-19 Lockdown Stratified by Age and Sex") +
  labs(colour = "Cancer", x="Time" , y="Prevalence") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

point_prev_qrs_plot_s

# save the plot as pdf
plotname <- paste0(analysis.name, db.name, "_point_prev_qrs_strat")

pdf(here("Results", db.name,"1_Cancers",paste0(plotname, ".pdf")),
    width = 10, height = 8)
print(point_prev_qrs_plot_s, newpage = FALSE)
dev.off()


# Save the plot as jpg
ggsave(here("Results", db.name , "1_Cancers", paste0(plotname, ".jpg")), point_prev_qrs_plot_s, dpi=300, scale = 1, width = 12, height = 9)



# POINT PREVALENCE IN MONTHS STRATIFIED BY AGE AND SEX

point_prev_months_plot_s <- study_results$prevalence_estimates %>%  
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "months") %>%
  mutate(outcome = case_when(outcome_cohort_name == "BreastCancer" ~ "Breast",
                             outcome_cohort_name == "ColorectalCancer" ~ "Colorectal",
                             outcome_cohort_name == "LungCancer" ~ "Lung",
                             outcome_cohort_name == "ProstateCancer" ~ "Prostate")) %>% 
  filter(!is.na(outcome)) %>%
  filter(!denominator_age_group %in% c("0;19")) %>%
  as.data.frame()

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
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  facet_grid(~denominator_age_group ~denominator_sex) +
  ggtitle("Point Prevalence of Cancer in Months Before and After COVID-19 Lockdown Stratified by Age and Sex") +
  labs(colour = "Cancer", x="Time" , y="Prevalence") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

point_prev_months_plot_s

# save the plot as pdf
plotname <- paste0(analysis.name, db.name, "_point_prev_months_strat")

pdf(here("Results", db.name,"1_Cancers",paste0(plotname, ".pdf")),
    width = 10, height = 8)
print(point_prev_months_plot_s, newpage = FALSE)
dev.off()


# Save the plot as jpg
ggsave(here("Results", db.name , "1_Cancers", paste0(plotname, ".jpg")), point_prev_months_plot_s, dpi=300, scale = 1, width = 12, height = 9)



# POINT PREVALENCE of 1st EVER CANCERS IN QUARTERS STRATIFIED BY AGE AND SEX
point_prev_1st_qrs_plot_s <- study_results$prevalence_estimates %>% 
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "quarters") %>%
  mutate(outcome = case_when(outcome_cohort_name == "1stEverBreastCancer" ~ "Breast",
                             outcome_cohort_name == "1stEverColorectalCancer" ~ "Colorectal",
                             outcome_cohort_name == "1stEverLungCancer" ~ "Lung",
                             outcome_cohort_name == "1stEverProstateCancer" ~ "Prostate")) %>% 
  filter(!is.na(outcome)) %>%
  filter(!denominator_age_group %in% c("0;19")) %>%
  as.data.frame()

point_prev_1st_qrs_plot_s <- 
  ggplot(point_prev_1st_qrs_plot_s, aes(x = prevalence_start_date, y=prevalence,
                                    ymin = prevalence_95CI_lower,
                                    ymax = prevalence_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  facet_grid(~denominator_age_group ~denominator_sex) +
  ggtitle("Point Prevalence of 1st Ever Cancers (by type) in Quarters Before and After COVID-19 Lockdown Stratified by Age and Sex") +
  labs(colour = "Cancer", x="Time" , y="Prevalence") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

point_prev_1st_qrs_plot_s

# save the plot as pdf
plotname <- paste0(analysis.name, db.name, "_point_prev_1st_qrs_strat")

pdf(here("Results", db.name,"1_Cancers",paste0(plotname, ".pdf")),
    width = 10, height = 8)
print(point_prev_1st_qrs_plot_s, newpage = FALSE)
dev.off()


# Save the plot as jpg
ggsave(here("Results", db.name , "1_Cancers", paste0(plotname, ".jpg")), point_prev_1st_qrs_plot_s, dpi=300, scale = 1, width = 12, height = 9)



# POINT PREVALENCE of 1st EVER CANCERS IN MONTHS STRATIFIED BY AGE AND SEX

point_prev_1st_months_plot_s <- study_results$prevalence_estimates %>%  
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "months") %>%
  mutate(outcome = case_when(outcome_cohort_name == "1stEverBreastCancer" ~ "Breast",
                             outcome_cohort_name == "1stEverColorectalCancer" ~ "Colorectal",
                             outcome_cohort_name == "1stEverLungCancer" ~ "Lung",
                             outcome_cohort_name == "1stEverProstateCancer" ~ "Prostate")) %>% 
  filter(!is.na(outcome)) %>%
  filter(!denominator_age_group %in% c("0;19")) %>%
  as.data.frame()

point_prev_1st_months_plot_s <- 
  ggplot(point_prev_1st_months_plot_s, aes(x = prevalence_start_date, y=prevalence,
                                       ymin = prevalence_95CI_lower,
                                       ymax = prevalence_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  facet_grid(~denominator_age_group ~denominator_sex) +
  ggtitle("Point Prevalence of 1st Ever Cancer (by type) in Months Before and After COVID-19 Lockdown Stratified by Age and Sex") +
  labs(colour = "Cancer", x="Time" , y="Prevalence") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

point_prev_1st_months_plot_s

# save the plot as pdf
plotname <- paste0(analysis.name, db.name, "_point_prev_1st_months_strat")

pdf(here("Results", db.name,"1_Cancers",paste0(plotname, ".pdf")),
    width = 10, height = 8)
print(point_prev_1st_months_plot_s, newpage = FALSE)
dev.off()


# Save the plot as jpg
ggsave(here("Results", db.name , "1_Cancers", paste0(plotname, ".jpg")), point_prev_1st_months_plot_s, dpi=300, scale = 1, width = 12, height = 9)




# INCIDENCE IN QUARTERS STRATIFIED BY AGE AND SEX

inc_qrs_plot_s <- study_results$incidence_estimates %>%  
  filter(analysis_outcome_washout == 365) %>% 
  filter(analysis_interval == "quarters") %>%
  mutate(outcome = case_when(outcome_cohort_name == "BreastCancer" ~ "Breast",
                             outcome_cohort_name == "ColorectalCancer" ~ "Colorectal",
                             outcome_cohort_name == "LungCancer" ~ "Lung",
                             outcome_cohort_name == "ProstateCancer" ~ "Prostate")) %>% 
  filter(!is.na(outcome)) %>%
  filter(!denominator_age_group %in% c("0;19")) %>%
  as.data.frame()

inc_qrs_plot_s <- 
  ggplot(inc_qrs_plot_s, aes(x = incidence_start_date, y=incidence_100000_pys,
                           ymin = incidence_100000_pys_95CI_lower,
                           ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, NA)) +
  facet_grid(~denominator_age_group ~denominator_sex) +
    ggtitle("Incidence Rates of Cancer in Quarters before and after COVID-19 Lockdown Stratified by Age and Sex") +
  labs(colour = "Cancer", x="Time" , y="Incidence per 100000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

inc_qrs_plot_s

analysis.name <- "cancers"
# save the plot as pdf
plotname <- paste0(analysis.name, db.name, "_inc_qrs_strat")

pdf(here("Results", db.name,"1_Cancers",paste0(plotname, ".pdf")),
    width = 10, height = 8)
print(inc_qrs_plot_s, newpage = FALSE)
dev.off()


# Save the plot as jpg
ggsave(here("Results", db.name , "1_Cancers", paste0(plotname, ".jpg")), inc_qrs_plot_s, dpi=300, scale = 1, width = 12, height = 9)




# INCIDENCE IN MONTHS STRATIFIED BY AGE AND SEX 

inc_months_plot_s <- study_results$incidence_estimates %>%  
  filter(analysis_interval == "months") %>%
  filter(analysis_outcome_washout == 365) %>% 
  mutate(outcome = case_when(outcome_cohort_name == "BreastCancer" ~ "Breast",
                             outcome_cohort_name == "ColorectalCancer" ~ "Colorectal",
                             outcome_cohort_name == "LungCancer" ~ "Lung",
                             outcome_cohort_name == "ProstateCancer" ~ "Prostate")) %>% 
  filter(!is.na(outcome)) %>%
  filter(!denominator_age_group %in% c("0;19")) %>%
  as.data.frame()

inc_months_plot_s <- 
  ggplot(inc_months_plot_s, aes(x = incidence_start_date, y=incidence_100000_pys,
                              ymin = incidence_100000_pys_95CI_lower,
                              ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  facet_grid(~denominator_age_group ~denominator_sex) +
    ggtitle("Incidence Rates of Cancer in Months Before and After COVID-19 Lockdown Stratified by Age and Sex") +
  labs(colour = "Cancer", x="Time" , y="Incidence per 100000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

inc_months_plot_s

analysis.name <- "cancers"
# save the plot as pdf
plotname <- paste0(analysis.name, db.name, "_inc_months_strat")

pdf(here("Results", db.name,"1_Cancers",paste0(plotname, ".pdf")),
    width = 10, height = 8)
print(inc_months_plot_s, newpage = FALSE)
dev.off()


# Save the plot as jpg
ggsave(here("Results", db.name , "1_Cancers", paste0(plotname, ".jpg")), inc_months_plot_s, dpi=300, scale = 1, width = 12, height = 9)



print(paste0("- Plots of incidence and prevalence results: cancer populations stratified by age and sex done"))
info(logger, "- Plots of incidence and prevalence results: cancer populations stratified by age and sex done")

print(paste0("- Analysis of all cancers done"))
info(logger, "- Analysis of all cancers done")
