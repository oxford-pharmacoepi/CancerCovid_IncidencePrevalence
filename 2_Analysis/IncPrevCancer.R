# ============================================================================ #
#                         Incidence/Prevalence for                             #
#                 Breast, Colorectal, Lung and Prostate Cancer                 #
#                              Nicola Barclay                                  #
#                                18-01-2023                                    #
# ============================================================================ #



## ======= Compute the denominator population (\~7 minutes in CPRD GOLD) ==== ##

print(paste0("- Getting denominator: general population"))
info(logger, "- Getting denominator: general population")

cdm$denominator <- generateDenominatorCohortSet(
  cdm = cdm,
  startDate = as.Date("2017-01-01"),
  endDate = as.Date("2020-06-29"),
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

## =================== CALCULATE POINT PREVALENCE =========================== ##

print(paste0("- Getting point prevalence: cancer populations"))
info(logger, "- Getting point prevalence: cancer populations")

# years
point_prev_yrs <- estimatePointPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = outcome_table_name_1, 
  outcomeCohortId = outcome_cohorts_1$cohortId,
  outcomeCohortName = outcome_cohorts_1$cohortName,
  outcomeLookbackDays = 365,
  interval = "years",
  timePoint = "start",
  minCellCount = 5,
  verbose = FALSE
)

point_prev_yrs %>%
  glimpse()


# months
point_prev_months <- estimatePointPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = outcome_table_name_1, 
  outcomeCohortId = outcome_cohorts_1$cohortId,
  outcomeCohortName = outcome_cohorts_1$cohortName,
  outcomeLookbackDays = 365,
  interval = "months",
  timePoint = "start",
  minCellCount = 5,
  verbose = FALSE
)

point_prev_months %>%
  glimpse()

print(paste0("- Got point prevalence: cancer populations"))
info(logger, "- Got point prevalence: cancer populations")


## =================== CALCULATE PERIOD PREVALENCE =========================== ##

print(paste0("- Getting period prevalence: cancer populations"))
info(logger, "- Getting period prevalence: cancer populations")

# years 
per_prev_yrs <- estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = outcome_table_name_1, 
  outcomeCohortId = outcome_cohorts_1$cohortId,
  outcomeCohortName = outcome_cohorts_1$cohortName,
  outcomeLookbackDays = 365,
  interval = "years",
  completeDatabaseIntervals = FALSE,
  fullContribution= FALSE,
  minCellCount = 5
)

per_prev_yrs %>%
  glimpse()



# months
per_prev_months <- estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = outcome_table_name_1, 
  outcomeCohortId = outcome_cohorts_1$cohortId,
  outcomeCohortName = outcome_cohorts_1$cohortName,
  outcomeLookbackDays = 365,
  interval = "Months",
  completeDatabaseIntervals = FALSE,
  fullContribution= FALSE,
  minCellCount = 5
)

per_prev_months %>%
  glimpse()


print(paste0("- Got period prevalence: cancer populations"))
info(logger, "- Got period prevalence: cancer populations")

save(point_prev_yrs, point_prev_months, per_prev_months, per_prev_yrs, file = here("Results", "prev.RData"))

## ======================== CALCULATE INCIDENCE ============================= ##

print(paste0("- Getting incidence: cancer populations"))
info(logger, "- Getting incidence: cancer populations")

# years
inc_yrs <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = outcome_table_name_1, 
  outcomeCohortId = outcome_cohorts_1$cohortId,
  outcomeCohortName = outcome_cohorts_1$cohortName,
  interval = "years",
  completeDatabaseIntervals = FALSE,
  outcomeWashout = c(0, NA, 180),
  repeatedEvents = TRUE,
  minCellCount = 5,
  verbose = FALSE
)

inc_yrs %>%
  glimpse()


# months
inc_months <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = outcome_table_name_1, 
  outcomeCohortId = outcome_cohorts_1_1$cohortId,
  outcomeCohortName = outcome_cohorts_1_1$cohortName,
  interval = "months",
  completeDatabaseIntervals = FALSE,
  outcomeWashout = NA,
  repeatedEvents = TRUE,
  minCellCount = 5,
  verbose = FALSE
)

inc_months %>%
  glimpse()

#save(inc_yrs, file = here("Results", "inc.RData"))
save(inc_yrs, inc_months, file = here("Results", "inc.RData"))


print(paste0("- Got incidence: cancer populations"))
info(logger, "- Got incidence: cancer populations")


## ======== GATHER ALL INCIDENCE AND PREVALENCE RESULTS ===================== ##

print(paste0("- Gathering incidence and prevalence results: cancer populations"))
info(logger, "- Gathering incidence and prevalence results: cancer populations")

study_results <- gatherIncidencePrevalenceResults(cdm=cdm,
                                                  resultList=list(point_prev_yrs, point_prev_months, per_prev_yrs, per_prev_months, inc_yrs, inc_months),
                                                  databaseName = db.name)


# save study results as a separate R.data file
save(study_results, file = here("Results", db.name, "StudyResults_cancers.RData"))

print(paste0("- Got incidence and prevalence results: cancer populations"))
info(logger, "- Got incidence and prevalence results: cancer populations")


## ======== EXPORT ALL INCIDENCE AND PREVALENCE RESULTS ===================== ##

print(paste0("- Exporting incidence and prevalence results: cancer populations"))
info(logger, "- Exporting incidence and Prevalence results: cancer populations")

exportIncidencePrevalenceResults(result=study_results, 
                                 zipName=paste0(db.name, "IncPrevCancerResults"),
                                 outputFolder=here("Results", db.name)) 

print(paste0("- Exported incidence and prevalence results: cancer populations"))
info(logger, "- Exported incidence and prevalence results: cancer populations")


## ===================== PLOTS FOR DENOMINATOR POP == 1 ===================== ##


print(paste0("- Plotting incidence and prevalence results: cancer populations"))
info(logger, "- Plotting incidence and prevalence results: cancer populations")

# POINT PREVALENCE IN YEARS FOR ALL AGE AND SEX STRATA

point_prev_yrs_plot <- study_results$prevalence_estimates %>% 
  filter(denominator_cohort_id == 1) %>% 
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "years") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "BreastCancer" ~ "Breast")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ColorectalCancer" ~ "Colorectal")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "LungCancer" ~ "Lung")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ProstateCancer" ~ "Prostate")) %>%
  mutate(time = format(prevalence_start_date, format="%Y")) %>%
  as.data.frame()

point_prev_yrs_plot <- 
  ggplot(point_prev_yrs_plot, aes(x = time, y=prevalence,
                                  ymin = prevalence_95CI_lower,
                                  ymax = prevalence_95CI_upper, color=outcome_cohort_name, group=outcome_cohort_name)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  ggtitle("Point Prevalence of Cancer in years before and after COVID-19 Lockdown") +
  labs(colour = "Cancer") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

print(point_prev_yrs_plot)

# save the plot as pdf
plotname <- paste0("point_prev_yrs", db.name, ".pdf")

pdf(here("Results", db.name ,plotname),
    width = 15, height = 5)
print(point_prev_yrs_plot, newpage = FALSE)
dev.off()

# save the plot as jpg
ggsave("Figures/FigureS3.rev_jpg.jpg",sup3,dpi=300)

# POINT PREVALENCE IN MONTHS FOR ALL AGE AND SEX STRATA

point_prev_months_plot <- study_results$prevalence_estimates %>%  # need to amend this bit of code to select the estimates relating to point_prev_yrs
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "months") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "BreastCancer" ~ "Breast")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ColorectalCancer" ~ "Colorectal")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "LungCancer" ~ "Lung")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ProstateCancer" ~ "Prostate")) %>%
  mutate(time = format(prevalence_start_date, format="%b %Y")) %>%
  as.data.frame()

point_prev_months_plot <- 
  ggplot(point_prev_months_plot, aes(x = time, y=prevalence,
                                     ymin = prevalence_95CI_lower,
                                     ymax = prevalence_95CI_upper, color=outcome_cohort_name, group=outcome_cohort_name)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  ggtitle("Point Prevalence of Cancer in months before and after COVID-19 Lockdown") +
  labs(colour = "Cancer") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-04-01"))),linetype=2, color="black")

point_prev_months_plot

# save the plot as pdf
plotname <- paste0("point_prev_months",".pdf")

pdf(here("Results", "Plots",plotname),
    width = 15, height = 5)
print(point_prev_months_plot, newpage = FALSE)
dev.off()

# save the plot as jpg
ggsave("Figures/FigureS3.rev_jpg.jpg",sup3,dpi=300)

# PERID PREVALENCE IN YEARS FOR ALL AGE AND SEX STRATA

per_prev_yrs_plot <- study_results$prevalence_estimates %>%  # need to amend this bit of code to select the estimates relating to per_prev_yrs
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_type == "period") %>% 
  filter(analysis_interval == "years") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "BreastCancer" ~ "Breast")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ColorectalCancer" ~ "Colorectal")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "LungCancer" ~ "Lung")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ProstateCancer" ~ "Prostate")) %>%
  mutate(time = format(prevalence_start_date, format="%Y")) %>%
  as.data.frame()

per_prev_yrs_plot <- 
  ggplot(per_prev_yrs_plot, aes(x = time, y=prevalence,
                                ymin = prevalence_95CI_lower,
                                ymax = prevalence_95CI_upper, color=outcome_cohort_name, group=outcome_cohort_name)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  ggtitle("Period Prevalence of Cancer in years before and after COVID-19 Lockdown") +
  labs(colour = "Cancer") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-04-01"))),linetype=2, color="black")

per_prev_yrs_plot

# save the plot as pdf
plotname <- paste0("per_prev_yrs",".pdf")

pdf(here("4_IncPrev", "Plots",plotname),
    width = 15, height = 5)
print(per_prev_yrs_plot, newpage = FALSE)
dev.off()

# PERIOD PREVALENCE IN MONTHS FOR ALL AGE AND SEX STRATA

per_prev_months_plot <- study_results$prevalence_estimates %>%  # need to amend this bit of code to select the estimates relating to per_prev_yrs
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_type == "period") %>% 
  filter(analysis_interval == "months") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "BreastCancer" ~ "Breast")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ColorectalCancer" ~ "Colorectal")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "LungCancer" ~ "Lung")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ProstateCancer" ~ "Prostate")) %>%
  mutate(time = format(prevalence_start_date, format="%b %Y")) %>%
  as.data.frame()

per_prev_months_plot <- 
  ggplot(per_prev_months_plot, aes(x = time, y=prevalence,
                                   ymin = prevalence_95CI_lower,
                                   ymax = prevalence_95CI_upper, color=outcome_cohort_name, group=outcome_cohort_name)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  ggtitle("Period Prevalence of Cancer in months before and after COVID-19 Lockdown") +
  labs(colour = "Cancer") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-04-01"))),linetype=2, color="black")

per_prev_months_plot


# save the plot as pdf
plotname <- paste0("per_prev_months",".pdf")

pdf(here("4_IncPrev", "Plots",plotname),
    width = 15, height = 5)
print(per_prev_months_plot, newpage = FALSE)
dev.off()

# INCIDENCE IN YEARS FOR ALL AGE AND SEX STRATA

inc_yrs_plot <- study_results$incidence_estimates %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_interval == "years") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "BreastCancer" ~ "Breast")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ColorectalCancer" ~ "Colorectal")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "LungCancer" ~ "Lung")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ProstateCancer" ~ "Prostate")) %>%
  mutate(time = format(incidence_start_date, format="%Y")) %>%
  as.data.frame()


inc_yrs_plot %>%
  ggplot(aes(x = time, y = ir_100000_pys,
             ymin = ir_100000_pys_95CI_lower,
             ymax = ir_100000_pys_95CI_upper, color=outcome_cohort_name, group=outcome_cohort_name)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, NA)) +
  ggtitle("Incidence Rates of Cancer in years before and after COVID-19 Lockdown") +
  labs(colour = "Cancer") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-04-01"))),linetype=2, color="black")

inc_yrs_plot

# save the plot as pdf
plotname <- paste0("inc_yrs",".pdf")

pdf(here("4_IncPrev", "Plots",plotname),
    width = 15, height = 5)
print(inc_yrs_plot, newpage = FALSE)
dev.off()

# INCIDENCE IN MONTHS FOR ALL AGE AND SEX STRATA

inc_months_plot <- study_results$incidence_estimates %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_interval == "months") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "BreastCancer" ~ "Breast")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ColorectalCancer" ~ "Colorectal")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "LungCancer" ~ "Lung")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ProstateCancer" ~ "Prostate")) %>%
  mutate(time = format(incidence_start_date, format="%b %Y")) %>%
  as.data.frame()

inc_months_plot %>%
  ggplot(aes(x = incidence_start_date, y = ir_100000_pys,
             ymin = ir_100000_pys_95CI_lower,
             ymax = ir_100000_pys_95CI_upper, color=outcome_cohort_name, group=outcome_cohort_name)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, NA)) +
  ggtitle("Incidence Rates of Cancer in months before and after COVID-19 Lockdown") +
  labs(colour = "Cancer") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-04-01"))),linetype=2, color="black")

inc_months_plot

# save the plot as pdf
plotname <- paste0("inc_months",".pdf")

pdf(here("4_IncPrev", "Plots",plotname),
    width = 15, height = 5)
print(inc_months_plot, newpage = FALSE)
dev.off()


# ========= PLOTS STRATIFIED BY AGE AND SEX ================================== #

# POINT PREVALENCE IN YEARS STRATIFIED BY AGE AND SEX
point_prev_yrs_plot_s <- study_results$prevalence_estimates %>% 
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "years") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "BreastCancer" ~ "Breast")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ColorectalCancer" ~ "Colorectal")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "LungCancer" ~ "Lung")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ProstateCancer" ~ "Prostate")) %>%
  mutate(time = format(incidence_start_date, format="%Y")) %>%
  as.data.frame()

point_prev_yrs_plot_s <- as.data.frame(point_prev_yrs_plot_s)

#my_title <- "Point Prevalence of Cancer in years before and after COVID-19 Lockdown Stratified by Age and Sex"
point_prev_yrs_plot_s <- 
  ggplot(point_prev_yrs_plot_s, aes(x = time, y=prevalence,
                                    ymin = prevalence_95CI_lower,
                                    ymax = prevalence_95CI_upper, color=outcome_cohort_name, group=outcome_cohort_name)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  facet_grid(~denominator_age_group ~denominator_sex) +
  ggtitle("Point Prevalence of Cancer in years before and after COVID-19 Lockdown Stratified by Age and Sex") +
  #labs(title = str_wrap(my_title, 60)) +
  labs(colour = "Cancer") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-04-01"))),linetype=2, color="black")

point_prev_yrs_plot_s

# save the plot as pdf
plotname <- paste0("point_prev_yrs_s",".pdf")

pdf(here("4_IncPrev", "Plots",plotname),
    width = 15, height = 15)
print(point_prev_yrs_plot_s, newpage = FALSE)
dev.off()



# POINT PREVALENCE IN MONTHS STRATIFIED BY AGE AND SEX

point_prev_months_plot_s <- study_results$prevalence_estimates %>%  # need to amend this bit of code to select the estimates relating to point_prev_yrs
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "months") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "BreastCancer" ~ "Breast")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ColorectalCancer" ~ "Colorectal")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "LungCancer" ~ "Lung")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ProstateCancer" ~ "Prostate")) %>%
  mutate(time = format(incidence_start_date, format="%b %Y")) %>%
  as.data.frame()

point_prev_months_plot_s <- 
  ggplot(point_prev_months_plot_s, aes(x = time, y=prevalence,
                                       ymin = prevalence_95CI_lower,
                                       ymax = prevalence_95CI_upper, color=outcome_cohort_name, group=outcome_cohort_name)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  facet_grid(~denominator_age_group ~denominator_sex) +
  ggtitle("Point Prevalence of Cancer in months before and after COVID-19 Lockdown Stratified by Age and Sex") +
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
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "BreastCancer" ~ "Breast")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ColorectalCancer" ~ "Colorectal")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "LungCancer" ~ "Lung")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ProstateCancer" ~ "Prostate")) %>%
  mutate(time = format(incidence_start_date, format="%Y")) %>%
  as.data.frame()

per_prev_yrs_plot_s <- 
  ggplot(per_prev_yrs_plot_s, aes(x = time, y=prevalence,
                                  ymin = prevalence_95CI_lower,
                                  ymax = prevalence_95CI_upper, color=outcome_cohort_name, group=outcome_cohort_name)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  facet_grid(~denominator_age_group ~denominator_sex) +
  ggtitle("Period Prevalence of Cancer in years before and after COVID-19 Lockdown Stratified by Age and Sex") +
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
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "BreastCancer" ~ "Breast")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ColorectalCancer" ~ "Colorectal")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "LungCancer" ~ "Lung")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ProstateCancer" ~ "Prostate")) %>%
  mutate(time = format(incidence_start_date, format="%b %Y")) %>%
  as.data.frame()

per_prev_months_plot_s <- 
  ggplot(per_prev_months_plot_s, aes(x = time, y=prevalence,
                                     ymin = prevalence_95CI_lower,
                                     ymax = prevalence_95CI_upper, color=outcome_cohort_name, group=outcome_cohort_name)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  facet_grid(~denominator_age_group ~denominator_sex) +
  ggtitle("Period Prevalence of Cancer in months before and after COVID-19 Lockdown Stratified by Age and Sex") +
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
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "BreastCancer" ~ "Breast")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ColorectalCancer" ~ "Colorectal")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "LungCancer" ~ "Lung")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ProstateCancer" ~ "Prostate")) %>%
  mutate(time = format(incidence_start_date, format="%Y")) %>%
  as.data.frame()


inc_yrs_plot_s %>%
  ggplot(aes(x = time, y = incidence_100000_pys,
             ymin = incidence_100000_pys_95CI_lower,
             ymax = incidence_100000_pys_95CI_upper, color=outcome_cohort_name, group=outcome_cohort_name)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, NA)) +
  facet_grid(~denominator_age_group ~denominator_sex) +
  ggtitle("Incidence Rates of Cancer in years before and after COVID-19 Lockdown Stratified by Age and Sex") +
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
  filter(analysis_interval == "months") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "BreastCancer" ~ "Breast")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ColorectalCancer" ~ "Colorectal")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "LungCancer" ~ "Lung")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ProstateCancer" ~ "Prostate")) %>%
  mutate(time = format(incidence_start_date, format="%b %Y")) %>%
  as.data.frame()

inc_months_plot_s %>%
  ggplot(aes(x = time, y = incidence_100000_pys,
             ymin = incidence_100000_pys_95CI_lower,
             ymax = incidence_100000_pys_95CI_upper, color=outcome_cohort_name, group=outcome_cohort_name)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, NA)) +
  facet_grid(~denominator_age_group ~denominator_sex) +
  ggtitle("Incidence Rates of Cancer in months before and after COVID-19 Lockdown Stratified by Age and Sex") +
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



