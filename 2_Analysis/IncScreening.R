# ============================================================================ #
#                               Incidence for                                  #
#                        Cancer related Screening Tests                        #
#                              Nicola Barclay                                  #
#                                29-06-2023                                    #
# ============================================================================ #

print(paste0("- 4. Incidence of Screening tests"))
info(logger, "- 4. Incidence of Screening tests")

## ======= Compute the denominator population (\~7 minutes in CPRD GOLD) ==== ##

print(paste0("- Getting denominator: general population"))
info(logger, "- Getting denominator: general population")

cdm <- generateDenominatorCohortSet(
  cdm = cdm, name = "denominator",
  cohortDateRange = as.Date(c("2017-01-01","2022-01-01")),
  ageGroup = list(c(0,150), c(0,19), c(20,39), c(40,59), c(60,79), c(80,150)),
  sex = c("Both", "Male", "Female"),
  daysPriorHistory = 365,
  temporary = TRUE
)

cdm$denominator %>% glimpse()

cdm$denominator %>% head()

cohortSet(cdm$denominator)
cohortCount(cdm$denominator)

print(paste0("- Got denominator: general population"))
info(logger, "- Got denominator: general population")


## ======================== CALCULATE INCIDENCE ============================= ##

print(paste0("- Getting incidence: Screening tests"))
info(logger, "- Getting incidence: Screening tests")


inc_screen <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = outcome_table_name_3, 
  outcomeCohortId = NULL,
  interval = c("months", "years"),
  completeDatabaseIntervals = FALSE,
  outcomeWashout = 30,
  repeatedEvents = TRUE,
  minCellCount = 5,
  temporary=TRUE,
  )


inc_screen %>%
  glimpse()



save(inc_screen, file = here("Results", db.name, "2_ScreeningTests", "inc_screen.RData"))

  
print(paste0("- Got incidence: Screening tests"))
info(logger, "- Got incidence: Screening tests")



# UPDATED CODE FOR EXPORT ######
exportIncidencePrevalenceResults(resultList = list("incidence" = inc_screen),
                                 zipName = "IncScreeningTestsResults_updated",
                                 outputFolder = here::here("Results", db.name, "2_ScreeningTests"))



## ===================== PLOTS FOR DENOMINATOR POP == 1 ===================== ##



print(paste0("- Plotting incidence Screening tests denominator 1"))
info(logger, "- Plotting incidence Screening tests denominator 1")


# INCIDENCE IN YEARS FOR ALL AGE AND SEX STRATA

# BREAST CANCER SCREENING AND DIAGNOSTIC TESTS IN YEARS

breast_inc_yrs_plot <- inc_screen %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_interval == "years") %>%
  filter(outcome_cohort_name %in% c("BiopsyOfBreast", "BreastCancerReferrals","ExcisionOfBreast","Mammograms","SeenBreastClinic","SeenBreastSurgeon")) %>%
  mutate(outcome = case_when(outcome_cohort_name == "BiopsyOfBreast" ~ "Biopsy of Breast",
                             outcome_cohort_name == "BreastCancerReferrals" ~ "Breast Cancer Referrals",
                             outcome_cohort_name == "ExcisionOfBreast" ~ "Excision Of Breast",
                             outcome_cohort_name == "Mammograms" ~ "Mammograms",
                             outcome_cohort_name == "SeenBreastClinic" ~ "Seen in Breast Clinic",
                             outcome_cohort_name == "SeenBreastSurgeon" ~ "Seen Breast Surgeon")) %>%
  as.data.frame()

breast_inc_yrs_plot <- 
  ggplot(breast_inc_yrs_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                                ymin = incidence_100000_pys_95CI_lower,
                                ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, NA)) +
  ggtitle("Incidence Rates of Breast Cancer Screening/ Diagnostic Tests/ Referrals in Years Before and After COVID-19 Lockdown") +
  labs(colour = "Screening/ Diagnostic Test", x="Time" , y="Incidence per 100,000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

breast_inc_yrs_plot

# save the plot as pdf
analysis.name <- "Screening"
plotname <- paste0(analysis.name, db.name, "breast_inc_yrs")

pdf(here("Results", db.name,"4_ScreeningTests",paste0(plotname, ".pdf")),
    width = 10, height = 8)
print(breast_inc_yrs_plot, newpage = FALSE)
dev.off()

# Save the plot as jpg
ggsave(here("Results", db.name , "2_ScreeningTests", paste0(plotname, ".jpg")), breast_inc_yrs_plot, dpi=600, scale = 1, width = 12, height = 9)


# BREAST CANCER SCREENING AND DIAGNOSTIC TESTS IN MONTHS

breast_inc_months_plot <- inc_screen %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_interval == "months") %>%
  filter(outcome_cohort_name %in% c("BiopsyOfBreast", "BreastCancerReferrals","ExcisionOfBreast","Mammograms","SeenBreastClinic","SeenBreastSurgeon")) %>%
  mutate(outcome = case_when(outcome_cohort_name == "BiopsyOfBreast" ~ "Biopsy of Breast",
                             outcome_cohort_name == "BreastCancerReferrals" ~ "Breast Cancer Referrals",
                             outcome_cohort_name == "ExcisionOfBreast" ~ "Excision Of Breast",
                             outcome_cohort_name == "Mammograms" ~ "Mammograms",
                             outcome_cohort_name == "SeenBreastClinic" ~ "Seen in Breast Clinic",
                             outcome_cohort_name == "SeenBreastSurgeon" ~ "Seen Breast Surgeon")) %>%
  as.data.frame()

breast_inc_months_plot <- 
  ggplot(breast_inc_months_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                                  ymin = incidence_100000_pys_95CI_lower,
                                  ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, NA)) +
  ggtitle("Incidence Rates of Breast Cancer Screening/ Diagnostic Tests/ Referrals in Months Before and After COVID-19 Lockdown") +
  labs(colour = "Screening/ Diagnostic Test", x="Time" , y="Incidence per 100,000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

breast_inc_months_plot

# save the plot as pdf
analysis.name <- "Screening"
plotname <- paste0(analysis.name, db.name, "breast_inc_months")

pdf(here("Results", db.name,"4_ScreeningTests",paste0(plotname, ".pdf")),
    width = 10, height = 8)
print(breast_inc_months_plot, newpage = FALSE)
dev.off()

# Save the plot as jpg
ggsave(here("Results", db.name , "2_ScreeningTests", paste0(plotname, ".jpg")), breast_inc_months_plot, dpi=600, scale = 1, width = 12, height = 9)


# INCIDENCE IN YEARS FOR ALL AGE AND SEX STRATA

# COLORECTAL CANCER SCREENING AND DIAGNOSTIC TESTS IN YEARS

colorectal_inc_yrs_plot <- inc_screen %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_interval == "years") %>%
  filter(outcome_cohort_name %in% c("BowelCancerScreeningProg", "Colonoscopy","QuantitativeFaecalImmunochemicalTests","ColorectalCancerReferrals", "Sigmoidoscopy")) %>%
  mutate(outcome = case_when(outcome_cohort_name == "BowelCancerScreeningProg" ~ "Bowel Cancer Screening Prog",
                             outcome_cohort_name == "Colonoscopy" ~ "Colonoscopy",
                             outcome_cohort_name == "QuantitativeFaecalImmunochemicalTests" ~ "Quantitative Faecal Immunochemical Tests",
                             outcome_cohort_name == "ColorectalCancerReferrals" ~ "Colorectal Cancer Referrals",
                             outcome_cohort_name == "Sigmoidoscopy" ~ "Sigmoidoscopy")) %>% 
  as.data.frame()

colorectal_inc_yrs_plot <- 
  ggplot(colorectal_inc_yrs_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                                  ymin = incidence_100000_pys_95CI_lower,
                                  ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, NA)) +
  ggtitle("Incidence Rates of Colorectal Cancer Screening/ Diagnostic Tests/ Referrals in Years Before and After COVID-19 Lockdown") +
  labs(colour = "Screening/ Diagnostic Test", x="Time" , y="Incidence per 100,000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

colorectal_inc_yrs_plot

# save the plot as pdf
analysis.name <- "Screening"
plotname <- paste0(analysis.name, db.name, "colorectal_inc_yrs")

pdf(here("Results", db.name,"4_ScreeningTests",paste0(plotname, ".pdf")),
    width = 10, height = 8)
print(colorectal_inc_yrs_plot, newpage = FALSE)
dev.off()

# Save the plot as jpg
ggsave(here("Results", db.name , "2_ScreeningTests", paste0(plotname, ".jpg")), colorectal_inc_yrs_plot, dpi=600, scale = 1, width = 12, height = 9)


# COLORECTAL CANCER SCREENING AND DIAGNOSTIC TESTS IN MONTHS

colorectal_inc_months_plot <- inc_screen %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_interval == "months") %>%
  filter(outcome_cohort_name %in% c("BowelCancerScreeningProg", "Colonoscopy","QuantitativeFaecalImmunochemicalTests","ColorectalCancerReferrals", "Sigmoidoscopy")) %>%
  mutate(outcome = case_when(outcome_cohort_name == "BowelCancerScreeningProg" ~ "Bowel Cancer Screening Prog",
                             outcome_cohort_name == "Colonoscopy" ~ "Colonoscopy",
                             outcome_cohort_name == "QuantitativeFaecalImmunochemicalTests" ~ "Quantitative Faecal Immunochemical Tests",
                             outcome_cohort_name == "ColorectalCancerReferrals" ~ "Colorectal Cancer Referrals",
                             outcome_cohort_name == "Sigmoidoscopy" ~ "Sigmoidoscopy")) %>% 
  as.data.frame()

colorectal_inc_months_plot <- 
  ggplot(colorectal_inc_months_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                                      ymin = incidence_100000_pys_95CI_lower,
                                      ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, NA)) +
  ggtitle("Incidence Rates of Colorectal Cancer Screening/ Diagnostic Tests/ Referrals in Months Before and After COVID-19 Lockdown") +
  labs(colour = "Screening/ Diagnostic Test", x="Time" , y="Incidence per 100,000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

colorectal_inc_months_plot

# save the plot as pdf
analysis.name <- "Screening"
plotname <- paste0(analysis.name, db.name, "colorectal_inc_months")

pdf(here("Results", db.name,"4_ScreeningTests",paste0(plotname, ".pdf")),
    width = 10, height = 8)
print(colorectal_inc_months_plot, newpage = FALSE)
dev.off()

# Save the plot as jpg
ggsave(here("Results", db.name , "2_ScreeningTests", paste0(plotname, ".jpg")), colorectal_inc_months_plot, dpi=600, scale = 1, width = 12, height = 9)




# INCIDENCE IN YEARS FOR ALL AGE AND SEX STRATA

# LUNG CANCER SCREENING AND DIAGNOSTIC TESTS IN YEARS

lung_inc_yrs_plot <- inc_screen %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_interval == "years") %>%
  filter(outcome_cohort_name %in% c("Bronchoscopy", "DiagnosticProceduresOfChest","LungCancerReferrals")) %>%
  mutate(outcome = case_when(outcome_cohort_name == "Bronchoscopy" ~ "Bronchoscopy",
                             outcome_cohort_name == "DiagnosticProceduresOfChest" ~ "Diagnostic Procedures Of Chest",
                             outcome_cohort_name == "LungCancerReferrals" ~ "Lung Cancer Referrals")) %>% 
  as.data.frame()

lung_inc_yrs_plot <- 
  ggplot(lung_inc_yrs_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                                      ymin = incidence_100000_pys_95CI_lower,
                                      ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, NA)) +
  ggtitle("Incidence Rates of Lung Cancer Screening/ Diagnostic Tests/ Referrals in Years Before and After COVID-19 Lockdown") +
  labs(colour = "Screening/ Diagnostic Test", x="Time" , y="Incidence per 100,000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

lung_inc_yrs_plot

# save the plot as pdf
analysis.name <- "Screening"
plotname <- paste0(analysis.name, db.name, "lung_inc_yrs")

pdf(here("Results", db.name,"4_ScreeningTests",paste0(plotname, ".pdf")),
    width = 10, height = 8)
print(lung_inc_yrs_plot, newpage = FALSE)
dev.off()

# Save the plot as jpg
ggsave(here("Results", db.name , "2_ScreeningTests", paste0(plotname, ".jpg")), lung_inc_yrs_plot, dpi=600, scale = 1, width = 12, height = 9)


# LUNG CANCER SCREENING AND DIAGNOSTIC TESTS IN MONTHS

lung_inc_months_plot <- inc_screen %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_interval == "months") %>%
  filter(outcome_cohort_name %in% c("Bronchoscopy", "DiagnosticProceduresOfChest","LungCancerReferrals")) %>%
  mutate(outcome = case_when(outcome_cohort_name == "Bronchoscopy" ~ "Bronchoscopy",
                             outcome_cohort_name == "DiagnosticProceduresOfChest" ~ "Diagnostic Procedures Of Chest",
                             outcome_cohort_name == "LungCancerReferrals" ~ "Lung Cancer Referrals")) %>% 
  as.data.frame()

lung_inc_months_plot <- 
  ggplot(lung_inc_months_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                                         ymin = incidence_100000_pys_95CI_lower,
                                         ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, NA)) +
  ggtitle("Incidence Rates of Lung Cancer Screening/ Diagnostic Tests/ Referrals in Months Before and After COVID-19 Lockdown") +
  labs(colour = "Screening/ Diagnostic Test", x="Time" , y="Incidence per 100,000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

lung_inc_months_plot

# save the plot as pdf
analysis.name <- "Screening"
plotname <- paste0(analysis.name, db.name, "lung_inc_months")

pdf(here("Results", db.name,"4_ScreeningTests",paste0(plotname, ".pdf")),
    width = 10, height = 8)
print(lung_inc_months_plot, newpage = FALSE)
dev.off()

# Save the plot as jpg
ggsave(here("Results", db.name , "2_ScreeningTests", paste0(plotname, ".jpg")), lung_inc_months_plot, dpi=600, scale = 1, width = 12, height = 9)



# INCIDENCE IN YEARS FOR ALL AGE AND SEX STRATA

# PROSTATE CANCER SCREENING AND DIAGNOSTIC TESTS IN YEARS

prostate_inc_yrs_plot <- inc_screen %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_interval == "years") %>%
  filter(outcome_cohort_name %in% c("BiopsyOfProstate", "ProstateSpecificAntigenTest")) %>%
  mutate(outcome = case_when(outcome_cohort_name == "BiopsyOfProstate" ~ "Biopsy Of Prostate",
                             outcome_cohort_name == "ProstateSpecificAntigenTest" ~ "Prostate Specific Antigen Test")) %>% 
  as.data.frame()

prostate_inc_yrs_plot <- 
  ggplot(prostate_inc_yrs_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                                ymin = incidence_100000_pys_95CI_lower,
                                ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, NA)) +
  ggtitle("Incidence Rates of Prostate Cancer Screening/ Diagnostic Tests/ Referrals in Years Before and After COVID-19 Lockdown") +
  labs(colour = "Screening/ Diagnostic Test", x="Time" , y="Incidence per 100,000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

prostate_inc_yrs_plot

# save the plot as pdf
analysis.name <- "Screening"
plotname <- paste0(analysis.name, db.name, "prostate_inc_yrs")

pdf(here("Results", db.name,"4_ScreeningTests",paste0(plotname, ".pdf")),
    width = 10, height = 8)
print(lung_inc_yrs_plot, newpage = FALSE)
dev.off()

# Save the plot as jpg
ggsave(here("Results", db.name , "2_ScreeningTests", paste0(plotname, ".jpg")), prostate_inc_yrs_plot, dpi=600, scale = 1, width = 12, height = 9)


# PROSTATE CANCER SCREENING AND DIAGNOSTIC TESTS IN MONTHS

prostate_inc_months_plot <- inc_screen %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_interval == "months") %>%
  filter(outcome_cohort_name %in% c("BiopsyOfProstate", "ProstateSpecificAntigenTest")) %>%
  mutate(outcome = case_when(outcome_cohort_name == "BiopsyOfProstate" ~ "Biopsy Of Prostate",
                             outcome_cohort_name == "ProstateSpecificAntigenTest" ~ "Prostate Specific Antigen Test")) %>% 
  as.data.frame()

prostate_inc_months_plot <- 
  ggplot(prostate_inc_months_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                                   ymin = incidence_100000_pys_95CI_lower,
                                   ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, NA)) +
  ggtitle("Incidence Rates of prostate Cancer Screening/ Diagnostic Tests/ Referrals in Months Before and After COVID-19 Lockdown") +
  labs(colour = "Screening/ Diagnostic Test", x="Time" , y="Incidence per 100,000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

prostate_inc_months_plot

# save the plot as pdf
analysis.name <- "Screening"
plotname <- paste0(analysis.name, db.name, "prostate_inc_months")

pdf(here("Results", db.name,"4_ScreeningTests",paste0(plotname, ".pdf")),
    width = 10, height = 8)
print(prostate_inc_months_plot, newpage = FALSE)
dev.off()

# Save the plot as jpg
ggsave(here("Results", db.name , "2_ScreeningTests", paste0(plotname, ".jpg")), prostate_inc_months_plot, dpi=600, scale = 1, width = 12, height = 9)


## ===================== PLOTS FACETED BY OUTCOME =========================== ##

# BREAST CANCER SCREENING AND DIAGNOSTIC TESTS IN MONTHS FACETED 

breast_inc_months_plot <- inc_screen %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_interval == "months") %>%
  filter(outcome_cohort_name %in% c("BiopsyOfBreast", "BreastCancerReferrals","ExcisionOfBreast","Mammograms","SeenBreastClinic","SeenBreastSurgeon")) %>%
  mutate(outcome = case_when(outcome_cohort_name == "BiopsyOfBreast" ~ "Biopsy of Breast",
                             outcome_cohort_name == "BreastCancerReferrals" ~ "Breast Cancer Referrals",
                             outcome_cohort_name == "ExcisionOfBreast" ~ "Excision Of Breast",
                             outcome_cohort_name == "Mammograms" ~ "Mammograms",
                             outcome_cohort_name == "SeenBreastClinic" ~ "Seen in Breast Clinic",
                             outcome_cohort_name == "SeenBreastSurgeon" ~ "Seen Breast Surgeon")) %>%
  as.data.frame()

breast_inc_months_plot <- 
  ggplot(breast_inc_months_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                                     ymin = incidence_100000_pys_95CI_lower,
                                     ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, NA)) +
  facet_wrap(~outcome, nrow=3, scales = "free_y") +
  scale_x_date(date_labels="%b %Y",date_breaks  ="3 month")+
  ggtitle("Incidence Rates of Breast Cancer Screening/ Diagnostic Tests/ Referrals in Months Before and After COVID-19 Lockdown") +
  labs(colour = "Screening/ Diagnostic Test", x=" " , y="Incidence per 100,000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

breast_inc_months_plot

# save the plot as pdf
analysis.name <- "Screening"
plotname <- paste0(analysis.name, db.name, "breast_inc_months_facet")

pdf(here("Results", db.name,"4_ScreeningTests",paste0(plotname, ".pdf")),
    width = 10, height = 8)
print(breast_inc_months_plot, newpage = FALSE)
dev.off()

# Save the plot as jpg
ggsave(here("Results", db.name , "2_ScreeningTests", paste0(plotname, ".jpg")), breast_inc_months_plot, dpi=900, scale = 1, width = 16, height = 9)




# COLORECTAL CANCER SCREENING AND DIAGNOSTIC TESTS IN MONTHS FACETED

colorectal_inc_months_plot <- inc_screen %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_interval == "months") %>%
  filter(outcome_cohort_name %in% c("BowelCancerScreeningProg", "Colonoscopy","QuantitativeFaecalImmunochemicalTests","ColorectalCancerReferrals", "Sigmoidoscopy")) %>%
  mutate(outcome = case_when(outcome_cohort_name == "BowelCancerScreeningProg" ~ "Bowel Cancer Screening Prog",
                             outcome_cohort_name == "Colonoscopy" ~ "Colonoscopy",
                             outcome_cohort_name == "QuantitativeFaecalImmunochemicalTests" ~ "Quantitative Faecal Immunochemical Tests",
                             outcome_cohort_name == "ColorectalCancerReferrals" ~ "Colorectal Cancer Referrals",
                             outcome_cohort_name == "Sigmoidoscopy" ~ "Sigmoidoscopy")) %>% 
  as.data.frame()

colorectal_inc_months_plot <- 
  ggplot(colorectal_inc_months_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                                         ymin = incidence_100000_pys_95CI_lower,
                                         ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, NA)) +
  facet_wrap(~outcome, nrow=2, scales = "free_y") +
  scale_x_date(date_labels="%b %Y",date_breaks  ="3 month")+
  ggtitle("Incidence Rates of Colorectal Cancer Screening/ Diagnostic Tests/ Referrals in Months Before and After COVID-19 Lockdown") +
  labs(colour = "Screening/ Diagnostic Test", x=" " , y="Incidence per 100,000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

colorectal_inc_months_plot

# save the plot as pdf
analysis.name <- "Screening"
plotname <- paste0(analysis.name, db.name, "colorectal_inc_months_facet")

pdf(here("Results", db.name,"4_ScreeningTests",paste0(plotname, ".pdf")),
    width = 10, height = 8)
print(colorectal_inc_months_plot, newpage = FALSE)
dev.off()

# Save the plot as jpg
ggsave(here("Results", db.name , "2_ScreeningTests", paste0(plotname, ".jpg")), colorectal_inc_months_plot, dpi=900, scale = 1, width = 16, height = 9)



# LUNG CANCER SCREENING AND DIAGNOSTIC TESTS IN MONTHS

lung_inc_months_plot <- inc_screen %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_interval == "months") %>%
  filter(outcome_cohort_name %in% c("Bronchoscopy", "DiagnosticProceduresOfChest","LungCancerReferrals")) %>%
  mutate(outcome = case_when(outcome_cohort_name == "Bronchoscopy" ~ "Bronchoscopy",
                             outcome_cohort_name == "DiagnosticProceduresOfChest" ~ "Diagnostic Procedures Of Chest",
                             outcome_cohort_name == "LungCancerReferrals" ~ "Lung Cancer Referrals")) %>% 
  as.data.frame()

lung_inc_months_plot <- 
  ggplot(lung_inc_months_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                                   ymin = incidence_100000_pys_95CI_lower,
                                   ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, NA)) +
  facet_wrap(~outcome, nrow=2, scales = "free_y") +
  scale_x_date(date_labels="%b %Y",date_breaks  ="3 month")+
  ggtitle("Incidence Rates of Lung Cancer Screening/ Diagnostic Tests/ Referrals in Months Before and After COVID-19 Lockdown") +
  labs(colour = "Screening/ Diagnostic Test", x=" " , y="Incidence per 100,000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

lung_inc_months_plot

# save the plot as pdf
analysis.name <- "Screening"
plotname <- paste0(analysis.name, db.name, "lung_inc_months_facet")

pdf(here("Results", db.name,"4_ScreeningTests",paste0(plotname, ".pdf")),
    width = 10, height = 8)
print(lung_inc_months_plot, newpage = FALSE)
dev.off()

# Save the plot as jpg
ggsave(here("Results", db.name , "2_ScreeningTests", paste0(plotname, ".jpg")), lung_inc_months_plot, dpi=900, scale = 1, width = 16, height = 9)


# PROSTATE CANCER SCREENING AND DIAGNOSTIC TESTS IN MONTHS

prostate_inc_months_plot <- inc_screen %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_interval == "months") %>%
  filter(outcome_cohort_name %in% c("BiopsyOfProstate", "ProstateSpecificAntigenTest")) %>%
  mutate(outcome = case_when(outcome_cohort_name == "BiopsyOfProstate" ~ "Biopsy Of Prostate",
                             outcome_cohort_name == "ProstateSpecificAntigenTest" ~ "Prostate Specific Antigen Test")) %>% 
  as.data.frame()

prostate_inc_months_plot <- 
  ggplot(prostate_inc_months_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                                       ymin = incidence_100000_pys_95CI_lower,
                                       ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, NA)) +
  facet_wrap(~outcome, nrow=1, scales = "free_y") +
  scale_x_date(date_labels="%b %Y",date_breaks  ="3 month")+
  ggtitle("Incidence Rates of prostate Cancer Screening/ Diagnostic Tests/ Referrals in Months Before and After COVID-19 Lockdown") +
  labs(colour = "Screening/ Diagnostic Test", x=" " , y="Incidence per 100,000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

prostate_inc_months_plot

# save the plot as pdf
analysis.name <- "Screening"
plotname <- paste0(analysis.name, db.name, "prostate_inc_months_facet")

pdf(here("Results", db.name,"4_ScreeningTests",paste0(plotname, ".pdf")),
    width = 10, height = 8)
print(prostate_inc_months_plot, newpage = FALSE)
dev.off()

# Save the plot as jpg
ggsave(here("Results", db.name , "2_ScreeningTests", paste0(plotname, ".jpg")), prostate_inc_months_plot, dpi=900, scale = 1, width = 18, height = 9)



  
print(paste0("- Analysis of screening tests done"))
info(logger, "- Analysis of screening tests done")
