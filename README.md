# CancerCovid_IncidencePrevalence
All analyses of incidence and prevalence of cancers and cancer-related treatments, before and after the COVID-19 lockdown, in years and months from 2017:
- breast, colorectal, lung and prostate cancer; 
- screening tests relevant to all four cancers (Biopsy Of Breast, Breast Cancer Referrals, Excision Of Breast, Mammograms, Seen Breast Clinic, Seen Breast Surgeon, Bowel Cancer Screening Prog, Colonoscopy, Quantitative Faecal Immunochemical Tests, Colorectal Cancer Referrals, Sigmoidoscopy, Bronchoscopy, Diagnostic Procedures Of Chest, Lung Cancer Referrals, Biopsy Of Prostate, Prostate Specific Antigen Test).

These analyses are featured in the following paper:

Barclay, N.L., Pineda Moncusi, M., Jödicke, A. M., Prieto-Alhambra, D., Raventós, B., Newby, D., Delmestri, A., Man, W-Y., Chen, X., & Català, M. (in preparation). Changes in Incidence of Breast, Colorectal, Lung and Prostate Cancer, and Screening and Diagnostic Tests, Before, During and After the UK National COVID-19 Lockdown: A Retrospective Cohort Study Using UK Primary Care Health Records

# Running the analyses
Download this entire repository (you can download as a zip folder using Code -> Download ZIP, or you can use GitHub Desktop).
Open the project CancerCovid_IncidencePrevalence.Rproj  in RStudio (when inside the project, you will see its name on the top-right of your RStudio session)

Open and work though the CodeToRun.R file which should be the only file that you need to interact with. Run the lines in the file, adding your database specific information and so on (see comments within the file for instructions). The last line of this file will run the study (source(here("RunStudy.R")).

After running you should then have a zip folder with results to share in your home directory.


# Associated repositories and documents
The paper (Barclay et al., in prep) contains analyses from other github repositories which can be found here:

https://github.com/oxford-pharmacoepi/CancerCovid_CohortDiagnostics

https://github.com/oxford-pharmacoepi/CancerCovid_Characterisations

https://github.com/oxford-pharmacoepi/CancerCovid_IncidencePrevalence

In addition, a shiny app through which to view additional cohort diagnostics can be found here: https://dpa-pde-oxford.shinyapps.io/CancerCovid_CohortDiagnosticsShiny_paper1/
