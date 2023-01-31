# CancerCovid_IncidencePrevalence
All analyses of incidence and prevalence of cancers and cancer-related treatments, before and after the COVID-19 lockdown:
- breast, colorectal, lung and prostate cancer; 
- use of endocrine treatments in breast and prostate cancer (Aromatase Inhibitors, Tamoxifen, Andropgen Deprivation Therapy with GnRH, GnRH as monotherapy for prostate; LHRH antagonists);
- associated treatment-related outcomes (osteoporosis, osteopenia, bone fracture, prescriptions of bisphosphonates and denosumab); 
- screening tests relevant to all four cancers (mammograms, biopsy of breast, bowel cancer screening programmes, sigmoidoscopy, colonoscopy, bronchoscopy, prostate specific antigen tests).

# Running the analysis
Download this entire repository (you can download as a zip folder using Code -> Download ZIP, or you can use GitHub Desktop).
Open the project CancerCovid_IncidencePrevalence.Rproj in RStudio (when inside the project, you will see its name on the top-right of your RStudio session)

Open and work though the CodeToRun.R file which should be the only file that you need to interact with. Run the lines in the file, adding your database specific information and so on (see comments within the file for instructions). The last line of this file will run the study (source(here("RunStudy.R")).

After running you should then have a 4 separate results file, specific to each of the outcomes above, with a zipped folder within them with the results to share. Here you will also find the PDFs of all the plots, and the .RData objects for each part of the analyses.

