# ============================================================================ #
#                 CODE TO RUN FOR CANCER/COVID INCIDENCE/PREVALENCE            #
#   ANALYSES - CANCERS, ENDOCRINE TREATMENTS, OSTEOPOROSIS-RELATED OUTCOMES,   #
#                              AND SCREENING TESTS                             #
#                                Nicola Barclay                                #
#                                 31-01-2023                                   #
#                                                                              #
#             THIS SHOULD BE THE ONLY FILE YOU NEED TO INTERACT WITH           #
#                                                                              #
# ============================================================================ #

# Load packages ------
#renv::activate()
#renv::restore()

# Load packages ------

# to install latest version of IncidencePrevalence
#remotes::install_github("darwin-eu-dev/IncidencePrevalence",force = TRUE)

# load r packages
library(CirceR)
library(IncidencePrevalence)
library(here)
library(DBI)
library(dbplyr)
library(dplyr)
library(readr)
library(log4r)
library(tidyr)
library(stringr)
library(CDMConnector)
library(ggplot2)
library(RPostgres)


# database metadata and connection details -----
# The name/ acronym for the database
db.name<-"..."

# Set output folder location -----
# the path to a folder where the results from this analysis will be saved
# to set the location within the project with folder called "ouput, we can use: here("output")
# but this file path could be set to somewhere else
output.folder<-here("Results", db.name)
output.folder1<-here("Results", db.name, "1_Cancers")
output.folder2<-here("Results", db.name, "2_ScreeningTests")


# Specify databaseConnector connection details -----
# database connection details
# connect to database
user<-"..."
password<- "..."
port<-"..."
host<-"..."
server_dbi<-"..."

# Specify cdm_reference via DBI connection details -----
# In this study we also use the DBI package to connect to the database
# set up the dbConnect details below (see https://dbi.r-dbi.org/articles/dbi for more details)
# you may need to install another package for this (although RPostgres is included with renv in case you are using postgres)
# see here for details: https://odyosg.github.io/CDMConnector/articles/DBI_connection_examples.html
db <- dbConnect("...",
                dbname = server_dbi,
                port = port,
                host = host, 
                user = user, 
                password = password)

# Set database details -----
# The name of the schema that contains the OMOP CDM with patient-level data
cdm_database_schema<-"..."

# The name of the schema that contains the vocabularies 
# (often this will be the same as cdm_database_schema)
vocabulary_database_schema<-cdm_database_schema

# The name of the schema where results tables will be created 
results_database_schema<-"..."

# Name of outcome and strata tables in the result table where the outcome cohorts will be stored
# Note, if there is an existing table in your results schema with the same names
# it will be overwritten 

outcome_table_stem <- "..."


# table names----
outcome_table_name_1 <- paste0(outcome_table_stem,"_cancers") # this is the four cancers
outcome_table_name_2 <- paste0(outcome_table_stem,"_1stevercancers") # this is the four cancers
outcome_table_name_3 <- paste0(outcome_table_stem,"_screening_outcomes_table") # this is the table for the screening tests as outcomes


# create cdm reference ---- 
cdm <- CDMConnector::cdm_from_con(con = db, 
                                  cdm_schema = cdm_database_schema,
                                  write_schema = results_database_schema)


# to check whether the DBI connection is correct, 
# running the next line should give you a count of your person table
cdm$person %>% 
  tally()

# Run the study ------
source(here("RunStudy.R"))
# after the study is run you should have a zip folder in your home directory to share
