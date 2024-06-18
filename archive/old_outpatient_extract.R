
# Outpatient Extract -------------------------------------------------------------

rm(list = ls())
gc()

############################################## Set up ##############################################
library(tidyverse)
library(DBI) # need this for connecting to SMR data
library(odbc) # need this for connecting to SMR data
library(zoo)

#File paths
uid_name <- "bradlr01"
# setwd("/PHI_conf/PrimaryCare/PCI/Cluster Profiles")
Data <- paste0("./Data/")
# Reference <- paste0("./Reference Files/")
Reference <- paste0("/Reference Files/")

folder_filepath <- "/conf/LIST_analytics/Borders/Ad-Hocs/2024-06 Outpatients"

# practices
practice_list <- c(' 16121', ' 16160', ' 16174', ' 16211',
              '16121', '16160', '16174', '16211')

# p_unlock(lib.loc = "/mnt/homes/bradlr01/R/x86_64-pc-linux-gnu-library/4.1")

# mergers <- read_csv("/conf/PrimaryCare/PCI/Analysis/Extracts/PCI Quarterly Update/Reference/mergers.csv",
#                     col_types = cols(.default = "c"))

############################################## Set Values ##############################################
# We need to capture data for two time periods: current FY and previous FY to be used as a comparison
# Set start and end date to include the quarters before and after quarter of interest
# eg for Financial Period 2019 Q1 (April 2019 - June 2019):
# Start would be: '31 December 2018' (this will give us from 2018 Q4 data)
# End would be: '30 September 2019' (this will give us to 2019 Q2 data) You want 9 months total data back for each time period
# NOTE - THESE MUST BE IN THE FORMAT "'31 December 2019'" or it will not work

# Previous Financial Year
ONE_START <- "'31 July 2022'"
ONE_END <- "'30 September 2022'"

# Current Financial Year
TWO_START <- "'31 July 2023'"
TWO_END <- "'30 September 2023'"

#Set specific financial years of interest (eg for 2018/19 - "2018")
FinancialYear1 <- "2022"
FinancialYear2 <- "2023"
FinancialQ <- "2"


Year1 <-"2022"
Year2 <-"2023"
FinancialQ <- "2"

############################################## Lookups ##############################################
# This is the lookup file for Practices that have merged.
# We only provide the profiles for merged practices that have specifically requested it.
# a list of merged Practices that we keep up to date manually
# mergers <- read_csv("/conf/PrimaryCare/PCI/Analysis/Extracts/PCI Quarterly Update/Reference/mergers.csv",
#                     col_types = cols(.default = "c"))
mergers <- read_csv("/PHI_conf/PrimaryCare/Look ups/mergers.csv",
                    col_types = cols(.default = "c"))


# This is a list of non Practices that we keep up to date manually
Exclude <- read_csv("/PHI_conf/PrimaryCare/Look ups/Exclude.csv",
                    col_types = cols(.default = "c")) |>
  rename(Practice = Code)

# a list of merged Practices that we keep up to date manually
# mergers <- read_csv("/PHI_conf/PrimaryCare/PCI/Analysis/Extracts/PCI Quarterly Update/Reference/mergers.csv",
#                     col_types = cols(.default = "c"))

Clusters <- read_csv("/PHI_conf/PrimaryCare/Look ups/ClustersReference.csv",
                     col_types = cols(.default = "c")) %>% # default are character
  select(PracticeCode, Cluster) %>%
  rename(Practice = PracticeCode)

GeographyCodes <- read_csv("https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/944765d7-d0d9-46a0-b377-abb3de51d08e/download/hscp16_hscp19.csv")


PeerGroups <- read_csv("/PHI_conf/PrimaryCare/PCI/Cluster Profiles/Reference Files/PeerGroupings2018.csv",
                       col_types = cols(.default = "c")) |>
  rename(Practice = `Practice Code`)




############################################## Log in to SMRA ##############################################
# If you don't have access to SMRA through R Studio server you need to request this through service protal.

channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid="bradlr01",
                                      pwd=.rs.askForPassword("SMRA Password:")))

# This will show you which data you have access to through RStudio Server. You need GRO_Deaths, SMR00, SMR01
odbcListObjects(channel, schema="ANALYSIS")

# This will show you all the variables in the dataset. You can use these to help build your querey.
odbcPreviewObject(channel, table="ANALYSIS.SMR00_PI", rowLimit=0)


############################################## Outpatients ##############################################
# This is around 2.5 million rows of data so it takes a wee while

Outpatients_raw <- as_tibble(dbGetQuery(channel,
                                        statement=paste0(#new
                                                         "SELECT SURNAME, FIRST_FORENAME, CI_CHI_NUMBER, POSTCODE, SEX, ETHNIC_GROUP, ",
                                                         #old
                                                         "UPI_NUMBER, SPECIALTY, REFERRAL_SOURCE, REFERRAL_TYPE, ",
                                                         "CLINIC_ATTENDANCE, GP_PRACTICE_CODE, CLINIC_DATE, AGE_IN_YEARS ", # select all these variables
                                                         "FROM  ANALYSIS.SMR00_PI ",  # get data from SMR0
                                                         "WHERE (((CLINIC_DATE between ",ONE_START, " AND ", ONE_END, ")", # between these dates
                                                         "OR (CLINIC_DATE between ", TWO_START, " AND ",TWO_END, "))", # and between these dates
                                                         "AND ((REFERRAL_TYPE=1) OR (REFERRAL_TYPE=2)))"))) # New outpatient appointments only


# UPI_NUMBER, SPECIALTY, REFERRAL_SOURCE, REFERRAL_TYPE, ",
# "CLINIC_ATTENDANCE, GP_PRACTICE_CODE, CLINIC_DATE, AGE_IN_YEARS

# SURNAME, FIRST FORENAME, CI_CHI_NUMBER, POSTCODE, SEX, ETHNIC GROUP,


# Tidy Outpatients Data
# Outpatients_raw2 <- Outpatients_raw %>%
#   filter(REFERRAL_SOURCE == "1") %>% # GP referrals only
#   rename(UPI = UPI_NUMBER,
#          Speciality = SPECIALTY,
#          Practice = GP_PRACTICE_CODE,
#          ClinicDate = CLINIC_DATE,
#          Age = AGE_IN_YEARS) %>% # rename variables
#   mutate(Practice = str_trim(Practice)) %>%  # remove white spaces from Practice codes
# # left_join(mergers, by = c("Practice" = "Code")) %>%  # join to mergers lookup
# mutate(#Practice = ifelse(is.na(`New Code`), Practice, `New Code`), # change Practice code if Practice is on Mergers.csv
#        CalendarYear = as.numeric(substr(ClinicDate,1,4)), # extract Calendar Year from Clinic Date
#        CalendarQuarter = as.numeric(substr(as.yearqtr(ClinicDate), 7, 7))) %>%  # extract Calendar Month from Clinic Date
# select(-`New Code`)

Outpatients_raw2 <- Outpatients_raw %>%
  filter(REFERRAL_SOURCE == "1") %>% # GP referrals only
  rename(UPI = UPI_NUMBER,
         Speciality = SPECIALTY,
         Practice = GP_PRACTICE_CODE,
         ClinicDate = CLINIC_DATE,
         Age = AGE_IN_YEARS) %>% # rename variables
  mutate(Practice = str_trim(Practice)) %>%  # remove white spaces from Practice codes
  left_join(mergers, by = c("Practice" = "Code")) %>%  # join to mergers lookup
  mutate(Practice = ifelse(is.na(`New Code`), Practice, `New Code`), # change Practice code if Practice is on Mergers.csv
         CalendarYear = as.numeric(substr(ClinicDate,1,4)), # extract Calendar Year from Clinic Date
         CalendarQuarter = as.numeric(substr(as.yearqtr(ClinicDate), 7, 7))) %>%  # extract Calendar Month from Clinic Date
  select(-`New Code`) |>
  filter(Practice %in% practice_list) %>%
  mutate(Practice_title = case_when(Practice == "16121" ~ "ST RONAN'S PRACTICE",
                              Practice == "16160" ~ "THE NEIDPATH PRACTICE",
                              Practice == "16174" ~ "WEST LINTON MEDICAL PRACTICE",
                              Practice == "16211" ~ "THE TWEED PRACTICE",TRUE ~ ""))

# saveRDS(Outpatients_raw, file = paste0(Data,"Outpatients_raw ", FinancialYear2, " Q", FinancialQ, ".rds"))

rm("channel")
gc()
