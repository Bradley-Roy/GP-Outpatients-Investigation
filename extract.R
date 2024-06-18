# GP Cluster Profiles
# Written by Megan McNicol
# Version 0.2  written: 17/02/2021
# This is a direct translation of '1. SMR Extract.sps' written by LIST analysts
# This syntax requires access to SMR0, SMR1, and GRO Deaths

# NOTE: change mergers file back when running in Jan/Feb 2024

# rm(list = ls())
# gc()

source("lookups.R")

############################################## Lookups ##############################################
# This is the lookup file for Practices that have merged.
# We only provide the profiles for merged practices that have specifically requested it.
# a list of merged Practices that we keep up to date manually
mergers <- read_csv("/PHI_conf/PrimaryCare/Look ups/mergers.csv",
                    col_types = cols(.default = "c"))


############################################## Log in to SMRA ##############################################
# If you don't have access to SMRA through R Studio server you need to request this through service protal.
user <- Sys.info()[["user"]]
channel <- suppressWarnings(dbConnect(odbc(),  dsn = "SMRA",
                                      uid = user,
                                      pwd = .rs.askForPassword("SMRA Password:")))

# This will show you which data you have access to through RStudio Server. You need GRO_Deaths, SMR00, SMR01
odbcListObjects(channel, schema="ANALYSIS")

# This will show you all the variables in the dataset. You can use these to help build your querey.
odbcPreviewObject(channel, table="ANALYSIS.SMR00_PI", rowLimit=0)


############################################## Outpatients ##############################################
# This is around 2.5 million rows of data so it takes a wee while

Outpatients_raw <- as_tibble(dbGetQuery(channel,
                                        statement=paste0("SELECT UPI_NUMBER, SPECIALTY, REFERRAL_SOURCE, REFERRAL_TYPE, CI_CHI_NUMBER, ",
                                                         "CLINIC_ATTENDANCE, GP_PRACTICE_CODE, CLINIC_DATE, DOB, AGE_IN_YEARS, ", # select all these variables
                                                         "POSTCODE, ETHNIC_GROUP, REFERRING_GP_GPD_GMC_NUMBER, REFERRAL_REASON_1, ",
                                                         "REFERRAL_REASON_2, REFERRAL_REASON_3, REFERRAL_REASON_4, LOCATION ",
                                                         "FROM  ANALYSIS.SMR00_PI ",  # get data from SMR0
                                                         "WHERE (((CLINIC_DATE between ",ONE_START, " AND ", ONE_END, ")", # between these dates
                                                         "OR (CLINIC_DATE between ", TWO_START, " AND ",TWO_END, "))", # and between these dates
                                                         "AND ((REFERRAL_TYPE=1) OR (REFERRAL_TYPE=2))", # New outpatient appointments only
                                                         "AND (REFERRAL_SOURCE= '1'))"))) # GP referrals only




# Tidy Outpatients Data
Outpatients_raw <- Outpatients_raw %>%
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



##############################################   End ##############################################
# clear up space.
#rm(list = ls())

rm("Deaths_raw", "channel")
gc()

