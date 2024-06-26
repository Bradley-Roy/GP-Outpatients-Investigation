
# rm(list = ls())
# gc()

############################################## Set up ##############################################
library("tidyverse")
library("lubridate")
library("haven") # read spss files
library("openxlsx") # read excel workbooks
library("readxl")


#File paths
setwd("~/GP-Outpatients-Investigation")
Data <- paste0("./Data/")
# Reference <- paste0("./Reference Files/")


############################################## Open Data - Demographics ##############################################
# This gets the latest list size and cluster lookup from open data website, dont change date in link, get new link from site.
# https://www.opendata.nhs.scot/dataset/gp-practice-populations

# Oct2022 <- "https://www.opendata.nhs.scot/dataset/e3300e98-cdd2-4f4e-a24e-06ee14fcc66c/resource/2c7dcb55-c83a-44bd-9128-47d1d6f339ba/download/practice_listsizes_oct2022-open-data.csv"
# Oct2023 <- "https://www.opendata.nhs.scot/dataset/e3300e98-cdd2-4f4e-a24e-06ee14fcc66c/resource/ab677c28-f495-4191-83e1-aaa0c3e6a9b4/download/practice_listsizes_oct2023-open-data.csv"
#
# # read link into R as csv
# ListYear1 <-  read_csv(Oct2022) # read link into R as csv
# ListYear2 <-  read_csv(Oct2023)

Jul2022 <- "https://www.opendata.nhs.scot/dataset/e3300e98-cdd2-4f4e-a24e-06ee14fcc66c/resource/64918d4f-f1d9-4e99-8e9f-130ddc890748/download/practice_listsizes_jul2022-open-data.csv"
Jul2023 <- "https://www.opendata.nhs.scot/dataset/e3300e98-cdd2-4f4e-a24e-06ee14fcc66c/resource/d7f423dd-9611-4ae9-a9c8-4dcc532ece22/download/practice_listsizes_jul2023-open-data.csv"

# read link into R as csv
ListYear1 <-  read_csv(Jul2022) # read link into R as csv
ListYear2 <-  read_csv(Jul2023)

##############################################  Lookups ##############################################
# This is the lookup file for Practices that have merged.
mergers <- read_csv("/PHI_conf/PrimaryCare/Look ups/mergers.csv",
                    col_types = cols(.default = "c"))

# this is a manually update csv or Practice that need to be excluded
Exclude <- read_csv("/PHI_conf/PrimaryCare/Look ups/Exclude.csv",
                    col_types = cols(.default = "c"))

# # This is created in syntax '0 Create Lookup' - used to make sure we have a case for every Practice even when numerator is zero
# LookupCluster <- readRDS(paste0(Reference,"LookupCluster ", FinancialYear2, " FQ", FinancialQ, ".rds")) %>%
#   select(Practice, FinancialQuarter, FinancialYear)
#
# # This is created in syntax '0 Create Lookup' - adds on list sizes
# LookupClusterLS <- readRDS(paste0(Reference,"LookupCluster ", FinancialYear2, " FQ", FinancialQ, ".rds")) %>%
#   select(Practice, FinancialQuarter, FinancialYear, ListSize)

# This is created in syntax '0 Create Lookup' - used to make sure we have a case for every Practice even when numerator is zero
LookupCluster <- readRDS(paste0(Reference,"LookupCluster ", FinancialYear2, " FQ", FinancialQ, ".rds")) %>%
  select(Practice, FinancialQuarter, FinancialYear, Cluster, HSCP, NHSBoard)

# This is created in syntax '0 Create Lookup' - adds on list sizes
LookupClusterLS <- readRDS(paste0(Reference,"LookupCluster ", FinancialYear2, " FQ", FinancialQ, ".rds")) %>%
  select(Practice, FinancialQuarter, FinancialYear, ListSize, Cluster, HSCP, NHSBoard)


##############################################  Demographics - Sex ##############################################

SexDemographics <- ListYear1 %>% # from open data read in at start of syntax 2.
  full_join(ListYear2) %>%
  select(-SexQF, -Ages0To4QF, -Ages5To14QF, -Ages25To44QF, -Ages15To24QF, -Ages45To64QF,
         -Ages65To74QF,-Ages75To84QF, -Ages85PlusQF, -HB, -HSCP) %>% # remove columns we don't want
  rename('All Ages' = AllAges,
         '0_4' = Ages0to4,
         '05_14' = Ages5to14,
         '15_24' = Ages15to24,
         '25_44' = Ages25to44,
         '45_64' = Ages45to64,
         '65_74' = Ages65to74,
         '75_84' = Ages75to84,
         '85plus' = Ages85plus) %>%
  gather(key=AgeGroups, value = Age_populations,'All Ages', '0_4':'85plus') %>% # flip data round so we have Age Group column and Population column
  mutate(Practice = as.character(PracticeCode),#Date = as.character(Date),
         CalendarYear = as.numeric(substr(as.character(Date),1,4)), # calendar year is the first 4 digits
         CalendarMonth = as.numeric(substr(as.character(Date), 5,6)),  # calendar month is the next 2 digits
         FinancialQuarter = as.character(ifelse(CalendarMonth %in% c(1, 2, 3), 4,
                                                ifelse(CalendarMonth %in% c(4, 5, 6), 1,
                                                       ifelse(CalendarMonth %in% c(7, 8, 9), 2, 3)))),
         FinancialYear = as.character(ifelse(FinancialQuarter %in% c("4"), CalendarYear-1, CalendarYear)),
         Indicator = Sex) %>%

  left_join(mergers, by = c("Practice" = "Code")) %>%  # join to mergers lookup
  mutate(Practice = ifelse(is.na(`New Code`), Practice, `New Code`)) %>% # Update Practice Codes
  group_by(FinancialYear, FinancialQuarter, Practice, AgeGroups, Indicator, Sex) %>%
  summarise(Age_populations = sum(Age_populations)) %>%  ungroup() %>% # aggregate List Size - this fixes populations for merged practices.

  anti_join(Exclude, by = c("Practice" = "Code")) # remove practices to be excluded


# Here we are first creating a masterlist of all Practices and indicators
# this is so a Practice will still have a case if the patient numbers are missing
# Then we match it up with the data
AgeSexDemographics <- SexDemographics %>%
  select(FinancialYear, FinancialQuarter, AgeGroups, Sex, Indicator) %>% distinct %>% # creates a dataframe of time period, Age, Sex. So we can create masterlist of all practices and add data back on
  full_join(LookupCluster) %>% # from lookups. Gives us all combinations of Practice, Time period, Age, Sex to add data back onto
  left_join(SexDemographics) %>% # add populations back on
  mutate(Age_populations = ifelse(is.na(Age_populations), 0, Age_populations)) # if practice is missing population but should have one... set to zero.

# saveRDS(AgeSexDemographics, file = paste0(Data,"SexDemographics ", FinancialYear2, " Q", FinancialQ, ".rds"))

#tidy up files
rm("ListYear1", "ListYear2", "SexDemographics")

##############################################  Demographics - SIMD ##############################################
# read in SIMD data that gets used for the Demographics dashboard
# This may not always be in the same folder. You might have to look in PSD extracts as well.
# JM Nov 23 - .sav file isn't updated so reading in csv instead.

#raw_SIMD <- read_sav("/PHI_conf/PrimaryCare/PCI/Analysis/Extracts/Demographics/Data/Demographics SIMD.sav")
raw_SIMD <- read_csv("/PHI_conf/PrimaryCare/PCI/Analysis/Extracts/Demographics/Data/Demographics SIMD.csv")

SIMD_tidy <- raw_SIMD %>%
  mutate(FinancialYear = as.character(substr(Quarter, 1,4)), # create FY variable
         FinancialQuarter = as.character(substr(Quarter, 11,11))) %>% # create FQ variable
  filter(FinancialYear == FinancialYear1 | FinancialYear == FinancialYear2,  # only the financial years that we want for cluster profiles.
         FinancialQuarter == FinancialQ) %>% # filter data to years and quarter we want
  mutate(Indicator = ifelse (Decile == "SIMD 1" | Decile == "SIMD 2",  "SIMD Quintile 1", # recode SIMD from deciles to quintiles
                             ifelse (Decile == "SIMD 3" | Decile == "SIMD 4",  "SIMD Quintile 2",
                                     ifelse (Decile == "SIMD 5" | Decile == "SIMD 6",  "SIMD Quintile 3",
                                             ifelse (Decile == "SIMD 7" | Decile == "SIMD 8",  "SIMD Quintile 4",
                                                     ifelse (Decile == "SIMD 9" | Decile == "SIMD 10", "SIMD Quintile 5", "missing"))))),
         Practice = as.character(Practice)) %>%  # formatting
  left_join(mergers, by = c("Practice" = "Code")) %>%  # join to mergers lookup
  mutate(Practice = ifelse(is.na(`New Code`), Practice, `New Code`)) %>% # Update Practice Codes
  group_by(Practice, Indicator, FinancialYear, FinancialQuarter) %>%
  summarise(Practice_populations=sum(Patients)) %>% ungroup() %>%  # aggregate patient numbers
  anti_join(Exclude, by = c("Practice" = "Code"))

# Use Lookup so a Practice will still have a case if the patient numbers are 0 or missing
# Then we match it up with the data
SIMD <- LookupCluster %>%  #everything in LookupCluster should have a case
  full_join( SIMD_tidy %>% # add all combinations of Year, Quarter and SIMD to Lookup Cluster
               select(FinancialYear, FinancialQuarter, Indicator) %>%
               distinct(), relationship = "many-to-many") %>%
  left_join(SIMD_tidy) %>% # join the data on
  mutate(Practice_populations = ifelse(is.na(Practice_populations), 0, Practice_populations)) # set missing values to 0

# saveRDS(SIMD, file = paste0(Data,"SIMD ", FinancialYear2, " Q", FinancialQ , ".rds"))

#tidy up files
rm(raw_SIMD, SIMD_tidy)



##############################################  Outpatients ##############################################
# Outpatients_raw <- read_rds(paste0(Data, "Outpatients_raw ", FinancialYear2, " Q", FinancialQ, ".rds"))

# merged practices were dealt with in syntax 1
Outpatients_tidy <- Outpatients_raw %>%
  mutate(AgeGroups = ifelse(Age<65, "Under 65", "65+"),
         FinancialYear = as.character(ifelse(CalendarQuarter == 1, CalendarYear-1, CalendarYear)),
         FinancialQuarter = as.character(ifelse(CalendarQuarter-1 == 0, 4, CalendarQuarter-1))) %>%
  filter(FinancialQuarter == FinancialQ) %>%  # filter to Financial quarter that we want
  select(-Age, -CalendarQuarter, -CalendarYear, -ClinicDate)

# Create aggregate data for All Ages
OutpatientsAllAges <- Outpatients_tidy %>%
  mutate(AgeGroups="All Ages")

# We are only interested in some specfic specialties. We also want a total for all specialties
Outpatients_tidy <- Outpatients_tidy %>%
  rbind(OutpatientsAllAges) %>%
  mutate(Indicator = ifelse(substr(Speciality, 1,2)=="A7", "Dermatology",
                            ifelse(substr(Speciality, 1,2)=="C1", "General Surgery",
                                   ifelse(substr(Speciality, 1,2)=="C8", "Trauma",
                                          ifelse(substr(Speciality, 1,2)=="C5", "ENT", Speciality))))) |>
  semi_join(LookupCluster) %>%
  left_join(LookupCluster)

# Create aggregated data for 'All specialities'
OutpatientsAllSpecialties <- Outpatients_tidy %>%
  mutate(Indicator = "All Specialities")

Outpatients_tidy2 <- Outpatients_tidy |>
  anti_join(Exclude, by = c("Practice" = "Code")) #%>%
  # semi_join(LookupCluster) %>%
  # left_join(LookupCluster)


Outpatients_tidy_indicator <- Outpatients_tidy2 %>% # join selected specialties
  bind_rows(OutpatientsAllSpecialties) |>
  # rbind(OutpatientsAllSpecialties) %>% # to all specialties
  # filter(Indicator %in% c("All Specialities", "Dermatology", "General Surgery", "Trauma", "ENT")) %>% # filter for ones we want
  filter(Indicator %in% c("Trauma"))


patient_list <- Outpatients_tidy_indicator |>
  select(Cluster, Practice, Practice_title, CI_CHI_NUMBER,  UPI, AgeGroups, FinancialYear,Speciality)

write_csv(patient_list, "//conf/LIST_analytics/Borders/Ad-Hocs/2024-06 Outpatients/patient_list.csv")

Outpatients_tidy_summary <- Outpatients_tidy_indicator %>%
  group_by(Indicator, Practice, FinancialYear, FinancialQuarter, AgeGroups) %>%
  summarise(Numerator = n()) %>%  ungroup() %>%
  filter(!(is.na(Practice)))
  # mutate(Indicator = ifelse(Indicator == "Dermatology", "New OP Rate per 1,000 Dermatology",
  #                           ifelse(Indicator == "ENT", "New OP Rate per 1,000 ENT",
  #                                  ifelse(Indicator == "Trauma", "New OP Rate per 1,000 Trauma",
  #                                         ifelse(Indicator == "General Surgery","New OP Rate per 1,000 GenSurg" ,
  #                                                "New OP Rate per 1,000 AllSpec")))))



# Use Lookup so a Practice will still have a case if the numerator is 0 or missing
# Then we match it up with the data
Outpatients_tidy <- LookupCluster %>%
  full_join(Outpatients_tidy_summary %>%
              select(FinancialYear, FinancialQuarter, AgeGroups, Indicator) %>%
              distinct) %>%
  left_join(Outpatients_tidy) %>%
  mutate(Numerator = ifelse(is.na(Numerator), 0, Numerator)) %>%
  mutate(Domain = "Outpatients",
         Term = "Financial Quarter")|>
  filter(Practice %in% practice_list)

# saveRDS(Outpatients_tidy, file=  paste0(Data,"Outpatients_tidy ", FinancialYear2, " Q", FinancialQ, ".rds"))

# rm("Outpatients_raw", "OutpatientsAllAges", "OutpatientsAllSpecialties")

# Outpatients_tidy <- read_rds(paste0(Data,"Outpatients_tidy ",FinancialYear2, " Q", FinancialQ ,".rds"))
# LookupDemographics <- readRDS(paste0(Data, "LookupDemographics ", FinancialYear2, " Q", FinancialQ, ".rds"))

Outpatients <- Outpatients_tidy %>%
  anti_join(Exclude, by = c("Practice" = "Code")) %>%
  semi_join(LookupCluster) %>%
  left_join(LookupCluster) %>%
  arrange(Practice) %>%
  group_by(Practice)  %>%
  fill(Cluster, .direction = "up") %>%
  # fill(PracticeName, .direction = "up") %>%
  fill(Practice_title, .direction = "up") %>%
  ungroup()

Outpatients <- Outpatients %>%
  mutate(Practice=paste0(Practice, " - ", Practice_title), # rewrite Practice variable to have Code and Name
         Scotland = "Scotland") %>%
  # select(-Practice_title, -NHSBoard, -HSCP, -ListSize) %>%
  select(-Practice_title, -NHSBoard, -HSCP) %>%
  gather(Practice, Cluster, #PeerGroup,
         Scotland, # collect these columns
         key = Geography, value = GeographyName)  # and place them into one column named "GeographyName"


OutpatientsGeographies <- Outpatients %>%
  filter(Geography == "Practice") %>%
  left_join(PracticeGeography %>% # To add on HSCP and NHS Board
              rename(GeographyName = Practice)) %>%
  select(-Geography, -GeographyName, -ListSize) %>%
  gather(HSCP, NHSBoard,   # collect these columns
         key = Geography, value = GeographyName)

Outpatients  <- Outpatients %>%
  rbind(OutpatientsGeographies) %>%
  group_by(FinancialYear, FinancialQuarter, AgeGroups,  Geography, GeographyName, Indicator, Term) %>%
  summarise(Numerator = sum(Numerator)) %>% ungroup() %>%
  left_join(LookupDemographics) %>% # to add on populations
  rename(Denominator = Population) %>%
  mutate(Rate =(1000*(Numerator/Denominator)),
         Domain = "Outpatients")

# rm("Outpatients_tidy", "OutpatientsGeographies")


rm(Outpatients_raw)

