# GP Cluster Profiles Project for LIST
# Written by Megan McNicol, revised by Net Guilding 09/2023
# This syntax creates the GP Practice lookup file to match on all data during the Cluster Profile analysis.
# It creates a list of all GP Practices and geographies with list sizes and recoding

# Reads in List sizes and Geography reference file from open data, and Peer groupings from ISD website,
# Exclude.csv and Merger.csv are manually updated and found in Reference File folder



rm(list = ls())
gc()

######################################## Set up ########################################
library("tidyverse")
library("stringr")

############################################## Set up ##############################################
library("tidyverse")
library("DBI") # need this for connecting to SMR data
library("odbc") # need this for connecting to SMR data
library("zoo")
library("lubridate")
library(janitor)
library(plotly)
library(scales)
library(glue)


#File paths
# setwd("/PHI_conf/PrimaryCare/PCI/Cluster Profiles")
# setwd("~")
Data <- paste0("./Data/")
# Reference <- paste0("./Reference Files/")
Reference <- "/PHI_conf/PrimaryCare/PCI/Cluster Profiles/Reference Files/"

practice_list <- c(' 16121', ' 16160', ' 16174', ' 16211',
                   '16121', '16160', '16174', '16211')


############################################## Set Values ##############################################
# We need to capture data for two time periods: current FY and previous FY to be used as a comparison
# Set start and end date to include the quarters before and after quarter of interest
# eg for Financial Period 2019 Q1 (April 2019 - June 2019):
# Start would be: '31 December 2018' (this will give us from 2018 Q4 data)
# End would be: '30 September 2019' (this will give us to 2019 Q2 data) You want 9 months total data back for each time period
# NOTE - THESE MUST BE IN THE FORMAT "'31 December 2019'" or it will not work



# Previous Financial Year
ONE_START <- "'31 March 2022'"
ONE_END <- "'31 December 2022'"

# Current Financial Year
TWO_START <- "'31 March 2023'"
TWO_END <- "'31 December 2023'"

# Previous Financial Year
# ONE_START <- "'31 March 2022'"
# ONE_END <- "'31 December 2022'"
#
# # Current Financial Year
# TWO_START <- "'31 March 2023'"
# TWO_END <- "'31 December 2023'"

#Set specific financial years of interest (eg for 2018/19 - "2018")
FinancialYear1 <- "2022"
FinancialYear2 <- "2023"
# FinancialQ <- "3"
FinancialQ <- "2"

Year1 <-"2022"
Year2 <-"2023"
FinancialQ <- "2"

######################################## Lookup - Practice Files ########################################
# This gets the latest list size and cluster lookup from open data website. I've kept the dates in so it's obvious which month and year we are using.
# https://www.opendata.nhs.scot/dataset/gp-practice-contact-details-and-list-sizes
Jul2022 <- "https://www.opendata.nhs.scot/dataset/f23655c3-6e23-4103-a511-a80d998adb90/resource/5273d444-5a79-4fad-a518-119a368e2161/download/practice_contactdetails_jul2022-open-data.csv"
Jul2023 <- "https://www.opendata.nhs.scot/dataset/f23655c3-6e23-4103-a511-a80d998adb90/resource/6e2b279f-5b9e-41ff-971c-1819c3df8ba9/download/practice_contactdetails_oct2023-open-data.csv"

# read link into R as csv
LookupCluster1 <-  read_csv(Jul2022) # read link into R as csv
LookupCluster2 <-  read_csv(Jul2023)

hospital_codes <- read_csv("https://www.opendata.nhs.scot/dataset/cbd1802e-0e04-4282-88eb-d7bdcfb120f0/resource/c698f450-eeed-41a0-88f7-c1e40a568acc/download/hospitals.csv") |>
  clean_names() |>
  select(hospital_code, hospital_name) |>
  rename(LOCATION = hospital_code)

######################################## Reference Files ########################################
# This is a list of non Practices that we keep up to date manually
Exclude <- read_csv("/PHI_conf/PrimaryCare/Look ups/Exclude.csv",
                    col_types = cols(.default = "c"))

# a list of merged Practices that we keep up to date manually
mergers <- read_csv("/PHI_conf/PrimaryCare/Look ups/mergers.csv",
                    col_types = cols(.default = "c"))

Clusters <- read_csv("/PHI_conf/PrimaryCare/Look ups/ClustersReference.csv",
                     col_types = cols(.default = "c")) %>% # default are character
  select(PracticeCode, Cluster) %>%
  rename(Practice = PracticeCode)

######################################## Lookup - Geography Codes ########################################
# Geography codes do change occasionally and are updated once in a while. Double check you have the correct file.
# If some of the geographies are not matching up later on this may be why.

# This is to match up HSCP and NHSBoard Codes
GeographyCodes <- read_csv("https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/944765d7-d0d9-46a0-b377-abb3de51d08e/download/hscp16_hscp19.csv")


######################################## Lookup - Peer Groups ########################################
# this is from https://www.isdscotland.org/Health-Topics/General-Practice/PCI/GP-practice-peer-groupings/
# These were last updated in 2018 so not all Practices are included.

PeerGroups <- read_csv(paste0(Reference, "PeerGroupings2018.csv"),
                       col_types = cols(.default = "c"))

PeerGroups <- PeerGroups %>%
  rename(Practice = `Practice Code`,
         PeerGroup = `Peer Group`) %>%
  select(Practice, PeerGroup)

# Note: the peer groupings are old and haven't been updated. Maybe we should ask LIST if they're still required?

########################################  Cluster Lookups ########################################
# Practice Names can change often so we extract these here and stick them on at the end.
PracticeNames <- LookupCluster2 %>% # add GP Practice names on at the end as these can change regularly and it creates duplicate cases
  mutate(Practice = as.character(PracticeCode),
         PracticeName = toupper(GPPracticeName))   %>%
  select(Practice, PracticeName)


# The open data is not always consistent with variable names so this will have to be looked at each time
# This is for Financial Year 1 (we set this at the start of the syntax)
LookupCluster1 <- LookupCluster1 %>%
  mutate(Practice = as.character(PracticeCode),
         FinancialYear = "Year1",
         FinancialQuarter = FinancialQ) %>% # we set this at the start of the syntax
  rename(ListSize = PracticeListSize) %>%
  left_join(mergers, by = c("Practice" = "Code")) %>% # join to our list of merged Practices. only need this for previous year
  mutate(Practice = ifelse(is.na(`New Code`), Practice, `New Code`)) %>%
  mutate(HSCP = ifelse(Practice == "26015", "S37000005", HSCP)) %>%  # to fix duplication issue, from practices from different HSCPs merging into 26015
  group_by(Practice, FinancialYear, FinancialQuarter, HB, HSCP) %>%
  summarise(ListSize = sum(ListSize)) %>% ungroup() %>%
  mutate(HSCP = ifelse(Practice == "10002", "S37000007",
                       ifelse(Practice == "52221", "S37000034",
                              ifelse(Practice == "52255", "S37000034",
                                     ifelse(Practice == "78359", "S37000030",
                                            ifelse(Practice == "80255", "S37000020",
                                                   ifelse(Practice == "13369", "S37000007",
                                                          ifelse(Practice == "80720", "S37000027", HSCP))))))))# recode this Practice. Part of a merge which isn't cooperating in the data

# This is Financial Year 2
LookupCluster2 <- LookupCluster2 %>%
  mutate(Practice = as.character(PracticeCode),
         FinancialYear = "Year2",
         FinancialQuarter = FinancialQ) %>%  # we set this at the start of the syntax
  rename(ListSize = PracticeListSize) %>%
  left_join(mergers, by = c("Practice" = "Code")) %>% # join to our list of merged Practices.
  mutate(Practice = ifelse(is.na(`New Code`), Practice, `New Code`)) %>%
  group_by(Practice, FinancialYear, FinancialQuarter, HB, HSCP) %>%
  summarise(ListSize = sum(ListSize)) %>% ungroup()


########################################  Tidy up lookup ########################################

# Tidy up lookup - fill in missing Peer groupings, HSCP, Cluster details
# Fix list sizes
LookupCluster <- LookupCluster2  %>%
  full_join(LookupCluster1) %>%
  left_join(Clusters) %>% # add in clusters
  anti_join(Exclude, by = c("Practice" = "Code")) %>% # remove excluded Practices
  left_join(GeographyCodes) %>%  # to match geography codes with geography names
  left_join(PeerGroups) %>% # add in Peer Groupings
  left_join(mergers, by = c("Practice" = "Code")) %>% # join to our list of merged Practices
  mutate(HSCP = (HSCPName),
         NHSBoard = (HBName),
         Practice = ifelse(is.na(`New Code`), Practice, `New Code`),
         PeerGroup = ifelse(!(is.na(`New Code`)), NA, PeerGroup),
         PeerGroup = case_when(Practice == "30985" ~ "23",
                               TRUE ~ PeerGroup)) %>%
  arrange(Practice) %>%
  group_by(Practice) %>%
  fill(NHSBoard, .direction = "updown") %>%  # fill in missing NHS Board names
  fill(HSCP, .direction = "updown") %>% # fill in missing HSCP names
  fill(Cluster, .direction = "updown") %>% # fill in blank GP Clusters
  fill(PeerGroup, .direction = "updown") %>%  # fill in missing PeerGroup
  group_by(Practice, Cluster, PeerGroup, HSCP, NHSBoard, FinancialYear, FinancialQuarter) %>% # select columns we want
  summarise(ListSize = sum(ListSize)) %>%
  spread(FinancialYear, ListSize) %>% # This part is to make sure that we have a list size for each time period for each practice
  group_by(Practice) %>% mutate(total=n()) %>%  # this gives us a count for how many times the Practice code appears. Used to check for duplicates.
  mutate(ListSizeDifference = Year2-Year1) # difference in list sizes between years. Investigate any large differences. Probably part of a merge...


######################################## Checkpoint - Duplicates ########################################

# Check for duplicates. Shouldn't be any.
LookupCluster %>%
  filter(total!=1)

# Check for missing clusters
missingcluster <- LookupCluster %>%
  filter(is.na(Cluster))
if(nrow(missingcluster)>0) warning('WARNING!!! There are rows with missing clusters')
# Missing clusters will need to be added into the Cluster Reference file, then reread in the file.
# Look at where they are on a map and what cluster the places around it are in

# Take a look at the Lookup and sense check it. Look into any big differences in list size between the years.
# view(LookupCluster)

#tidy up environment
rm("LookupCluster1", "LookupCluster2", "GeographyCodes", "PeerGroups", "Exclude", "mergers", "Clusters")


########################################  Lookup ########################################

# This is our final Lookup
LookupCluster <- LookupCluster %>% #
  select(-ListSizeDifference, -total) %>%
  gather(FinancialYear, ListSize, Year1, Year2) %>% # put all listsizes back into one column
  filter(!(is.na(ListSize))) %>% # remove cases that are missing list sizes
  filter(ListSize != 0) %>%
  mutate(FinancialYear = if_else(FinancialYear=="Year1", Year1, Year2)) %>%
  left_join(PracticeNames) %>% # this is from earlier in the syntax.
  ungroup()

######################################## Checkpoint - missing names ############
missingName <- LookupCluster %>%
  filter(is.na(PracticeName) & FinancialYear == Year2)

# If there are missing names, run the below code

names_lookup <- read_csv("/PHI_conf/PrimaryCare/NPCCD/Data/NPCCD Extracts/practices.csv") %>%
  mutate(Practice = as.character(Code),
         missing_name = toupper(Name)) %>%
  select(Practice, missing_name)

missingName <- missingName %>%
  select(Practice) %>%
  left_join(names_lookup)

LookupCluster <- LookupCluster %>%
  left_join(missingName) %>%
  mutate(PracticeName = ifelse(is.na(PracticeName), missing_name, PracticeName)) %>%
  select(-missing_name)

#### Check for missing names again ####
missingName <- LookupCluster %>%
  filter(is.na(PracticeName) & FinancialYear == Year2)
if(nrow(missingName)>0) warning('WARNING!!! There are rows with missing practice names')

# If missing names still exist, find them and add them in manually as below
# eg. LookupCluster$PracticeName[LookupCluster$Practice=="31738"] <- "FYVIE OLDMELDRUM MEDICAL GROUP"


######### Save #########
# This will act as our lookup and masterlist of all Practices to send the report to
# saveRDS(LookupCluster, file = paste0(Reference,"LookupCluster ", Year2, " FQ", FinancialQ, ".rds"))


######################################## Practice Geographies ########################################

# Save. Some Clusters and Peer Groups straddle HSCPs and NHS Boards so we need a separate geography lookup to calculate rates
PracticeGeography <- LookupCluster %>%
  # select(-Cluster, -PeerGroup)
  select(-PeerGroup)

# saveRDS(PracticeGeography, file = paste0(Reference,"PracticeGeography ", Year2, " FQ", FinancialQ, ".rds"))

