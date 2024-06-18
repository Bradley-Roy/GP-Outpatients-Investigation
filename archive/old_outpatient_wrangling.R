
# Outpatients_raw <- read_rds(paste0(Data, "Outpatients_raw ", FinancialYear2, " Q", FinancialQ, ".rds"))


# Lookup Cluster ----------------------------------------------------------
# Oct2021 <- "https://www.opendata.nhs.scot/dataset/f23655c3-6e23-4103-a511-a80d998adb90/resource/14ef935c-a00d-4d39-9f04-228911ca5d0c/download/practice_contactdetails_oct2021-open-data.csv"
# Oct2020 <- "https://www.opendata.nhs.scot/dataset/f23655c3-6e23-4103-a511-a80d998adb90/resource/42391720-7dcb-48a2-8070-b9d63b246ac6/download/practice_contactdetails_oct2020-open-data.csv"

lookup_new <- "https://www.opendata.nhs.scot/dataset/f23655c3-6e23-4103-a511-a80d998adb90/resource/a160fa97-8a3c-429a-9683-3835cafe1701/download/practice_contactdetails_jul2023-open-data.csv"
lookup_old <- "https://www.opendata.nhs.scot/dataset/f23655c3-6e23-4103-a511-a80d998adb90/resource/5273d444-5a79-4fad-a518-119a368e2161/download/practice_contactdetails_jul2022-open-data.csv"

# read link into R as csv
LookupCluster1 <-  read_csv(lookup_old) # read link into R as csv
LookupCluster2 <-  read_csv(lookup_new)


##############################################  Cluster Lookups ##############################################
# Practice Names can change often so we extract these here and stick them on at the end.
PracticeNames <- LookupCluster2 %>% # add GP Practice names on at the end as these can change regularly and it creates duplicate cases
  mutate(Practice = as.character(PracticeCode),
         PracticeName = toupper(GPPracticeName)) %>%
  select(Practice, PracticeName)


# The open data is not always consistent with variable names so this will have to be looked at each time
# This is for Financial Year 1 (we set this at the start of the syntax)
LookupCluster1 <- LookupCluster1 %>%
  filter(PracticeCode %in% practice_list) %>%
  mutate(Practice = as.character(PracticeCode),
         FinancialYear = "Year1",
         FinancialQuarter = FinancialQ) %>% # we set this at the start of the syntax
  rename(ListSize = PracticeListSize) %>%
  left_join(mergers, by = c("Practice" = "Code")) %>% # join to our list of merged Practices. only need this for previous year
  mutate(PracticeCode = ifelse(is.na(`New Code`), Practice, `New Code`)) %>%
  group_by(Practice, FinancialYear, FinancialQuarter, HB, HSCP) %>%
  summarise(ListSize = sum(ListSize)) %>% ungroup() %>%
  mutate(HSCP = if_else(Practice == "26020", "S37000005", HSCP)) # recode this Practice. Part of a merge which isn't cooperating in the data

# This is Financial Year 2
LookupCluster2 <- LookupCluster2 %>%
  filter(PracticeCode %in% practice_list) %>%
  mutate(Practice = as.character(PracticeCode),
         FinancialYear = "Year2",
         FinancialQuarter = FinancialQ) %>%  # we set this at the start of the syntax
  rename(ListSize = PracticeListSize) %>%
  left_join(mergers, by = c("Practice" = "Code")) %>% # join to our list of merged Practices. only need this for previous year
  mutate(PracticeCode = ifelse(is.na(`New Code`), Practice, `New Code`)) %>%
  group_by(Practice, FinancialYear, FinancialQuarter, HB, HSCP) %>%
  summarise(ListSize = sum(ListSize)) %>% ungroup() %>%
  left_join(Clusters) # only latest year as these are the most recent


##############################################  Tidy up lookup ##############################################

# Tidy up lookup - fill in missing Peer groupings, HSCP, Cluster details
# Fix list sizes
LookupCluster <- LookupCluster2  %>%
  full_join(LookupCluster1) %>% #ookupCluster %>%
  anti_join(Exclude) %>% # remove excluded Practices
  left_join(GeographyCodes) %>%  # to match geography codes with geography names
  left_join(PeerGroups) %>% # add in Peer Groupings
  left_join(mergers, by = c("Practice" = "Code")) %>% # join to our list of merged Practices
  mutate(HSCP = (HSCPName),
         NHSBoard = (HBName),
         Practice = ifelse(is.na(`New Code`), Practice, `New Code`),
         PeerGroup = ifelse(!(is.na(`New Code`)), NA, `Peer Group`)) %>%
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


# Check - If any Practices are still missing Clusters these will need fixed.
missingcluster <- LookupCluster %>%
  filter(is.na(Cluster))
# Look at where they are on a map and what cluster the places around it are in
# LookupCluster$Cluster[LookupCluster$Practice=="10553"] <- "Kinross/Errol/Bridge of Earn"
# LookupCluster$Cluster[LookupCluster$Practice=="80631"] <- "Three Towns Cluster"
# LookupCluster$Cluster[LookupCluster$Practice=="80970"] <- "Kilmarnock Cluster"

##############################################  Lookup ##############################################

# This is our final Lookup
LookupCluster <- LookupCluster %>% #
  select(-ListSizeDifference, -total) %>%
  gather(FinancialYear, ListSize, Year1, Year2) %>% # put all listsizes back into one column
  filter(!(is.na(ListSize))) %>% # remove cases thare are missing list sizes
  mutate(FinancialYear = if_else(FinancialYear=="Year1", Year1, Year2)) %>%
  left_join(PracticeNames) %>% # this is from earlier in the syntax.
  mutate(PracticeName = case_when(Practice == "46108" ~ "MEADOWPARK SURGERY",
                                  Practice == "78043" ~ "BARCLAY MEDICAL PRACTICE-FAULDHOUSE",
                                  Practice == "21261" ~ "SCOONIE MEDICAL PRACTICE",
                                  Practice == "31649" ~ "ADEN HEALTH CENTRE",
                                  TRUE ~ PracticeName)) %>%
  filter(ListSize != 0)

missingName <- LookupCluster %>%
  filter(is.na(PracticeName) & FinancialYear == Year2) # if missing Practice Name for most recent time period - add in manually

# Save. This will act as our lookup and masterlist of all Practices to send the report to
# saveRDS(LookupCluster, file = paste0(Reference,"LookupCluster ", Year2, " FQ", FinancialQ, ".rds"))

# Outpatient_wrangling ----------------------------------------------------

############################################## Checkpoint - Duplicates ##############################################

# Check for duplicates. Shouldn't be any.
# LookupCluster %>%
#   filter(total!=1)

# read link into R as csv
LookupCluster1 <-  read_csv(lookup_old) |> # read link into R as csv
  filter(PracticeCode %in% practice_list)
LookupCluster2 <-  read_csv(lookup_new) |>
  filter(PracticeCode %in% practice_list)

# merged practices were dealt with in syntax 1
Outpatients_tidy <- Outpatients_raw2 %>%
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
                                          ifelse(substr(Speciality, 1,2)=="C5", "ENT", Speciality)))))

# Create aggregated data for 'All specialities'
OutpatientsAllSpecialties <- Outpatients_tidy %>%
  mutate(Indicator = "All Specialities")

Outpatients_cleaned <- Outpatients_tidy %>% # join selected specialties
  rbind(OutpatientsAllSpecialties) %>% # to all specialties
  filter(Indicator %in% c("All Specialities", "Dermatology", "General Surgery", "Trauma", "ENT")) %>% # filter for ones we want
  group_by(Indicator, Practice, FinancialYear, FinancialQuarter, AgeGroups) %>%
  summarise(Numerator = n()) %>%  ungroup() %>%
  filter(!(is.na(Practice))) %>%
  mutate(Indicator = ifelse(Indicator == "Dermatology", "New OP Rate per 1,000 Dermatology",
                            ifelse(Indicator == "ENT", "New OP Rate per 1,000 ENT",
                                   ifelse(Indicator == "Trauma", "New OP Rate per 1,000 Trauma",
                                          ifelse(Indicator == "General Surgery","New OP Rate per 1,000 GenSurg" ,
                                                 "New OP Rate per 1,000 AllSpec")))))


# Use Lookup so a Practice will still have a case if the numerator is 0 or missing
# Then we match it up with the data
Outpatients_final <- LookupCluster %>%
  full_join(Outpatients_cleaned %>%
              select(FinancialYear, FinancialQuarter, AgeGroups, Indicator) %>%
              distinct) %>%
  left_join(Outpatients_cleaned) %>%
  mutate(Numerator = ifelse(is.na(Numerator), 0, Numerator))%>%
  mutate(Domain = "Outpatients",
         Term = "Financial Quarter")

# saveRDS(Outpatients_tidy, file=  paste0(Data,"Outpatients_tidy ", FinancialYear2, " Q", FinancialQ, ".rds"))

rm("Outpatients_raw", "OutpatientsAllAges", "OutpatientsAllSpecialties")



