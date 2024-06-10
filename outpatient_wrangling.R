
# Outpatient_wrangling ----------------------------------------------------

##############################################  Outpatients ##############################################
Outpatients_raw <- read_rds(paste0(Data, "Outpatients_raw ", FinancialYear2, " Q", FinancialQ, ".rds"))

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
                                          ifelse(substr(Speciality, 1,2)=="C5", "ENT", Speciality)))))

# Create aggregated data for 'All specialities'
OutpatientsAllSpecialties <- Outpatients_tidy %>%
  mutate(Indicator = "All Specialities")

Outpatients_tidy <- Outpatients_tidy %>% # join selected specialties
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
Outpatients_tidy <- LookupCluster %>%
  full_join(Outpatients_tidy %>%
              select(FinancialYear, FinancialQuarter, AgeGroups, Indicator) %>%
              distinct) %>%
  left_join(Outpatients_tidy) %>%
  mutate(Numerator = ifelse(is.na(Numerator), 0, Numerator))%>%
  mutate(Domain = "Outpatients",
         Term = "Financial Quarter")

saveRDS(Outpatients_tidy, file=  paste0(Data,"Outpatients_tidy ", FinancialYear2, " Q", FinancialQ, ".rds"))

rm("Outpatients_raw", "OutpatientsAllAges", "OutpatientsAllSpecialties")



