

# Normal output -----------------------------------------------------------

Outpatients_tidy_summary <- Outpatients_tidy_indicator %>%
  group_by(Indicator, Practice, FinancialYear, FinancialQuarter, AgeGroups) %>%
  summarise(Numerator = n()) %>%  ungroup() %>%
  filter(!(is.na(Practice)))
# mutate(Indicator = ifelse(Indicator == "Dermatology", "New OP Rate per 1,000 Dermatology",
#                           ifelse(Indicator == "ENT", "New OP Rate per 1,000 ENT",
#                                  ifelse(Indicator == "Trauma", "New OP Rate per 1,000 Trauma",
#                                         ifelse(Indicator == "General Surgery","New OP Rate per 1,000 GenSurg" ,
#                                                "New OP Rate per 1,000 AllSpec")))))

Outpatients_tidy_summary2 <- Outpatients_tidy_summary |>
  select(FinancialYear, FinancialQuarter, AgeGroups, Indicator, Numerator)
# Use Lookup so a Practice will still have a case if the numerator is 0 or missing
# Then we match it up with the data


Outpatients_tidy <- LookupCluster %>%
  # full_join(Outpatients_tidy_summary %>%
  #             select(FinancialYear, FinancialQuarter, AgeGroups, Indicator) %>%
  #             distinct) %>%
  full_join(Outpatients_tidy_summary2) %>%
  left_join(Outpatients_tidy) %>%
  mutate(Numerator = ifelse(is.na(Numerator), 0, Numerator)) %>%
  mutate(Domain = "Outpatients",
         Term = "Financial Quarter")|>
  filter(Practice %in% practice_list)

# saveRDS(Outpatients_tidy, file=  paste0(Data,"Outpatients_tidy ", FinancialYear2, " Q", FinancialQ, ".rds"))

# rm("Outpatients_raw", "OutpatientsAllAges", "OutpatientsAllSpecialties")

# Outpatients_tidy <- read_rds(paste0(Data,"Outpatients_tidy ",FinancialYear2, " Q", FinancialQ ,".rds"))
LookupDemographics <- readRDS(paste0(Reference, "LookupDemographics 2021 Q4.rds"))

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
  bind_rows(OutpatientsGeographies) %>%
  group_by(FinancialYear, FinancialQuarter, AgeGroups,  Geography, GeographyName, Indicator, Term) %>%
  summarise(Numerator = sum(Numerator)) %>% ungroup() %>%
  left_join(LookupDemographics) %>% # to add on populations
  rename(Denominator = Population) %>%
  mutate(Rate =(1000*(Numerator/Denominator)),
         Domain = "Outpatients")

# rm("Outpatients_tidy", "OutpatientsGeographies")


rm(Outpatients_raw)

