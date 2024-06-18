


# ##############################################  Out of Hours ##############################################
# # This comes from Boxi report OOH. See SOP.
# OOHYear1 <- read_csv(unzip((paste0(Data, "OOH", FinancialYear1, "Q", FinancialQ, ".zip"))))
# OOHYear2 <- read_csv(unzip((paste0(Data, "OOH", FinancialYear2, "Q", FinancialQ, ".zip"))))
#
# # CHECK - check to see that the Boxi reports appear complete. There has been previous examples of Boxi reports returning incomplete datasets.
# # The 2 dataframes below should show in the region of 1850 records and 200,000 cases.
# OOH_check_Year1 <- OOHYear1 %>%
#   group_by(FinancialQuarter) %>%
#   summarise(rows=n(),
#             Cases=sum(OOHCases))
#
# OOH_check_Year2 <- OOHYear2 %>%
#   group_by(FinancialQuarter) %>%
#   summarise(rows=n(),
#             Cases=sum(OOHCases))
#
# # Clean data
# OOH_tidy <- OOHYear1 %>%
#   rbind(OOHYear2) %>% # join two Financial Years data together
#   mutate(Practice = as.character(Practice),
#          FinancialYear = as.character(FinancialYear),
#          FinancialQuarter = as.character(FinancialQuarter),
#          OOHCases = as.numeric(OOHCases),
#          AgeGroups = ifelse(AgeBand == "0-64", "Under 65", AgeBand)) %>%
#   left_join(mergers, by = c("Practice" = "Code")) %>%  # join to mergers lookup
#   mutate(Practice = ifelse(is.na(`New Code`), Practice, `New Code`)) %>% # Update Practice Codes
#   select(-AgeBand, -`New Code`) %>%
#   anti_join(Exclude, by = c("Practice" = "Code"))
#
# # New dataframe for "All Ages"
# OOHAllAges <- OOH_tidy %>%
#   mutate(AgeGroups = "All Ages")
#
# # Use Lookup so a Practice will still have a case if the patient numbers are 0 or missing
# # Then we match it up with the data
# OOH <- LookupCluster %>% # everything in LookupCluster should have a case
#   full_join(OOH_tidy %>% rbind(OOHAllAges) %>% #creates all Year, Quarter, Age group combinations and joins to LookupCluster
#               select(FinancialYear, FinancialQuarter, AgeGroups) %>%
#               distinct(), relationship = "many-to-many") %>%
#   full_join(OOH_tidy) %>% ungroup() %>%  # join on data
#   rbind(OOHAllAges) %>% # join all ages to agegroup data
#   group_by(FinancialYear, FinancialQuarter, Practice, AgeGroups) %>%
#   mutate(OOHCases = ifelse(is.na(OOHCases), 0, OOHCases)) %>% # if NA cases then change to 0
#   summarise(Numerator = sum(OOHCases)) %>% ungroup() %>%   # aggregate OOH Cases - need this for "All Ages" and merges
#   mutate(Term = "Financial Quarter",
#          Domain = "OOH",
#          Indicator = "Out of hours attendance rate per 1000 population")
#
# # saveRDS(OOH, file=paste0(Data, "OOH ", FinancialYear2, " Q", FinancialQ, ".rds")) # save data to the folder.
#
# rm("OOHYear1", "OOHYear2", "OOHAllAges", "OOH_tidy", "OOH_check_Year1", "OOH_check_Year2")
#
# ##############################################  A&E ##############################################
# # this is from the boxi report A&E. See SOP.
# AEYear1 <- read_csv(unzip((paste0(Data, "AE", FinancialYear1, "Q", FinancialQ, ".zip"))))
# AEYear2 <- read_csv(unzip((paste0(Data, "AE", FinancialYear2, "Q", FinancialQ, ".zip"))))
#
# # CHECK - check to see that the Boxi reports appear complete. There has been previous examples of Boxi reports returning incomplete datasets.
# # The 2 dataframes below should show in the region of 1950 records and 340,000 cases.
# AE_check_Year1 <- AEYear1 %>%
#   group_by(FinancialQuarter) %>%
#   summarise(rows=n(),
#             Cases=sum(Attendances))
#
# AE_check_Year2 <- AEYear2 %>%
#   group_by(FinancialQuarter) %>%
#   summarise(rows=n(),
#             Cases=sum(Attendances))
#
# # Clean data
# AE_tidy <- AEYear1 %>%
#   rbind(AEYear2) %>% # join Financial Years data together
#   mutate(Practice = as.character(Practice),
#          AgeBand = as.character(AgeBand),
#          Admissions =  ifelse(is.na(Admissions), 0, Admissions), # change missing values to 0
#          GPReferrals = ifelse(is.na(GPReferrals), 0, GPReferrals), # change missing valeu to 0
#          FinancialYear = as.character(FinancialYear),
#          FinancialQuarter = as.character(FinancialQuarter),
#          AgeGroups = as.character(ifelse(AgeBand=="0-64", "Under 65", "65+"))) %>%
#   left_join(mergers, by = c("Practice" = "Code")) %>%  # join to mergers lookup
#   mutate(Practice = ifelse(is.na(`New Code`), Practice, `New Code`)) %>% # Update Practice Codes
#   anti_join(Exclude, by = c("Practice" = "Code")) # remove atypical practices
#
# # Create new dataframe for all age groups
# AEAllAges <- AE_tidy %>%
#   mutate(AgeGroups = "All Ages")
#
# # Use Lookup so a Practice will still have a case if the patient numbers are 0 or missing
# # Then we match it up with the data
# AE <-LookupCluster %>% # everything in LookupCluster should have a case
#   full_join(AE_tidy %>% rbind(AEAllAges) %>% #creates all Year, Quarter, Age group combinations and joins to LookupCluster
#               select(FinancialYear, FinancialQuarter, AgeGroups) %>%
#               distinct(), relationship = "many-to-many") %>%
#   left_join(AE_tidy) %>% ungroup() %>% # join on data
#   rbind(AEAllAges) %>% # join all ages to age group data
#   mutate(Attendances = ifelse(is.na(Attendances), 0, Attendances), # set missing values to 0
#          Admissions = ifelse(is.na(Admissions), 0, Admissions),
#          GPReferrals = ifelse(is.na(GPReferrals), 0, GPReferrals)) %>%
#   group_by(FinancialYear, FinancialQuarter, Practice, AgeGroups) %>%
#   summarise(Attendances = sum(Attendances), # aggregate Attendances, Admissions and GP Referrals. This is for "all ages" and merges
#             Admissions = sum(Admissions),
#             GPReferrals = sum(GPReferrals)) %>% ungroup() %>%
#   filter(!(is.na(Practice))) %>%  # filter to cases that have a Pratice Code # these will be dummy practices
#   mutate(Domain = "A & E",
#          Term = "Financial Quarter")
#
# # saveRDS(AE, file=paste0(Data, "AE ", FinancialYear2, " Q", FinancialQ, ".rds")) # save data to the folder.
#
# rm("AEYear1", "AEYear2", "AEAllAges", "AE_tidy", "AE_check_Year1", "AE_check_Year2")
#
#
# ##############################################  Prescribing - BNF ##############################################
# # This data comes from the Boxi Report 'BNF'. See SOP
# BNFYear1 <- read_csv(unzip((paste0(Data, "BNF", FinancialYear1, "Q", FinancialQ, ".zip"))))
# BNFYear2 <- read_csv(unzip((paste0(Data, "BNF", FinancialYear2, "Q", FinancialQ, ".zip"))))
#
# #### 02/22 The csv file has the ChapterorSection in the format 1, 2, 3, 401. But when read in it's 01, 02, 0401
# BNFYear1 <- BNFYear1 %>%
#   mutate(ChapterorSection = as.numeric(ChapterorSection))
# BNFYear2 <- BNFYear2 %>%
#   mutate(ChapterorSection = as.numeric(ChapterorSection))
#
# # CHECK - check to see that the Boxi reports appear complete. There has been previous examples of Boxi reports returning incomplete datasets.
# # The 2 dataframes below should show in the region of 8300 records and 15m cases.
# BNF_check_Year1 <- BNFYear1 %>%
#   group_by(FinancialQuarter) %>%
#   summarise(rows=n(),
#             Cases=sum(Items))
#
# BNF_check_Year2 <- BNFYear2 %>%
#   group_by(FinancialQuarter) %>%
#   summarise(rows=n(),
#             Cases=sum(Items))
#
# # Join both years together, deal with merged Practices.
# BNF_tidy <- BNFYear1 %>%
#   rbind(BNFYear2) %>% # join Financial Years data together
#   mutate(Practice = as.character(Practice), # formatting
#          ChapterorSection = as.character(ChapterorSection),
#          FinancialYear = as.character(FinancialYear),
#          FinancialQuarter = as.character(FinancialQuarter),
#          HBWeightedListSize = ifelse(is.na(HBWeightedListSize),0, HBWeightedListSize)) %>%
#   left_join(mergers, by = c("Practice" = "Code")) %>%  # join to mergers lookup
#   mutate(Practice = ifelse(is.na(`New Code`), Practice, `New Code`)) %>% # Update Practice Codes
#   anti_join(Exclude, by = c("Practice" = "Code")) %>%
#   group_by(FinancialYear, FinancialQuarter, ChapterorSection, Practice) %>%
#   summarise(GIC = sum(GIC), # aggregate GIC, and items
#             Items = sum(Items),
#             HBWeightedListSize = sum(HBWeightedListSize)) %>% ungroup()
#
# # matches data with our masterlist and fills in missing values
# BNF_tidy <- LookupClusterLS %>%
#   full_join(BNF_tidy %>% # use our lookup masterlist to get all Practices and indicators
#               select(FinancialYear, FinancialQuarter, ChapterorSection) %>% distinct) %>%
#   left_join(BNF_tidy) %>% # join on data
#   mutate(Items = ifelse(is.na(Items), 0, Items), # set missing values to 0
#          GIC = ifelse(is.na(GIC), 0, GIC), # set missing values to 0
#          HBWeightedListSize = ifelse(HBWeightedListSize==0, NA, HBWeightedListSize))# set 0 values to missing. Fill in weighted list sizes in next section
#
# # This section is fairly complicated. It fixes weighted list sizes.
# # the issues are mostly merged Practices and missing weighted list sizes.
# # merged practice list sizes are dealt with in the usual way
# # missing weighted list sizes take the previous/next years list size,
# # if still missing it takes the regular list size
# BNF_tidy <- BNF_tidy %>%
#   group_by(Practice, FinancialYear, FinancialQuarter) %>%
#   fill(HBWeightedListSize, .direction = c("updown")) %>%  # fills in weighted list sizes for merged Practices and for practices that have 0 prescribing
#   group_by(Practice) %>% # this will fill in missing list sizes with previous or next years list size
#   fill(HBWeightedListSize, .direction = c("updown")) %>%  # fills in weighted list sizes for merged Practices
#   group_by(FinancialYear, FinancialQuarter, Practice, ChapterorSection, ListSize, HBWeightedListSize) %>%
#   summarise(GIC = sum(GIC), # aggregate GIC, and items
#             Items = sum(Items)) %>%
#   group_by(FinancialYear, FinancialQuarter, Practice, ChapterorSection) %>%
#   summarise(GIC = sum(GIC), # aggregate GIC, and items
#             Items = sum(Items),
#             ListSize = sum(ListSize),
#             HBWeightedListSize = sum(HBWeightedListSize))%>% ungroup() %>%
#   mutate(HBWeightedListSize = ifelse(is.na(HBWeightedListSize), ListSize, HBWeightedListSize)) # if weighted list size is still missing use regular list size
#
# # formatting data and renaming indicators
# BNF <- BNF_tidy %>%
#   gather(GIC, Items, key=NumeratorDescription, value = Numerator) %>%   # puts GIC and Items into single column named "NumeratorDescription"
#   mutate(Indicator = ifelse(ChapterorSection=="1", "Gastro-intestinal", # Add names for the BNF codes
#                             ifelse(ChapterorSection=="2", "Cardiovascular",
#                                    ifelse(ChapterorSection=="3", "Respiratory",
#                                           ifelse(ChapterorSection=="401", "Hypnotics and anxiolytics",
#                                                  ifelse(ChapterorSection=="402", "Antipsychotics and related drugs",
#                                                         ifelse(ChapterorSection=="403", "Antidepressants",
#                                                                ifelse(ChapterorSection=="5", "Antibacterial drugs",
#                                                                       ifelse(ChapterorSection=="601", "Drugs used in diabetes", "missing")))))))),
#          NumeratorDescription = ifelse(NumeratorDescription=="GIC", " cost/1,000 HB wtd list size", # recode GIC and Items to be more descriptive
#                                        ifelse(NumeratorDescription=="Items", " items/1,000 HB wtd list size", "missing"))) %>%
#   mutate(Indicator = paste0(Indicator, NumeratorDescription), # recode Indicator with full description
#          Term = "Financial Quarter",
#          Domain = "Prescribing",
#          AgeGroups = "All Ages") %>%
#   rename(Denominator = HBWeightedListSize) %>% # rename Weighted List Size column to "Denominator"
#   select(-ChapterorSection, -NumeratorDescription, -ListSize) # remove these columns
#
# # saveRDS(BNF, file=paste0(Data, "BNF ", FinancialYear2, " Q", FinancialQ, ".rds")) # save data to the folder.
#
# rm("BNFYear1", "BNFYear2", "BNF_tidy", "BNF_check_Year1", "BNF_check_Year2")
#
# ##############################################  Prescribing - NTI01c Lidocaine plasters â GIC per 1000 patients per day ##############################################
# NTI01cYear1 <- read_csv(unzip((paste0(Data, "NTI01c", FinancialYear1, "Q", FinancialQ, ".zip"))))
# NTI01cYear2 <- read_csv(unzip((paste0(Data, "NTI01c", FinancialYear2, "Q", FinancialQ, ".zip"))))
#
# # CHECK - check to see that the Boxi reports appear complete. There has been previous examples of Boxi reports returning incomplete datasets.
# # The 2 dataframes below should show in the region of 900 records and 3m cases.
# NTI_check_Year1 <- NTI01cYear1 %>%
#   group_by(FinancialQuarter) %>%
#   summarise(rows=n(),
#             Cases=sum(GIC, na.rm=TRUE))
#
# NTI_check_Year2 <- NTI01cYear2 %>%
#   group_by(FinancialQuarter) %>%
#   summarise(rows=n(),
#             Cases=sum(GIC, na.rm=TRUE))
#
# # Clean data
# NTI01c_tidy <- NTI01cYear1 %>%
#   rbind(NTI01cYear2) %>% # join Financial years together
#   mutate(Practice = as.character(Practice),
#          Domain = "Prescribing",
#          AgeGroups = "All Ages",
#          FinancialQuarter = as.character(FinancialQuarter),
#          FinancialYear = as.character(ifelse (substr(FinancialYear, 1,3)=="YTD", substr(FinancialYear,6,9), substr(FinancialYear,1,4)))) %>% # formatting  %>% # formatting
#   left_join(mergers, by = c("Practice" = "Code")) %>%  # join to mergers lookup
#   mutate(Practice = ifelse(is.na(`New Code`), Practice, `New Code`)) %>% # Update Practice Codes
#   group_by(FinancialYear, FinancialQuarter, Practice, Indicator, AgeGroups,  Term, Domain) %>%
#   summarise(Numerator = sum(GIC)) %>% # aggregate GIC, and List Size
#   ungroup() %>%
#   anti_join(Exclude, by = c("Practice" = "Code"))
#
# # Use Lookup so a Practice will still have a case if the patient numbers are 0 or missing
# # Then we match it up with the data
# NTI01c <- LookupCluster %>%
#   full_join(NTI01c_tidy %>%
#               select(FinancialYear, FinancialQuarter, AgeGroups, Term, Domain) %>%
#               distinct) %>% # use our lookup masterlist
#   left_join(NTI01c_tidy) %>% # join on data
#   mutate(Numerator = ifelse(is.na(Numerator), 0, Numerator), # set missing values to 0
#          Indicator = "NTI01c") %>%
#   left_join(LookupClusterLS) %>% # add on list sizes
#   rename(Denominator = ListSize)
#
# # saveRDS(NTI01c, file=paste0(Data, "NTI01c", FinancialYear2, "Q", FinancialQ, ".rds")) # save data to the folder.
#
# rm("NTI01cYear1", "NTI01cYear2", "NTI01c_tidy", "NTI_check_Year1", "NTI_check_Year2")
#
# ##############################################  Prescribing - NTI05b Number of Antibiotics items per 1000 list size per day ##############################################
# NTI05bYear1 <- read_csv(unzip((paste0(Data, "NTI05b", FinancialYear1, "Q", FinancialQ, ".zip"))))
# NTI05bYear2 <- read_csv(unzip((paste0(Data, "NTI05b", FinancialYear2, "Q", FinancialQ, ".zip"))))
#
# # CHECK - check to see that the Boxi reports appear complete. There has been previous examples of Boxi reports returning incomplete datasets.
# # The 2 dataframes below should show in the region of 900 records and 700,000 cases.
# NTI_check_Year1 <- NTI05bYear1 %>%
#   group_by(FinancialQuarter) %>%
#   summarise(rows=n(),
#             Cases=sum(Items, na.rm=TRUE))
#
# NTI_check_Year2 <- NTI05bYear2 %>%
#   group_by(FinancialQuarter) %>%
#   summarise(rows=n(),
#             Cases=sum(Items, na.rm=TRUE))
#
# # Clean data
# NTI05b_tidy <- NTI05bYear1 %>%
#   rbind(NTI05bYear2) %>%
#   mutate(Practice = as.character(Practice),
#          Domain = "Prescribing",
#          AgeGroups = "All Ages",
#          FinancialQuarter = as.character(FinancialQuarter),
#          FinancialYear = as.character(ifelse (substr(FinancialYear, 1,3)=="YTD", substr(FinancialYear,6,9), substr(FinancialYear,1,4)))) %>% # formatting
#   left_join(mergers, by = c("Practice" = "Code")) %>%  # join to mergers lookup
#   mutate(Practice = ifelse(is.na(`New Code`), Practice, `New Code`)) %>% # Update Practice Codes
#   group_by(FinancialYear, FinancialQuarter, Practice, Indicator, AgeGroups, Term, Domain) %>%
#   summarise(Numerator = sum(Items))%>% # aggregate GIC, and List Size
#   ungroup() %>%  anti_join(Exclude, by = c("Practice" = "Code"))
#
# # Use Lookup so a Practice will still have a case if the patient numbers are 0
# # Then we match it up with the data
# NTI05b <-LookupCluster %>%
#   full_join(NTI05b_tidy %>%
#               select(FinancialYear, FinancialQuarter, AgeGroups, Term, Domain) %>%
#               distinct) %>% # use our lookup masterlist
#   left_join(NTI05b_tidy) %>% # join on data
#   mutate(Numerator = ifelse(is.na(Numerator), 0, Numerator), # set missing values to 0
#          Indicator = "NTI05b") %>%
#   left_join(LookupClusterLS) %>%
#   rename(Denominator = ListSize)
#
#
# # saveRDS(NTI05b, file=paste0(Data, "NTI05b", FinancialYear2, "Q", FinancialQ, ".rds")) # save data to the folder.
#
# rm("NTI05bYear1", "NTI05bYear2", "NTI05b_tidy", "NTI_check_Year1", "NTI_check_Year2")
#
# ##############################################  Prescribing - NTI08a Number of people aged â¥65 years co-prescribed a NSAID and an ACE inhibitor/angiotensin receptor blocker and a diuretic ##############################################
# # Number of people aged â¥65 years co-prescribed a NSAID and an ACE inhibitor/angiotensin receptor blocker and a diuretic
# # as a percentage of all people aged â¥65 years prescribed an ACE inhibitor/angiotensin receptor blocker and a diuretic
# NTI08aYear1 <- read_csv(unzip((paste0(Data, "NTI08a", FinancialYear1, "Q", FinancialQ, ".zip"))))
# NTI08aYear2 <- read_csv(unzip((paste0(Data, "NTI08a", FinancialYear2, "Q", FinancialQ, ".zip"))))
#
# # CHECK - check to see that the Boxi reports appear complete. There has been previous examples of Boxi reports returning incomplete datasets.
# # The 2 dataframes below should show in the region of 600 records and 800,000 cases.
# NTI_check_Year1 <- NTI08aYear1 %>%
#   group_by(Indicator) %>%
#   summarise(rows=n(),
#             Cases=sum(Patientsaged65registeredwiththepractice, na.rm=TRUE))
#
# NTI_check_Year2 <- NTI08aYear2 %>%
#   group_by(Indicator) %>%
#   summarise(rows=n(),
#             Cases=sum(Patientsaged65registeredwiththepractice, na.rm=TRUE))
#
# # Clean data
# NTI08a_tidy <- NTI08aYear1 %>%
#   rbind(NTI08aYear2) %>% # join years together
#   fill(FinancialYear,
#        FinancialQuarter) %>%
#   mutate(Practice = as.character(Practice),
#          FinancialYear = as.character(FinancialYear),
#          FinancialQuarter = as.character(FinancialQuarter),
#          Domain = "Prescribing",
#          AgeGroups = "65+") %>% # formatting
#   left_join(mergers, by = c("Practice" = "Code")) %>%  # join to mergers lookup
#   mutate(Practice = ifelse(is.na(`New Code`), Practice, `New Code`)) %>% # Update Practice Codes
#   group_by(FinancialYear, FinancialQuarter, Practice, Indicator, AgeGroups, Term, Domain) %>%
#   summarise(Numerator = sum(Patientsaged65prescribedNSAIDandanACEinhibitorangioten),
#             Denominator = sum(Patientsaged65registeredwiththepractice))%>% # aggregate Numerator and Denominator
#   ungroup() %>%  anti_join(Exclude, by = c("Practice" = "Code"))
#
# # Use Lookup so a Practice will still have a case if the patient numbers are 0
# # Then we match it up with the data
# NTI08a <- LookupCluster %>%
#   full_join(NTI08a_tidy %>%
#               select(FinancialYear, FinancialQuarter, AgeGroups, Term, Domain) %>%
#               distinct) %>% # use our lookup masterlist
#   left_join(NTI08a_tidy) %>% # join on data
#   mutate(Numerator = ifelse(is.na(Numerator), 0, Numerator), # set missing values to 0
#          Denominator = ifelse(is.na(Denominator), 0, Denominator),
#          Indicator = "NTI08a")
#
# # saveRDS(NTI08a, file=paste0(Data, "NTI08a", FinancialYear2, "Q", FinancialQ, ".rds")) # save data to the folder.
#
# rm("NTI08aYear1", "NTI08aYear2", "NTI08a_tidy", "NTI_check_Year1", "NTI_check_Year2")
#
# ##############################################  Prescribing - NTI08b Number of people prescribed >4 antibiotics in a year as a percentage of people who received any antibiotic item in a year ##############################################
# NTI08bYear1 <- read_csv(unzip((paste0(Data, "NTI08b", FinancialYear1, "Q", FinancialQ, ".zip"))))
# NTI08bYear2 <- read_csv(unzip((paste0(Data, "NTI08b", FinancialYear2, "Q", FinancialQ, ".zip"))))
#
# # Recode Financial Years and Financial Quarters to be the last FY and FQ found in the data
# # This will be on the 4 row as there are 4 quarters in the data.
# NTI08bYear1$FinancialYear = NTI08bYear1$FinancialYear[4]
# NTI08bYear2$FinancialYear = NTI08bYear2$FinancialYear[4]
# NTI08bYear1$FinancialQuarter = NTI08bYear1$FinancialQuarter[4]
# NTI08bYear2$FinancialQuarter = NTI08bYear2$FinancialQuarter[4]
#
# # CHECK - check to see that the Boxi reports appear complete. There has been previous examples of Boxi reports returning incomplete datasets.
# # The 2 dataframes below should show in the region of 1000 records and 1.3m cases.
# NTI_check_Year1 <- NTI08bYear1 %>%
#   group_by(Indicator) %>%
#   summarise(rows=n(),
#             Cases=sum(PatientsDispensedAbx, na.rm=TRUE))
#
# NTI_check_Year2 <- NTI08bYear2 %>%
#   group_by(Indicator) %>%
#   summarise(rows=n(),
#             Cases=sum(PatientsDispensedAbx, na.rm=TRUE))
#
# # Clean data
# NTI08b_tidy <- NTI08bYear1 %>%
#   rbind(NTI08bYear2) %>%
#   mutate(Practice = as.character(Practice),
#          FinancialYear = as.character(FinancialYear),
#          FinancialQuarter = as.character(FinancialQuarter),
#          Domain = "Prescribing",
#          AgeGroups = "All Ages") %>% # formatting
#   left_join(mergers, by = c("Practice" = "Code")) %>%  # join to mergers lookup
#   mutate(Practice = ifelse(is.na(`New Code`), Practice, `New Code`)) %>% # Update Practice Codes
#   group_by(FinancialYear, FinancialQuarter, Practice, Indicator, AgeGroups, Term, Domain) %>%
#   summarise(Numerator = sum(PatientsDispensedover4Abx),
#             Denominator = sum(PatientsDispensedAbx))%>% # aggregate Numerator and Denominator
#   ungroup() %>% anti_join(Exclude, by = c("Practice" = "Code"))
#
# # Use Lookup so a Practice will still have a case if the patient numbers are 0
# # Then we match it up with the data
# NTI08b <- LookupCluster %>%
#   full_join(NTI08b_tidy %>%
#               select(FinancialYear, FinancialQuarter, AgeGroups, Term, Domain) %>%
#               distinct ) %>% # use our lookup masterlist
#   left_join(NTI08b_tidy) %>% # join on data
#   mutate(Numerator = ifelse(is.na(Numerator), 0, Numerator), # set missing values to 0
#          Denominator = ifelse(is.na(Denominator), 0, Denominator),
#          Indicator = "NTI08b")
#
# # saveRDS(NTI08b, file=paste0(Data, "NTI08b", FinancialYear2, "Q", FinancialQ, ".rds")) # save data to the folder.
#
# rm("NTI08bYear1", "NTI08bYear2", "NTI08b_tidy", "NTI_check_Year1", "NTI_check_Year2")
#
# ##############################################  Prescribing - NTI08d Number of people prescribed 3+ categories of antidiabetic drugs ##############################################
# NTI08dYear1 <- read_csv(unzip((paste0(Data, "NTI08d", FinancialYear1, "Q", FinancialQ, ".zip"))))
# NTI08dYear2 <- read_csv(unzip((paste0(Data, "NTI08d", FinancialYear2, "Q", FinancialQ, ".zip"))))
#
# # CHECK - check to see that the Boxi reports appear complete. There has been previous examples of Boxi reports returning incomplete datasets.
# # The 2 dataframes below should show in the region of 1000 records and 200,000 cases.
# NTI_check_Year1 <- NTI08dYear1 %>%
#   group_by(Indicator) %>%
#   summarise(rows=n(),
#             Cases=sum(Patientsprescribedantidiabeticdrugs, na.rm=TRUE))
#
# NTI_check_Year2 <- NTI08dYear2 %>%
#   group_by(Indicator) %>%
#   summarise(rows=n(),
#             Cases=sum(Patientsprescribedantidiabeticdrugs, na.rm=TRUE))
#
# # Clean data
# NTI08d_tidy <- NTI08dYear1 %>%
#   rbind(NTI08dYear2) %>%
#   mutate(Practice = as.character(Practice),
#          FinancialYear = as.character(FinancialYear),
#          FinancialQuarter = as.character(FinancialQuarter),
#          Domain = "Prescribing",
#          AgeGroups = "All Ages") %>% # formatting
#   left_join(mergers, by = c("Practice" = "Code")) %>%  # join to mergers lookup
#   mutate(Practice = ifelse(is.na(`New Code`), Practice, `New Code`)) %>% # Update Practice Codes
#   group_by(FinancialYear, FinancialQuarter, Practice, Indicator, AgeGroups, Term, Domain) %>%
#   summarise(Numerator = sum(Patientsprescribed3ormorecategoriesofantidiabeticdrugs),
#             Denominator = sum(Patientsprescribedantidiabeticdrugs))%>% # aggregate Numerator and Denominator
#   ungroup() %>%
#   anti_join(Exclude, by = c("Practice" = "Code"))
#
#
# # Use Lookup so a Practice will still have a case if the patient numbers are 0
# # Then we match it up with the data
# NTI08d <- LookupCluster  %>%
#   full_join(NTI08d_tidy %>%
#               select(FinancialYear, FinancialQuarter, AgeGroups, Term, Domain) %>%
#               distinct) %>% # use our lookup masterlist
#   left_join(NTI08d_tidy) %>% # join on data
#   mutate(Numerator = ifelse(is.na(Numerator), 0, Numerator), # set missing values to 0
#          Denominator = ifelse(is.na(Denominator), 0, Denominator),
#          Indicator = "NTI08d")
#
#
# # saveRDS(NTI08d, file=paste0(Data, "NTI08d", FinancialYear2, "Q", FinancialQ, ".rds")) # save data to the folder.
#
# rm("NTI08dYear1", "NTI08dYear2", "NTI08d_tidy", "NTI_check_Year1", "NTI_check_Year2")
#
# ##############################################  Prescribing - NTI08i Number of people aged â¥ 75 years prescribed sulfonylureas ##############################################
# # Number of people aged â¥ 75 years prescribed sulfonylureas
# # as a percentage of all people aged â¥ 75 years prescribed an anti-diabetic drugs
# NTI08iYear1 <- read_csv(unzip((paste0(Data, "NTI08i", FinancialYear1, "Q", FinancialQ, ".zip"))))
# NTI08iYear2 <- read_csv(unzip((paste0(Data, "NTI08i", FinancialYear2, "Q", FinancialQ, ".zip"))))
#
# # CHECK - check to see that the Boxi reports appear complete. There has been previous examples of Boxi reports returning incomplete datasets.
# # The 2 dataframes below should show in the region of 950 records and 50,000 cases.
# NTI_check_Year1 <- NTI08iYear1 %>%
#   group_by(Indicator) %>%
#   summarise(rows=n(),
#             Cases=sum(Patientsprescribedantidiabeticdrugs, na.rm=TRUE))
#
# NTI_check_Year2 <- NTI08iYear2 %>%
#   group_by(Indicator) %>%
#   summarise(rows=n(),
#             Cases=sum(Patientsprescribedantidiabeticdrugs, na.rm=TRUE))
#
# # Clean data
# NTI08i_tidy <- NTI08iYear1 %>%
#   rbind(NTI08iYear2) %>%
#   mutate(Practice = as.character(Practice),
#          FinancialYear = as.character(FinancialYear),
#          FinancialQuarter = as.character(FinancialQuarter),
#          Domain = "Prescribing",
#          AgeGroups = "75+") %>% # formatting
#   left_join(mergers, by = c("Practice" = "Code")) %>%  # join to mergers lookup
#   mutate(Practice = ifelse(is.na(`New Code`), Practice, `New Code`)) %>% # Update Practice Codes
#   group_by(FinancialYear, FinancialQuarter, Practice, Indicator, AgeGroups, Term, Domain) %>%
#   summarise(Numerator = sum(Patientsprescribedsulfonylureas),
#             Denominator = sum(Patientsprescribedantidiabeticdrugs))%>% # aggregate Numerator and Denominator
#   ungroup() %>%  anti_join(Exclude, by = c("Practice" = "Code"))
#
# # Use Lookup so a Practice will still have a case if the patient numbers are 0
# # Then we match it up with the data
# NTI08i <- LookupCluster  %>%
#   full_join(NTI08i_tidy %>%
#               select(FinancialYear, FinancialQuarter, AgeGroups, Term, Domain) %>%
#               distinct) %>% # use our lookup masterlist
#   left_join(NTI08i_tidy) %>% # join on data
#   mutate(Numerator = ifelse(is.na(Numerator), 0, Numerator), # set missing values to 0
#          Denominator = ifelse(is.na(Denominator), 0, Denominator),
#          Indicator = "NTI08i")
#
# # saveRDS(NTI08i, file=paste0(Data, "NTI08i", FinancialYear2, "Q", FinancialQ, ".rds")) # save data to the folder.
#
# rm("NTI08iYear1", "NTI08iYear2", "NTI08i_tidy", "NTI_check_Year1", "NTI_check_Year2")
#
# ##############################################  Prescribing - NTI09b Number of people prescribed mucolytics long term (> 2 years) per 1000 list Size ##############################################
#
# NTI09bYear1 <- read_csv(unzip((paste0(Data, "NTI09b", FinancialYear1, "Q", FinancialQ, ".zip"))))
# NTI09bYear2 <- read_csv(unzip((paste0(Data, "NTI09b", FinancialYear2, "Q", FinancialQ, ".zip"))))
#
# # Recode Financial Years and Financial Quarters to be the last FY and FQ found in the data
# # This will be on the 8 row as there are 8 quarters in the data.
# NTI09bYear1$FinancialYear = NTI09bYear1$FinancialYear[8]
# NTI09bYear2$FinancialYear = NTI09bYear2$FinancialYear[8]
# NTI09bYear1$FinancialQuarter = NTI09bYear1$FinancialQuarter[8]
# NTI09bYear2$FinancialQuarter = NTI09bYear2$FinancialQuarter[8]
#
# # CHECK - check to see that the Boxi reports appear complete. There has been previous examples of Boxi reports returning incomplete datasets.
# # The 2 dataframes below should show in the region of 1200 records and 10,000 cases.
# NTI_check_Year1 <- NTI09bYear1 %>%
#   group_by(Indicator) %>%
#   summarise(rows=n(),
#             Cases=sum(Numberofpatientsonlongtermmucolytics, na.rm=TRUE))
#
# NTI_check_Year2 <- NTI09bYear2 %>%
#   group_by(Indicator) %>%
#   summarise(rows=n(),
#             Cases=sum(Numberofpatientsonlongtermmucolytics, na.rm=TRUE))
#
#
# NTI09b_tidy <- NTI09bYear1 %>%
#   rbind(NTI09bYear2) %>%
#   mutate(Practice = as.character(Practice),
#          FinancialYear = as.character(FinancialYear),
#          FinancialQuarter = as.character(FinancialQuarter),
#          Domain = "Prescribing",
#          AgeGroups = "All Ages") %>% # formatting
#   left_join(mergers, by = c("Practice" = "Code")) %>%  # join to mergers lookup
#   mutate(Practice = ifelse(is.na(`New Code`), Practice, `New Code`)) %>% # Update Practice Codes # I don't think this works for this NTI??
#   group_by(FinancialYear, FinancialQuarter, Practice, Indicator, AgeGroups, Term, Domain) %>%
#   summarise(Numerator = sum(Numberofpatientsonlongtermmucolytics))%>% # aggregate Numerator and Denominator
#   ungroup() %>%  anti_join(Exclude, by = c("Practice" = "Code"))
#
# # Use Lookup so a Practice will still have a case if the patient numbers are 0
# # Then we match it up with the data
# NTI09b <- LookupCluster  %>%
#   full_join( NTI09b_tidy %>%
#                select(FinancialYear, FinancialQuarter, AgeGroups, Term, Domain) %>%
#                distinct) %>% # use our lookup masterlist
#   left_join(NTI09b_tidy) %>% # join on data
#   mutate(Numerator = ifelse(is.na(Numerator), 0, Numerator), # set missing values to 0
#          Indicator = "NTI09b") %>%
#   left_join(LookupClusterLS) %>%
#   rename(Denominator = ListSize)
#
# # saveRDS(NTI09b, file=paste0(Data, "NTI09b", FinancialYear2, "Q", FinancialQ, ".rds")) # save data to the folder.
#
# rm("NTI09bYear1", "NTI09bYear2", "NTI09b_tidy", "NTI_check_Year1", "NTI_check_Year2")
#
# ##############################################  Prescribing ##############################################
#
# PrescribingNTIs <- NTI01c %>%
#   full_join(NTI05b) %>%
#   full_join(NTI08a) %>%
#   full_join(NTI08b) %>%
#   full_join(NTI08d) %>%
#   full_join(NTI08i) %>%
#   full_join(NTI09b)
#
# saveRDS(PrescribingNTIs, file=paste0(Data, "PrescribingNTIs ", FinancialYear2, " Q", FinancialQ, ".rds")) # save data to the folder.
#
# rm("NTI01c", "NTI05b", "NTI08a", "NTI08b", "NTI08d", "NTI08i", "NTI09b")
#
#
#
# ##############################################  Unscheduled Care ##############################################
#
# UnscheduledCare_raw <- read_rds(paste0(Data,"UnscheduledCare_raw ", FinancialYear2, " Q", FinancialQ, ".rds"))
#
# UnscheduledCare_tidy <- UnscheduledCare_raw %>%
#   mutate(Practice = ifelse(is.na(`New Code`), Practice, `New Code`)) %>% # recode mergers
#   anti_join(Exclude, by = c("Practice" = "Code")) %>%
#   mutate(UnplannedEmergency = if_else(AdmissionType %in% c(20:22, 30:39) & FirstIPDC=="I", 1, 0), # create flag for emergency admissions
#          FallCode = if_else(Diagnosis1 %in% c("W0", "W1"), 1, # falls can't be coded in diagnosis position 0
#                             if_else(Diagnosis2 %in% c("W0", "W1"), 1,
#                                     if_else(Diagnosis3 %in% c("W0", "W1"), 1,
#                                             if_else(Diagnosis4 %in% c("W0", "W1"), 1,
#                                                     if_else(Diagnosis5 %in% c("W0", "W1"), 1, 0))))), # Fall codes are anything that starts W0 or W1
#          Falls = if_else((FallCode == 1 & UnplannedEmergency == 1), 1, 0), # create a flag for unplanned emergency due to fall
#          Deaths = ifelse(substr(DischargeType,1,1) == "4" | DischargeDate==DATE_OF_DEATH, 1, 0), # if discharge code is death or DoD is the same as the discharge date, flag as death
#          Deaths = ifelse(substr(DischargeType,1,1) != "4" & (is.na(DATE_OF_DEATH)),0, Deaths), # if discharge code is not death and DoD is blank, flag as not a death. This should capture everything
#          Stay = if_else(Deaths == 1, 0, 1), # only count cases when patient didn't die during admission
#          AgeGroups = if_else(Age < 65, "Under 65", "65+"),
#          LengthofStay = if_else(UnplannedEmergency==0, 0, LengthofStay)) %>%  # LoS is used to calculate unplanned bed days. So change LoS to 0 for planned admissions
#   select(LINK_NO, CIS_MARKER, AdmissionType, AdmissionDate, DischargeDate, LengthofStay, Practice,
#          UnplannedEmergency, Falls, Deaths, Stay, FinancialQuarter, FinancialYear, AgeGroups)
#
#
# # This part takes ages(13.5 minutes). It calcualtes the number of days between admissions. Need this for 28 day readmissions.
# UnscheduledCare_tidy <- UnscheduledCare_tidy %>%
#   arrange(LINK_NO, DischargeDate, CIS_MARKER) %>%
#   group_by(LINK_NO) %>% # group by patient ID
#   mutate(difference = (time_length(difftime(lead(as.Date(AdmissionDate)), as.Date(DischargeDate)),"days")), # calculate days between discharge date and the next admission date
#          Unplanned28 = lead(UnplannedEmergency))  # is the next admission an unplanned emergency
#
# # splitting code up because it takes ages to run.
# UnscheduledCare_tidy <- UnscheduledCare_tidy %>%
#   filter(FinancialQuarter==FinancialQ) %>%
#   mutate(Readmission = if_else(difference <=28 & Unplanned28 == 1, 1, 0), # flag cases with a 28day unplanned emergency readmission
#          Readmission = if_else(is.na(Readmission), 0, Readmission)) %>% ungroup() # code missing as 0
#
# # Create aggregate data for All Ages
# UnscheduledCareAllAges <- UnscheduledCare_tidy %>%
#   mutate(AgeGroups="All Ages")
#
# UnscheduledCare_tidy <- LookupCluster %>%
#   full_join(UnscheduledCare_tidy %>%
#               rbind(UnscheduledCareAllAges) %>%
#               select(FinancialYear, FinancialQuarter, AgeGroups) %>%
#               distinct) %>% # join to our lookup masterlist
#   left_join(UnscheduledCare_tidy) %>% # join on data
#
#   full_join(UnscheduledCareAllAges) %>%
#   mutate(UnplannedEmergency = ifelse(is.na(UnplannedEmergency), 0,UnplannedEmergency),
#          Falls = ifelse(is.na(Falls), 0, Falls),
#          Stay = ifelse(is.na(Stay), 0,Stay),
#          LengthofStay = ifelse(is.na(LengthofStay), 0,LengthofStay),
#          Readmission = ifelse(is.na(Readmission), 0, Readmission)) %>%
#   group_by(Practice, FinancialQuarter, FinancialYear, AgeGroups) %>%
#   summarise(UnplannedEmergency = sum(UnplannedEmergency),
#             Falls = sum(Falls),
#             Admissions = sum(Stay),
#             BedDays = sum(LengthofStay),
#             Readmission = sum(Readmission)) %>% ungroup()  %>%
#   mutate(Domain = "Acute",
#          Term = "Financial Quarter")
#
#
# # saveRDS(UnscheduledCare_tidy, file = paste0(Data, "UnscheduledCare_tidy ", FinancialYear2, " Q", FinancialQ,".rds"))
#
# rm("UnscheduledCare_raw", "UnscheduledCareAllAges")



