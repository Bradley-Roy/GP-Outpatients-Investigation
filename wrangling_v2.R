
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
         FinancialQuarter = as.character(ifelse(CalendarQuarter-1 == 0, 4, CalendarQuarter-1)),
         month_date = ymd(floor_date(ClinicDate, "month")),
         month_name = month(month_date, abbr = FALSE, label = TRUE)) %>%
  left_join(hospital_codes)
  # filter(FinancialQuarter == FinancialQ) %>%  # filter to Financial quarter that we want
  # select(-CalendarQuarter, -CalendarYear)

# Create aggregate data for All Ages
OutpatientsAllAges <- Outpatients_tidy %>%
  mutate(AgeGroups="All Ages")

# We are only interested in some specfic specialties. We also want a total for all specialties
Outpatients_tidy <- Outpatients_tidy %>%
  bind_rows(OutpatientsAllAges) %>%
  mutate(Indicator = ifelse(substr(Speciality, 1,2)=="A7", "Dermatology",
                            ifelse(substr(Speciality, 1,2)=="C1", "General Surgery",
                                   ifelse(substr(Speciality, 1,2)=="C8", "Trauma",
                                          ifelse(substr(Speciality, 1,2)=="C5", "ENT", Speciality))))) |>
  # semi_join(LookupCluster) %>%
  left_join(LookupCluster)

# Create aggregated data for 'All specialities'
OutpatientsAllSpecialties <- Outpatients_tidy %>%
  mutate(Indicator = "All Specialities")

# Outpatients_tidy2 <- Outpatients_tidy |>
#   anti_join(Exclude, by = c("Practice" = "Code")) #%>%
# semi_join(LookupCluster) %>%
# left_join(LookupCluster)


Outpatients_tidy_indicator <- Outpatients_tidy %>% # join selected specialties
  bind_rows(OutpatientsAllSpecialties) |>
  # rbind(OutpatientsAllSpecialties) %>% # to all specialties
  # filter(Indicator %in% c("All Specialities", "Dermatology", "General Surgery", "Trauma", "ENT")) %>% # filter for ones we want
  filter(Indicator %in% c("Trauma"))


patient_list <- Outpatients_tidy_indicator |>
  select(Cluster, Practice, Practice_title, CI_CHI_NUMBER,  UPI, AgeGroups, FinancialYear,Speciality)

write_csv(patient_list, "//conf/LIST_analytics/Borders/Ad-Hocs/2024-06 Outpatients/patient_list.csv")


Outpatients_tidy_indicator <- Outpatients_tidy %>% # join selected specialties
  bind_rows(OutpatientsAllSpecialties) |>
  # rbind(OutpatientsAllSpecialties) %>% # to all specialties
  filter(Indicator %in% c("All Specialities", "Dermatology", "General Surgery", "Trauma", "ENT")) # filter for ones we want
  # filter(Indicator %in% c("Trauma"))


# Age Group ---------------------------------------------------------------

average_monthly_total <- Outpatients_tidy_indicator |>
  filter(AgeGroups == "All Ages") |>
  filter(Indicator != "All Specialities") |>
  group_by(FinancialYear, FinancialQuarter, month_date, month_name, AgeGroups) %>%
  summarise(Numerator = n()) %>%  ungroup() %>%
  clean_names() |>
  group_by(age_groups) |>
  mutate(average = mean(numerator)) |>
  ungroup()


Outpatients_tidy_agegroup <- Outpatients_tidy_indicator %>%
  filter(AgeGroups != "All Ages") |>
  filter(Indicator != "All Specialities") |>
  group_by(FinancialYear, FinancialQuarter, month_date, month_name, AgeGroups) %>%
  summarise(Numerator = n()) %>%  ungroup() %>%
  clean_names() |>
  group_by(age_groups) |>
  mutate(average = mean(numerator)) |>
  ungroup()


g_monthly <- ggplot(Outpatients_tidy_agegroup, aes(x = month_date, y = numerator, fill = age_groups, text = paste('Age Group: ', age_groups,
                                                                                                                  '<br>Date: ', format.Date(month_date, format = "%B %Y"),
                                                                                                                  '<br>Financial Quarter: ', financial_quarter,
                                                                                                                 '<br>Numerator: ', numerator))) +
  geom_bar(stat = "identity") +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y %b") +
  labs(x = "Date",
       y = "Appointments",
       fill = "Age Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10))

plotly_monthly_agegroup <- ggplotly(g_monthly, tooltip = "text")




#######total age groups
Outpatients_total_age <- Outpatients_tidy_indicator %>%
  filter(AgeGroups != "All Ages") |>
  filter(Indicator == "Trauma",
         FinancialYear %in% c(2022, 2023),
         FinancialQuarter %in% c(1,2)) |>
  group_by(FinancialYear, FinancialQuarter, AgeGroups) %>%
  summarise(Numerator = n()) %>%  ungroup() %>%
  clean_names() |>
  mutate(financial_quarter_name = paste0("Quarter ", financial_quarter))


g_monthly <- ggplot(Outpatients_total_age, aes(x = financial_year, y = numerator, fill = age_groups,
                                                    text = paste('Age Groups: ', age_groups,
                                                                 '<br>Financial Year: ', financial_year,
                                                                 '<br>Financial Quarter: ', financial_quarter,
                                                                 '<br>Numerator: ', numerator))) +
  geom_bar(stat = "identity") +
  labs(x = "Date",
       y = "Appointments",
       fill = "Age Group") +
  facet_wrap(~financial_quarter_name) +
  # coord_flip() +
  theme_minimal() +
  theme(#axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    panel.grid.major.x = element_blank(),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10))

plotly_total_age <- ggplotly(g_monthly, tooltip = "text")



# Monthly -----------------------------------------------------------------

average_monthly_total_avg <- Outpatients_tidy_indicator |>
  # filter(AgeGroups == "All Ages") |>
  filter(Indicator == "Trauma") |>
  group_by(FinancialYear, FinancialQuarter, month_date, month_name, AgeGroups) %>%
  summarise(Numerator = n()) %>%  ungroup() %>%
  clean_names() |>
  group_by(age_groups) |>
  mutate(average = mean(numerator)) |>
  ungroup()


Outpatients_tidy_month <- Outpatients_tidy_indicator %>%
  filter(Indicator != "All Specialities") |>
  filter(AgeGroups != "All Ages") |>
  group_by(FinancialYear, FinancialQuarter, Indicator, month_date, month_name, AgeGroups) %>%
  summarise(Numerator = n()) %>%  ungroup() %>%
  clean_names()


g_monthly <- ggplot(Outpatients_tidy_month, aes(x = month_date, y = numerator, fill = age_groups, text = paste('Indicator: ', indicator,
                                                                                                               '<br>Age Group: ', age_groups,
                                                                                                               '<br>Date: ', format.Date(month_date, format = "%B %Y"),
                                                                                                               '<br>Financial Quarter: ', financial_quarter,
                                                                                                          '<br>Numerator: ', numerator))) +
  geom_bar(stat = "identity") +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y %b") +
  facet_wrap(~indicator) +
  labs(x = "Date",
       y = "Appointments",
       fill = "Age Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10))

plotly_monthly_indicator <- ggplotly(g_monthly, tooltip = "text")



# Hospital ----------------------------------------------------------------

Outpatients_tidy_hospital <- Outpatients_tidy_indicator %>%
  filter(AgeGroups != "All Ages") |>
  filter(Indicator == "Trauma") |>
  group_by(FinancialYear, FinancialQuarter, month_date, month_name, hospital_name) %>%
  summarise(Numerator = n()) %>%  ungroup() %>%
  clean_names()


g_monthly <- ggplot(Outpatients_tidy_hospital, aes(x = month_date, y = numerator, fill = hospital_name, text = paste('Hospital: ', hospital_name,
                                                                                                                     '<br>Date: ', format.Date(month_date, format = "%B %Y"),
                                                                                                                     '<br>Financial Quarter: ', financial_quarter,
                                                                                                                  '<br>Numerator: ', numerator))) +
  geom_bar(stat = "identity") +
  # geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y %b") +
  # facet_wrap(~indicator) +
  labs(x = "Date",
       y = "Appointments",
       fill = "Hospital") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10))

plotly_monthly_hospital <- ggplotly(g_monthly, tooltip = "text")



# Practice ----------------------------------------------------------------

############ Practice totals

Outpatients_total_practice <- Outpatients_tidy_indicator %>%
  filter(AgeGroups != "All Ages") |>
  filter(Indicator == "Trauma",
         FinancialYear %in% c(2022, 2023),
         FinancialQuarter %in% c(1,2)) |>
  group_by(FinancialYear, FinancialQuarter, Practice_title) %>%
  summarise(Numerator = n()) %>%  ungroup() %>%
  clean_names() |>
  mutate(financial_quarter_name = paste0("Quarter ", financial_quarter))


# g_monthly <- ggplot(Outpatients_total_practice, aes(x = financial_year, y = numerator, fill = practice_title,
#                                                     text = paste('Practice: ', practice_title,
#                                                                  '<br>Financial Year: ', financial_year,
#                                                                  '<br>Financial Quarter: ', financial_quarter,
#                                                                  '<br>Numerator: ', numerator))) +
#   geom_bar(stat = "identity") +
#   facet_wrap(~financial_quarter_name) +
#   labs(x = "Date",
#        y = "Appointments",
#        fill = "Practice") +
#   # coord_flip() +
#   theme_minimal() +
#   theme(#axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#     panel.grid.major.x = element_blank(),
#     legend.text = element_text(size = 10),
#     legend.title = element_text(size = 10))


g_monthly <- ggplot(Outpatients_total_practice, aes(x = financial_quarter_name, y = numerator, fill = financial_year,
                                                    text = paste('Practice: ', practice_title,
                                                                 '<br>Financial Year: ', financial_year,
                                                                 '<br>Financial Quarter: ', financial_quarter,
                                                                 '<br>Numerator: ', numerator))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~practice_title, scales = "free") +
  # coord_flip() +
  labs(x = "Date",
       y = "Appointments",
       fill = "Year") +
  # coord_flip() +
  theme_minimal() +
  theme(#axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    panel.grid.major.x = element_blank(),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10))

plotly_total_practice <- ggplotly(g_monthly, tooltip = "text")

######### Practice Monthly

Outpatients_tidy_practice <- Outpatients_tidy_indicator %>%
  filter(AgeGroups != "All Ages") |>
  filter(Indicator == "Trauma") |>
  group_by(FinancialYear, FinancialQuarter, month_date, month_name, Practice_title) %>%
  summarise(Numerator = n()) %>%  ungroup() %>%
  clean_names() |>
  group_by(month_date) |>
  mutate(total = sum(numerator)) |>
  ungroup() |>
  mutate(percentage = percent(round_half_up(numerator / total, 2)))


g_monthly <- ggplot(Outpatients_tidy_practice, aes(x = month_date, y = numerator, fill = practice_title, text = paste('Practice: ', practice_title,
                                                                                                                      '<br>Date: ', format.Date(month_date, format = "%B %Y"),
                                                                                                                     '<br>Financial Quarter: ', financial_quarter,
                                                                                                                     '<br>Numerator: ', numerator,
                                                                                                                     '<br>Percent: ', percentage))) +
  geom_bar(stat = "identity", position = "fill") +
  # geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y %b") +
  scale_y_continuous(labels = percent) +
  # facet_wrap(~indicator) +
  labs(x = "Date",
       y = "Precent",
       fill = "Practice") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10))

plotly_monthly_practice_per <- ggplotly(g_monthly, tooltip = "text")


g_monthly <- ggplot(Outpatients_tidy_practice, aes(x = month_date, y = numerator, fill = practice_title, text = paste('Practice: ', practice_title,
                                                                                                                      '<br>Date: ', format.Date(month_date, format = "%B %Y"),
                                                                                                                      '<br>Financial Quarter: ', financial_quarter,
                                                                                                                      '<br>Numerator: ', numerator,
                                                                                                                      '<br>Percent: ', percentage))) +
  geom_bar(stat = "identity") +
  # geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y %b") +
  # facet_wrap(~financial_quarter) +
  labs(x = "Date",
       y = "Appointments",
       fill = "Practice") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10))

plotly_monthly_practice_numerator <- ggplotly(g_monthly, tooltip = "text")



