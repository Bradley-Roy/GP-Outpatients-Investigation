
# rm(list = ls())
# gc()

############################################## Set up ##############################################


#File paths
# setwd("~/GP-Outpatients-Investigation")
# Data <- paste0("./Data/")
# Reference <- paste0("./Reference Files/")


############################################## Open Data - Demographics ##############################################

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

# This is created in syntax '0 Create Lookup' - used to make sure we have a case for every Practice even when numerator is zero
LookupCluster <- readRDS(paste0(Reference,"LookupCluster ", FinancialYear2, " FQ", FinancialQ, ".rds")) %>%
  select(Practice, FinancialQuarter, FinancialYear, Cluster, HSCP, NHSBoard)

# This is created in syntax '0 Create Lookup' - adds on list sizes
LookupClusterLS <- readRDS(paste0(Reference,"LookupCluster ", FinancialYear2, " FQ", FinancialQ, ".rds")) %>%
  select(Practice, FinancialQuarter, FinancialYear, ListSize, Cluster, HSCP, NHSBoard)


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
  left_join(hospital_codes) |>
  # mutate(FinancialYear_cd = extract_fin_year(ClinicDate)) |>
  # mutate(FinancialYear_q = paste0(FinancialYear, " - ", FinancialQuarter)) |>
  mutate(fin_year_cd = extract_fin_year(ClinicDate)) |>
  mutate(financial_year_q = paste0(fin_year_cd, " - Q", FinancialQuarter)) |>
  filter(ClinicDate >= "2021-04-01")


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

Outpatients_tidy_indicator <- Outpatients_tidy %>% # join selected specialties
  bind_rows(OutpatientsAllSpecialties) |>
  # rbind(OutpatientsAllSpecialties) %>% # to all specialties
  filter(Indicator %in% c("All Specialities", "Dermatology", "General Surgery", "Trauma", "ENT")) # filter for ones we want



Outpatients_tidy_indicator_totals <- Outpatients_tidy_indicator |>
  filter(AgeGroups != "All Ages") |>
  filter(Indicator != "All Specialities") |>
  group_by(Indicator, financial_year_q) |>
  summarise(numerator = n()) |>
  ungroup() |>
  mutate(total = sum(numerator),
         average = percent(numerator / total))


Outpatients_tidy_indicator_totals <- Outpatients_tidy_indicator |>
  filter(AgeGroups != "All Ages") |>
  filter(Indicator != "All Specialities") |>
  group_by(Indicator) |>
  summarise(numerator = n()) |>
  ungroup() |>
  mutate(total = sum(numerator),
         average = percent(numerator / total))

# All Specialties Age Group  - Quarterly ---------------------------------------------------------------

Outpatients_tidy_agegroup <- Outpatients_tidy_indicator %>%
  filter(AgeGroups != "All Ages") |>
  filter(Indicator != "All Specialities") |>
  group_by(financial_year_q, fin_year_cd, FinancialQuarter, AgeGroups) %>%
  summarise(Numerator = n()) %>%
  ungroup() %>%
  clean_names() |>
  group_by(age_groups) |>
  mutate(average = mean(numerator)) |>
  ungroup() |>
  mutate(average_all = mean(numerator))


g_q_agegroups <- ggplot(Outpatients_tidy_agegroup, aes(x = financial_year_q, y = numerator, fill = age_groups,
                                                       text = paste('Age Group: ', age_groups,
                                                                    '<br>Financial Year: ', fin_year_cd,
                                                                    '<br>Financial Quarter: ', financial_quarter,
                                                                    '<br>Numerator: ', numerator))) +
  geom_bar(stat = "identity", position = "dodge") +
  # paletteer::scale_fill_paletteer_d("Redmonder::qMSOGn", direction = -1) +
  paletteer::scale_fill_paletteer_d("LaCroixColoR::Pamplemousse", direction = -1) +
  # scale_x_date(date_breaks = "1 month", date_labels = "%Y %b") +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Date",
       y = "Appointments",
       fill = "Age Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10))

plotly_q_agegroup <- ggplotly(g_q_agegroups, tooltip = "text")



# Quarterly Indicator -----------------------------------------------------------------

average_q_total_avg <- Outpatients_tidy_indicator |>
  filter(AgeGroups == "All Ages") |>
  filter(Indicator == "Trauma") |>
  group_by(financial_year_q, fin_year_cd, FinancialQuarter, Indicator, AgeGroups) %>%
  summarise(Numerator = n()) %>%  ungroup() %>%
  clean_names() |>
  group_by(age_groups) |>
  mutate(average = mean(numerator),
         age_total = sum(numerator)) |>
  ungroup() |>
  mutate(total = sum(numerator))


Outpatients_tidy_q<- Outpatients_tidy_indicator %>%
  filter(Indicator != "All Specialities") |>
  filter(AgeGroups != "All Ages") |>
  mutate(fin_year_cd = extract_fin_year(ClinicDate)) |>
  group_by(financial_year_q, fin_year_cd, FinancialQuarter, Indicator, AgeGroups) %>%
  summarise(Numerator = n()) %>%  ungroup() %>%
  # mutate(financial_year_q = paste0(fin_year_cd, " - ", FinancialQuarter)) |>
  clean_names()|>
  mutate(average_all = mean(numerator)) |>
  group_by(age_groups) |>
  mutate(average = mean(numerator),
         age_total = sum(numerator)) |>
  ungroup()


g_indicator_quart <- ggplot(Outpatients_tidy_q, aes(x = financial_year_q, y = numerator, fill = age_groups, text = paste('Indicator: ', indicator,
                                                                                                                         '<br>Age Group: ', age_groups,
                                                                                                                         '<br>Financial Year: ', fin_year_cd,
                                                                                                                         '<br>Financial Quarter: ', financial_quarter,
                                                                                                                         '<br>Numerator: ', numerator))) +
  geom_bar(stat = "identity", position = "dodge") +
  # scale_x_date(date_breaks = "1 month", date_labels = "%Y %b") +
  # paletteer::scale_fill_paletteer_d("Redmonder::qMSOGn", direction = -1) +
  paletteer::scale_fill_paletteer_d("LaCroixColoR::Pamplemousse", direction = -1) +
  facet_wrap(~indicator, scales = "free") +
  labs(x = "Date",
       y = "Appointments",
       fill = "Age Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        panel.spacing = unit(2, "lines"))

plotly_q_indicator <- ggplotly(g_indicator_quart, tooltip = "text")




# Trauma age groups -------------------------------------------------------
#
# Outpatients_total_age <- Outpatients_tidy_indicator %>%
#   filter(AgeGroups != "All Ages") |>
#   filter(Indicator == "Trauma") |>
#   filter(financial_year_q %in% c("2021/22 - Q2",
#                                  "2022/23 - Q2",
#                                  "2023/24 - Q2")) |>
#   group_by(financial_year_q, fin_year_cd, FinancialQuarter, AgeGroups) %>%
#   summarise(Numerator = n()) %>%  ungroup() %>%
#   clean_names() |>
#   mutate(average_all = mean(numerator))
#
#
# g_monthly <- ggplot(Outpatients_total_age, aes(x = financial_year_q, y = numerator, fill = age_groups,
#                                                text = paste('Age Groups: ', age_groups,
#                                                             '<br>Financial Year: ', fin_year_cd,
#                                                             '<br>Financial Quarter: ', financial_quarter,
#                                                             '<br>Numerator: ', numerator))) +
#   geom_bar(stat = "identity", position = "dodge") +
#   # paletteer::scale_fill_paletteer_d("Redmonder::qMSOGn", direction = -1) +
#   paletteer::scale_fill_paletteer_d("LaCroixColoR::Pamplemousse", direction = -1) +
#   labs(x = "Date",
#        y = "Appointments",
#        fill = "Age Group") +
#   # facet_wrap(~financial_quarter_name) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#     panel.grid.major.x = element_blank(),
#     legend.text = element_text(size = 10),
#     legend.title = element_text(size = 10),
#     panel.spacing = unit(2, "lines"))
#
# plotly_trauma_age_q2 <- ggplotly(g_monthly, tooltip = "text")


# Practice ----------------------------------------------------------------

Outpatients_total_practice <- Outpatients_tidy_indicator %>%
  filter(AgeGroups != "All Ages") |>
  filter(Indicator == "Trauma") |>
  group_by(financial_year_q, fin_year_cd, FinancialQuarter, Practice_title, AgeGroups) %>%
  # group_by(financial_year_q, fin_year_cd, FinancialQuarter, Practice_title, AgeGroups) |>
  summarise(Numerator = n()) %>%
  ungroup() %>%
  clean_names() |>
  group_by(practice_title, age_groups) |>
  mutate(average = mean(numerator),
         age_total = sum(numerator)) |>
  ungroup()
# mutate(financial_quarter_name = paste0("Quarter ", financial_quarter))


g_q_prac <- ggplot(Outpatients_total_practice, aes(x = financial_year_q, y = numerator, fill = age_groups,
                                                   text = paste('Practice: ', practice_title,
                                                                '<br>Age Group: ', age_groups,
                                                                '<br>Financial Year: ', fin_year_cd,
                                                                '<br>Financial Quarter: ', financial_quarter,
                                                                '<br>Numerator: ', numerator))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~practice_title, scales = "free") +
  # coord_flip() +
  paletteer::scale_fill_paletteer_d("LaCroixColoR::Pamplemousse", direction = -1) +
  labs(x = "Date",
       y = "Appointments",
       fill = "Year") +
  # coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        panel.spacing = unit(2, "lines"))

plotly_q_practice <- ggplotly(g_q_prac, tooltip = "text")


# Hospital Quarterly ----------------------------------------------------------------

Outpatients_tidy_hospital <- Outpatients_tidy_indicator %>%
  filter(AgeGroups != "All Ages") |>
  filter(Indicator == "Trauma") |>
  mutate(hospital_name = case_when(hospital_name == "Borders General Hospital" ~ "Borders General Hospital",
                                   TRUE ~ "Other")) |>
  group_by(financial_year_q, fin_year_cd, FinancialQuarter, hospital_name) %>%
  summarise(Numerator = n()) %>%  ungroup() %>%
  clean_names() |>
  mutate(hospital_name = replace_na(hospital_name, "Unknown")) |>
  group_by(hospital_name) |>
  mutate(average = mean(numerator),
         age_total = sum(numerator)) |>
  ungroup()


g_q_hospital <- ggplot(Outpatients_tidy_hospital, aes(x = financial_year_q, y = numerator, fill = hospital_name, text = paste('Hospital: ', hospital_name,
                                                                                                                           '<br>Financial Year: ', fin_year_cd,
                                                                                                                           '<br>Financial Quarter: ', financial_quarter,
                                                                                                                           '<br>Numerator: ', numerator))) +
  geom_bar(stat = "identity") +
  paletteer::scale_fill_paletteer_d("PNWColors::Shuksan2") +
  labs(x = "Date",
       y = "Appointments",
       fill = "Hospital") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10))

plotly_q_hospital <- ggplotly(g_q_hospital, tooltip = "text")




