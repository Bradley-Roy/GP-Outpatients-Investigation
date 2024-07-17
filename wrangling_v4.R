
# rm(list = ls())
# gc()

############################################## Set up ##############################################


#File paths
# setwd("~/GP-Outpatients-Investigation")
# Data <- paste0("./Data/")
# Reference <- paste0("./Reference Files/")


############################################## Open Data - Demographics ##############################################

Jul2021 <- "https://www.opendata.nhs.scot/dataset/e3300e98-cdd2-4f4e-a24e-06ee14fcc66c/resource/0779e100-1aaf-4e43-8536-57c8b99ca710/download/practice_listsizes_jul2021-open-data.csv"
Jul2022 <- "https://www.opendata.nhs.scot/dataset/e3300e98-cdd2-4f4e-a24e-06ee14fcc66c/resource/64918d4f-f1d9-4e99-8e9f-130ddc890748/download/practice_listsizes_jul2022-open-data.csv"
Jul2023 <- "https://www.opendata.nhs.scot/dataset/e3300e98-cdd2-4f4e-a24e-06ee14fcc66c/resource/d7f423dd-9611-4ae9-a9c8-4dcc532ece22/download/practice_listsizes_jul2023-open-data.csv"

# read link into R as csv
ListYear21 <-  read_csv(Jul2021)
ListYear21_pivot <- ListYear21 |>
  clean_names() |>
  mutate(`Under 65` = ages0to4 + ages5to14 + ages15to24 + ages25to44 + ages45to64,
         `65+` = ages65to74 + ages75to84 + ages85plus) |>
  filter(sex == "All" &
           practice_code %in% practice_list)

ListYear22 <-  read_csv(Jul2022) # read link into R as csv
ListYear22_pivot <- ListYear22 |>
  clean_names() |>
  mutate(`Under 65` = ages0to4 + ages5to14 + ages15to24 + ages25to44 + ages45to64,
         `65+` = ages65to74 + ages75to84 + ages85plus) |>
  filter(sex == "All" &
           practice_code %in% practice_list)

ListYear23 <-  read_csv(Jul2023)
ListYear23_pivot <- ListYear23 |>
  clean_names() |>
  mutate(`Under 65` = ages0to4 + ages5to14 + ages15to24 + ages25to44 + ages45to64,
         `65+` = ages65to74 + ages75to84 + ages85plus) |>
  filter(sex == "All" &
           practice_code %in% practice_list)

ListYear_all_pivot <- bind_rows(ListYear21_pivot, ListYear22_pivot, ListYear23_pivot) |>
  select(date, practice_code, all_ages, `Under 65`, `65+`) |>
  mutate(practice_code = as.character(practice_code)) |>
  pivot_longer(cols = c(`Under 65`, `65+`), values_to = "population") |>
  rename(Practice = practice_code,
         AgeGroups = name) |>
  mutate(fin_year_cd = extract_fin_year(ymd(date))) |>
  select(-date)

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
  filter(Indicator %in% c("All Specialities", "Dermatology", "General Surgery", "Trauma", "ENT")) |> # filter for ones we want
  left_join(ListYear_all_pivot, by = c("fin_year_cd", "Practice", "AgeGroups"))


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

# All Specialties Age Group  - Quarterly - bar ---------------------------------------------------------------

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



# Quarterly Indicator - line -----------------------------------------------------------------
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
  ungroup() |>
  group_by(age_groups, indicator) |>
  mutate(average_indicator = mean(numerator),
         age_total_indicator = sum(numerator)) |>
  ungroup()

Outpatients_line_q <- Outpatients_tidy_q |>
  mutate(age_groups = recode(age_groups, "65+" = "65 and over"))

g_indicator_quart <- ggplot(Outpatients_line_q, aes(x = financial_year_q, y = numerator, colour = indicator, text = paste('Indicator: ', indicator,
                                                                                                                          '<br>Age Group: ', age_groups,
                                                                                                                          '<br>Financial Year: ', fin_year_cd,
                                                                                                                          '<br>Financial Quarter: ', financial_quarter,
                                                                                                                          '<br>Numerator: ', numerator))) +
  geom_line(aes(group = indicator), size = 0.9) +
  geom_point(aes(group = indicator)) +
  paletteer::scale_colour_paletteer_d("LaCroixColoR::Pamplemousse", direction = -1) +
  # facet_wrap(~age_groups) +
  facet_wrap(~factor(age_groups, c("Under 65", "65 and over"))) +
  scale_y_continuous(expand = c(0,0), limits = c(0,130), breaks = breaks_width(20)) +
  labs(x = "Date",
       y = "Appointments",
       colour = "Indicator") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        panel.spacing = unit(2, "lines"))

plotly_q_indicator <- ggplotly(g_indicator_quart, tooltip = "text") |>
  layout(legend = list(orientation = "h", y =-0.3, x=0.1))


# Quarterly Practice - line -----------------------------------------------------------------

Outpatients_total_practice <- Outpatients_tidy_indicator %>%
  filter(AgeGroups != "All Ages") |>
  filter(Indicator == "Trauma") |>
  group_by(financial_year_q, fin_year_cd, FinancialQuarter, Practice_title, AgeGroups, population) %>%
  # group_by(financial_year_q, fin_year_cd, FinancialQuarter, Practice_title, AgeGroups) |>
  summarise(Numerator = n()) %>%
  ungroup() %>%
  clean_names() |>
  group_by(practice_title, age_groups) |>
  mutate(average = mean(numerator),
         age_total = sum(numerator)) |>
  ungroup() |>
  mutate(rate = round_half_up(((numerator / population)*100000)), 2) |>
  group_by(practice_title, age_groups) |>
  mutate(average_rate = mean(rate),
         age_total_rate = sum(rate)) |>
  ungroup()

Outpatients_line_q <- Outpatients_total_practice |>
  mutate(age_groups = recode(age_groups, "65+" = "65 and over"),
         practice_title = str_remove(practice_title, " PRACTICE"))

g_practice_quart <- ggplot(Outpatients_line_q, aes(x = financial_year_q, y = rate, colour = practice_title, text = paste('Indicator: ', practice_title,
                                                                                                                              '<br>Age Group: ', age_groups,
                                                                                                                              '<br>Financial Year: ', fin_year_cd,
                                                                                                                              '<br>Financial Quarter: ', financial_quarter,
                                                                                                                              '<br>Rate per 100,000 population: ', comma(rate)))) +
  geom_line(aes(group = practice_title), size = 0.9) +
  geom_point(aes(group = practice_title)) +
  paletteer::scale_colour_paletteer_d("LaCroixColoR::Pamplemousse", direction = -1) +
  facet_wrap(~factor(age_groups, c("Under 65", "65 and over"))) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1800), breaks = breaks_width(500),
                     label = comma) +
  labs(x = "Date",
       y = "Appointments (Rate per 100,000 population)",
       colour = "Practice") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        panel.spacing = unit(2, "lines"),
        legend.position = "bottom")

plotly_q_practice <- ggplotly(g_practice_quart, tooltip = "text") |>
  layout(legend = list(orientation = "h", y =-0.3, x = -0.1))


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


