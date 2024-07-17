
rm(list = ls())
gc()

# Previous Financial Year
ONE_START <- "'31 March 2021'"
ONE_END <- "'31 December 2023'"

#Set specific financial years of interest (eg for 2018/19 - "2018")
FinancialYear1 <- "2022"
FinancialYear2 <- "2023"
# FinancialQ <- "3"
FinancialQ <- "2"

Year1 <-"2022"
Year2 <-"2023"
FinancialQ <- "2"


source("lookups.R")
source("extract_v2.R")
source("wrangling_v4.R")

#monthly run
rmarkdown::render("gp_outpatient_markdown_v3.Rmd",
                  output_file = glue("//conf/LIST_analytics/Borders/Ad-Hocs/2024-06 Outpatients/output/Borders West Cluster Analysis {Sys.Date()}.html")
)


