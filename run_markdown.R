
source("lookups.R")
source("extract_v2.R")
source("wrangling_v2.R")

#monthly run
rmarkdown::render("gp_outpatient_markdown.Rmd",
                  output_file = glue("//conf/LIST_analytics/Borders/Ad-Hocs/2024-06 Outpatients/output/Borders West Cluster Analysis {Sys.Date()}.html")
)


