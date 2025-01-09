library(quarto)
library(tidyverse)

# List of counties
county_data <- read_csv("data/BBS_HTS_SUMMARY_DATA.csv") |>
  select(KP_TOPOLOGY, COUNTY) |>
  mutate(
    KP_TOPOLOGY = case_when(
      COUNTY == "Trans Nzoia" & KP_TOPOLOGY == "NONE" ~ "FSW",
      TRUE ~ KP_TOPOLOGY
    ),
    COUNTY = case_when(
      COUNTY == "Trans Nzoia" ~ "Nakuru",
      TRUE ~ COUNTY
    )
  )


counties <- county_data |> 
  distinct(COUNTY) |> 
  pull()

counties <- c("Nairobi") #All


# Render the Quarto document for each county
for (county in counties) {
  quarto_render(
    input = "summary-sheets.qmd",
    output_file = paste0("BBS_REPORT_", county, ".docx"),
    execute_params = list(county = county)
  )
}

