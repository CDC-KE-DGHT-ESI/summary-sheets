# List of packages to check and install if missing
packages <- c("tidyverse", "readxl", "scales", "DT", "gt", "haven", "rio", "janitor", 
              "data.table", "sjlabelled", "sjmisc", "freqtables", "ggtext", "scales", 
              "prismatic", "gtsummary", "survey", "srvyr", "psych", "extrafont", 
              "likert", "broom", "prettyunits", "tidytext", "RDS", "openxlsx2",
              "summarytools")


install_if_missing <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
      library(package, character.only = TRUE)
    }
  }
}


all_data <- rio::import("data/box/weighted_data_13_09_2024.xlsx") 

all_data |>
  tabyl(cr_eligible) |>
  adorn_totals()

# Minimum data
all_clean <- all_data |>
  filter(cr_eligible == "Yes") %>%
  transmute(
    County = q5_county_living_in,
    age = q4_age_at_last_bday,
    age_cat_0 = cut(age,
                    breaks = c(0, 17, 24, 34, 44, 54, Inf),
                    labels = c("<18", "18-24", 
                               "25-34", "35-44", "45-54", "55+")),
    age_cat_1 = cut(age,
                    breaks = c(0, 17, 24, 29, 34, 39, 44, 49, Inf),
                    labels = c("<18", "18-24", "25-29", "30-34", 
                               "35-39", "40-44", "45-49", "50+")),
    age_cat_2 = cut(age,
                    breaks = c(0, 17, 24, 34, Inf),
                    labels = c("<18", "18-34", "35-44", "45+")),
    typology = case_when(typology == "1" ~ "FSW",
                         typology == "2" ~ "MSM",
                         typology == "3" ~ "PWID",
                         typology == "4" ~ "TG",
                         TRUE ~ NA_character_)
  ) 

write_csv(all_clean, "data/clean_data.csv")


table_one_county <- all_clean %>%
  select(age_cat_0, typology ) %>%
  tbl_summary(
    by = typology,
    statistic = list(all_continuous() ~ "{median} ({sd})", all_categorical() ~ "{n} ({p}%)"),
    digits = all_continuous() ~ 2,
    missing = "no"
  ) %>%
  add_overall() %>%
  modify_header(label ~ "**Characteristic**") %>%
  modify_spanning_header(-c("label", "stat_0") ~ "**Typology**") %>%
  as_gt() %>%
  tab_style(
    style = list(cell_borders(sides = "all", color = "gray", weight = px(1)),
                 cell_text(color = "black")),
    locations = cells_body()
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold", color = "black"),
      cell_fill(color = "#FFFDD0")
    ),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold", color = "black"),
      cell_fill(color = "#FFFDD0")
    ),
    locations = cells_column_spanners()
  ) %>%
  opt_table_font(
    font = list(
      google_font(name = "Roboto Condensed")
    )
  ) |>
  tab_options(data_row.padding = px(1))

table_one_county


# Data Labelling
all_clean <- all_clean |>
  mutate(County = set_label(county, "County"),
         age = set_label(age, "Age"),
         age_cat_0 = set_label(age_cat_0, "Age group"),
         age_cat_1 = set_label(age_cat_1, "Age group"),
         age_cat_2 = set_label(age_cat_2, "Age group"),
         typology = set_label(typology, "Typology")
  )


# # Save results
# library(openxlsx2)
# wb <- wb_workbook(theme = "Office 2013 - 2022 Theme")
# wb$add_worksheet("Overall")
# 
# ## modify base font to size 10 Aptos Narrow in red
# wb$set_base_font(font_size = 10, 
#                  font_color = wb_color("black"), 
#                  font_name = "Arial")
# 
# #wb$add_data(x = overall)
# 
# ## font color does not affect tables
# wb$add_data_table(x = overall, 
#                   dims = wb_dims(from_col = 1))
# 
# ## get the base font
# wb_save(wb, "95_95_95.xlsx", overwrite = TRUE)
# 






