library(readxl)
library(dplyr)
library(ggplot2)
library(igraph)
library(janitor)
library(freqtables)
library(sjmisc)

# Import wgt data ---------------
wdata <- read_excel('data/box/weighted_data_13_09_2024.xlsx', col_types = "text")
dim(wdata)
head(wdata)
sum(is.na(wdata$cr_barcode))

# EDA-------------------------------------------------------------------------------
# Recode participation
wdata$participated <- ifelse(is.na(wdata$cr_barcode), 'ID_Missing', 'ID_Available')
table(wdata$participated)

# Number by County
tabyl(wdata$cr_county)

# Records with no county
sum(is.na(wdata$cr_county))

# Number of records with no age
sum(is.na(wdata$cr_age))

# Number eligible
wdata %>% count(cr_eligible)

# Completed interview
wdata %>% count(cr_completed_interview)



# Transformation ----------------------------------------------------------------
# Record age and create age bands
wdata <- wdata %>%
  mutate(cr_age = as.numeric(cr_age)) %>%
  mutate(age_band2 = case_when(
    is.na(cr_age) ~ 'Missing',
    cr_age < 25 ~ '18-24',
    cr_age >= 25 & cr_age < 35 ~ '25-34',
    cr_age >= 35 & cr_age < 45 ~ '35-44',
    cr_age >= 45 ~ '45-54',
    TRUE ~ NA_character_),
    
    age_band5 = cut(cr_age,
                    breaks = c(0, 17, 24, 29, 34, 39, 44, Inf),
                    labels = c("<18", "18-24", "25-29", "30-34", 
                               "35-39", "40-44", "45+"))
    )

# Calculate the quartiles and IQR
Q1 <- quantile(wdata$cr_age, 0.25, na.rm = TRUE)
Q3 <- quantile(wdata$cr_age, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Define the lower and upper fences
lower_fence <- Q1 - 1.5 * IQR
upper_fence <- Q3 + 1.5 * IQR

# Replace outliers with the upper and lower fence where appropriate
wdata$age_capped <- ifelse(wdata$cr_age < lower_fence, lower_fence,
                           ifelse(wdata$cr_age > upper_fence, upper_fence, 
                                  wdata$cr_age))



# Age distribution
all_clean <- wdata |>
  filter(cr_eligible == "Yes") %>%
  transmute(
    County = q5_county_living_in,
    age = as.numeric(age_capped),
    age_cat_0 = cut(age,
                    breaks = c(0, 17, 24, 34, 44, Inf),
                    labels = c("<18", "18-24", 
                               "25-34", "35-44", "45+")),
    age_cat_1 = cut(age,
                    breaks = c(0, 17, 24, 29, 34, 39, 44, 49, Inf),
                    labels = c("<18", "18-24", "25-29", "30-34", 
                               "35-39", "40-44", "45-49", "50+")),
    age_cat_2 = cut(age,
                    breaks = c(0, 17, 24, 34, Inf),
                    labels = c("<18", "18-34", "35-44", "45+")),
    typology = typology,
    
    weight = as.numeric(cr_weights)
  ) 

write.csv(all_clean, "data/clean_data.csv")


# Weighted density plot
# Filter out rows with non-finite weights
all_clean %>%
  filter(is.finite(weight)) %>%  # Ensure weights are finite
  ggplot(aes(x = age, fill = typology, color = typology, weight = weight)) + 
  stat_density(position = "identity", alpha = 0.5) +  # Use stat_density with weight
  labs(title = "Age Distribution by Key Population Group", 
       x = "Age", 
       y = "Percentage",  # Change the y-axis label
       fill = "Typology", 
       color = "Typology") +
  theme_minimal() +
  scale_y_continuous(labels = scales::label_percent()) +  # Scale to percent
  guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL))


library(ggplot2)
library(dplyr)
library(scales)

all_clean %>%
  filter(is.finite(weight)) %>%  
  ggplot(aes(x = age, 
             fill = typology, 
             color = typology, 
             weight = weight)) + 
  stat_density(position = "identity", alpha = 0.5) +  
  labs(#title = "Age Distribution by Key Population Group", 
       x = "Age", 
       y = "Percentage",  
       fill = "Typology", 
       color = "Typology") +
  theme_minimal(base_size = 14) +  
  scale_y_continuous(labels = label_percent(), expand = c(0, 0)) +  
  scale_x_continuous(expand = c(0, 0)) +  
  scale_fill_brewer(palette="Dark2") +  
  scale_color_brewer(palette = "Dark2") +  
  guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), 
    axis.title = element_text(size = 14, face = "bold"), 
    axis.text = element_text(size = 14),  
    legend.position = "top", 
    legend.text = element_text(size = 14) 
  )



# Get those whose blood was collected among those who completed the interview
wdata <- wdata %>%
  mutate(blood_collected = case_when(
    cr_completed_interview == 'Yes' & cr_blood_sample_collected == 'Yes' ~ 'Yes',
    TRUE ~ 'No'
  ))

# Count values of blood_collected
wdata %>% count(blood_collected)

# Sum weights
wdata$cr_weights <- as.numeric(wdata$cr_weights)
sum(wdata$cr_weights, na.rm = TRUE)

# Get final HIV status based on conditions
# wdata <- wdata %>%
#   mutate(final_HIV_status = case_when(
#     (blood_collected == 'Yes' & FIN_HIV_RES == 'Positive') |
#       (blood_collected == 'Yes' & CLAB__HIV_RAPID_RES == 'Reactive') |
#       (blood_collected == 'Yes' & CLAB__FINAL_HIV_RES == 'Positive') ~ 'Positive',
#     
#     (blood_collected == 'Yes' & FIN_HIV_RES == 'Negative') |
#       (blood_collected == 'Yes' & is.na(cr_self_report_hiv_status) & CLAB__HIV_RAPID_RES == 'Non-Reactive') |
#       (blood_collected == 'Yes' & cr_self_report_hiv_status == 'HIV Negative' & CLAB__HIV_RAPID_RES == 'Non-Reactive') |
#       (blood_collected == 'Yes' & CLAB__FINAL_HIV_RES == 'Negative') ~ 'Negative',
#     
#     blood_collected == 'Yes' & is.na(CLAB__FINAL_HIV_RES) ~ 'Missing',
#     
#     TRUE ~ NA_character_
#   ))

# Count values of final_HIV_status
wdata %>% count(final_hiv_status)


# Get new positives and known positives
wdata <- wdata %>%
  mutate(knowledge_status = case_when(
    final_hiv_status == 'Positive' & cr_self_report_hiv_status != 'HIV Positive' ~ 'New Positive',
    final_hiv_status == 'Positive' & cr_self_report_hiv_status == 'HIV Positive' ~ 'Known Positive',
    final_hiv_status == 'Negative' ~ 'Negative',
    final_hiv_status == 'Missing' ~ 'Missing',
    TRUE ~ NA_character_
  ))

# Count values of status
wdata %>% count(knowledge_status)

# On ART
wdata <- wdata %>%
  mutate(art = case_when(
    knowledge_status == 'Known Positive' & cr_currently_on_arv == 'Yes' ~ 'Yes',
    knowledge_status == 'Known Positive' & cr_currently_on_arv != 'Yes' ~ 'No',
    TRUE ~ NA_character_
  ))

# Count values of art
wdata %>% count(art)



# Get final HIV status from the dataset
wdata <- wdata %>%
  mutate(hiv_status = case_when(
    blood_collected == "Yes" & cr_final_hiv_status == "Positive" ~ "Positive",
    blood_collected == "Yes" & cr_final_hiv_status == "Negative" ~ "Negative",
    blood_collected == "Yes" & is.na(cr_final_hiv_status) ~ "Missing",
    TRUE ~ NA_character_
  ))

# Count values of final_hiv_status1
wdata %>% count(hiv_status)


# Get testing cascade
cascade <- wdata %>%
  group_by(typology) %>%
  summarise(
    participated = sum(participated == 'ID_Available', na.rm = TRUE),
    eligible = sum(cr_eligible == 'Yes', na.rm = TRUE),
    interviewed = sum(cr_completed_interview == 'Yes', na.rm = TRUE),
    blood_collected = sum(blood_collected == 'Yes', na.rm = TRUE),
    known_positive = sum(knowledge_status == 'Known Positive', na.rm = TRUE),
    on_art = sum(art == 'Yes', na.rm = TRUE),
    new_positive = sum(knowledge_status == 'New Positive', na.rm = TRUE),
    total_positive = sum(hiv_status == 'Positive', na.rm = TRUE),
    negative = sum(hiv_status == 'Negative', na.rm = TRUE),
    Missing = sum(hiv_status == 'Missing', na.rm = TRUE)
  )

# Add a total row
total_row <- cascade %>%
  summarise(across(where(is.numeric), sum)) %>%
  mutate(typology1 = "Total")

# Combine the cascade data with the total row
cascade_with_total <- bind_rows(cascade, total_row)

cascade_with_total

# Function to clean and convert the entries to numeric values
clean_convert <- function(entry) {
  if (is.na(entry)) {
    return(NA_real_)  # Return NA for missing values
  } else if (is.character(entry)) {  # Check if entry is a string
    entry <- tolower(str_trim(entry))
    if (str_detect(entry, 'ldl')) {
      return(0)
    } else if (str_detect(entry, 'copies') & str_detect(entry, 'ml')) {
      return(as.numeric(str_extract(entry, "\\d+")))
    } else if (entry == 'run failed' || !str_detect(entry, "\\d")) {
      return(NA_real_)  # Return NA for failed runs or non-numeric entries
    } else if (str_detect(entry, "^\\d+$")) {
      return(as.numeric(entry))
    } else {
      return(NA_real_)  # Return NA for other cases
    }
  } else {
    return(NA_real_)  # Handle non-string entries (e.g., NA)
  }
}

# How many records have a VL result
(
wdata |>
  filter(hiv_status == "Positive") |>
  filter(grepl("ldl|copies", clab__viral_load_res, ignore.case = TRUE)) |>
  tally() |>
  pull() 
/
  wdata |>
  filter(hiv_status == "Positive") |>
  tally() |>
    pull()
)

# Apply the function to the relevant column in the lab section
library(stringr)
wdata <- wdata %>%
  mutate(vls_result = sapply(clab__viral_load_res, clean_convert))



# 50 copies and below cut off
wdata <- wdata %>%
  mutate(vls_50 = case_when(
    hiv_status == 'Positive' & vls_result < 50 ~ 'Suppressed',
    hiv_status == 'Positive' & vls_result >= 50 ~ 'Not Suppressed',
    hiv_status == 'Positive' & is.na(vls_result) ~ NA_character_,
    TRUE ~ NA_character_
  ))

# 200 copies and below cut off
wdata <- wdata %>%
  mutate(vls_200 = case_when(
    hiv_status == 'Positive' & vls_result < 200 ~ 'Suppressed',
    hiv_status == 'Positive' & vls_result >= 200 ~ 'Not Suppressed',
    hiv_status == 'Positive' & is.na(vls_result) ~ NA_character_,
    TRUE ~ NA_character_
  ))

# Below 1000 copies cut off
wdata <- wdata %>%
  mutate(vls_1000 = case_when(
    hiv_status == 'Positive' & vls_result < 1000 ~ 'Suppressed',
    hiv_status == 'Positive' & vls_result >= 1000 ~ 'Not Suppressed',
    hiv_status == 'Positive' & is.na(vls_result) ~ NA_character_,
    TRUE ~ NA_character_
  ))


# National HIV Prevalence
# uwgt --------------------------------------------------------------

national_overall <- wdata |>
  filter(hiv_status != "Missing") |>
  frq(hiv_status, weights = cr_weights)

national_overall_df <- national_overall[[1]] |>
  mutate(county = NA_character_,
         typo = NA_character_)

# National prevalence by typology
national_overall_typo <- wdata |>
  filter(hiv_status != "Missing") |>
  group_by(typology) |>
  frq(hiv_status, weights = cr_weights)

national_overall_df <- national_overall[[1]] |>
  mutate(county = NA_character_,
         typo = NA_character_)


overall_prevalence_raw <- wdata |>
  filter(!is.na(hiv_status)) |>
  freq_table(hiv_status, percent_ci = 99)

overall_prevalence_wgt <- wdata |>
  #filter(!is.na(hiv_status)) |>
  frq(hiv_status, weights = cr_weights)

overall_prevalence_wgt

freq_table(wdata, hiv_status, weights = cr_weights)


# Overall suppression
wdata |>
  filter(final_hiv_status == "Positive") |>
  frq(vls_200, weights = cr_weights)



library(dplyr)
library(broom)

freq_summarize <- function(data, categorical_column, weight_column, group_by_columns = NULL, confidence_level = 0.95) {
  
  # Convert column names to symbols for use with dplyr
  categorical_column <- enquo(categorical_column)
  weight_column <- enquo(weight_column)
  
  # Handle group_by_columns if provided
  if (!is.null(group_by_columns)) {
    group_by_columns <- syms(group_by_columns)
    data <- data %>%
      group_by(!!!group_by_columns)
  }
  
  # Calculate uwgt frequencies
  uwgt_freq <- data %>%
    count(!!!group_by_columns, !!categorical_column) %>%
    rename(uwgt_n = n)
  
  # Calculate wgt frequencies
  wgt_freq <- data %>%
    group_by(!!!group_by_columns, !!categorical_column) %>%
    summarise(wgt_n = sum(!!weight_column, na.rm = TRUE)) %>%
    ungroup()
  
  # Merge the uwgt and wgt frequencies
  freq_table <- uwgt_freq %>%
    inner_join(wgt_freq, by = c(map_chr(group_by_columns, quo_name), quo_name(categorical_column)))
  
  # Calculate total N for both uwgt and wgt data within each group
  freq_table <- freq_table %>%
    group_by(!!!group_by_columns) %>%
    mutate(
      total_uwgt_N = sum(uwgt_n),
      total_wgt_N = sum(wgt_n)
    ) %>%
    ungroup()
  
  # Calculate proportions
  freq_table <- freq_table %>%
    mutate(
      uwgt_prop = uwgt_n / total_uwgt_N,
      wgt_prop = wgt_n / total_wgt_N
    )
  
  # Calculate uwgt and wgt confidence intervals
  freq_table <- freq_table %>%
    rowwise() %>%
    mutate(
      uwgt_lcl = prop.test(uwgt_n, total_uwgt_N, conf.level = confidence_level)$conf.int[1],
      uwgt_ucl = prop.test(uwgt_n, total_uwgt_N, conf.level = confidence_level)$conf.int[2],
      wgt_lcl = prop.test(wgt_n, total_wgt_N, conf.level = confidence_level)$conf.int[1],
      wgt_ucl = prop.test(wgt_n, total_wgt_N, conf.level = confidence_level)$conf.int[2]
    ) %>%
    ungroup()
  
  return(freq_table)
}


# HIV Prevalence ------------------------------------------------------
library(purrr)

hiv_prevalence_all <- freq_summarize(
  data = wdata |> 
    filter(hiv_status != "Missing") |>
    filter(!is.na(hiv_status)),
  categorical_column = hiv_status,
  weight_column = cr_weights,
  confidence_level = 0.95
) |>
  mutate(county = NA_character_,
         typo = NA_character_) 

hiv_prevalence_all_typo <- freq_summarize(
  data = wdata |> 
    filter(hiv_status != "Missing") |>
    filter(!is.na(hiv_status)),
  categorical_column = hiv_status,
  weight_column = cr_weights,
  group_by_columns = c("typology"),
  confidence_level = 0.95
) |>
  mutate(county = NA_character_,
         typo = typology) |>
  select(-c(typology))

hiv_prevalence_county <- freq_summarize(
  data = wdata |> 
    filter(hiv_status != "Missing") |>
    filter(!is.na(hiv_status)),
  categorical_column = hiv_status,
  weight_column = cr_weights,
  group_by_columns = c("cr_county"),
  confidence_level = 0.95
) |>
  mutate(county = cr_county,
         typo = NA_character_) |>
  select(-c(cr_county))

hiv_prevalence_county_typo <- freq_summarize(
  data = wdata |> 
    filter(hiv_status != "Missing") |>
    filter(!is.na(hiv_status)),
  categorical_column = hiv_status,
  weight_column = cr_weights,
  group_by_columns = c("cr_county", "typology"),
  confidence_level = 0.95
) |>
  mutate(county = cr_county,
         typo = typology) |>
  select(-c(cr_county, typology))  



hiv_prevalence <- bind_rows(hiv_prevalence_all,
                        hiv_prevalence_county,
                        hiv_prevalence_all_typo,
                        hiv_prevalence_county_typo) |>
  filter(hiv_status == "Positive") |>
  rename_with(~ paste0("prevalence_", .), .cols = -c(county, typo))

library(data.table)

fwrite(hiv_prevalence, "data/Results/prevalence.csv")


hiv_prevalence_county_typo_age <- freq_summarize(
  data = wdata |> 
    filter(hiv_status != "Missing") |>
    filter(age_band5 != "<18") |>
    filter(!is.na(age_band5)) |>
    filter(!is.na(hiv_status)),
  categorical_column = hiv_status,
  weight_column = cr_weights,
  group_by_columns = c("cr_county", "typology", "age_band5"),
  confidence_level = 0.95
) |>
  mutate(county = cr_county,
         typo = typology) |>
  select(-c(cr_county, typology))


hiv_prevalence_all_typo_age <- freq_summarize(
  data = wdata |> 
    filter(hiv_status != "Missing") |>
    filter(age_band5 != "<18") |>
    filter(!is.na(age_band5)) |>
    filter(!is.na(hiv_status)),
  categorical_column = hiv_status,
  weight_column = cr_weights,
  group_by_columns = c("typology", "age_band5"),
  confidence_level = 0.95
) |>
  mutate(county = "All",
         typo = typology) |>
  select(-c(typology))


hiv_prevalence_typo_age <- bind_rows(hiv_prevalence_county_typo_age, hiv_prevalence_all_typo_age) |>
  filter(hiv_status == "Positive") |>
  rename_with(~ paste0("prev_", .), .cols = -c(county, typo))


fwrite(hiv_prevalence_typo_age, "data/Results/prevalence_by_age.csv")

# HIV Viral Load Suppression (cut off: 200 copies) ------------------------------------------------------

suppression_all <- freq_summarize(
  data = wdata |> 
    filter(vls_200 != "Missing") |>
    filter(!is.na(vls_200)),
  categorical_column = vls_200,
  weight_column = cr_weights,
  confidence_level = 0.95
) |>
  mutate(county = NA_character_,
         typo = NA_character_) 

suppression_all_typo <- freq_summarize(
  data = wdata |> 
    filter(vls_200 != "Missing") |>
    filter(!is.na(vls_200)),
  categorical_column = vls_200,
  weight_column = cr_weights,
  group_by_columns = c("typology"),
  confidence_level = 0.95
) |>
  mutate(county = NA_character_,
         typo = typology) |>
  select(-c(typology))

suppression_county <- freq_summarize(
  data = wdata |> 
    filter(vls_200 != "Missing") |>
    filter(!is.na(vls_200)),
  categorical_column = vls_200,
  weight_column = cr_weights,
  group_by_columns = c("cr_county"),
  confidence_level = 0.95
) |>
  mutate(county = cr_county,
         typo = NA_character_) |>
  select(-c(cr_county))

suppression_county_typo <- freq_summarize(
  data = wdata |> 
    filter(vls_200 != "Missing") |>
    filter(!is.na(vls_200)),
  categorical_column = vls_200,
  weight_column = cr_weights,
  group_by_columns = c("cr_county", "typology"),
  confidence_level = 0.95
) |>
  mutate(county = cr_county,
         typo = typology) |>
  select(-c(cr_county, typology))  



suppression <- bind_rows(suppression_all,
                            suppression_county,
                            suppression_all_typo,
                            suppression_county_typo) |>
  filter(vls_200 == "Suppressed") |>
  rename_with(~ paste0("suppression_", .), .cols = -c(county, typo))

fwrite(suppression, "data/Results/suppression_200.csv")


suppression_200_county_typo_age <- freq_summarize(
  data = wdata |> 
    filter(vls_200 != "Missing") |>
    filter(age_band5 != "<18") |>
    filter(!is.na(age_band5)) |>
    filter(!is.na(vls_200)),
  categorical_column = vls_200,
  weight_column = cr_weights,
  group_by_columns = c("cr_county", "typology", "age_band5"),
  confidence_level = 0.95
) |>
  mutate(county = cr_county,
         typo = typology) |>
  select(-c(cr_county, typology))


suppression_200_all_typo_age <- freq_summarize(
  data = wdata |> 
    filter(vls_200 != "Missing") |>
    filter(age_band5 != "<18") |>
    filter(!is.na(age_band5)) |>
    filter(!is.na(vls_200)),
  categorical_column = vls_200,
  weight_column = cr_weights,
  group_by_columns = c("typology", "age_band5"),
  confidence_level = 0.95
) |>
  mutate(county = "All",
         typo = typology) |>
  select(-c(typology))


suppression_200_typo_age <- bind_rows(suppression_200_county_typo_age, suppression_200_all_typo_age) |>
  filter(vls_200 == "Suppressed") |>
  rename_with(~ paste0("supp_", .), .cols = -c(county, typo))

fwrite(suppression_200_typo_age, "data/Results/suppression_200_age.csv")


# HIV Viral Load Suppression (cut off: 1000 copies) ------------------------------------------------------

suppression_all <- freq_summarize(
  data = wdata |> 
    filter(vls_1000 != "Missing") |>
    filter(!is.na(vls_1000)),
  categorical_column = vls_1000,
  weight_column = cr_weights,
  confidence_level = 0.95
) |>
  mutate(county = NA_character_,
         typo = NA_character_) 

suppression_all_typo <- freq_summarize(
  data = wdata |> 
    filter(vls_1000 != "Missing") |>
    filter(!is.na(vls_1000)),
  categorical_column = vls_1000,
  weight_column = cr_weights,
  group_by_columns = c("typology"),
  confidence_level = 0.95
) |>
  mutate(county = NA_character_,
         typo = typology) |>
  select(-c(typology))

suppression_county <- freq_summarize(
  data = wdata |> 
    filter(vls_1000 != "Missing") |>
    filter(!is.na(vls_1000)),
  categorical_column = vls_1000,
  weight_column = cr_weights,
  group_by_columns = c("cr_county"),
  confidence_level = 0.95
) |>
  mutate(county = cr_county,
         typo = NA_character_) |>
  select(-c(cr_county))

suppression_county_typo <- freq_summarize(
  data = wdata |> 
    filter(vls_1000 != "Missing") |>
    filter(!is.na(vls_1000)),
  categorical_column = vls_1000,
  weight_column = cr_weights,
  group_by_columns = c("cr_county", "typology"),
  confidence_level = 0.95
) |>
  mutate(county = cr_county,
         typo = typology) |>
  select(-c(cr_county, typology))  



suppression <- bind_rows(suppression_all,
                         suppression_county,
                         suppression_all_typo,
                         suppression_county_typo) |>
  filter(vls_1000 == "Suppressed") |>
  rename_with(~ paste0("suppression_", .), .cols = -c(county, typo))

fwrite(suppression, "data/Results/suppression_1000_cut_off.csv")



suppression_1000_county_typo_age <- freq_summarize(
  data = wdata |> 
    filter(vls_1000 != "Missing") |>
    filter(age_band5 != "<18") |>
    filter(!is.na(age_band5)) |>
    filter(!is.na(vls_1000)),
  categorical_column = vls_1000,
  weight_column = cr_weights,
  group_by_columns = c("cr_county", "typology", "age_band5"),
  confidence_level = 0.95
) |>
  mutate(county = cr_county,
         typo = typology) |>
  select(-c(cr_county, typology))


suppression_1000_all_typo_age <- freq_summarize(
  data = wdata |> 
    filter(vls_1000 != "Missing") |>
    filter(age_band5 != "<18") |>
    filter(!is.na(age_band5)) |>
    filter(!is.na(vls_1000)),
  categorical_column = vls_1000,
  weight_column = cr_weights,
  group_by_columns = c("typology", "age_band5"),
  confidence_level = 0.95
) |>
  mutate(county = "All",
         typo = typology) |>
  select(-c(typology))


suppression_1000_typo_age <- bind_rows(suppression_1000_county_typo_age, suppression_1000_all_typo_age) |>
  filter(vls_1000 == "Suppressed") |>
  rename_with(~ paste0("supp_", .), .cols = -c(county, typo))

fwrite(suppression_1000_typo_age, "data/Results/suppression_1000_age.csv")




# 95-95-95 deep dive

# First 95: awareness of status
filtered_data$aware_ <- ifelse(filtered_data$hivstatus == 1, 2, NA)
filtered_data$aware_[filtered_data$cr_self_report_hiv_status == "HIV Positive" & filtered_data$hivstatus == 1] <- 1
filtered_data$aware_[filtered_data$vls_200 == 1 & filtered_data$hivstatus == 1] <- 1


cascade_data <- wdata |>
  transmute(
    seed = cr_barcode,
    age = age_cat_0
  )


cascade_data <- wdata |>
  filter(cr_eligible == "Yes") %>%
  filter(cr_final_hiv_status == "Positive") %>%
  transmute(
    seed = cr_barcode,
    county = q5_county_living_in,
    age = as.numeric(age_capped),
    age_cat_0 = cut(age,
                    breaks = c(0, 17, 24, 34, 44, 54, Inf),
                    labels = c("<18", "18-24", 
                               "25-34", "35-44", "45-54", "55+")),
    typology = typology,
    
    weight = as.numeric(cr_weights),
    hiv_status = cr_final_hiv_status,
    self_report = cr_self_report_hiv_status
  ) %>%
  mutate(
    aware = case_when(self_report == "HIV Positive" ~ "Aware",
                      self_report %in% c("Dont know", "HIV Negative", "") ~ "Unaware",
                      TRUE ~ NA_character_)
  )


cascade_data %>% filter(!is.na(aware)) %>% freq_table(aware)

freq_summarize(
  data = cascade_data |> filter(!is.na(aware)),
  categorical_column = aware,
  weight_column = weight,
  confidence_level = 0.95
)













