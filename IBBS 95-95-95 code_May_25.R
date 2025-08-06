
#*******************************************************************************
#*
# Set environment and global settings for this analysis
.libPaths(c("C:/rlibs/dir",.libPaths()))

# Set working directory
setwd("C:/Users/nez9/OneDrive - CDC/EO Compliant Files/BBS")
# free memory
rm(list = ls()) # run this to free memory. Caution! this deletes all the data frames and objects
gc()# to do garbage collection clear unused/unneeded memory 

getOption("max.print") #to show how many records can be displayed, set to suit needs
max.print = 5000000 #This can be varied depending on the user's computer capacity
memory.limit(size = 5000000) #This can vary per needs, the higher the memory size the more data one can load 
options(scipen=7)
getOption("max.print")

#Store system date
currentDate <- Sys.Date()

##this function prints the rds dataset from the for loop
print.rds <- function(x, Den, Num, as.percentage=TRUE, ...) {
  # # Safeguard: Return NULL if object or required data is missing
  # if (is.null(x) || is.null(x$interval)) {
  #   warning("print.rds: Input object or 'interval' is NULL. Returning NULL.")
  #   return(NULL)
  # }
  fmt <- function(x, ...) {
    format(x, ..., scientific=FALSE)
  }
  
  matest <- matrix(x$interval, ncol = 6, byrow = FALSE)
  
  if (nrow(matest) > 1) {
    rownames(matest) <- names(x$interval)[1:nrow(matest)]
  } 
  else {
    rownames(matest) <- x$outcome.variable
    names(x$interval) <- rep(x$outcome.variable, length(x$interval))
  }
  
  rownames(matest)[is.na(rownames(matest))] <- "NA"
  colnames(matest) <- c("point", "lower", "upper", "Design_Effect", "s.e.", "n")
  
  if (is.null(as.percentage)) {
    as.percentage <- attr(x, "is.cts")
    as.percentage <- if (is.logical(as.percentage) && (as.percentage == FALSE)) {
      TRUE
    } else {
      FALSE
    }
  }
  
  if (as.percentage) {
    matest[, c(1, 2, 3, 5)] <- 100 * matest[, c(1, 2, 3, 5)]
  }
  
  matest <- data.frame(matest)
  mm <- setNames(cbind(rownames(matest), matest, row.names = NULL), 
                 c(x$outcome.variable, "point", "lower", "upper", 
                   "Design_Effect", "s.e.", "n"))
  
  # Ensure confidence intervals stay within [0, 100]
  mm$lower <- pmax(mm$lower, 0)  # Set lower limit to 0%
  mm$upper <- pmin(mm$upper, 100)  # Set upper limit to 100%
  
  
  # Adjust upper if point is greater than upper
  mm$upper <- ifelse(mm$point > mm$upper, mm$point + (mm$point - mm$lower), mm$upper)
 
  mm$weighted_N = Num
  
  mm$weighted_D = Den
  
  mm$sample <- sum(mm$n)  # Total sample size
  
  return(mm)
}

##printing function
print.age.rds <- function(x, Den, Num, as.percentage=TRUE, ...) {
  # Helper function to format numbers
  fmt <- function(x, ...) {
    format(x, ..., scientific=FALSE)
  }
  
  # Initialize an empty list to store results
  mm <- list()
  
  # Loop over all names in the list x
  
  
  for(name in names(x)) {
    # Convert the 'interval' column to a matrix with 6 columns
    matest <- matrix(x[[name]]$interval, ncol = 6, byrow = FALSE)
    
    # Handle row names
    if (nrow(matest) > 1) {
      rownames(matest) <- names(x[[name]]$interval)[1:nrow(matest)]
    } else {
      rownames(matest) <- x[[name]]$outcome.variable
      names(x[[name]]$interval) <- rep(x[[name]]$outcome.variable, length(x[[name]]$interval))
    }
    rownames(matest)[is.na(rownames(matest))] <- "NA"
    
    # Set column names for the matrix
    colnames(matest) <- c("point", "lower", "upper", "Design_Effect", "s.e.", "n")
    
    # Convert to percentage if required
    if(as.percentage) {
      matest[, c(1, 2, 3, 5)] <- 100 * matest[, c(1, 2, 3, 5)]
    }
    
    # Convert matrix to data frame
    matest_df <- data.frame(matest)
    
    # Update mm with the processed data frame
    mm[[name]] <- setNames(cbind(rownames(matest_df), matest_df), 
                           c(x[[name]]$outcome.variable, "point", "lower", "upper", 
                             "Design_Effect", "s.e.", "n"))
    row.names(mm[[name]]) <- NULL
    
    # Calculate weighted_N, weighted_D and add sample size
    mm[[name]]$weighted_N <- ceiling(x[[name]]$weighted_N)
    mm[[name]]$weighted_D <- ceiling(x[[name]]$weighted_D)
    
    #mm$weighted_N = Num
    
    #mm$weighted_D = Den
    
    # Calculate total sample size
    mm[[name]]$sample <- sum(mm[[name]]$n)
    mm[[name]]$name <- name
    mm[[name]]$lower <- pmax(mm[[name]]$lower, 0)  # Set lower limit to 0%
    mm[[name]]$upper <- pmin(mm[[name]]$upper, 100)  # Set upper limit to 100%
    
    # Adjust upper if point is greater than upper
    mm[[name]]$upper <- ifelse(mm[[name]]$point > mm[[name]]$upper, mm[[name]]$point + (mm[[name]]$point - mm[[name]]$lower), mm[[name]]$upper)
  }
  
  return(mm)
}


# Define a vector of required package names
packages <- c("gridExtra", "ggplot2", "network", "igraph", "dplyr", "reshape2", "scales", 
              "anytime", "Hmisc", "statnet.common", "ergm", "isotone", "survey",'RDS','xfun', "sspse", 
              "testthat", "tidyverse", "officer","tidyr", "readxl", "Hmisc", "stringr","writexl", "labelled", "ggplot2", "tidyr")  

# Check if each package is installed and install it if missing
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Apply the function to all packages
lapply(packages, install_if_missing)

# Call needed libraries
library(gridExtra)
library(ggplot2)
library(network)
library(igraph)
library(reshape2)
library(scales)
library(anytime)
library(Hmisc)
library(statnet.common)
library(ergm)
library(isotone)
library(survey)
library(sspse)
library(testthat)
library(tidyr)
library(RDS)
library(readxl)
library(dplyr)
library(stringr)
library(labelled)
library(writexl)
library(tidyverse)
library(officer)

# Set working directory
setwd("C:/Users/nez9/OneDrive - CDC/EO Compliant Files/BBS")

# Load Excel file
data <- read_excel("weighted_mapped_all_vars_21_02_2025.xlsx", sheet = "Sheet1")

# Rename all variables to lowercase
colnames(data) <- tolower(colnames(data))

# problem seeds

problem_seed = c("KMB020544", "KLC030547", "MKB020268"  , "MSC030011","NBB020310", "NBB020584", "NBB020535", "NBC030104", "NBC030226", "NBD040452", "NKB020204", "NBB020310")

# MAke columns unique to avoid duplicate columns
names(data) <- make.unique(names(data))
# Filter the dataset based on the conditions
data <- data %>%
  filter(cr_completed_interview == "Yes" | cr_barcode %in% problem_seed)

# Count observations
nrow(data)

# Check which records whose recruiter id is not in the barcode
data <- data %>%
  mutate(
    valid = case_when(
      !(cr_rid %in% cr_barcode) ~ 'Invalid',   # If recruiter ID is not in barcode column
      TRUE ~ 'Valid'                            # Otherwise, it's valid
    )
  )
table(data$valid)

# cross tabulate valid column with completed interview column
cross_tab <- table(data$valid, data$cr_completed_interview)

cross_tab

# count how many seeds we have
count_seed <- nrow(data$cr_rid == 'seed')
count_seed

##This one ID recruited themselves so we force them to be seed
data$cr_rid[data$cr_rid == "NBD040452"]<-"seed"
# List of barcodes for which cr_rid should be assigned 'seed'
barcode_list <- c("KMB020544", "KLC030547", "MKB020268", 
                  "MSC030011", "NBB020310", "NBB020584", 
                  "NBB020535", "NBC030104", "NBC030226", 
                  "NBD040452", "NKB020204", "NBB020310")

# Assign 'seed' to cr_rid for records whose cr_barcode is in the specified list
data$cr_rid[data$cr_barcode %in% barcode_list] <- 'seed'

data$cr_rid[is.na(data$cr_rid)]<-"seed"

# Rename columns 
names(data)[names(data) == "cr_county"] <- "County"


names(data)[names(data) == "cr_typology"] <- "Typology"


names(data)[names(data) == "cr_age"] <- "Age"

# Recode age into bands  - smaller age bands
# data$age_band <- ifelse(is.na(data$Age), "Age_Missing",
#                         ifelse(data$Age < 25, "18-24",
#                                ifelse(data$Age >= 25 & data$Age < 35, "25-34",
#                                       ifelse(data$Age >= 35 & data$Age < 45, "35-44", "45+"))))
# Recode age into bands  - wider age bands
data$age_band <- ifelse(is.na(data$Age), "Age_Missing",
                        ifelse(data$Age < 35, "18-34", "35+"))

# Filter out missing ages
data <- data %>%
  filter(age_band != "Age_Missing")

# Summary of the recoded age bands
table(data$age_band)

# Recode final_HIV_status 
data <- data %>%
  mutate(
    final_HIV_status = case_when(
      (cr_final_hiv_status =='Negative') ~ 'Negative',  
      (cr_final_hiv_status =='Positive'& clab__viral_load_res %in% c(NA, 'Haemolysed sample','Run Failed Collect New Sample')) ~ 'Positive but No VL Results',
      (cr_final_hiv_status =='Positive') ~ 'Positive',
      TRUE ~ 'N/A'                            # Otherwise, it's valid
    )
  )

# # filter out those with no valid VL results
# data <- data %>%
#   filter(
#     final_HIV_status %in% c("N/A", "Negative") | 
#       (final_HIV_status == "Positive" & !clab__viral_load_res %in% c(NA, 'Haemolysed sample','Run Failed Collect New Sample'))
#   )

# Summarize the recoded final_HIV_status column
table(data$final_HIV_status)

# Self reported art
data <- data %>%
  mutate(art1 = case_when(
    # No condition
    (cr_currently_on_arv =='No') ~ 'No',
    
    # Yes condition
    (cr_currently_on_arv =='Yes') ~ 'Yes',
    
    # Default case
    TRUE ~ 'N/A'
  ))

# Check the distribution of the `art1` variable
table(data$art1)

# Self reported HIV awareness
data <- data %>%
  mutate(aware1 = case_when(
    # Conditions for 'Yes'
    cr_self_report_hiv_status %in% c('Positive') ~ 'Yes',
    
    # Default case
    TRUE ~ 'No'
  ))

# Check the distribution of the `aware1` variable
table(data$aware1)

# Function to clean and convert VL entries to numeric values
clean_convert <- function(entry) {
  if (is.character(entry)) {  # Check if entry is a string
    entry <- tolower(trimws(entry))
    
    if (grepl("ldl", entry)) {
      return(0)
    } else if (grepl("target not detected", entry)) {
      return(0)
    } else if (grepl("copies", entry) & grepl("ml", entry)) {
      # Extract digits only
      return(as.numeric(gsub("\\D", "", entry)))
    } else if (entry == "run failed" | !grepl("\\d", entry)) {
      return(NA)  # Or use NA if preferred
    } else if (grepl("^\\d+$", entry)) {
      return(as.numeric(entry))
    } else {
      return(NA)  # Handle other cases if necessary
    }
  } else {
    return(NA)  # Handle non-string entries (e.g., NA)
  }
}

# Apply the function to the relevant column 
data$vls_result <- sapply(data$clab__viral_load_res, clean_convert) # cr_vl_results

# Check if any missing cr_barcode remains
sum(is.na(data$cr_barcode)) # Should return 0

# Ensure IDs are character
data$cr_barcode <- as.character(data$cr_barcode)
data$cr_rid <- as.character(data$cr_rid)

# Impute missing network sizes and ensure they are positive
data$cr_network[is.na(data$cr_network) | data$cr_network <= 0] <- median(data$cr_network, na.rm = TRUE)

# Check for missing or invalid IDs
any(is.na(data$cr_barcode))  # Should return FALSE
any(is.na(data$cr_rid))      # Seeds can have NA or 'Seed', but others should not

# How many seeds do we have
sum(data$cr_rid == "seed")

# Check data types for id and recruiter.id columns
str(data$cr_barcode)
str(data$cr_rid)

# Check for missing or invalid network sizes
any(is.na(data$cr_network))  # Should return FALSE after imputing
any(data$cr_network <= 0)    # Should also return FALSE
'-------------------------------------------------------------------------------------------------------------------'
# Create a new column 'vls_1000' based on viral load results cut off pointof <1000
data <- data %>%
  mutate(vls_1000 = case_when(
    is.na(vls_result) ~ 'Missing',           # Check for NA first
    vls_result < 1000 ~ 'Suppressed',        # Viral load suppressed
    vls_result >= 1000 ~ 'Unsuppressed'      # Viral load unsuppressed (>= 1000)
  ))

# Get the frequency count of 'vls_1000' categories
table(data$vls_1000)

# Create a new variable called 'vls200 showing those suppressed based on the cutoff of <200 copies
data <- data %>%
  mutate(vls_200 = case_when(
    is.na(vls_result) ~ 'Missing',           # Check for NA first
    vls_result < 200 ~ 'Suppressed',        # Viral load suppressed
    vls_result >= 200 ~ 'Unsuppressed'      # Viral load unsuppressed (>= 200)
  ))

# Get the frequency count of 'vls_200' categories
table(data$vls_200)

# Create a new variable called 'vls50 showing those suppressed based on the cutoff of <50 copies
data <- data %>%
  mutate(vls_50 = case_when(
    is.na(vls_result) ~ 'Missing',           # Check for NA first
    vls_result < 50 ~ 'Suppressed',        # Viral load suppressed
    vls_result >= 50 ~ 'Unsuppressed'      # Viral load unsuppressed (>= 50)
  ))

# Get the frequency count of 'vls_200' categories
table(data$vls_50)

'-------------------------------------------------------------------------------------------------------------------------'
# Get a small working file
# Subset the data to keep only the specified variables
working_data <- (data %>%
               select(cr_rid, cr_barcode, cr_consented_blood_sample, County, age_band, Typology, cr_sex,cr_network, final_HIV_status, cr_weights, aware1, art1, vls_result, vls_50, vls_200, vls_1000))
head(working_data)

# Mutate new variables
working_data <- working_data %>%
  mutate(
   # Total HIV Positive
    hiv_status = case_when(
     final_HIV_status=='Positive' & cr_consented_blood_sample == 'Yes' ~ 1, # assign 1 if positive
     final_HIV_status=='Negative'~ 0, # assign 0 if negative
     TRUE ~ NA # All other cases are assigned to missing
    )
    ) %>%
    
    # Those aware of HIV status among PLHIV - either self reported yes or VLS_200 suppressed
    mutate(
      aware = case_when(
      hiv_status == 1 & (aware1=='Yes' | vls_200=='Suppressed')~1, #| art1 =='Yes')~ 1,
      hiv_status == 1~ 0,
      TRUE ~ NA
    ) 
    ) %>%
    
    # On ART among those aware - aware and reported yes or VLS_200 is suppressed
    mutate (
      art_con = case_when(
      aware == 1 & (art1 == 'Yes'| vls_200=='Suppressed')~ 1, # either self report on ART or virally suppressed
      aware == 1 ~ 0,
      TRUE ~ NA
    )
    ) %>%
    
    # suppressed among those on ART based on 50 copies cut off
    mutate(
      suppressed_con_50 = case_when(
      art_con == 1 & vls_50 == 'Suppressed' ~ 1,
      art_con == 1 & vls_50 == 'Unsuppressed' ~ 0,
      TRUE ~ NA
    ),
    
    # suppressed among those on ART based on 200 copies cut off
    suppressed_con_200 = case_when(
      art_con == 1  & vls_200 == 'Suppressed' ~ 1,
      art_con == 1 & vls_200 == 'Unsuppressed' ~ 0,
      TRUE ~ NA
    ),
    
    # suppressed among those on ART based on 1000 copies cut off
    suppressed_con_1000 = case_when(
      art_con == 1 & vls_1000 == 'Suppressed' ~ 1,
      art_con == 1 & vls_1000 == 'Unsuppressed' ~ 0,
      TRUE ~ NA
    ),
    
    # On ART among all PLHIV - either self reported or VLS_200 suppressed
    art_uncon = case_when(
      hiv_status == 1 & (vls_200 == 'Suppressed' | art1 == 'Yes')~ 1,
      hiv_status == 1 ~ 0,
      TRUE ~ NA
    ),
    
    # suppressed among all PLHIV based on 50 copies cut off
    suppressed_uncon_50 = case_when(
      hiv_status == 1 & vls_50 == 'Suppressed' ~ 1,
      hiv_status == 1 & vls_50 == 'Unsuppressed' ~ 0,
      TRUE ~ NA
    ),
    
    # suppressed among all PLHIV based on 200 copies cut off
    suppressed_uncon_200 = case_when(
      hiv_status == 1  & vls_200 == 'Suppressed' ~ 1,
      hiv_status == 1 & vls_200 == 'Unsuppressed' ~ 0,
      TRUE ~ NA
    ),
    
    # suppressed among all PLHIV based on 1000 copies cut off
    suppressed_uncon_1000 = case_when(
      hiv_status == 1 & vls_1000 == 'Suppressed' ~ 1,
      hiv_status == 1 & vls_1000 == 'Unsuppressed' ~ 0,
      TRUE ~ NA
    )
  )

# Observe the data
table(working_data$hiv_status)
table(working_data$aware)
table(working_data$art_con)
table(working_data$suppressed_con_50)
table(working_data$suppressed_con_200)
table(working_data$suppressed_con_1000)
table(working_data$art_uncon)
table(working_data$suppressed_uncon_50)
table(working_data$suppressed_uncon_200)
table(working_data$suppressed_uncon_1000)

# Get the right dataset for conditional cascade analysis
con_data <- (working_data %>%
                   select(cr_rid, cr_barcode, County, age_band, Typology, cr_sex, cr_network, cr_weights, aware, art_con, suppressed_con_50, suppressed_con_1000, suppressed_con_200))

# Get the right dataset for non conditional cascade analysis
non_data <- (working_data %>%
               select(cr_rid, cr_barcode, County, age_band, Typology, cr_sex, cr_network,cr_weights, aware, art_uncon, suppressed_uncon_50, suppressed_uncon_1000, suppressed_uncon_200))

csvFileName <- paste("C:/Users/nez9/OneDrive - CDC/EO Compliant Files/BBS/non_data.csv")
write.csv(non_data, file=csvFileName)

csvFileName1 <- paste("C:/Users/nez9/OneDrive - CDC/EO Compliant Files/BBS/con_data.csv")
write.csv(con_data, file=csvFileName1)

'-----------------------------------------------------------------------------------------------'
##read in the PSE
population = read_excel("anchored_pse_compliant.xlsx")

population$name = paste0(population$County,".", population$Typology)

population$typo = paste0(population$Typology)

# read in population by age and sex based on weights
AgePop = read_excel("AgePop.xlsx")
SexPop = read_excel("SexPop.xlsx")

##create a column with names of the splits see: names(prev_typo)

AgePop$name = paste0(AgePop$County,".", AgePop$Typology)

AgePop$typo = paste0(AgePop$Typology)

AgePop$age = paste0(AgePop$County, ".", AgePop$Typology, ".", AgePop$age_band)

AgePop$typoage = paste0(AgePop$Typology, ".", AgePop$age_band)

SexPop$sex = paste0(SexPop$County, ".", SexPop$Typology, ".", SexPop$cr_sex)

'*******************************************95-95-95 ANALYSIS BY TYPOLOGY, AGE AND COUNTY: **************************************

What this code chunk does and notes: 
1. Analysis by typology, age and county
2. Essentially gives an output of the sites, each with its own typology

NOTE: In the unconditional cascades, the 2nd 95 is not retricted to those who were aware of HIV status, the 3rd 95 is not 
restricted to PLHIV on ARV. All 95s are based on the PLHIV.  

**********************************************************************************************************************'

# Conditional Cascade Analysis

# Get the data
con_data = read.csv("con_data.csv")
# Remove the unnamed column 
con_data <- con_data[, -1]

con_data$cr_rid[con_data$cr_rid == 'seed'] <- '0'
# Replace "age missing" with NA
con_data$age_band[con_data$age_band == "Age_Missing"] <- NA

# Convert any factor to characters
con_data$County <- as.character(con_data$County)
con_data$Typology <- as.character(con_data$Typology)
con_data$age_band <- as.character(con_data$age_band)
con_data$cr_sex <- as.character(con_data$cr_sex)

# Split the data
prev_typo_overall_con = split(con_data, f = list(con_data$Typology)) # for overall without county
prev_typo_con = split(con_data, f = list(con_data$County, con_data$Typology)) # by county and typology
prev_typo_age_con = split(con_data, f = list(con_data$County, con_data$Typology, con_data$age_band)) # by county, age and typology
prev_typo_typoage_con = split(con_data, f = list(con_data$Typology, con_data$age_band)) # by age and typology
prev_typo_sex_con = split(con_data, f = list(con_data$County, con_data$Typology, con_data$cr_sex)) # by county, age and typology

##create empty lists for typology only
prev_typo = vector(mode = "list", length = length(prev_typo_overall_con))## create empty list of size 23 (length(rds))name = names(rds[i])
res_typo = vector(mode = "list", length = length(prev_typo_overall_con))
data_typo.rds = vector(mode = "list", length = length(prev_typo_overall_con))
results_typo = vector(mode = "list", length = length(prev_typo_overall_con))
Num = vector(mode = "list", length = length(prev_typo_overall_con))
Den = vector(mode = "list", length = length(prev_typo_overall_con))

##create empty lists for county and typology
prev_county = vector(mode = "list", length = length(prev_typo_con))## create empty list of size 23 (length(rds))name = names(rds[i])
res_county = vector(mode = "list", length = length(prev_typo_con))
data_county.rds = vector(mode = "list", length = length(prev_typo_con))
results_county = vector(mode = "list", length = length(prev_typo_con))
Num = vector(mode = "list", length = length(prev_typo_con))
Den = vector(mode = "list", length = length(prev_typo_con))

##create empty lists for county, age and typology
#prev_age = vector(mode = "list", length = length(prev_typo_age_con))## create empty list of size 23 (length(rds))name = names(rds[i])
#res_age = vector(mode = "list", length = length(prev_typo_age_con))
#data_age.rds = vector(mode = "list", length = length(prev_typo_age_con))
#results_age = vector(mode = "list", length = length(prev_typo_age_con))
#Num = vector(mode = "list", length = length(prev_typo_age_con))
#Den = vector(mode = "list", length = length(prev_typo_age_con))

'****************************##1st 95: KPLHIV aware of their HIV status*********************************'
# Conditional cascade

# for typology only
# Set seed to ensure reproducibility
set.seed(12345)  # You can choose any number as the seed

##loop over split data (awareness of HIV status)
for(i in 1:length(prev_typo_overall_con)) {
  print(paste("Iteration:", i))
  
  if(nrow(prev_typo_overall_con[[i]])>0){
    
    name = names(prev_typo_overall_con[i])
    prev_typo_overall_con[[i]]$cr_network[is.na(prev_typo_overall_con[[i]]$cr_network)]<-median(prev_typo_overall_con[[i]]$cr_network, na.rm = TRUE)## assign 
    
    data_typo.rds[[i]] <- RDS::as.rds.data.frame(prev_typo_overall_con[[i]], id='cr_barcode', recruiter.id='cr_rid', network.size='cr_network', population.size=sum(population$N[population$typo == as.character(name)]))
    
    data_typo.rds[[i]]$seed <- get.seed.id(data_typo.rds[[i]])
    data_typo.rds[[i]]$wave <- get.wave(data_typo.rds[[i]])
    
    res_typo[[i]] <- RDS.bootstrap.intervals(data_typo.rds[[i]], outcome.variable=c("aware"), 
                                        weight.type="Gile's SS", uncertainty="Gile", 
                                        confidence.level=0.95, number.of.bootstrap.samples=1000, 
                                        to.factor=TRUE, 
                                        N=sum(population$N[population$typo == as.character(name)]), 
                                        number.ss.samples.per.iteration=1000)
   
    Num[[i]] = ceiling(sum(res_typo[[i]]$weights[data_typo.rds[[i]]$aware == 1], na.rm = TRUE))
    Den[[i]] = ceiling(sum(res_typo[[i]]$weights[data_typo.rds[[i]]$aware %in% c(0,1)], na.rm = TRUE))
    
    results_typo[[i]] = print.rds(res_typo[[i]], Den[[i]], Num[[i]])
    results_typo[[i]]$prev_typo_overall_con = names(prev_typo_overall_con[i])
  }
}

aware_typo = bind_rows(results_typo)
# Create new columns for overal age and county
aware_typo <- aware_typo %>%
  mutate(age_band = "Overall",
         County = "Overall")%>%
  rename("Typology"= "prev_typo_overall_con")

# rearrange the columns
aware_typo <- (aware_typo %>%
                   select(aware, point, lower, upper, Design_Effect,s.e.,n, weighted_N, weighted_D, sample, age_band, County, Typology)) 

# for typology and county
# Set seed to ensure reproducibility
set.seed(12345)  # You can choose any number as the seed

##loop over split data (awareness of HIV status)
for(i in 1:length(prev_typo_con)) {
  print(paste("Iteration:", i))
  
  if(nrow(prev_typo_con[[i]])>0){
    # Check if art_con has both 0 and 1 (i.e., shows variability)
    aware_vals <- unique(na.omit(prev_typo_con[[i]]$aware))
    if (!all(c(0, 1) %in% aware_vals)) {
      cat(" -> Skipping (art_con lacks variability)\n")
      next
    }
    name = names(prev_typo_con[i])
    prev_typo_con[[i]]$cr_network[is.na(prev_typo_con[[i]]$cr_network)]<-median(prev_typo_con[[i]]$cr_network, na.rm = TRUE)## assign 
    
    data_county.rds[[i]] <- RDS::as.rds.data.frame(prev_typo_con[[i]], id='cr_barcode', recruiter.id='cr_rid', network.size='cr_network', population.size=sum(population$N[population$name == as.character(name)]))
    
    data_county.rds[[i]]$seed <- get.seed.id(data_county.rds[[i]])
    data_county.rds[[i]]$wave <- get.wave(data_county.rds[[i]])
    
    res_county[[i]] <- RDS.bootstrap.intervals(data_county.rds[[i]], outcome.variable=c("aware"), 
                                        weight.type="Gile's SS", uncertainty="Gile", 
                                        confidence.level=0.95, number.of.bootstrap.samples=1000, 
                                        to.factor=TRUE, 
                                        N=sum(population$N[population$name == as.character(name)]), 
                                        number.ss.samples.per.iteration=1000)
    
    Num[[i]] = ceiling(sum(res_county[[i]]$weights[data_county.rds[[i]]$aware == 1], na.rm = TRUE))
    Den[[i]] = ceiling(sum(res_county[[i]]$weights[data_county.rds[[i]]$aware %in% c(0,1)], na.rm = TRUE))
    
    results_county[[i]] = print.rds(res_county[[i]], Den[[i]], Num[[i]])
    results_county[[i]]$prev_typo_con = names(prev_typo_con[i])
  }
}

aware_county = bind_rows(results_county)

# Create a new column and seperate Prev_typo_con column
aware_county <- aware_county %>%
  mutate(age_band = "Overall") %>%
  separate(prev_typo_con, into = c("County", "Typology"), sep = "\\.")
# rearrange the columns
aware_county <- (aware_county %>%
               select(aware, point, lower, upper, Design_Effect,s.e.,n, weighted_N, weighted_D, sample, age_band, County, Typology)) 

# By County, Typology and age

# Initialize the list for storing results
res <- list()

# Loop over each iteration
for (i in 1:length(prev_typo_con)) {
  print(paste("Iteration:", i))

  # Wrap the entire iteration block with tryCatch to handle errors
  tryCatch({
    if (nrow(prev_typo_con[[i]]) > 0) {
      name <- names(prev_typo_con[i])
      
      # Handle missing values in 'cr_network'
      prev_typo_con[[i]]$cr_network[is.na(prev_typo_con[[i]]$cr_network)] <- median(prev_typo_con[[i]]$cr_network, na.rm = TRUE)
      
      # Create the RDS object for the current dataset
      data_county.rds[[i]] <- as.rds.data.frame(
        prev_typo_con[[i]],
        id = 'cr_barcode',
        recruiter.id = 'cr_rid',
        network.size = 'cr_network',
        population.size = sum(AgePop$N[AgePop$name == as.character(name)])
      )
      
      # Assign seed and wave information
      data_county.rds[[i]]$seed <- get.seed.id(data_county.rds[[i]])
      data_county.rds[[i]]$wave <- get.wave(data_county.rds[[i]])
      
      # Get unique age bands and filter out NAs
      age_bands <- unique(data_county.rds[[i]]$age_band)
      age_bands <- age_bands[!is.na(age_bands)]
      
      # Initialize list to store results for each age band
      res_age <- list()

      # Loop over each age band
      for (age_band in age_bands) {
        # Wrap inner loop with tryCatch to ensure it continues if there’s an error
        tryCatch({
          age_band_char <- as.character(age_band)
          
          # Copy data for the current age band
          age_band_data <- data_county.rds[[i]]
          
          # Check if the required columns exist in the dataset
          if (!all(c('cr_barcode', 'cr_rid', 'cr_network', 'age_band') %in% names(age_band_data))) {
            print(paste("Skipping age band", age_band, "due to missing variables"))
            return()  # Skip to the next iteration of the loop
          }

          # Run the RDS bootstrap intervals for the current age band
          res_age[[age_band]] <- tryCatch({
              RDS.bootstrap.intervals(
              age_band_data,
              outcome.variable = c("aware"),
              weight.type = "Gile's SS",
              uncertainty = "Gile",
              confidence.level = 0.95,
              number.of.bootstrap.samples = 30000,
              to.factor = TRUE,
              N = sum(AgePop$N[AgePop$name == as.character(name)], na.rm = TRUE),
              number.ss.samples.per.iteration = 5000,
              subset = age_band_data$age_band == age_band_char
            )
          }, error = function(e) {
            cat("Error for age_band", age_band, ":", conditionMessage(e), "\n")
            NULL  # Return NULL if an error occurs
          })
          # Attach the filtered input data to the RDS result
          res_age[[age_band]]$variables <- age_band_data[age_band_data$age_band == age_band_char, ]
          
          # Check if the results have valid data
          if (is.null(res_age[[age_band]]$interval)) {
            print(paste("No interval data for age_band", age_band))
            res_age[[age_band]] <- NULL  # Assign NULL or some placeholder
          } else {
            print(paste("Interval data exists for age_band", age_band))
            
            # Attach filtered input data used in RDS estimation
            res_age[[age_band]]$variables <- age_band_data[age_band_data$age_band == age_band_char, ]
            
            # Safety check: only compute weighted_N if weights are present
            if (!is.null(res_age[[age_band]]$weights)) {
              res_age[[age_band]]$weighted_N <- sum(
                res_age[[age_band]]$weights[res_age[[age_band]]$variables$aware == 1],
                na.rm = TRUE
              )
              res_age[[age_band]]$weighted_D <- sum(
                res_age[[age_band]]$weights[res_age[[age_band]]$variables$aware %in% c(0,1)],
                na.rm = TRUE
              )
            } else {
              res_age[[age_band]]$weighted_N <- NA  # fallback
              res_age[[age_band]]$weighted_D <- NA
            }
          }
        }, error = function(e) {
          cat("Error in age band loop for age_band", age_band, ":", conditionMessage(e), "\n")
        })
      }

      # Store results by iteration if valid
      if (length(res_age) > 0) {
        res[[i]] <- res_age
      } else {
        res[[i]] <- NULL  # Assign NULL if no valid results
      }
    }
  }, error = function(e) {
    cat("Error in main loop for iteration", i, ":", conditionMessage(e), "\n")
  })
}

# Final processing of results
final_res <- list()
dat <- list()

for (i in 1:length(res)) {
  print(i)
  
  # Check if res[[i]] exists and is valid
  if (is.null(res[[i]])) {
    print(paste("Skipping iteration", i, "due to NULL results"))
    next  # Skip this iteration if no valid results
  }

  # Process the valid res[[i]]
  x = res[[i]]
  
  # call function to process the RDS results
  final_res[[i]] = print.age.rds(res[[i]]) #,Den[[i]], Num[[i]])
  
  # Combine results into a single dataframe
  dat[[i]] = bind_rows(final_res[[i]])
  
  # Add site name from the original data
  dat[[i]]$Site = names(prev_typo_con[i])
}

# bind all data into one dataframe
aware_age <- bind_rows(dat)
aware_age <- aware_age %>%
  separate(Site, into = c("County", "Typology"), sep = "\\.")
names(aware_age)[names(aware_age) == "name"] <- "age_band"

# This section of the code splits data by county typology and age
# checks if there is variability in the data for the variable of interest
# where there is no variability, calculates proportions directly

# Step 1: Split the dataset by County, Typology, and age_band
# prev_typo_age_con <- split(con_data, f = list(con_data$County, con_data$Typology, con_data$age_band))

# Initialize an empty list to store results
result_list <- list()
result_list1 <- list()
# Step 2: Loop through each subset of data
for (i in names(prev_typo_age_con)) {
  
  # Get the current subset of data
  subset_data <- prev_typo_age_con[[i]]
  
  # Skip if the subset is NULL or has no rows
  if (is.null(subset_data) || nrow(subset_data) == 0) {
    next
  }
  # Clean the aware column: convert "NA" and "" to proper NA
  subset_data$aware <- ifelse(
    subset_data$aware %in% c("NA", "", NA),
    NA,
    as.numeric(subset_data$aware)
  )
  
  aware_vals <- subset_data$aware
  
  # Skip if all values are NA
  if (all(is.na(aware_vals))) {
    next
  }
  
  # Check if all entries in aware are either 1 or 1 and NA
  
  unique_vals <- unique(aware_vals)
  
  # Check if the unique values are only 1 and/or NA
  if (all(unique_vals %in% c(1, NA))) {
    print(paste("Processing subset:", i))
    
    # Calculate n (count of 1 or 1 and NA in aware)
    aware <- 1
    point <- 100
    lower <- 100
    upper <- 100
    Design_Effect <- 0
    s.e. <-0
    n <- sum(aware_vals == 1, na.rm = TRUE)  # Count where aware_1000 is 1  
    weighted_N <- ceiling(sum(subset_data$cr_weights[aware_vals == 1], na.rm = TRUE))  # Sum where aware == 1
    weighted_D <- ceiling(sum(subset_data$cr_weights[aware_vals == 1], na.rm = TRUE))  # Sum where aware is 0 or 1
    sample  <- sum(aware_vals == 1, na.rm = TRUE)
    
    # Bind the calculated values into a data frame for this subset
    result_subset <- data.frame(
      aware = aware,
      point = point,
      lower = lower,
      upper = upper,
      Design_Effect = Design_Effect,
      s.e. = s.e.,
      n = n,
      weighted_N = weighted_N,
      weighted_D = weighted_D,
      sample = sample,
      age_band = unique(subset_data$age_band),
      County = unique(subset_data$County),
      Typology = unique(subset_data$Typology)
    )
    
    # Add this result to the result_list
    result_list <- append(result_list, list(result_subset))
  }
}

# Step 3: Combine all the result subsets into a final data frame
final_result <- do.call(rbind, result_list)

# View the final result
print(final_result)

# Repeat the above steps for aware is either 0 or 0 and NA
# Step 2: Loop through each subset of data
for (i in names(prev_typo_age_con)) {
  
  # Get the current subset of data
  subset_data <- prev_typo_age_con[[i]]
  
  # Skip if the subset is NULL or has no rows
  if (is.null(subset_data) || nrow(subset_data) == 0) {
    next
  }
  # Clean the aware column: convert "NA" and "" to proper NA
  subset_data$aware <- ifelse(
    subset_data$aware %in% c("NA", "", NA),
    NA,
    as.numeric(subset_data$aware)
  )
  
  aware_vals <- subset_data$aware
  
  # Skip if all values are NA
  if (all(is.na(aware_vals))) {
    next
  }
  
  # Check if all entries in aware are either 1 or 1 and NA
  
  unique_vals <- unique(aware_vals)
  
  # Check if the unique values are only 0 and/or NA
  if (all(unique_vals %in% c(0, NA))) {
    print(paste("Processing subset:", i))
    
    # Calculate n (count of 1 or 1 and NA in aware)
    aware <- 0
    point <- 0
    lower <- 0
    upper <- 0
    Design_Effect <- 0
    s.e. <-0
    n <- sum(aware_vals == 0)  # Count where aware is 0 or NA
    weighted_N <- ceiling(sum(subset_data$cr_weights[aware_vals == 0], na.rm = TRUE))  # Sum where aware == 1
    weighted_D <- ceiling(sum(subset_data$cr_weights[aware_vals == 0], na.rm = TRUE))  # Sum where aware is 0 or 1
    sample  <- sum(aware_vals == 0)
    
    # Bind the calculated values into a data frame for this subset
    result_subset1 <- data.frame(
      aware = aware,
      point = point,
      lower = lower,
      upper = upper,
      Design_Effect = 'Design_Effect',
      s.e. = s.e.,
      n = n,
      weighted_N = weighted_N,
      weighted_D = weighted_D,
      sample = sample,
      age_band = unique(subset_data$age_band),
      County = unique(subset_data$County),
      Typology = unique(subset_data$Typology)
    )
    
    # Add this result to the result_list
    result_list1 <- append(result_list1, list(result_subset1))
  }
}

# Step 3: Combine all the result subsets into a final data frame
final_result1 <- do.call(rbind, result_list1)

# View the final result
print(final_result1)

# By Typology and age

# Initialize the list for storing results
res <- list()

# Loop over each iteration
for (i in 1:length(prev_typo_overall_con)) {
  print(paste("Iteration:", i))
  
  # Wrap the entire iteration block with tryCatch to handle errors
  tryCatch({
    if (nrow(prev_typo_overall_con[[i]]) > 0) {
      name <- names(prev_typo_overall_con[i])
      
      # Handle missing values in 'cr_network'
      prev_typo_overall_con[[i]]$cr_network[is.na(prev_typo_overall_con[[i]]$cr_network)] <- median(prev_typo_overall_con[[i]]$cr_network, na.rm = TRUE)
      
      # Create the RDS object for the current dataset
      data_typo.rds[[i]] <- as.rds.data.frame(
        prev_typo_overall_con[[i]],
        id = 'cr_barcode',
        recruiter.id = 'cr_rid',
        network.size = 'cr_network',
        population.size = sum(AgePop$N[AgePop$typo == as.character(name)])
      )
      
      # Assign seed and wave information
      data_typo.rds[[i]]$seed <- get.seed.id(data_typo.rds[[i]])
      data_typo.rds[[i]]$wave <- get.wave(data_typo.rds[[i]])
      
      # Get unique age bands and filter out NAs
      age_bands <- unique(data_typo.rds[[i]]$age_band)
      age_bands <- age_bands[!is.na(age_bands)]
      
      # Initialize list to store results for each age band
      res_age <- list()
      
      # Loop over each age band
      for (age_band in age_bands) {
        # Wrap inner loop with tryCatch to ensure it continues if there’s an error
        tryCatch({
          age_band_char <- as.character(age_band)
          
          # Copy data for the current age band
          age_band_data <- data_typo.rds[[i]]
          
          # Check if the required columns exist in the dataset
          if (!all(c('cr_barcode', 'cr_rid', 'cr_network', 'age_band') %in% names(age_band_data))) {
            print(paste("Skipping age band", age_band, "due to missing variables"))
            return()  # Skip to the next iteration of the loop
          }
          
          # Run the RDS bootstrap intervals for the current age band
          res_age[[age_band]] <- tryCatch({
            RDS.bootstrap.intervals(
              age_band_data,
              outcome.variable = c("aware"),
              weight.type = "Gile's SS",
              uncertainty = "Gile",
              confidence.level = 0.95,
              number.of.bootstrap.samples = 30000,
              to.factor = TRUE,
              N = sum(AgePop$N[AgePop$typo == as.character(name)], na.rm = TRUE),
              number.ss.samples.per.iteration = 5000,
              subset = age_band_data$age_band == age_band_char
            )
          }, error = function(e) {
            cat("Error for age_band", age_band, ":", conditionMessage(e), "\n")
            NULL  # Return NULL if an error occurs
          })
          # Attach the filtered input data to the RDS result
          res_age[[age_band]]$variables <- age_band_data[age_band_data$age_band == age_band_char, ]
          
          # Check if the results have valid data
          if (is.null(res_age[[age_band]]$interval)) {
            print(paste("No interval data for age_band", age_band))
            res_age[[age_band]] <- NULL  # Assign NULL or some placeholder
          } else {
            print(paste("Interval data exists for age_band", age_band))
            
            # Attach filtered input data used in RDS estimation
            res_age[[age_band]]$variables <- age_band_data[age_band_data$age_band == age_band_char, ]
            
            # Safety check: only compute weighted_N if weights are present
            if (!is.null(res_age[[age_band]]$weights)) {
              res_age[[age_band]]$weighted_N <- sum(
                res_age[[age_band]]$weights[res_age[[age_band]]$variables$aware == 1],
                na.rm = TRUE
              )
              res_age[[age_band]]$weighted_D <- sum(
                res_age[[age_band]]$weights[res_age[[age_band]]$variables$aware %in% c(0,1)],
                na.rm = TRUE
              )
            } else {
              res_age[[age_band]]$weighted_N <- NA  # fallback
              res_age[[age_band]]$weighted_D <- NA
            }
          }
        }, error = function(e) {
          cat("Error in age band loop for age_band", age_band, ":", conditionMessage(e), "\n")
        })
      }
      
      # Store results by iteration if valid
      if (length(res_age) > 0) {
        res[[i]] <- res_age
      } else {
        res[[i]] <- NULL  # Assign NULL if no valid results
      }
    }
  }, error = function(e) {
    cat("Error in main loop for iteration", i, ":", conditionMessage(e), "\n")
  })
}

# Final processing of results
final_res <- list()
dat <- list()

for (i in 1:length(res)) {
  print(i)
  
  # Check if res[[i]] exists and is valid
  if (is.null(res[[i]])) {
    print(paste("Skipping iteration", i, "due to NULL results"))
    next  # Skip this iteration if no valid results
  }
  
  # Process the valid res[[i]]
  x = res[[i]]
  
  # call function to process the RDS results
  final_res[[i]] = print.age.rds(res[[i]]) #,Den[[i]], Num[[i]])
  
  # Combine results into a single dataframe
  dat[[i]] = bind_rows(final_res[[i]])
  
  # Add site name from the original data
  dat[[i]]$Site = names(prev_typo_overall_con[i])
}

# bind all data into one dataframe
aware_typoage <- bind_rows(dat)
aware_typoage <- aware_typoage %>%
  mutate(County = "Overall")%>%
  rename("Typology" = "Site")
names(aware_typoage)[names(aware_typoage) == "name"] <- "age_band"

# Rearrange columns
aware_typoage <- aware_typoage %>%
  select(aware, point, lower, upper, Design_Effect,s.e.,n, weighted_N, weighted_D, sample, age_band, County, Typology)

# This section of the code splits data by county typology and age
# checks if there is variability in the data for the variable of interest
# where there is no variability, calculates proportions directly

# Step 1: Split the dataset by County, Typology, and age_band
#prev_typo_age_con <- split(con_data, f = list(con_data$County, con_data$Typology, con_data$age_band))

# Initialize an empty list to store results
result_list_typoage <- list()
result_list_typoage1 <- list()
# Step 2: Loop through each subset of data
for (i in names(prev_typo_typoage_con)) {
  
  # Get the current subset of data
  subset_data <- prev_typo_typoage_con[[i]]
  
  # Skip if the subset is NULL or has no rows
  if (is.null(subset_data) || nrow(subset_data) == 0) {
    next
  }
  # Clean the aware column: convert "NA" and "" to proper NA
  subset_data$aware <- ifelse(
    subset_data$aware %in% c("NA", "", NA),
    NA,
    as.numeric(subset_data$aware)
  )
  
  aware_vals <- subset_data$aware
  
  # Skip if all values are NA
  if (all(is.na(aware_vals))) {
    next
  }
  
  # Check if all entries in aware are either 1 or 1 and NA
  
  unique_vals <- unique(aware_vals)
  
  # Check if the unique values are only 1 and/or NA
  if (all(unique_vals %in% c(1, NA))) {
    print(paste("Processing subset:", i))
    
    # Calculate n (count of 1 or 1 and NA in aware)
    aware <- 1
    point <- 100
    lower <- 100
    upper <- 100
    Design_Effect <- 0
    s.e. <-0
    n <- sum(aware_vals == 1, na.rm = TRUE)  # Count where aware_1000 is 1  
    weighted_N <- ceiling(sum(subset_data$cr_weights[aware_vals == 1], na.rm = TRUE))  # Sum where aware == 1
    weighted_D <- ceiling(sum(subset_data$cr_weights[aware_vals == 1], na.rm = TRUE))  # Sum where aware is 0 or 1
    sample  <- sum(aware_vals == 1, na.rm = TRUE)
    
    # Bind the calculated values into a data frame for this subset
    result_subset <- data.frame(
      aware = aware,
      point = point,
      lower = lower,
      upper = upper,
      Design_Effect = Design_Effect,
      s.e. = s.e.,
      n = n,
      weighted_N = weighted_N,
      weighted_D = weighted_D,
      sample = sample,
      age_band = unique(subset_data$age_band),
      County = unique(subset_data$County),
      Typology = unique(subset_data$Typology)
    )
    
    # Add this result to the result_list
    result_list_typoage <- append(result_list_typoage, list(result_subset))
  }
}

# Step 3: Combine all the result subsets into a final data frame
final_result_typoage <- do.call(rbind, result_list_typoage)

# View the final result
print(final_result_typoage)

# Repeat the above steps for aware is either 0 or 0 and NA
# Step 2: Loop through each subset of data
for (i in names(prev_typo_typoage_con)) {
  
  # Get the current subset of data
  subset_data <- prev_typo_typoage_con[[i]]
  
  # Skip if the subset is NULL or has no rows
  if (is.null(subset_data) || nrow(subset_data) == 0) {
    next
  }
  # Clean the aware column: convert "NA" and "" to proper NA
  subset_data$aware <- ifelse(
    subset_data$aware %in% c("NA", "", NA),
    NA,
    as.numeric(subset_data$aware)
  )
  
  aware_vals <- subset_data$aware
  
  # Skip if all values are NA
  if (all(is.na(aware_vals))) {
    next
  }
  
  # Check if all entries in aware are either 1 or 1 and NA
  
  unique_vals <- unique(aware_vals)
  
  # Check if the unique values are only 0 and/or NA
  if (all(unique_vals %in% c(0, NA))) {
    print(paste("Processing subset:", i))
    
    # Calculate n (count of 1 or 1 and NA in aware)
    aware <- 0
    point <- 0
    lower <- 0
    upper <- 0
    Design_Effect <- 0
    s.e. <-0
    n <- sum(aware_vals == 0)  # Count where aware is 0 or NA
    weighted_N <- ceiling(sum(subset_data$cr_weights[aware_vals == 0], na.rm = TRUE))  # Sum where aware == 1
    weighted_D <- ceiling(sum(subset_data$cr_weights[aware_vals == 0], na.rm = TRUE))  # Sum where aware is 0 or 1
    sample  <- sum(aware_vals == 0)
    
    # Bind the calculated values into a data frame for this subset
    result_subset1 <- data.frame(
      aware = aware,
      point = point,
      lower = lower,
      upper = upper,
      Design_Effect = 'Design_Effect',
      s.e. = s.e.,
      n = n,
      weighted_N = weighted_N,
      weighted_D = weighted_D,
      sample = sample,
      age_band = unique(subset_data$age_band),
      County = unique(subset_data$County),
      Typology = unique(subset_data$Typology)
    )
    
    # Add this result to the result_list
    result_list_typoage1 <- append(result_list_typoage1, list(result_subset_typoage1))
  }
}

# Step 3: Combine all the result subsets into a final data frame
final_result_typoage1 <- do.call(rbind, result_list_typoage1)

# View the final result
print(final_result_typoage1)

# Combine all the datasets into one
aware_typo$aware <- as.character(aware_typo$aware)
aware_age$aware <- as.character(aware_age$aware)
aware_typoage$aware <- as.character(aware_typoage$aware)
aware_county$aware <- as.character(aware_county$aware)
final_result$aware <- as.character(final_result$aware)
final_result1$aware <- as.character(final_result1$aware)
final_result_typoage$aware <- as.character(final_result_typoage$aware)
final_result_typoage1$aware <- as.character(final_result_typoage1$aware)
aware_age <- bind_rows(aware_typo,aware_county,aware_typoage,aware_age, final_result, final_result1, final_result_typoage, final_result_typoage1)
aware_age <- aware_age %>%
  mutate('95_Cascade' = "Aware_of_HIV_Status")
aware_age = aware_age[aware_age$aware == 1,]
aware_age
'**********************************************************************************************************'
'**************************##2nd 95: KPLHIV On ARVS out of all KPLHIV aware of their status****************'
# for second and 3rd 95's repeat the above.
# repeat for unconditional cascade
# if possible, you can come up with a loop to repeat these recursive procedures

