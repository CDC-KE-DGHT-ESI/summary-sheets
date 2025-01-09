library(RDS)
library(readxl)
library(tidyr)
library(tidyverse)


##read data
data = read_excel("data/weighted_data_02_10_2024.xlsx",
                  col_types = "text")



#recode age and do summary

#Recode cr_age into cr_age bands

data$age_band = NA
data$age_band[data$cr_age < 25] <- '18-24'
data$age_band[data$cr_age >= 25 & data$cr_age < 35] <- '25-34'
data$age_band[data$cr_age >= 35 & data$cr_age < 45] <- '35-44'
#data$age_band[data$cr_age >= 44 & data$cr_age < 55] <- '45-54'
data$age_band[data$cr_age >= 45] <- '45+'

# data$age_band[data$cr_age >= 18 & data$cr_age < 25] <- '18-24'
# data$age_band[data$cr_age >= 25 & data$cr_age < 30] <- '25-39'
# data$age_band[data$cr_age >= 30 & data$cr_age < 35] <- '30-34'
# data$age_band[data$cr_age >= 35 & data$cr_age < 40] <- '35-39'
# data$age_band[data$cr_age >= 40 & data$cr_age < 45] <- '40-44'
# data$age_band[data$cr_age >= 45 & data$cr_age < 50] <- '45-49'
# data$age_band[data$cr_age >= 50] <- '50+'




# Summary of age_band
#summary(as.factor(data$age_band))

##subset those who completed interview and the 9 who were no shows but had recruits: Total = 12106+9
##These are the 9 IDS that have issues
problem_seed = c("KMB020544", "KLC030547", "MKB020268", "MSC030011","NBB020310", "NBB020584", "NBB020535", "NBC030104", "NBC030226", "NBD040452", "NKB020204", "NBB020310")
data = data[!is.na(data$cr_completed_interview) & (data$cr_barcode %in% problem_seed|data$cr_completed_interview == "Yes"),]

##Get those who gave blood
data$blood_collected <- ifelse(!is.na(data$cr_blood_sample_collected) & data$cr_completed_interview == 'Yes' & data$cr_blood_sample_collected == 'Yes', 'Yes', 'No')


##create final HIV status variable
data$final_HIV_status = NA

data$final_HIV_status[(data$blood_collected == 'Yes' & data$fin_hiv_res == 'Negative') |
                        (data$blood_collected == 'Yes' & is.na(data$cr_self_report_hiv_status) & data$clab__hiv_rapid_res == 'Non-Reactive') |
                        (data$blood_collected == 'Yes' & data$cr_self_report_hiv_status == 'HIV Negative' & data$clab__hiv_rapid_res == 'Non-Reactive') |
                        (data$blood_collected == 'Yes' & data$clab__final_hiv_res == 'Negative')] <-'Negative'

data$final_HIV_status[(data$blood_collected == 'Yes' & data$fin_hiv_res == 'Positive') |
                        (data$blood_collected == 'Yes' & data$clab__hiv_rapid_res == 'Reactive') |
                        (data$blood_collected == 'Yes' & data$clab__final_hiv_res == 'Positive')] <- 'Positive'



summary(as.factor(data$final_HIV_status))


##This one ID recruited themselves so we force them to be seed
data$cr_rid[data$cr_rid == "NBD040452"]<-"seed"
data$cr_rid[is.na(data$cr_rid)]<-"seed"
#-------------------------------------------
##read in the PSE
population = read_excel("data/Results/anchored_pse.xlsx")

##create a column with names of the splits see: names(prev_typo)
population$name = paste0(population$County,".", population$Tyology)
prev_typo = split(data, f = list(data$cr_county, data$cr_typology))

######------------------------------------
##this function prints the rds dataset from the for loop
print.rds <- function(x, as.percentage=TRUE, ...) {
  fmt <- function(x,...){
    format(x,...,scientific=FALSE)
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
  mnames <- max(nchar(names(x$interval)[1:nrow(matest)]))
  colnames(matest) <- c("point", "lower", "upper", "Design Effect", 
                        "s.e.", "n")
  
  if(is.null(as.percentage)){
    as.percentage <- attr(x, "is.cts")
    as.percentage <- if(is.logical(as.percentage) && (as.percentage==FALSE)){
      as.percentage <- TRUE
    }else{as.percentage <- FALSE}
  }
  
  if(as.percentage){
    matest[,c(1,2,3,5)] <- 100*matest[,c(1,2,3,5)]
  }
  
  matest = data.frame(matest)
  mm  = setNames(cbind(rownames(matest), matest, row.names = NULL), 
                 c(x$outcome.variable,"point", "lower", "upper", "Design Effect", 
                   "s.e.", "n"))
  
  
  mm$weighted_N = round((x$N*mm$point)/100,0)
  mm$weighted_D = x$N
  
  mm$sample = sum(mm$n)
  mm$lower <- pmax(mm$lower, 0)  # Set lower limit to 0%
  mm$upper <- pmin(mm$upper, 100)  # Set lower limit to 0%
  
  return(mm)
}


population = read_excel("data/Results/anchored_pse.xlsx")
population$name = paste0(population$County)
##split data by typoplogy
#prev_typo = split(data, f = paste(data$cr_typology));length(prev_typo)
# 
# prev_typo = split(data, f = c(paste(data$cr_county), paste(data$cr_typology)));length(prev_typo)
prev_typo = split(data, f = list(data$cr_county))



##create empty lists
prev = vector(mode = "list", length = length(prev_typo))## create empty list of size 23 (length(rds))name = names(rds[i])
res = vector(mode = "list", length = length(prev_typo))#
data.rds = vector(mode = "list", length = length(prev_typo))#
results = vector(mode = "list", length = length(prev_typo))#


##loop over splitted data
for(i in 1:length(prev_typo)) {
  print(paste("Iteration:", i))
  
  if(nrow(prev_typo[[i]]>0)){
    name = names(prev_typo[i])
    prev_typo[[i]]$cr_network[is.na(prev_typo[[i]]$cr_network)]<-median(prev_typo[[i]]$cr_network, na.rm = TRUE)## assign 
    
    data.rds[[i]] <- as.rds.data.frame(prev_typo[[i]], id='cr_barcode', recruiter.id='cr_rid', network.size='cr_network', population.size=sum(population$N[population$name == as.character(name)]))
    
    data.rds[[i]]$seed <- get.seed.id(data.rds[[i]])
    data.rds[[i]]$wave <- get.wave(data.rds[[i]])
    
    res[[i]] <- RDS.bootstrap.intervals(data.rds[[i]], outcome.variable=c("final_HIV_status"), weight.type="Gile's SS", uncertainty="Gile", confidence.level=0.95, number.of.bootstrap.samples=1000, to.factor=TRUE, N=sum(population$N[population$name == as.character(name)]), number.ss.samples.per.iteration=1000)
    
    results[[i]] = print.rds(res[[i]])
    results[[i]]$prev_typo = names(prev_typo[i])
  }
}

rds_results = bind_rows(results)

rds_results = rds_results[rds_results$final_HIV_status == "Positive",]

write.csv(rds_results, "county_prev_results.csv")



###TYPOLOGY
population = read_excel("data/Results/anchored_pse.xlsx")
population$name = paste0(population$Tyology)
##split data by typoplogy
prev_typo = split(data, f = paste(data$cr_typology));length(prev_typo)
##create empty lists
prev = vector(mode = "list", length = length(prev_typo))## create empty list of size 23 (length(rds))name = names(rds[i])
res = vector(mode = "list", length = length(prev_typo))#
data.rds = vector(mode = "list", length = length(prev_typo))#
results = vector(mode = "list", length = length(prev_typo))#


##loop over splitted data
for(i in 1:length(prev_typo)) {
  print(paste("Iteration:", i))
  
  if(nrow(prev_typo[[i]]>0)){
    name = names(prev_typo[i])
    prev_typo[[i]]$cr_network[is.na(prev_typo[[i]]$cr_network)]<-median(prev_typo[[i]]$cr_network, na.rm = TRUE)## assign 
    
    data.rds[[i]] <- as.rds.data.frame(prev_typo[[i]], id='cr_barcode', recruiter.id='cr_rid', network.size='cr_network', population.size=sum(population$N[population$name == as.character(name)]))
    
    data.rds[[i]]$seed <- get.seed.id(data.rds[[i]])
    data.rds[[i]]$wave <- get.wave(data.rds[[i]])
    
    res[[i]] <- RDS.bootstrap.intervals(data.rds[[i]], outcome.variable=c("final_HIV_status"), weight.type="Gile's SS", uncertainty="Gile", confidence.level=0.95, number.of.bootstrap.samples=1000, to.factor=TRUE, N=sum(population$N[population$name == as.character(name)]), number.ss.samples.per.iteration=1000)
    
    results[[i]] = print.rds(res[[i]])
    results[[i]]$prev_typo = names(prev_typo[i])
  }
}

typo_results = bind_rows(results)

typo_results = typo_results[typo_results$final_HIV_status == "Positive",]

write.csv(typo_results, "typo_prev_results.csv")     


###County-Typology

population = read_excel("data/Results//anchored_pse.xlsx")

##create a column with names of the splits see: names(prev_typo)
population$name = paste0(population$County,".", population$Tyology)
prev_typo = split(data, f = list(data$cr_county, data$cr_typology))

prev = vector(mode = "list", length = length(prev_typo))## create empty list of size 23 (length(rds))name = names(rds[i])
res = vector(mode = "list", length = length(prev_typo))#
data.rds = vector(mode = "list", length = length(prev_typo))#
results = vector(mode = "list", length = length(prev_typo))#


##loop over splitted data
for(i in 1:length(prev_typo)) {
  print(paste("Iteration:", i))
  
  if(nrow(prev_typo[[i]]>0)){
    name = names(prev_typo[i])
    prev_typo[[i]]$cr_network[is.na(prev_typo[[i]]$cr_network)]<-median(prev_typo[[i]]$cr_network, na.rm = TRUE)## assign 
    
    data.rds[[i]] <- as.rds.data.frame(prev_typo[[i]], id='cr_barcode', recruiter.id='cr_rid', network.size='cr_network', population.size=sum(population$N[population$name == as.character(name)]))
    
    data.rds[[i]]$seed <- get.seed.id(data.rds[[i]])
    data.rds[[i]]$wave <- get.wave(data.rds[[i]])
    
    res[[i]] <- RDS.bootstrap.intervals(data.rds[[i]], outcome.variable=c("final_HIV_status"), weight.type="Gile's SS", uncertainty="Gile", confidence.level=0.95, number.of.bootstrap.samples=1000, to.factor=TRUE, N=sum(population$N[population$name == as.character(name)]), number.ss.samples.per.iteration=1000)
    
    results[[i]] = print.rds(res[[i]])
    results[[i]]$prev_typo = names(prev_typo[i])
  }
}

county_typo_results = bind_rows(results)

county_typo_results = county_typo_results[county_typo_results$final_HIV_status == "Positive",]


county_typo_results <- county_typo_results %>%
  separate(prev_typo, into = c("County", "Typology"), sep = "\\.")


write.csv(county_typo_results, "data/Results/estimation/county_typo_results.csv")  


###typology age

population = read_excel("data/Results/anchored_pse.xlsx")
population$name = paste0(population$Tyology)
##split data by typoplogy
prev_typo = split(data, f = paste(data$cr_typology));length(prev_typo)

##create empty lists
prev = vector(mode = "list", length = length(prev_typo))## create empty list of size 23 (length(rds))name = names(rds[i])
res = vector(mode = "list", length = length(prev_typo))#
data.rds = vector(mode = "list", length = length(prev_typo))#
results = vector(mode = "list", length = length(prev_typo))#

# Loop for all datasets in prev_typo
for (i in 1:length(prev_typo)) {   
  print(paste("Iteration:", i))  
  
  if (nrow(prev_typo[[i]]) > 0) {   
    name <- names(prev_typo[i])
    
    # Handle missing values in 'cr_network'
    prev_typo[[i]]$cr_network[is.na(prev_typo[[i]]$cr_network)] <- median(prev_typo[[i]]$cr_network, na.rm = TRUE)
    
    # Create the RDS object for the current dataset
    data.rds[[i]] <- as.rds.data.frame(
      prev_typo[[i]], 
      id = 'cr_barcode', 
      recruiter.id = 'cr_rid', 
      network.size = 'cr_network', 
      population.size = sum(population$N[population$name == as.character(name)])
    )
    
    # Assign seed and wave information
    data.rds[[i]]$seed <- get.seed.id(data.rds[[i]])
    data.rds[[i]]$wave <- get.wave(data.rds[[i]])
    # Get unique age bands and filter out NAs
    age_bands <- unique(data.rds[[i]]$age_band)
    age_bands <- age_bands[!is.na(age_bands)]
    # Initialize list to store results for each age band
    res_age <- list()
    
    # Loop over each age band
    for (age_band in age_bands) {
      
      age_band_char <- as.character(age_band)
      # Copy data for the current age band
      age_band_data <- data.rds[[i]]
      # # 
      # # Create a logical index for rows not in the current age band
      # not_in_age_band <- age_band_data$age_band != age_band
      # 
      # # Replace NA values in the logical vector with TRUE
      # not_in_age_band[is.na(not_in_age_band)] <- TRUE
      # 
      # # Assign NA to all columns except 'cr_barcode', 'cr_rid', and 'cr_network' for rows not in the age band
      # age_band_data[not_in_age_band, !(names(age_band_data) %in% c("cr_barcode", "cr_rid", "cr_network"))] <- NA
      # 
      # Run the RDS bootstrap intervals for the current age band
      res_age[[age_band]] <- RDS.bootstrap.intervals(
        age_band_data, 
        outcome.variable = c("final_HIV_status"), 
        weight.type = "Gile's SS", 
        uncertainty = "Gile", 
        confidence.level = 0.95, 
        number.of.bootstrap.samples = 10000, 
        to.factor = TRUE, 
        N = sum(population$N[population$name == as.character(name)]), 
        number.ss.samples.per.iteration = 1000,
        subset = age_band_data$age_band == age_band_char
      )
      
      # Debugging: Check if res_age contains valid data
      if (is.null(res_age[[age_band]]$interval)) {
        print(paste("Warning: No interval data for age_band", age_band))
      } else {
        print(paste("Interval data exists for age_band", age_band))
        
      }
    }
    
    # Store results by iteration
    

    #result[[i]] <- lapply(res[[i]], print.rds)
    
    res[[i]] <- res_age
    
    
    # Debugging: Check the structure of res[[i]] before matrix creation
    #print(str(res[[i]]))
    
    # Convert results to matrix if data exists
    if (!is.null(res[[i]]$interval)) {
      results[[i]] <- matrix(res[[i]]$interval, ncol = 6, byrow = FALSE)
    } else {
      print(paste("No interval data for iteration", i))
    }
    
    
  } 
}


print.age.rds <- function(x, as.percentage=TRUE, ...) {
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
    colnames(matest) <- c("point", "lower", "upper", "Design Effect", "s.e.", "n")
    
    # Convert to percentage if required
    if(as.percentage) {
      matest[, c(1, 2, 3, 5)] <- 100 * matest[, c(1, 2, 3, 5)]
    }
    
    # Convert matrix to data frame
    matest_df <- data.frame(matest)
    
    # Update mm with the processed data frame
    mm[[name]] <- setNames(cbind(rownames(matest_df), matest_df), 
                           c(x[[name]]$outcome.variable, "point", "lower", "upper", 
                             "Design Effect", "s.e.", "n"))
    row.names(mm[[name]]) <- NULL
    # Calculate weighted_N and add sample size
    mm[[name]]$weighted_N <- round((x[[name]]$N * mm[[name]]$point) / 100, 0)
    mm[[name]]$weighted_D <- x[[name]]$N
    
    # Calculate total sample size
    mm[[name]]$sample <- sum(mm[[name]]$n)
    mm[[name]]$name <- name
  }
  
  return(mm)
}





final_res = list()
dat = list()
for(i in 1:length(res)){
  print(i)
  x = res[[i]]
  
  final_res[[i]] = print.age.rds(x)
  
  dat[[i]] = bind_rows(final_res[[i]])
  
  dat[[i]]$Site = names(prev_typo[i])
}

fin_dat = bind_rows(dat)

write.csv(fin_dat, "data/Results/estimation/typo_age.csv")
##Split site

# fin_dat <- fin_dat %>%
#   separate(Site, into = c("County", "Typology"), sep = "\\.")



###county-typology-age
#create empty lists
##read in the PSE
population = read_excel("data/Results/anchored_pse.xlsx")

##create a column with names of the splits see: names(prev_typo)
population$name = paste0(population$County,".", population$Tyology)
prev_typo = split(data, f = list(data$cr_county, data$cr_typology))


prev = vector(mode = "list", length = length(prev_typo))## create empty list of size 23 (length(rds))name = names(rds[i])
res = vector(mode = "list", length = length(prev_typo))#
data.rds = vector(mode = "list", length = length(prev_typo))#
results = vector(mode = "list", length = length(prev_typo))#

# Loop for all datasets in prev_typo
for (i in 1:length(prev_typo)) {   
  print(paste("Iteration:", i))  
  
  if (nrow(prev_typo[[i]]) > 0) {   
    name <- names(prev_typo[i])
    
    # Handle missing values in 'cr_network'
    prev_typo[[i]]$cr_network[is.na(prev_typo[[i]]$cr_network)] <- median(prev_typo[[i]]$cr_network, na.rm = TRUE)
    
    # Create the RDS object for the current dataset
    data.rds[[i]] <- as.rds.data.frame(
      prev_typo[[i]], 
      id = 'cr_barcode', 
      recruiter.id = 'cr_rid', 
      network.size = 'cr_network', 
      population.size = sum(population$N[population$name == as.character(name)])
    )
    
    # Assign seed and wave information
    data.rds[[i]]$seed <- get.seed.id(data.rds[[i]])
    data.rds[[i]]$wave <- get.wave(data.rds[[i]])
    # Get unique age bands and filter out NAs
    age_bands <- unique(data.rds[[i]]$age_band)
    age_bands <- age_bands[!is.na(age_bands)]
    # Initialize list to store results for each age band
    res_age <- list()
    
    # Loop over each age band
    for (age_band in age_bands) {
      
      age_band_char <- as.character(age_band)
      # Copy data for the current age band
      age_band_data <- data.rds[[i]]
      # # 
      # # Create a logical index for rows not in the current age band
      # not_in_age_band <- age_band_data$age_band != age_band
      # 
      # # Replace NA values in the logical vector with TRUE
      # not_in_age_band[is.na(not_in_age_band)] <- TRUE
      # 
      # # Assign NA to all columns except 'cr_barcode', 'cr_rid', and 'cr_network' for rows not in the age band
      # age_band_data[not_in_age_band, !(names(age_band_data) %in% c("cr_barcode", "cr_rid", "cr_network"))] <- NA
      # 
      # Run the RDS bootstrap intervals for the current age band
      res_age[[age_band]] <- RDS.bootstrap.intervals(
        age_band_data, 
        outcome.variable = c("final_HIV_status"), 
        weight.type = "Gile's SS", 
        uncertainty = "Gile", 
        confidence.level = 0.95, 
        number.of.bootstrap.samples = 10000, 
        to.factor = TRUE, 
        N = sum(population$N[population$name == as.character(name)]), 
        number.ss.samples.per.iteration = 1000,
        subset = age_band_data$age_band == age_band_char
      )
      
      # Debugging: Check if res_age contains valid data
      if (is.null(res_age[[age_band]]$interval)) {
        print(paste("Warning: No interval data for age_band", age_band))
      } else {
        print(paste("Interval data exists for age_band", age_band))
        
      }
    }
    
    # Store results by iteration
    
    
    #result[[i]] <- lapply(res[[i]], print.rds)
    
    res[[i]] <- res_age
    
    
    # Debugging: Check the structure of res[[i]] before matrix creation
    #print(str(res[[i]]))
    
    # Convert results to matrix if data exists
    if (!is.null(res[[i]]$interval)) {
      results[[i]] <- matrix(res[[i]]$interval, ncol = 6, byrow = FALSE)
    } else {
      print(paste("No interval data for iteration", i))
    }
    
    
  } 
}


final_res = list()
dat = list()
for(i in 1:length(res)){
  print(i)
  x = res[[i]]
  
  final_res[[i]] = print.age.rds(x)
  
  dat[[i]] = bind_rows(final_res[[i]])
  
  dat[[i]]$Site = names(prev_typo[i])
}

fin_dat = bind_rows(dat)

fin_dat <- fin_dat %>%
  separate(Site, into = c("County", "Typology"), sep = "\\.")


write.csv(fin_dat, "data/Results/estimation/county_typo_age_prev.csv")



## Who have you told that you have sex with men?
## ========================================================================
## =======================================================================

##create MSM disclosure outcome variable
data <- data %>%
  mutate(
    msm_disclosure = case_when(
      str_detect(q1004_people_you_told_your_msm, "1") ~ "Told Nobody",
      q1004_people_you_told_your_msm != "1" ~ "Told Anybody",
      TRUE ~ NA_character_
      )
  )

data_msm <- data %>%
  dplyr::filter(cr_typology == "MSM")

# mutate(who_was_told = case_when(
#   q1004_people_you_told_your_msm == "1" ~ "No one",
#   q1004_people_you_told_your_msm == "2" ~ "Family",
#   q1004_people_you_told_your_msm == "2 3" ~ "Family, Partner",
#   q1004_people_you_told_your_msm == "2 3 4" ~ "Family, Partner, Friends",
#   q1004_people_you_told_your_msm == "2 3 4 5" ~ "Family, Partner, Friends, Healthcare",
#   q1004_people_you_told_your_msm == "2 3 4 5 6" ~ "Family, Partner, Friends, Healthcare, Work",
#   q1004_people_you_told_your_msm == "2 3 4 5 6 7" ~ "Family, Partner, Friends, Healthcare, Work, Neighbors",
#   q1004_people_you_told_your_msm == "2 3 4 5 6 7 8" ~ "Family, Partner, Friends, Healthcare, Work, Neighbors, Others",
#   q1004_people_you_told_your_msm == "2 3 4 6 7" ~ "Family, Partner, Friends, Work, Neighbors",
#   TRUE ~ "Other" # Default category
# ))


population = read_excel("data/Results/anchored_pse.xlsx")
population$name = paste0(population$County)
##split data by typoplogy
#prev_typo = split(data, f = paste(data$cr_typology));length(prev_typo)
# 
# prev_typo = split(data, f = c(paste(data$cr_county), paste(data$cr_typology)));length(prev_typo)
prev_typo = split(data_msm, f = list(data_msm$cr_county))



##create empty lists
prev = vector(mode = "list", length = length(prev_typo))## create empty list of size 23 (length(rds))name = names(rds[i])
res = vector(mode = "list", length = length(prev_typo))#
data.rds = vector(mode = "list", length = length(prev_typo))#
results = vector(mode = "list", length = length(prev_typo))#


##loop over splitted data
for(i in 1:length(prev_typo)) {
  print(paste("Iteration:", i))
  
  if(nrow(prev_typo[[i]]>0)){
    name = names(prev_typo[i])
    prev_typo[[i]]$cr_network[is.na(prev_typo[[i]]$cr_network)]<-median(prev_typo[[i]]$cr_network, na.rm = TRUE)## assign 
    
    data.rds[[i]] <- as.rds.data.frame(prev_typo[[i]], id='cr_barcode', 
                                       recruiter.id='cr_rid', 
                                       network.size='cr_network', 
                                       population.size=sum(population$N[population$name == as.character(name)]))
    
    data.rds[[i]]$seed <- get.seed.id(data.rds[[i]])
    data.rds[[i]]$wave <- get.wave(data.rds[[i]])
    
    res[[i]] <- RDS.bootstrap.intervals(data.rds[[i]], outcome.variable=c("msm_disclosure"), 
                                        weight.type="Gile's SS", 
                                        uncertainty="Gile", 
                                        confidence.level=0.95, 
                                        number.of.bootstrap.samples=1000, 
                                        to.factor=TRUE, N=sum(population$N[population$name == as.character(name)]), 
                                        number.ss.samples.per.iteration=1000)
    
    results[[i]] = print.rds(res[[i]])
    results[[i]]$prev_typo = names(prev_typo[i])
  }
}

rds_results = bind_rows(results)

#rds_results = rds_results[rds_results$final_HIV_status == "Positive",]

write.csv(rds_results, "data/Results/estimation/msm_disclosure.csv")





# Load required packages
# Create the table using gt
table <- rds_results %>%
  select(prev_typo, msm_disclosure, n, weighted_N, point, lower, upper) %>%
  gt() %>%
  tab_header(
    title = "MSM Disclosure Rates by County",
    subtitle = "Weighted and Unweighted Summary Statistics"
  ) %>%
  cols_label(
    prev_typo = "County",
    msm_disclosure = "MSM Disclosure",
    n = "Sample Size (n)",
    weighted_N = "Weighted Total (N)",
    point = "Point Estimate",
    lower = "Lower CI",
    upper = "Upper CI"
  ) %>%
  fmt_number(
    columns = c(point, lower, upper),
    decimals = 2
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold", color = "black")
    ),
    locations = cells_column_labels(
      columns = everything()
    )
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  opt_table_outline() %>%
  opt_row_striping() %>%
  tab_source_note(
    source_note = "Note: Confidence intervals (CI) are presented for the weighted point estimates."
  )

# Display the table
table





# Load required libraries
library(gt)
library(dplyr)

# Example dataset (replace this with your actual dataset)
data <- tibble::tribble(
  ~county, ~msm_disclosure, ~n, ~weighted_N, ~point, ~lower, ~upper,
  "County A", "Yes", 50, 500, 0.45, 0.40, 0.50,
  "County A", "No", 30, 300, 0.55, 0.50, 0.60,
  "County B", "Yes", 40, 400, 0.60, 0.55, 0.65,
  "County B", "No", 20, 200, 0.40, 0.35, 0.45
)

# Create a grouped table by county
table <- data %>%
  arrange(county) %>% # Ensure data is sorted by county
  gt() %>%
  tab_header(
    title = "Disclosure Rates by County",
    subtitle = "Weighted and Unweighted Summary Statistics"
  ) %>%
  cols_label(
    county = "County",
    msm_disclosure = "MSM Disclosure",
    n = "Sample Size (n)",
    weighted_N = "Weighted Total (N)",
    point = "Point Estimate",
    lower = "Lower CI",
    upper = "Upper CI"
  ) %>%
  fmt_number(
    columns = c(point, lower, upper),
    decimals = 2
  ) %>%
  # Add row groups for each county
  tab_row_group(
    label = "County A", # Label for County A
    rows = county == "County A"
  ) %>%
  tab_row_group(
    label = "County B", # Label for County B
    rows = county == "County B"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", color = "black"),
    locations = cells_column_labels(everything())
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  opt_row_striping() %>%
  tab_source_note(
    source_note = "Note: Confidence intervals (CI) are presented for the weighted point estimates."
  )

# Display the table
table














