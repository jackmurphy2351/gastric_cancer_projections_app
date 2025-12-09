### Preparing SEER*Stat Data for Presentation in IM Modelling Master Document ##

## Libraries

library(writexl)
library(tidyverse)
library(readxl)
source("R/nordpred.s")

# List of raceth values, case data sets, and population data sets
raceth_values <- c("NHW", "NHB", "NHAIAN", "NHAAPI", "HISP", "ALL")

get_data_frame_name <- function(data_frame) {
  as.character(substitute(data_frame))
}

# Create function for widening and filtering data sets
widen <- function(input_dat, var1, var2, var3) {
  output <- input_dat %>%
    select(raceth:year_grp, var3) %>%
    filter(subsite == var1, hist == var2, agecat_all != "<40", agecat_all != "all") %>%
    pivot_wider(names_from = "year_grp", values_from = var3)%>%
    left_join(y = census_ready, by = c("raceth", "agecat_all")) # Join to census data
}

# Define a function to run NordPred and output Excel files over all race/ethnicity groups and outcomes of interest
process_NordPred <- function(raceth_value, cases, population) {
  # Filter wide_mort_cases and mort_SEER_Census_Bound for the given raceth_value
  cases_data <- cases %>% # Cases needs to be in WIDE format
    filter(raceth == raceth_value, agecat_all %in% c("00-04", "05-09", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")) %>%
    select(`00-04`:`15-19`)

  pop_data <- population %>% # Population needs to be in WIDE format
    filter(raceth == raceth_value, agecat_all %in% c("00-04", "05-09", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")) %>%
    select(`00-04`:`30-34`)

  # Nordpred estimates
  est_data <- nordpred.estimate(
    cases = cases_data,
    pyr = pop_data,
    noperiod = 4,
    startestage = 9,
    linkfunc = "power5"
  )

  # Nordpred predictions
  pred_data <- nordpred.prediction(
    est_data,
    startuseage = 9,
    cuttrend = c(0.01, 0.01, 0.01),
    recent = FALSE
  )


  # Results - Rates
  rates_data <- nordpred.getpred(
    pred_data,
    incidence = TRUE,
    standpop = NULL,
    excludeobs = FALSE,
    byage = TRUE,
    agegroups = c(1:18)
  )

  # Export rates to Excel
  rates_filename <- paste(raceth_value, get_data_frame_name(cases), "Rate Predictions", format(Sys.Date(), "%Y-%m-%d"), ".xlsx")
  write_xlsx(x = rates_data, path = rates_filename)


  # Results - Counts
  counts_data <- nordpred.getpred(
    pred_data,
    incidence = FALSE,
    standpop = NULL,
    excludeobs = FALSE,
    byage = TRUE,
    agegroups = c(1:18)
  )

  # Export counts to Excel
  counts_filename <- paste(raceth_value, get_data_frame_name(cases), "Count Predictions", format(Sys.Date(), "%Y-%m-%d"), ".xlsx")
  write_xlsx(x = counts_data, path = counts_filename)

  # Return a message indicating completion
  return(paste("Processing for raceth =", raceth_value, "and", get_data_frame_name(cases), "completed. Files saved."))
}

# Get the current date in "year_month_day" format
current_date <- format(Sys.Date(), "%Y%m%d")

# Census projections preparation

## NEXT STEP: Fix Census 2017 projection data (put in age groups including <40)

# Read in data
census_17_raw <- read_csv(file = "data/Raw Census 2017 Projections.csv", col_names = T, trim_ws = T, col_types = "n")

# Cleaning step 1: remove YEAR < 2020; remove SEX = 0; remove ORIGIN = 0; remove RACE = 0 and 6-11;
# Collapse Asian and NHPI into NHAAPI, with ORIGIN = 1
# Collapse 85+ age groups

census_17_clean <- census_17_raw %>%
  filter(2019 < YEAR & YEAR <= 2034, SEX > 0, ORIGIN >= 0, RACE %in% c(0:5)) %>%
  mutate(raceth = case_when(ORIGIN == 1 & RACE == 1 ~ "NHW",
                            ORIGIN == 1 & RACE == 2 ~ "NHB",
                            ORIGIN == 1 & RACE == 3 ~ "NHAIAN",
                            ORIGIN == 1 & RACE == 4 | RACE == 5 ~ "NHAAPI",
                            ORIGIN == 2 ~ "HISP",
                            ORIGIN == 0 & RACE == 0 ~ "ALL"),
         year_grp = case_when(YEAR <= 2024 ~ "20-24",
                              2025 <= YEAR & YEAR <= 2029 ~ "25-29",
                              2030 <= YEAR & YEAR <= 2034 ~ "30-34")
  ) %>%
  select(raceth, year_grp, POP_0:POP_100) %>%
  filter(!is.na(raceth))

# Cleaning step 2: Collapse ages into 5-year groups
census_17_clean2 <- census_17_clean %>%
  mutate(POP_00_04 = rowSums(select(., POP_0:POP_4)),
         POP_05_09 = rowSums(select(., POP_5:POP_9)),
         POP_10_14 = rowSums(select(., POP_10:POP_14)),
         POP_15_19 = rowSums(select(., POP_15:POP_19)),
         POP_20_24 = rowSums(select(., POP_20:POP_24)),
         POP_25_29 = rowSums(select(., POP_25:POP_29)),
         POP_30_34 = rowSums(select(., POP_30:POP_34)),
         POP_35_39 = rowSums(select(., POP_35:POP_39)),
         POP_under_40 = rowSums(select(., POP_0:POP_39)),
         POP_40_44 = rowSums(select(., POP_40:POP_44)),
         POP_45_49 = rowSums(select(., POP_45:POP_49)),
         POP_50_54 = rowSums(select(., POP_50:POP_54)),
         POP_55_59 = rowSums(select(., POP_55:POP_59)),
         POP_60_64 = rowSums(select(., POP_60:POP_64)),
         POP_65_69 = rowSums(select(., POP_65:POP_69)),
         POP_70_74 = rowSums(select(., POP_70:POP_74)),
         POP_75_79 = rowSums(select(., POP_75:POP_79)),
         POP_80_84 = rowSums(select(., POP_80:POP_84)),
         POP_85_plus = rowSums(select(., POP_85:POP_100)),
         POP_all = rowSums(select(., POP_0:POP_100))) %>%
  select(raceth, year_grp, POP_00_04:POP_all)

# Check that summing worked

# Function to calculate and check the sum for a row
calculate_and_check_sum <- function(row_number, data_clean, data_clean2) {
  # Initialize a list to store the results
  results <- list()

  # Iterate through groups of 5 columns at a time
  for (start_col in seq(1, ncol(data_clean), by = 5)) {
    end_col <- min(start_col + 4, ncol(data_clean))

    # Calculate the sum for the specified columns for the row in the first data frame
    sum_cols <- sum(dplyr::select(data_clean, where(is.numeric)))

    # Create the corresponding variable name in the second data frame
    var_name <- paste0("POP_", sprintf("%02d", start_col - 1), "_", sprintf("%02d", end_col - 1))

    # Check if it equals the corresponding variable in the second data frame
    is_equal <- sum_cols == data_clean2[[var_name]][row_number]

    # Store the result
    results[[var_name]] <- is_equal
  }

  return(results)
}

# Use map to apply the function to each row
results_clean <- map(1:nrow(census_17_clean), ~calculate_and_check_sum(.x, census_17_clean, census_17_clean2))

# Convert age group columns to rows
census_17_clean3 <- census_17_clean2 %>%
  pivot_longer(cols = c(POP_00_04:POP_all),
               names_to = "agecat_all", values_to = "population")

census_17_clean3 <- census_17_clean3 %>%
  mutate(agecat_all = str_replace(agecat_all, "POP_", "") %>%  # Remove "POP_"
           str_replace("_", "-"))                # Replace underscore with hyphen

census_17_clean3 <- census_17_clean3 %>%
  mutate(agecat_all = ifelse(agecat_all == "under-40", "<40",
                             ifelse(agecat_all == "85-plus", "85+", agecat_all)))


census_ready <- census_17_clean3 %>%
  pivot_wider(names_from = "year_grp", values_from = "population", values_fn = sum)

# Write to Excel for general use

## Create the file name with the current date
census_file_name <- paste("Census 2017 Data Ready for NordPred", current_date, ".xlsx")

## Export the data frame to Excel
write_xlsx(x = census_ready, path = census_file_name)

### Get annual percentage changes ###
# Get raw census data with annual values
census_17_apcs_1 <- census_17_raw %>%
  filter(2019 < YEAR & YEAR <= 2034, SEX > 0, ORIGIN >= 0, RACE %in% c(0:5)) %>%
  mutate(raceth = case_when(ORIGIN == 1 & RACE == 1 ~ "NHW",
                            ORIGIN == 1 & RACE == 2 ~ "NHB",
                            ORIGIN == 1 & RACE == 3 ~ "NHAIAN",
                            ORIGIN == 1 & RACE == 4 | RACE == 5 ~ "NHAAPI",
                            ORIGIN == 2 ~ "HISP",
                            ORIGIN == 0 & RACE == 0 ~ "ALL"),
         year = YEAR
  ) %>%
  select(raceth, year, POP_0:POP_100) %>%
  filter(!is.na(raceth))

# Group by 5-year age group
# Cleaning step 2: Collapse ages into 5-year groups
census_17_apcs_2 <- census_17_apcs_1 %>%
  mutate(POP_00_04 = rowSums(select(., POP_0:POP_4)),
         POP_05_09 = rowSums(select(., POP_5:POP_9)),
         POP_10_14 = rowSums(select(., POP_10:POP_14)),
         POP_15_19 = rowSums(select(., POP_15:POP_19)),
         POP_20_24 = rowSums(select(., POP_20:POP_24)),
         POP_25_29 = rowSums(select(., POP_25:POP_29)),
         POP_30_34 = rowSums(select(., POP_30:POP_34)),
         POP_35_39 = rowSums(select(., POP_35:POP_39)),
         POP_under_40 = rowSums(select(., POP_0:POP_39)),
         POP_40_44 = rowSums(select(., POP_40:POP_44)),
         POP_45_49 = rowSums(select(., POP_45:POP_49)),
         POP_50_54 = rowSums(select(., POP_50:POP_54)),
         POP_55_59 = rowSums(select(., POP_55:POP_59)),
         POP_60_64 = rowSums(select(., POP_60:POP_64)),
         POP_65_69 = rowSums(select(., POP_65:POP_69)),
         POP_70_74 = rowSums(select(., POP_70:POP_74)),
         POP_75_79 = rowSums(select(., POP_75:POP_79)),
         POP_80_84 = rowSums(select(., POP_80:POP_84)),
         POP_85_plus = rowSums(select(., POP_85:POP_100)),
         POP_all = rowSums(select(., POP_0:POP_100))) %>%
  select(raceth, year, POP_00_04:POP_all)

# Convert age group columns to rows
census_17_apcs_3 <- census_17_apcs_2 %>%
  pivot_longer(cols = c(POP_00_04:POP_all),
               names_to = "agecat_all", values_to = "population")

census_17_apcs_4 <- census_17_apcs_3 %>%
  mutate(agecat_all = str_replace(agecat_all, "POP_", "") %>%  # Remove "POP_"
           str_replace("_", "-"))                # Replace underscore with hyphen

census_17_apcs_5 <- census_17_apcs_4 %>%
  mutate(agecat_all = ifelse(agecat_all == "under-40", "<40",
                             ifelse(agecat_all == "85-plus", "85+", agecat_all)))

census_17_apcs_ready <- census_17_apcs_5 %>%
  pivot_wider(names_from = "year", values_from = "population", values_fn = sum)

# Calculate APCs
# Calculate the annual percentage change (APC)

# Compute percentage changes for each year across rows
census_17_apcs_ready2 <- census_17_apcs_ready %>%
  rowwise() %>%
  mutate(apc = list(c(NA, diff(c_across(starts_with("20")))) / c_across(starts_with("20")) * 100)) %>%
  ungroup()
census_17_apcs_ready2[1,18]

# Flatten the apc column into a matrix
apc_matrix <- do.call(rbind, census_17_apcs_ready2$apc)

# Calculate row-wise means for the matrix
census_17_apcs_ready3 <- census_17_apcs_ready2 %>%
  mutate(apc2 = rowMeans(apc_matrix, na.rm = TRUE))

census_17_apcs_final <- census_17_apcs_ready3 %>%
  select(-apc) %>%
  rename(apc = apc2)

census_apcs_filename <- paste("Census APCs 2020-2034", current_date, ".xlsx")

write_xlsx(x = census_17_apcs_final, path = census_apcs_filename)



## Read back in from Excel
#census_ready <- read.xlsx(xlsxFile = "Census 2017 Data Ready for NordPred 20231024.xlsx")


# Incidence

raw_inc_dat <- read_csv(file = "data/NordPred Incidence by Subsite and Histological Subtype Matrix 20231109.csv", col_names = T, na = "^", trim_ws = T, )

## Update values

clean_inc_dat <- raw_inc_dat %>%
  rename(rate = `Crude Rate`,
         count = Count,
         population = Population) %>%
  mutate(raceth = case_when(`IM Race-Ethnicity no Unknown` == 0 ~ "NHW",
                            `IM Race-Ethnicity no Unknown` == 1 ~ "NHB",
                            `IM Race-Ethnicity no Unknown` == 2 ~ "NHAIAN",
                            `IM Race-Ethnicity no Unknown` == 3 ~ "NHAAPI",
                            `IM Race-Ethnicity no Unknown` == 4 ~ "HISP",
                            `IM Race-Ethnicity no Unknown` == 5 ~ "ALL",),
         agecat_all = case_when(`18_Age Categories for NordPred` == 0 ~ "00-04",
                                `18_Age Categories for NordPred` == 1 ~ "05-09",
                                `18_Age Categories for NordPred` == 2 ~ "10-14",
                                `18_Age Categories for NordPred` == 3 ~ "15-19",
                                `18_Age Categories for NordPred` == 4 ~ "20-24",
                                `18_Age Categories for NordPred` == 5 ~ "25-29",
                                `18_Age Categories for NordPred` == 6 ~ "30-34",
                                `18_Age Categories for NordPred` == 7 ~ "35-39",
                                `18_Age Categories for NordPred` == 8 ~ "<40",
                                `18_Age Categories for NordPred` == 9 ~ "40-44",
                                `18_Age Categories for NordPred` == 10 ~ "45-49",
                                `18_Age Categories for NordPred` == 11 ~ "50-54",
                                `18_Age Categories for NordPred` == 12 ~ "55-59",
                                `18_Age Categories for NordPred` == 13 ~ "60-64",
                                `18_Age Categories for NordPred` == 14 ~ "65-69",
                                `18_Age Categories for NordPred` == 15 ~ "70-74",
                                `18_Age Categories for NordPred` == 16 ~ "75-79",
                                `18_Age Categories for NordPred` == 17 ~ "80-84",
                                `18_Age Categories for NordPred` == 18 ~ "85+",
                                `18_Age Categories for NordPred` == 19 ~ "all"),
         subsite = case_when(`IM Modelling Subsite` == 0 ~ "all",
                             `IM Modelling Subsite` == 1 ~ "Cardia",
                             `IM Modelling Subsite` == 2 ~ "Non-cardia",
                             `IM Modelling Subsite` == 3 ~ "Overlap/Unspecified"),
         hist = case_when(`IM Histology with All` == 0 ~ "all",
                          `IM Histology with All` == 1 ~ "Adenocarcinoma",
                          `IM Histology with All` == 2 ~ "Non-adenocarcinoma"),
         year_grp = case_when(`Incidence 5-year LATEST` == 0 ~ "00-04",
                              `Incidence 5-year LATEST` == 1 ~ "05-09",
                              `Incidence 5-year LATEST` == 2 ~ "10-14",
                              `Incidence 5-year LATEST` == 3 ~ "15-19"),
         rate = (count/population) * 100000
  ) %>%
  select(raceth, agecat_all, subsite, hist, year_grp, rate, count, population)


## Create new agecat variable with <40

short_inc_dat <- clean_inc_dat %>%
  rename(agecat = agecat_all) %>%
  filter(agecat %in% c("<40", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+", "all"))

inc_file_name <- paste("Comprehensive SEER_Stat Incidence Data", current_date, ".xlsx")

## Export the data frame to Excel
write_xlsx(x = short_inc_dat, path = inc_file_name)

# NordPred for Incidence

# Now pivot incidence data wider and join 'em together

wide_inc_totalGC_pop <- widen(input_dat = clean_inc_dat, var1 = "all", var2 = "all", var3 = "population")

  inc_totalGC_NordPred_filename <- paste("NordPred-ready SEER_Stat and Census Total GC Incidence Data", current_date, ".xlsx")
  ## Export the data frame to Excel
  write_xlsx(x = wide_inc_totalGC_pop, path = inc_totalGC_NordPred_filename)

wide_inc_NoncardiaAdeno_pop <- widen(input_dat = clean_inc_dat, var1 = "Non-cardia", var2 = "Adenocarcinoma", var3 = "population")

  inc_NoncardiaAdeno_NordPred_filename <- paste("NordPred-ready SEER_Stat and Census Non-cardia Adenocarcinoma Incidence Data", current_date, ".xlsx")
  ## Export the data frame to Excel
  write_xlsx(x = wide_inc_NoncardiaAdeno_pop, path = inc_NoncardiaAdeno_NordPred_filename)

wide_inc_OverlapUnspecAdeno_pop <- widen(input_dat = clean_inc_dat, var1 = "Overlap/Unspecified", var2 = "Adenocarcinoma", var3 = "population")

inc_OverlapUnspecAdeno_NordPred_filename <- paste("NordPred-ready SEER_Stat and Census Overlapping-Unspecified Adenocarcinoma Incidence Data", current_date, ".xlsx")
  ## Export the data frame to Excel
  write_xlsx(x = wide_inc_OverlapUnspecAdeno_pop, path = inc_OverlapUnspecAdeno_NordPred_filename)


# Incidence Total GC cases data set
wide_inc_totalGC_cases <- clean_inc_dat %>%
  select(raceth:year_grp, count) %>%
  filter(subsite == "all", hist == "all") %>%
  pivot_wider(names_from = "year_grp", values_from = "count")

# Incidence Non-cardia Adenocarcinoma cases data set
wide_inc_NoncardiaAdeno_cases <- clean_inc_dat %>%
  select(raceth:year_grp, count) %>%
  filter(subsite == "Non-cardia", hist == "Adenocarcinoma") %>%
  pivot_wider(names_from = "year_grp", values_from = "count")

# Incidence Non-cardia Adenocarcinoma cases data set
wide_inc_OverlapUnspecAdeno_cases <- clean_inc_dat %>%
  select(raceth:year_grp, count) %>%
  filter(subsite == "Overlap/Unspecified", hist == "Adenocarcinoma") %>%
  pivot_wider(names_from = "year_grp", values_from = "count")

for (i in raceth_values) {
  process_NordPred(raceth_value = i,
                   cases = wide_inc_OverlapUnspecAdeno_cases,
                   population = wide_inc_OverlapUnspecAdeno_pop)
}

## Incidence-based Mortality ##

# Tidy up APCs from SEER data
## Read in data
ibmort_apcs_dat <- read_csv("data/Gastric Cancer Incidence-Based Mortality APCs 20231128.csv", col_names = T, na = "~")

## Update level names
clean_ibmort_apcs_dat <- ibmort_apcs_dat %>%
  mutate(raceth = case_when(`IM Race-Ethnicity no Unknown` == 0 ~ "NHW",
                            `IM Race-Ethnicity no Unknown` == 1 ~ "NHB",
                            `IM Race-Ethnicity no Unknown` == 2 ~ "NHAIAN",
                            `IM Race-Ethnicity no Unknown` == 3 ~ "NHAAPI",
                            `IM Race-Ethnicity no Unknown` == 4 ~ "HISP",
                            `IM Race-Ethnicity no Unknown` == 5 ~ "ALL",),
         agecat_all = case_when(`Age at death 18_Age Categories for NordPred` == 0 ~ "00-04",
                                `Age at death 18_Age Categories for NordPred` == 1 ~ "05-09",
                                `Age at death 18_Age Categories for NordPred` == 2 ~ "10-14",
                                `Age at death 18_Age Categories for NordPred` == 3 ~ "15-19",
                                `Age at death 18_Age Categories for NordPred` == 4 ~ "20-24",
                                `Age at death 18_Age Categories for NordPred` == 5 ~ "25-29",
                                `Age at death 18_Age Categories for NordPred` == 6 ~ "30-34",
                                `Age at death 18_Age Categories for NordPred` == 7 ~ "35-39",
                                `Age at death 18_Age Categories for NordPred` == 8 ~ "<40",
                                `Age at death 18_Age Categories for NordPred` == 9 ~ "40-44",
                                `Age at death 18_Age Categories for NordPred` == 10 ~ "45-49",
                                `Age at death 18_Age Categories for NordPred` == 11 ~ "50-54",
                                `Age at death 18_Age Categories for NordPred` == 12 ~ "55-59",
                                `Age at death 18_Age Categories for NordPred` == 13 ~ "60-64",
                                `Age at death 18_Age Categories for NordPred` == 14 ~ "65-69",
                                `Age at death 18_Age Categories for NordPred` == 15 ~ "70-74",
                                `Age at death 18_Age Categories for NordPred` == 16 ~ "75-79",
                                `Age at death 18_Age Categories for NordPred` == 17 ~ "80-84",
                                `Age at death 18_Age Categories for NordPred` == 18 ~ "85+",
                                `Age at death 18_Age Categories for NordPred` == 19 ~ "all"),
         subsite = case_when(`IM Subsite` == 0 ~ "all",
                             `IM Subsite` == 1 ~ "Cardia",
                             `IM Subsite` == 2 ~ "Non-cardia",
                             `IM Subsite` == 3 ~ "Overlap/Unspecified"),
         hist = case_when(`IM Histology with All` == 0 ~ "all",
                          `IM Histology with All` == 1 ~ "Adenocarcinoma",
                          `IM Histology with All` == 2 ~ "Non-adenocarcinoma"),
         apc = `APCs 2000-2019`
  ) %>%
  select(raceth, agecat_all, subsite, hist, apc)

# Merge APCs with population amounts


raw_ibmort_dat <- read_csv(file = "data/Gastric Cancer Incidence-Based Mortality 20231128.csv", col_names = T, na = "^", trim_ws = T)

## Update values

clean_ibmort_dat <- raw_ibmort_dat %>%
  rename(rate = `Crude Rate`,
         count = Count,
         population = Population) %>%
  mutate(raceth = case_when(`IM Race-Ethnicity no Unknown` == 0 ~ "NHW",
                            `IM Race-Ethnicity no Unknown` == 1 ~ "NHB",
                            `IM Race-Ethnicity no Unknown` == 2 ~ "NHAIAN",
                            `IM Race-Ethnicity no Unknown` == 3 ~ "NHAAPI",
                            `IM Race-Ethnicity no Unknown` == 4 ~ "HISP",
                            `IM Race-Ethnicity no Unknown` == 5 ~ "ALL",),
         agecat_all = case_when(`Age at death 18_Age Categories for NordPred` == 0 ~ "00-04",
                               `Age at death 18_Age Categories for NordPred` == 1 ~ "05-09",
                               `Age at death 18_Age Categories for NordPred` == 2 ~ "10-14",
                               `Age at death 18_Age Categories for NordPred` == 3 ~ "15-19",
                               `Age at death 18_Age Categories for NordPred` == 4 ~ "20-24",
                               `Age at death 18_Age Categories for NordPred` == 5 ~ "25-29",
                               `Age at death 18_Age Categories for NordPred` == 6 ~ "30-34",
                               `Age at death 18_Age Categories for NordPred` == 7 ~ "35-39",
                               `Age at death 18_Age Categories for NordPred` == 8 ~ "<40",
                               `Age at death 18_Age Categories for NordPred` == 9 ~ "40-44",
                               `Age at death 18_Age Categories for NordPred` == 10 ~ "45-49",
                               `Age at death 18_Age Categories for NordPred` == 11 ~ "50-54",
                               `Age at death 18_Age Categories for NordPred` == 12 ~ "55-59",
                               `Age at death 18_Age Categories for NordPred` == 13 ~ "60-64",
                               `Age at death 18_Age Categories for NordPred` == 14 ~ "65-69",
                               `Age at death 18_Age Categories for NordPred` == 15 ~ "70-74",
                               `Age at death 18_Age Categories for NordPred` == 16 ~ "75-79",
                               `Age at death 18_Age Categories for NordPred` == 17 ~ "80-84",
                               `Age at death 18_Age Categories for NordPred` == 18 ~ "85+",
                               `Age at death 18_Age Categories for NordPred` == 19 ~ "all"),
         subsite = case_when(`IM Subsite` == 0 ~ "all",
                             `IM Subsite` == 1 ~ "Cardia",
                             `IM Subsite` == 2 ~ "Non-cardia",
                             `IM Subsite` == 3 ~ "Overlap/Unspecified"),
         hist = case_when(`IM Histology with All` == 0 ~ "all",
                          `IM Histology with All` == 1 ~ "Adenocarcinoma",
                          `IM Histology with All` == 2 ~ "Non-adenocarcinoma"),
         year_grp = case_when(`IM 5-year LATEST` == 0 ~ "00-04",
                              `IM 5-year LATEST` == 1 ~ "05-09",
                              `IM 5-year LATEST` == 2 ~ "10-14",
                              `IM 5-year LATEST` == 3 ~ "15-19"),
         rate = (count/population) * 100000
  ) %>%
  select(raceth, agecat_all, subsite, hist, year_grp, rate, count, population)

# Update NHAIAN missing values
clean_ibmort_dat <- clean_ibmort_dat %>%
  mutate(count = ifelse(raceth == "NHAIAN" & subsite == "Non-cardia" & hist == "Adenocarcinoma" & is.na(count), 0.5, count))

## Create new agecat variable with <40

short_ibmort_dat <- clean_ibmort_dat %>%
  rename(agecat = agecat_all) %>%
  filter(agecat %in% c("<40", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+", "all"))

ibmort_file_name <- paste("Comprehensive SEER_Stat Incidence-based Mortality Data", current_date, ".xlsx")

## Export the data frame to Excel
write_xlsx(x = short_ibmort_dat, path = ibmort_file_name)

# Join APCs to clean_ibmort_dat
clean_ibmort_dat_apcs <- left_join(x = clean_ibmort_dat, y = clean_ibmort_apcs_dat) %>%
  select(-year_grp) %>%
  group_by(raceth, agecat_all, subsite, hist, apc)
clean_ibmort_dat_apcs2 <- clean_ibmort_dat_apcs %>% group_by(raceth, agecat_all, subsite, hist, apc) %>%
  summarise()

# NordPred for Incidence-based Mortality

# Now pivot incidence-based mortality data wider and join 'em together
wide_ibmort_totalGC_pop <- widen(input_dat = clean_ibmort_dat, var1 = "all", var2 = "all", var3 = "population")

ibmort_totalGC_NordPred_filename <- paste("NordPred-ready SEER_Stat and Census Total GC IB-Mortality Data", current_date, ".xlsx")
## Export the data frame to Excel
write_xlsx(x = wide_ibmort_totalGC_cases, path = ibmort_totalGC_NordPred_filename)

wide_ibmort_NoncardiaAdeno_pop <- widen(input_dat = clean_ibmort_dat, var1 = "Non-cardia", var2 = "Adenocarcinoma", var3 = "population")

ibmort_NoncardiaAdeno_NordPred_filename <- paste("NordPred-ready SEER_Stat and Census Non-cardia Adenocarcinoma IB-Mortality Data", current_date, ".xlsx")
## Export the data frame to Excel
write_xlsx(x = wide_ibmort_totalGC_pop, path = ibmort_NoncardiaAdeno_NordPred_filename)

# IB-Mortality Total GC cases data set
wide_ibmort_totalGC_cases <- clean_ibmort_dat %>%
  select(raceth:year_grp, count) %>%
  filter(subsite == "all", hist == "all") %>%
  pivot_wider(names_from = "year_grp", values_from = "count")


# IB-Mortality Non-cardia Adenocarcinoma cases data set
wide_ibmort_NoncardiaAdeno_cases <- clean_ibmort_dat %>%
  select(raceth:year_grp, count) %>%
  filter(subsite == "Non-cardia", hist == "Adenocarcinoma") %>%
  pivot_wider(names_from = "year_grp", values_from = "count")

# NordPred for IB-Mortality Total GC
process_NordPred(raceth_value = "ALL",
                 cases = wide_ibmort_totalGC_cases,
                 population = wide_ibmort_totalGC_pop)


# NordPred for IB-Mortality Non-cardia Adenocarcinoma
process_NordPred(raceth_value = "NHAIAN",
                 cases = wide_ibmort_NoncardiaAdeno_cases,
                 population = wide_ibmort_NoncardiaAdeno_pop)

## Regular Mortality ##

raw_mort_dat <- read_csv(file = "data/IM Modelling Mortality by Race-Ethnicity and Age 20231025.csv", col_names = T, na = "^", trim_ws = T)

## Update values

clean_mort_dat <- raw_mort_dat %>%
  rename(count = Count,
         population = Population) %>%
  mutate(raceth = case_when(`IM race_eth` == 0 ~ "NHW",
                            `IM race_eth` == 1 ~ "NHB",
                            `IM race_eth` == 2 ~ "NHAIAN",
                            `IM race_eth` == 3 ~ "NHAAPI",
                            `IM race_eth` == 4 ~ "HISP",
                            `IM race_eth` == 5 ~ "ALL",),
         agecat_all = case_when(`IM All Age Groups` == 0 ~ "00-04",
                                `IM All Age Groups` == 1 ~ "05-09",
                                `IM All Age Groups` == 2 ~ "10-14",
                                `IM All Age Groups` == 3 ~ "15-19",
                                `IM All Age Groups` == 4 ~ "20-24",
                                `IM All Age Groups` == 5 ~ "25-29",
                                `IM All Age Groups` == 6 ~ "30-34",
                                `IM All Age Groups` == 7 ~ "35-39",
                                `IM All Age Groups` == 8 ~ "<40",
                                `IM All Age Groups` == 9 ~ "40-44",
                                `IM All Age Groups` == 10 ~ "45-49",
                                `IM All Age Groups` == 11 ~ "50-54",
                                `IM All Age Groups` == 12 ~ "55-59",
                                `IM All Age Groups` == 13 ~ "60-64",
                                `IM All Age Groups` == 14 ~ "65-69",
                                `IM All Age Groups` == 15 ~ "70-74",
                                `IM All Age Groups` == 16 ~ "75-79",
                                `IM All Age Groups` == 17 ~ "80-84",
                                `IM All Age Groups` == 18 ~ "85+"),
         year_grp = case_when(`IM Modelling 5-Year Year of Death Groups` == 0 ~ "00-04",
                              `IM Modelling 5-Year Year of Death Groups` == 1 ~ "05-09",
                              `IM Modelling 5-Year Year of Death Groups` == 2 ~ "10-14",
                              `IM Modelling 5-Year Year of Death Groups` == 3 ~ "15-19"),
         rate = (count/population) * 100000
  ) %>%
  select(raceth, agecat_all, year_grp, rate, count, population)


## Create new agecat variable with <40

short_mort_dat <- clean_mort_dat %>%
  rename(agecat = agecat_all) %>%
  filter(agecat %in% c("<40", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+"))

mort_file_name <- paste("Comprehensive SEER_Stat Mortality Data", current_date, ".xlsx")

## Export the data frame to Excel
write_xlsx(x = short_mort_dat, path = mort_file_name)

# Now pivot mortality data wider and join 'em together
wide_mort_totalGC_pop <- clean_mort_dat %>%
  select(raceth:year_grp, population) %>%
  filter(agecat_all %in% c("00-04", "05-09", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")) %>%
  pivot_wider(names_from = "year_grp", values_from = "population") %>%
  left_join(y = census_ready, by = c("raceth", "agecat_all")) # Join to census data

mort_totalGC_NordPred_filename <- paste("NordPred-ready SEER_Stat and Census Total GC Mortality Data", current_date, ".xlsx")
## Export the data frame to Excel
write_xlsx(x = wide_mort_totalGC_pop, path = mort_totalGC_NordPred_filename)


# Mortality Total GC cases data set
wide_mort_totalGC_cases <- clean_mort_dat %>%
  select(raceth:year_grp, count) %>%
  filter(agecat_all %in% c("00-04", "05-09", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")) %>%
  pivot_wider(names_from = "year_grp", values_from = "count") %>%
  left_join(y = census_ready, by = c("raceth", "agecat_all")) # Join to census data

# NordPred for Mortality All GC
process_NordPred(raceth_value = "HISP",
                 cases = wide_mort_totalGC_cases,
                 population = wide_mort_totalGC_pop)


## Purrr cycle over raceth

library(dplyr)
library(purrr)
library(openxlsx)  # Assuming you want to use write_xlsx function

process_NordPred(raceth_value = "ALL", cases = wide_inc_totalGC_cases, population = wide_inc__totalGC_pop)

# List of raceth values, case data sets, and population data sets
raceth_values <- c("NHW", "NHB", "NHAIAN", "NHAAPI", "HISP", "ALL")
case_data_sets <- list(wide_inc_totalGC_cases, wide_inc_NoncardiaAdeno_cases, wide_ibmort_totalGC_cases, wide_ibmort_NoncardiaAdeno_cases, wide_mort_totalGC_cases)  # Replace with your data frames
pop_data_sets <- list(wide_inc_totalGC_pop, wide_inc_NoncardiaAdeno_pop, wide_ibmort_totalGC_pop, wide_ibmort_NoncardiaAdeno_pop,wide_mort_totalGC_pop)  # Replace with your data frames


  # Use map2 to apply the process_NordPred function to the three vectors
  results <- map2(raceth_values, case_data_sets, pop_data_sets, .f = process_NordPred)


# Print the results
print(results)
