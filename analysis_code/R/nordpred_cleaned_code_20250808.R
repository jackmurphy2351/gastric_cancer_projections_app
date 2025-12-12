################################################################################
# NORDPRED CANCER PROJECTION ANALYSIS - REFACTORED WITH TIDYVERSE
#
# This script performs cancer incidence and mortality projections using the
# NordPred methodology. It has been refactored to use tidyverse functional
# programming principles to reduce code repetition.
#
# For SAS users: R uses <- for assignment (like = in SAS), and the pipe %>%
# passes the result of one function to the next (similar to SAS macro chains)
################################################################################

# Set working directory (equivalent to SAS LIBNAME for default file location)
# This tells R where to look for and save files by default
setwd("~/im_modelling_proj")

# Load required packages (similar to SAS PROC statements that load procedures)
# tidyverse: suite of packages for data manipulation (like SAS DATA steps)
# purrr: functional programming tools (like SAS macro loops but more powerful)
library(tidyverse)
library(purrr)

# Source external R script (like %INCLUDE in SAS - runs code from another file)
# This loads custom NordPred functions needed for the analysis
source("~/im_modelling_proj/R/nordpred.s")

# Change to data directory for reading input files
setwd("~/im_modelling_proj/data")

# Read CSV files into data frames (like SAS datasets)
# read_csv() automatically detects column types (unlike SAS PROC IMPORT)
pop_dat_all <- read_csv("SEER Census Combined Incidence Population 230418.csv")
incidence_dat <- read_csv("SEER Incidence Cases WIDE 230418.csv")
pop_dat_mortality_all <- read_csv("SEER Census Combined Mortality Population 230418.csv")

################################################################################
#    CONFIGURATION SECTION
#
#    Define all analysis parameters in one place for easy maintenance
#    These act like SAS macro variables that control the entire analysis
################################################################################

# Define race/ethnicity groups as a named vector
# In SAS, this would be like a format: PROC FORMAT; VALUE $racefmt ...
# The name (left) is the code, the value (right) is the full description
race_groups <- c("NHW" = "Non-Hisp White",
                 "NHB" = "Non-Hisp Black",
                 "NHAIAN" = "Non-Hisp American Indian_Alaskan Native",
                 "NHAAPI" = "Non-Hisp Asian-American_Pacific Islander",
                 "Hispanic" = "Hispanic")

# Define cancer subsites to analyze
# These represent anatomical locations of gastric cancer
subsites <- c("Non-cardia" = "NC",      # Non-cardia region of stomach
              "Cardia" = "C",            # Cardia region of stomach
              "All" = "All")             # All regions combined

# Define histology types to analyze
# These represent different cancer cell types
histologies <- c("Adenocarcinoma" = "Adeno",        # Most common type
                 "Non-adenocarcinoma" = "Non_adeno", # All other histology types
                 "All" = "All")                      # All histology types combined

# Create all possible combinations for analysis
# expand_grid() creates a Cartesian product (like PROC SQL CROSS JOIN)
# This creates every combination of race × subsite × histology
analysis_combinations <- expand_grid(
  race = names(race_groups),
  subsite = names(subsites),
  histo = names(histologies)
) %>%
  # Add a column to identify which combinations need NA handling
  # NHAIAN group often has sparse data requiring special treatment
  mutate(replace_na = (race == "NHAIAN"))

# For mortality analyses, define which combinations have data files
# This prevents errors from trying to read non-existent files
mortality_file_specs <- tribble(
  ~race,      ~subsite,      ~histo,           ~file_exists,
  "NHW",      "Non-cardia",  "Adenocarcinoma", TRUE,
  "NHB",      "Non-cardia",  "Adenocarcinoma", TRUE,
  "NHAIAN",   "Non-cardia",  "Adenocarcinoma", TRUE,
  "NHAAPI",   "Non-cardia",  "Adenocarcinoma", TRUE,
  "Hispanic", "Non-cardia",  "Adenocarcinoma", TRUE,
  "NHW",      "All",         "All",            TRUE,
  "NHB",      "All",         "All",            TRUE,
  "NHAIAN",   "All",         "All",            TRUE,
  "NHAAPI",   "All",         "All",            TRUE,
  "Hispanic", "All",         "All",            TRUE
)

################################################################################
#    HELPER FUNCTIONS SECTION
#
#    In R, we define reusable functions to avoid repetition.
#    This is similar to SAS macros but more flexible.
################################################################################

# Main analysis function - runs the complete NordPred workflow
# Think of this as a SAS macro that encapsulates a series of PROC steps
run_nordpred_analysis <- function(cases_data,      # Input: case counts data
                                  pop_data,          # Input: population data
                                  output_prefix,     # Input: prefix for output files
                                  replace_na = FALSE,# Input: should we replace missing values?
                                  na_value = 0.5) {  # Input: value to use for missing replacement

  # Handle missing values if requested
  # In SAS: IF variable = . THEN variable = 0.5;
  if (replace_na) {
    cases_data <- cases_data %>% replace(is.na(.), na_value)
  }

  # STEP 1: Estimate the model (like PROC GENMOD or PROC REG in SAS)
  np_est <- nordpred.estimate(
    cases = cases_data,
    pyr = pop_data,
    noperiod = 4, # 5-year periods of observed data used to fit the model
    startestage = 8,
    linkfunc = "power5"
  )

  # STEP 2: Make predictions based on the model (like PROC FORECAST in SAS)
  np_pred <- nordpred.prediction(
    np_est,
    startuseage = 9,
    cuttrend = c(0.01, 0.01, 0.01),
    recent = TRUE
  )

  # STEP 3: Extract rates from the prediction object
  np_rates <- nordpred.getpred(
    np_pred,
    incidence = TRUE,
    standpop = NULL,
    excludeobs = FALSE,
    byage = TRUE,
    agegroups = c(1:18)
  )

  # STEP 4: Extract counts from the prediction object
  np_counts <- nordpred.getpred(
    np_pred,
    incidence = FALSE,
    standpop = NULL,
    excludeobs = FALSE,
    byage = TRUE,
    agegroups = c(1:18)
  )

  # Return all results as a list
  list(
    rates = np_rates,
    counts = np_counts,
    population = pop_data,
    prefix = output_prefix
  )
}

# Function to save analysis results to CSV files
save_nordpred_results <- function(results,
                                  type = "incidence",
                                  subsite_code = "NC",
                                  histo_code = "Adeno") {

  # Build appropriate filename based on analysis type
  if (type == "incidence") {
    # Incidence files use one naming pattern
    write_csv(results$rates,
              paste0(results$prefix, " NordPred Rates.csv"))
    write_csv(results$counts,
              paste0(results$prefix, " NordPred Counts.csv"))
    write_csv(results$population,
              paste0(results$prefix, " NordPred Population Counts.csv"))
  } else {
    # Mortality files use different naming patterns
    if (subsite_code == "All" && histo_code == "All") {
      # "All" mortality analyses
      write_csv(results$rates,
                paste0(results$prefix, " All NordPred Mortality Rates.csv"))
      write_csv(results$counts,
                paste0(results$prefix, " NordPred All Deaths.csv"))
    } else {
      # Specific subsite/histology mortality analyses
      write_csv(results$rates,
                paste0(results$prefix, " ", subsite_code, " ", histo_code,
                       " NordPred Mortality Rates.csv"))
      write_csv(results$counts,
                paste0(results$prefix, " NordPred Deaths.csv"))
    }
    write_csv(results$population,
              paste0(results$prefix, " NordPred Mortality Population Counts.csv"))
  }
}

# Function to read mortality data files with error handling
read_mortality_file <- function(race_abbrev,
                                subsite_code = "NC",
                                histo_code = "Adeno") {

  # Build filename from components
  if (subsite_code == "All" && histo_code == "All") {
    filename <- paste0(race_abbrev, " All NordPred.csv")
  } else {
    filename <- paste0(race_abbrev, " ", subsite_code, " ", histo_code, " NordPred.csv")
  }

  # Check if file exists before attempting to read
  if (file.exists(filename)) {
    return(read_csv(filename, na = "NA"))
  } else {
    warning(paste("File not found:", filename))
    return(NULL)
  }
}

################################################################################
#     INCIDENCE PROJECTIONS SECTION
#
#     This section runs projections for cancer incidence (new cases)
#     Now handles all combinations of race × subsite × histology
################################################################################

# Filter for relevant incidence combinations
# In the original code, only Non-cardia Adenocarcinoma was analyzed for incidence
# Modify this filter as needed to include other combinations
incidence_combinations <- analysis_combinations %>%
  filter(subsite == "Non-cardia",
         histo == "Adenocarcinoma")

# Run incidence projections for all combinations using pmap()
# pmap() is like map() but takes multiple inputs from each row
# Think of it as a SAS macro loop with multiple parameters
incidence_results <- pmap(incidence_combinations, function(race, subsite, histo, replace_na) {

  message(paste("Processing incidence for:", race, subsite, histo))

  # Subset population data for this combination
  pop_subset <- pop_dat_all %>%
    filter(raceth == race,
           subsite == subsite,
           histo == histo) %>%
    select(yr00_04:yr30_34)

  # Subset cases data for this combination
  cases_subset <- incidence_dat %>%
    filter(raceth == race,
           subsite == subsite,
           histo == histo) %>%
    select(yr00_04:yr15_19)

  # Check if we have data to analyze
  if (nrow(cases_subset) == 0 || nrow(pop_subset) == 0) {
    message(paste("  No data available for:", race, subsite, histo))
    return(NULL)
  }

  # Run the complete analysis pipeline
  results <- run_nordpred_analysis(
    cases_data = cases_subset,
    pop_data = pop_subset,
    output_prefix = race_groups[race],
    replace_na = replace_na,
    na_value = 0.5
  )

  # Save all results to CSV files
  save_nordpred_results(results,
                        type = "incidence",
                        subsite_code = subsites[subsite],
                        histo_code = histologies[histo])

  # Add metadata to results for tracking
  results$race <- race
  results$subsite <- subsite
  results$histo <- histo

  return(results)
})

# Create named list for easy access
# Names combine all three dimensions for unique identification
names(incidence_results) <- paste(incidence_combinations$race,
                                  incidence_combinations$subsite,
                                  incidence_combinations$histo,
                                  sep = "_")

################################################################################
#    MORTALITY PROJECTIONS SECTION
#
#    This section runs projections for cancer mortality (deaths)
#    Now handles all valid combinations based on available data files
################################################################################

# Run mortality projections for all valid combinations
mortality_results <- pmap(mortality_file_specs,
                          function(race, subsite, histo, file_exists) {

                            # Skip if file doesn't exist
                            if (!file_exists) {
                              message(paste("Skipping mortality for:", race, subsite, histo, "- no file expected"))
                              return(NULL)
                            }

                            message(paste("Processing mortality for:", race, subsite, histo))

                            # Handle special case: Hispanic files use "HISP" abbreviation
                            race_file <- if_else(race == "Hispanic", "HISP", race)

                            # Read mortality data file
                            deaths_data <- read_mortality_file(race_file,
                                                               subsites[subsite],
                                                               histologies[histo])

                            # Skip analysis if no data available
                            if (is.null(deaths_data)) {
                              message(paste("  No data found for:", race, subsite, histo))
                              return(NULL)
                            }

                            # Subset population data for mortality analysis
                            pop_subset <- pop_dat_mortality_all %>%
                              filter(raceth == race,
                                     subsite == subsite,
                                     histo == histo) %>%
                              select(yr00_04:yr30_34)

                            # Check if we have population data
                            if (nrow(pop_subset) == 0) {
                              message(paste("  No population data for:", race, subsite, histo))
                              return(NULL)
                            }

                            # Determine if NA replacement is needed
                            replace_na_flag <- (race == "NHAIAN")

                            # Run the NordPred analysis pipeline
                            results <- run_nordpred_analysis(
                              cases_data = deaths_data,
                              pop_data = pop_subset,
                              output_prefix = race_groups[race],
                              replace_na = replace_na_flag
                            )

                            # Save results with mortality-specific filenames
                            save_nordpred_results(results,
                                                  type = "mortality",
                                                  subsite_code = subsites[subsite],
                                                  histo_code = histologies[histo])

                            # Add metadata to results
                            results$race <- race
                            results$subsite <- subsite
                            results$histo <- histo

                            return(results)
                          })

# Create named list
names(mortality_results) <- paste(mortality_file_specs$race,
                                  mortality_file_specs$subsite,
                                  mortality_file_specs$histo,
                                  sep = "_")

################################################################################
#    SUMMARY & REPORTING SECTION
#
#    Creates comprehensive summary statistics of all results
#    Now includes subsite and histology breakdowns
################################################################################

# Enhanced summary function that includes all dimensions
create_detailed_summary <- function(results_list,
                                    analysis_type) {

  # Filter out NULL results
  valid_results <- results_list[!sapply(results_list, is.null)]

  # If no valid results, return empty tibble
  if (length(valid_results) == 0) {
    return(tibble())
  }

  # Create detailed summary with all dimensions
  map_df(valid_results, function(result) {

    # Calculate summary statistics
    tibble(
      race_ethnicity = result$race,
      subsite = result$subsite,
      histology = result$histo,
      analysis_type = analysis_type,
      total_projected_counts = sum(result$counts, na.rm = TRUE),
      mean_rate = mean(result$rates, na.rm = TRUE),
      median_rate = median(result$rates, na.rm = TRUE),
      max_rate = max(result$rates, na.rm = TRUE),
      min_rate = min(result$rates, na.rm = TRUE)
    )
  })
}

# Generate comprehensive summaries
incidence_summary <- create_detailed_summary(incidence_results, "incidence")
mortality_summary <- create_detailed_summary(mortality_results, "mortality")

# Combine all summaries
full_summary <- bind_rows(incidence_summary, mortality_summary)

# Create cross-tabulation summary by race and analysis type
# This is like PROC FREQ with multiple CLASS variables
summary_by_race_type <- full_summary %>%
  group_by(race_ethnicity, analysis_type) %>%
  summarise(
    n_analyses = n(),  # Count of analyses per group
    total_counts = sum(total_projected_counts, na.rm = TRUE),
    avg_rate = mean(mean_rate, na.rm = TRUE),
    .groups = "drop"  # Ungroup after summarizing
  )

# Create summary by subsite and histology
# This shows which cancer types have the highest projections
summary_by_cancer_type <- full_summary %>%
  group_by(subsite, histology, analysis_type) %>%
  summarise(
    n_races_analyzed = n_distinct(race_ethnicity),
    total_counts = sum(total_projected_counts, na.rm = TRUE),
    avg_rate = mean(mean_rate, na.rm = TRUE),
    .groups = "drop"
  )

# Save all summaries
write_csv(full_summary, "NordPred_Full_Analysis_Summary.csv")
write_csv(summary_by_race_type, "NordPred_Summary_by_Race_Type.csv")
write_csv(summary_by_cancer_type, "NordPred_Summary_by_Cancer_Type.csv")

# Print summary statistics to console
# This gives immediate feedback about the analysis
print(paste(rep("=", 70), collapse = ""))  # Print separator line
print("NORDPRED ANALYSIS COMPLETE")
print(paste(rep("=", 70), collapse = ""))
print(paste("Total analyses attempted:",
            nrow(incidence_combinations) + nrow(mortality_file_specs)))
print(paste("Successful incidence analyses:", sum(!sapply(incidence_results, is.null))))
print(paste("Successful mortality analyses:", sum(!sapply(mortality_results, is.null))))
print(paste(rep("=", 70), collapse = ""))
print("Summary by Race/Ethnicity and Analysis Type:")
print(summary_by_race_type)
print(paste(rep("=", 70), collapse = ""))
print("Summary by Cancer Type:")
print(summary_by_cancer_type)

################################################################################
#    VISUALIZATION SECTION
#
#    Creates plots with enhanced breakdown by subsite and histology
################################################################################

library(ggplot2)

# Read data for summary plot
sumplot_data <- read_csv("IM Summary Figure Code for R 230419.csv")

# Calculate derived variable: percentage of lives saved
sumplot_data <- sumplot_data %>%
  mutate(perc_ls = (lives_saved/mortality_2032)*100)

# Create enhanced visualization with facets if multiple groups exist
if (n_distinct(full_summary$subsite) > 1 || n_distinct(full_summary$histology) > 1) {

  # Create faceted plot showing different subsites/histologies
  comparison_plot <- full_summary %>%
    filter(analysis_type == "incidence") %>%
    ggplot(aes(x = race_ethnicity, y = mean_rate, fill = subsite)) +
    geom_col(position = "dodge") +
    facet_wrap(~ histology, scales = "free_y") +
    labs(title = "Projected Incidence Rates by Race, Subsite, and Histology",
         x = "Race/Ethnicity",
         y = "Mean Projected Rate (per 100,000)",
         fill = "Subsite") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave("incidence_comparison_plot.png", comparison_plot, width = 12, height = 8)
}

# Original summary plot
summary_plot <- sumplot_data %>%
  count(mortality_2032, lives_saved) %>%
  ggplot() +
  geom_col(aes(x = mortality_2032,
               y = n,
               fill = as.character(lives_saved)),
           position = 'fill') +
  labs(title = "Projected Mortality and Lives Saved in 2032",
       x = "Mortality in 2032",
       y = "Proportion",
       fill = "Lives Saved") +
  theme_minimal()

ggsave("mortality_projection_plot.png", summary_plot, width = 10, height = 6)

print("All output files have been saved to the working directory.")
print("Analysis complete!")