# Gastric Cancer Mortality Reduction Calculator
# Based on Equation 1 algorithm for projecting reduction in non-cardia gastric adenocarcinoma mortality

# Load required libraries
library(dplyr)
library(readr)

#' Calculate Reduction in Gastric Cancer Mortality
#'
#' This function implements Equation 1 to calculate the potential reduction in
#' gastric cancer mortality from screening and H. pylori eradication interventions
#'
#' @param data_file Path to CSV file containing baseline parameters (default: "gastric_cancer_parameters.csv")
#' @param gastric_cancer_type Type of gastric cancer to analyze: "total" or "non-cardia adenocarcinoma"
#' @param racial_ethnic_groups Vector of racial/ethnic groups to analyze (default: all groups)
#' @param age_groups Vector of age groups to analyze (default: all groups)
#' @param chi_hp Compliance with H. pylori eradication therapy (0-1, default: varies by user)
#' @param chi_screen Compliance with gastric screening (0-1, default: varies by user)
#' @param p_hp_effectiveness Effectiveness of H. pylori eradication therapy (0-1, default: 0.9)
#' @param r_screen0 Relative risk reduction from screening with full compliance (default: 0.6)
#' @param r_hp0 Relative risk reduction from H. pylori treatment with full compliance (default: 0.6)
#'
#' @return Data frame with columns:
#'   - racial_ethnic_group: Racial/ethnic group
#'   - age_group: Age group
#'   - cancer_type: Type of gastric cancer analyzed
#'   - baseline_mortality_per_100k: Baseline projected mortality 2030-2034
#'   - mortality_reduction_per_100k: Reduction in mortality per 100,000 (Column AC equivalent)
#'   - percent_reduction: Percentage reduction in mortality (Column AD equivalent)
#'
#' @examples
#' # Analyze total gastric cancer
#' results_total <- calculate_gastric_mortality_reduction(
#'   gastric_cancer_type = "total",
#'   chi_hp = 0.8,           # 80% compliance with H. pylori treatment
#'   chi_screen = 0.7,       # 70% compliance with screening
#'   p_hp_effectiveness = 0.9  # 90% effectiveness of H. pylori treatment
#' )
#'
#' # Analyze non-cardia adenocarcinoma only
#' results_noncardia <- calculate_gastric_mortality_reduction(
#'   gastric_cancer_type = "non-cardia adenocarcinoma",
#'   chi_hp = 0.8,
#'   chi_screen = 0.7
#' )

calculate_gastric_mortality_reduction <- function(
    data_file = "gastric_cancer_parameters.csv",
    gastric_cancer_type = c("total", "non-cardia adenocarcinoma"),
    racial_ethnic_groups = NULL,
    age_groups = NULL,
    chi_hp = NULL,                    # User-defined: Compliance with H. pylori treatment (Cell AF4)
    chi_screen = NULL,                # User-defined: Compliance with screening (Cell AG4)
    p_hp_effectiveness = 0.9,         # User-defined: Effectiveness of H. pylori therapy (Cell AI4)
    r_screen0 = 0.6,                  # Default: relative risk with full screening compliance
    r_hp0 = 0.667                     # Default: relative risk with full H. pylori compliance
) {

  # ============================================================================
  # STEP 1: VALIDATE CANCER TYPE SELECTION
  # ============================================================================

  # Match the gastric_cancer_type argument
  gastric_cancer_type <- match.arg(gastric_cancer_type)

  # Determine which column prefixes to use based on cancer type
  if (gastric_cancer_type == "total") {
    # Use columns with "total_gc_" prefix for total gastric cancer
    incidence_col_2015_2019 <- "total_gc_incidence_2015_2019"
    incidence_col_2030_2034 <- "total_gc_incidence_2030_2034"
    ibmortality_col_2015_2019 <- "total_gc_ibmortality_2015_2019"
    mortality_col_2015_2019 <- "total_gc_mortality_2015_2019"
    mortality_col_2030_2034 <- "total_gc_projected_mortality_2030_2034"


    cat("Analyzing TOTAL gastric cancer mortality\n")

  } else if (gastric_cancer_type == "non-cardia adenocarcinoma") {
    # Use columns with "noncardia_adeno_gc_" prefix for non-cardia adenocarcinoma
    incidence_col_2015_2019 <- "noncardia_adeno_gc_incidence_2015_2019"
    incidence_col_2030_2034 <- "noncardia_adeno_gc_incidence_2030_2034"
    ibmortality_col_2015_2019 <- "noncardia_adeno_gc_ibmortality_2015_2019"
    mortality_col_2030_2034 <- "noncardia_adeno_gc_projected_mortality_2030_2034"
    # Note: There's no noncardia_adeno_gc_mortality_2015_2019 in the column list,
    # so we'll use total if needed or skip if not essential
    mortality_col_2015_2019 <- NULL  # Set to NULL if this column doesn't exist

    cat("Analyzing NON-CARDIA ADENOCARCINOMA gastric cancer mortality\n")
  }

  # ============================================================================
  # STEP 2: LOAD AND PREPARE DATA
  # ============================================================================

  # Read the main parameter file
  # This corresponds to the Excel sheet with the gastric cancer parameters
  df <- read_csv(data_file, show_col_types = FALSE)

  # Select and rename the relevant columns based on cancer type
  # Core columns that are always needed
  df_selected <- df %>%
    select(
      racial_ethnic_group,
      age_group,
      gamma,                    # Column L equivalent - proportion attributable to IM
      f11,                      # Column U equivalent - low-risk IM, H. pylori negative
      f12,                      # Column V equivalent - high-risk IM, H. pylori negative
      f21,                      # Column W equivalent - low-risk IM, H. pylori positive
      f22,                      # Column X equivalent - high-risk IM, H. pylori positive
      # Select the appropriate mortality column based on cancer type
      baseline_mortality = all_of(mortality_col_2030_2034)
    )

  # Filter out rows with missing essential data
  # Some age groups may not have data for certain parameters
  df_selected <- df_selected %>%
    filter(
      !is.na(gamma),
      !is.na(f11),
      !is.na(f12),
      !is.na(f21),
      !is.na(f22),
      !is.na(baseline_mortality)
    )

  # Filter for specified groups if provided
  if (!is.null(racial_ethnic_groups)) {
    df_selected <- df_selected %>%
      filter(racial_ethnic_group %in% racial_ethnic_groups)
  }
  if (!is.null(age_groups)) {
    df_selected <- df_selected %>%
      filter(age_group %in% age_groups)
  }

  # ============================================================================
  # STEP 3: VALIDATE USER INPUTS
  # ============================================================================

  # Check compliance parameters are between 0 and 1
  if (!is.null(chi_hp)) {
    if (chi_hp < 0 || chi_hp > 1) {
      stop("chi_hp (H. pylori compliance) must be between 0 and 1")
    }
  } else {
    stop("chi_hp (H. pylori compliance) must be specified")
  }

  if (!is.null(chi_screen)) {
    if (chi_screen < 0 || chi_screen > 1) {
      stop("chi_screen (screening compliance) must be between 0 and 1")
    }
  } else {
    stop("chi_screen (screening compliance) must be specified")
  }

  if (p_hp_effectiveness < 0 || p_hp_effectiveness > 1) {
    stop("p_hp_effectiveness must be between 0 and 1")
  }

  # ============================================================================
  # STEP 4: CALCULATE INTERVENTION EFFECTS
  # ============================================================================

  # Calculate r_hp: the actual risk reduction from H. pylori treatment given compliance
  # This corresponds to the formula: r_hp = χ_hp × r_hp0 + (1 - χ_hp)
  r_hp <- (r_hp0 * chi_hp * p_hp_effectiveness) + (1 - chi_hp) + (1 - p_hp_effectiveness)

  # Calculate r_screen: the actual risk reduction from screening given compliance
  # This corresponds to the formula: r_screen = χ_screen × r_screen0 + (1 - χ_screen)
  # When compliance is 0, r_screen = 1 (no effect)
  # When compliance is 1, r_screen = r_screen0 (full effect)
  r_screen <- (r_screen0 * chi_screen) + (1 - chi_screen)

  # ============================================================================
  # STEP 5: APPLY EQUATION 1 TO CALCULATE MORTALITY WITH INTERVENTIONS
  # ============================================================================

  df_results <- df_selected %>%
    mutate(
      # Store the cancer type being analyzed
      cancer_type = gastric_cancer_type,

      # Calculate the effect on IM-attributable gastric cancer
      # This is the first part of the equation: γ × [f11 + f12×r_screen + f21×r_hp + f22×r_screen×r_hp]
      im_attributable_effect = gamma *
        (
        f11 +                           # Low-risk IM, H. pylori negative (no intervention effect)
          (f12 * r_screen) +                # High-risk IM, H. pylori negative (screening effect only)
          (f21 * r_hp) +                    # Low-risk IM, H. pylori positive (H. pylori treatment effect only)
          (f22 * r_screen * r_hp)           # High-risk IM, H. pylori positive (both interventions)
      ),

      # Calculate the effect on non-IM-attributable gastric cancer
      # This is the second part: (1 - γ) × p_hp_effectiveness × r_hp
      # Note: p_hp_effectiveness represents (p_Hp+|IM-) in the equation
      # non_im_attributable_effect = (1 - gamma) * p_hp_effectiveness * r_hp,

      # Total effect combining both IM and non-IM pathways
      total_intervention_effect = im_attributable_effect, # + non_im_attributable_effect,

      # Calculate mortality with interventions
      # The baseline mortality is multiplied by the intervention effect
      mortality_with_intervention = baseline_mortality * total_intervention_effect,

      # Calculate the reduction in mortality
      # This is the absolute reduction per 100,000 population
      mortality_reduction_per_100k = baseline_mortality - mortality_with_intervention,

      # Calculate the percentage reduction
      # This is the relative reduction as a percentage
      percent_reduction = (mortality_reduction_per_100k / baseline_mortality) * 100
    )

  # ============================================================================
  # STEP 6: PREPARE AND RETURN OUTPUT
  # ============================================================================

  # Select and reorder columns - parameters first, then results
  results <- df_results %>%
    mutate(
      # Add the intervention parameters as columns
      chi_hp = chi_hp,
      chi_screen = chi_screen,
      p_hp_effectiveness = p_hp_effectiveness,
      r_hp = r_hp,
      r_screen = r_screen
    ) %>%
    select(
      # Identification columns
      racial_ethnic_group,
      age_group,
      cancer_type,
      # Baseline value
      baseline_mortality_per_100k = baseline_mortality,
      # Intervention parameters
      chi_hp,
      chi_screen,
      p_hp_effectiveness,
      r_hp,
      r_screen,
      im_attributable_effect,
      # non_im_attributable_effect, # Decided this is incorrect
      total_intervention_effect,
      # Results columns at the end (rightmost)
      mortality_reduction_per_100k,
      percent_reduction
    )

  # Add intervention parameters to results as attributes for reference
  attr(results, "intervention_parameters") <- list(
    gastric_cancer_type = gastric_cancer_type,
    chi_hp = chi_hp,
    chi_screen = chi_screen,
    p_hp_effectiveness = p_hp_effectiveness,
    r_screen0 = r_screen0,
    r_hp0 = r_hp0,
    r_screen_actual = r_screen,
    r_hp_actual = r_hp
  )

  # Print summary
  cat("\n=== Gastric Cancer Mortality Reduction Analysis ===\n")
  cat(sprintf("Cancer Type: %s\n", gastric_cancer_type))
  cat(sprintf("H. pylori treatment compliance: %.1f%%\n", chi_hp * 100))
  cat(sprintf("Screening compliance: %.1f%%\n", chi_screen * 100))
  cat(sprintf("H. pylori treatment effectiveness: %.1f%%\n", p_hp_effectiveness * 100))
  cat(sprintf("Actual risk reduction from screening: %.3f\n", r_screen))
  cat(sprintf("Actual risk reduction from H. pylori treatment: %.3f\n", r_hp))
  cat(sprintf("Number of groups analyzed: %d\n", nrow(results)))

  # Check for calculation issues
  na_count <- sum(is.na(results$mortality_reduction_per_100k))
  if (na_count > 0) {
    cat(sprintf("WARNING: %d rows have NA values in calculations\n", na_count))
    cat("This may be due to missing data in the input file.\n")
  }

  cat("\n")

  return(results)
}

# ============================================================================
# EXAMPLE USAGE
# ============================================================================

# Example 1: Analyze total gastric cancer with specific compliance rates
# results_total <- calculate_gastric_mortality_reduction(
#   gastric_cancer_type = "total",
#   chi_hp = 0.8,           # 80% compliance with H. pylori treatment
#   chi_screen = 0.7,       # 70% compliance with screening
#   p_hp_effectiveness = 0.9   # 90% effectiveness (default)
# )
# print(results_total)

# Example 2: Analyze non-cardia adenocarcinoma for specific groups
# results_noncardia <- calculate_gastric_mortality_reduction(
#   gastric_cancer_type = "non-cardia adenocarcinoma",
#   racial_ethnic_groups = c("Hispanic", "Non-Hispanic Asian American/Pacific Islander"),
#   age_groups = c("60-64", "65-69", "70-74"),
#   chi_hp = 0.75,
#   chi_screen = 0.65,
#   p_hp_effectiveness = 0.85
# )
# print(results_noncardia)

# Example 3: Scenario analysis for different compliance levels
# scenario_analysis <- function(gastric_cancer_type = "non-cardia adenocarcinoma") {
#   scenarios <- list(
#     low_compliance = list(chi_hp = 0.3, chi_screen = 0.2),
#     medium_compliance = list(chi_hp = 0.6, chi_screen = 0.5),
#     high_compliance = list(chi_hp = 0.9, chi_screen = 0.8)
#   )
#
#   results_list <- lapply(names(scenarios), function(scenario_name) {
#     s <- scenarios[[scenario_name]]
#     res <- calculate_gastric_mortality_reduction(
#       gastric_cancer_type = gastric_cancer_type,
#       chi_hp = s$chi_hp,
#       chi_screen = s$chi_screen
#     )
#     res$scenario <- scenario_name
#     return(res)
#   })
#
#   combined_results <- bind_rows(results_list)
#   return(combined_results)
# }

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Summarize results by racial/ethnic group
#'
#' @param results Output from calculate_gastric_mortality_reduction
#' @return Summary statistics by racial/ethnic and age group
summarize_by_group <- function(results) {
  summary <- results %>%
    group_by(racial_ethnic_group, age_group) %>%
    summarise(
      mean_baseline_mortality = mean(baseline_mortality_per_100k, na.rm = TRUE),
      mean_reduction = mean(mortality_reduction_per_100k, na.rm = TRUE),
      mean_percent_reduction = mean(percent_reduction, na.rm = TRUE),
      total_lives_saved_per_100k = sum(mortality_reduction_per_100k, na.rm = TRUE),
      .groups = "drop"
    )
  return(summary)
}

#' Plot intervention effects
#'
#' @param results Output from calculate_gastric_mortality_reduction
#' @return ggplot object
plot_intervention_effects <- function(results) {
  library(ggplot2)

  # Create subtitle based on cancer type(s) in the data
  cancer_types <- unique(results$cancer_type)
  subtitle_text <- paste("Cancer Type:", paste(cancer_types, collapse = " & "))

  p <- ggplot(results, aes(x = age_group, y = percent_reduction,
                           fill = racial_ethnic_group)) +
    geom_bar(stat = "identity", position = "dodge", ) +
    theme_minimal() +
    labs(
      title = "Percentage Reduction in Gastric Cancer Mortality (2030-2034)",
      subtitle = subtitle_text,
      x = "Age Group",
      y = "Mortality Reduction (%)",
      fill = "Race/Ethnicity"
    ) +
    coord_flip() + # Make horizontal
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # Add faceting if multiple cancer types
  if (length(cancer_types) > 1) {
    p <- p + facet_wrap(~ cancer_type, ncol = 1)
  }

  return(p)
}

#' Export results to Excel-friendly format
#'
#' @param results Output from calculate_gastric_mortality_reduction
#' @param filename Name of output file (optional - will auto-generate if not provided)
export_results <- function(results, filename = NULL) {
  # Get intervention parameters
  params <- attr(results, "intervention_parameters")

  # Extract the cancer type from the results (it's the same for all rows)
  cancer_type <- unique(results$cancer_type)[1]

  # Generate filename if not provided
  if (is.null(filename)) {
    # Replace spaces with underscores and make lowercase for cleaner filenames
    cancer_type_clean <- gsub(" ", "_", tolower(cancer_type))
    filename <- paste(cancer_type_clean, "mortality_reduction_results.csv", sep = "_")
  }

  # Add parameters as columns for easy reference
  results_export <- results %>%
    mutate(
      chi_hp = params$chi_hp,
      chi_screen = params$chi_screen,
      p_hp_effectiveness = params$p_hp_effectiveness
    ) |>
    select(-cancer_type)  # Remove cancer_type column

  # Write to CSV
  write_csv(results_export, filename)
  cat(sprintf("Results exported to %s\n", filename))

  return(invisible(results_export))
}