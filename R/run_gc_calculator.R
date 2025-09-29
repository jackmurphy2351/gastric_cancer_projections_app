# Load the function
source("R/gastric_mortality_calculator.R")

# Example 1: Analyze total gastric cancer
results_total <- calculate_gastric_mortality_reduction(
  data_file = "data/gastric_cancer_parameters.csv",
  gastric_cancer_type = "total",
  chi_hp = 0.9,             # 90% compliance with H. pylori treatment
  chi_screen = 0.9,         # 90% compliance with screening
  p_hp_effectiveness = 0.9  # 90% effectiveness of H. pylori treatment
)

# Export results
export_results(
  results = results_total
)

# Example 2: Analyze non-cardia adenocarcinoma only
results_noncardia <- calculate_gastric_mortality_reduction(
  data_file = "data/gastric_cancer_parameters.csv",
  gastric_cancer_type = "non-cardia adenocarcinoma",
  chi_hp = 0.8,             # 90% compliance with H. pylori treatment
  chi_screen = 0.7,         # 90% compliance with screening
  p_hp_effectiveness = 0.9  # 90% effectiveness of H. pylori treatment
)

# Export results
export_results(
  results = results_noncardia
)

# Plot results_noncardia using plot_intervention_effects
plot_intervention_effects(results_noncardia)

# Example 3: Use `scenario_analysis` to compare different compliance scenarios
scenarios <- list(
  list(chi_hp = 0.9, chi_screen = 0.9, p_hp_effectiveness = 0.9),
  list(chi_hp = 0.8, chi_screen = 0.7, p_hp_effectiveness = 0.9),
  list(chi_hp = 0.7, chi_screen = 0.6, p_hp_effectiveness = 0.8)
)
results_scenarios <- scenario_analysis(
  data_file = "data/gastric_cancer_parameters.csv",
  gastric_cancer_type = "total",
  scenarios = scenarios
)

# Export results
export_results(
  results = results_scenarios,
  file_name = "results/scenario_analysis_results.csv"
)