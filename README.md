# Gastric Cancer Mortality Reduction Projections

An R Shiny application for gastric cancer researchers and public health professionals to estimate potential reductions in gastric cancer mortality from endogastroduodenoscopy (EGD) screening and *Helicobacter pylori* eradication therapy.

## Overview

This interactive tool projects gastric cancer mortality reductions (2030-2034) that could be achieved through:
- **Endogastroduodenoscopy (EGD) screening** for intestinal metaplasia (IM)
- ***H. pylori* eradication therapy**

Projections are stratified by:
- **Race/ethnicity** (Non-Hispanic White, Non-Hispanic Black, Hispanic, Non-Hispanic Asian American/Pacific Islander)
- **Age group** (5-year intervals from 50-54 to 80-84)
- **Cancer type** (Total gastric cancer or non-cardia adenocarcinoma)

## Data Sources

This project uses data from:
- **SEER Program**: National Cancer Institute's Surveillance, Epidemiology, and End Results Program
- **U.S. Census Bureau**: Population projections and demographics
- **Projections**: Generated using the `nordpred` R package

## Features

- Interactive parameter adjustment for:
  - Screening compliance rates (χ_screen)
  - *H. pylori* treatment compliance rates (χ_hp)
  - Treatment effectiveness (p_hp)
- Real-time calculation of mortality reductions
- Visualization of results by demographic groups
- Downloadable results in CSV format
- Comparison of different intervention scenarios

## Installation

### Prerequisites

- R (≥ 4.0.0)
- RStudio (recommended)

### Required R Packages

```r
install.packages(c(
  "shiny",
  "dplyr",
  "readr",
  "ggplot2",
  "DT",
  "shinythemes"
))
```

### Running the App Locally

1. Clone this repository:
```bash
git clone https://github.com/YOUR-USERNAME/gastric_cancer_projections_app.git
cd gastric_cancer_projections_app
```

2. Open RStudio and set the working directory to the project folder

3. Run the app:
```r
shiny::runApp()
```

Or open `app.R` in RStudio and click "Run App"

## File Structure

```
gastric_cancer_projections_app/
├── app.R                              # Shiny application (UI + Server)
├── gastric_mortality_calculator.R     # Core calculation functions
├── gastric_cancer_parameters.csv      # Baseline epidemiological parameters
├── README.md                          # This file
├── LICENSE                            # MIT License
└── .gitignore                         # Git ignore rules
```

## Methodology

The application implements a mathematical model (Equation 1) that estimates mortality reduction based on:

**IM-attributable pathway:**
- γ × [f₁₁ + f₁₂×r_screen + f₂₁×r_hp + f₂₂×r_screen×r_hp]

Where:
- **γ**: Proportion of gastric cancer attributable to intestinal metaplasia (IM)
- **f₁₁**: Proportion with low-risk IM, *H. pylori* negative
- **f₁₂**: Proportion with high-risk IM, *H. pylori* negative
- **f₂₁**: Proportion with low-risk IM, *H. pylori* positive
- **f₂₂**: Proportion with high-risk IM, *H. pylori* positive
- **r_screen**: Relative risk reduction from screening (compliance-adjusted)
- **r_hp**: Relative risk reduction from *H. pylori* treatment (compliance-adjusted)

## Usage Example

1. Launch the application
2. Select cancer type (Total gastric cancer or Non-cardia adenocarcinoma)
3. Adjust intervention parameters:
   - Screening compliance: 70%
   - *H. pylori* treatment compliance: 80%
   - Treatment effectiveness: 90%
4. View results table and visualizations
5. Download results for further analysis

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request. For major changes, please open an issue first to discuss what you would like to change.

## Citation

If you use this tool in your research, please cite:

```
John D. Murphy, Ph.D. M.P.H. (2025). Gastric Cancer Mortality Reduction Projections. 
GitHub repository: https://github.com/YOUR-USERNAME/gastric_cancer_projections_app
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Author

**John D. Murphy, Ph.D. M.P.H.**  
EPAM Systems, Inc./Data Science Team
jackmurphy2351@gmail.com

## Acknowledgments

- National Cancer Institute SEER Program for epidemiological data
- U.S. Census Bureau for population projections
- nordpred R package developers for projection methodology

## Support

For questions, issues, or feature requests, please open an issue on the [GitHub repository](https://github.com/jackmurphy2351/gastric_cancer_projections_app/issues).

## Disclaimer

This tool is for research and educational purposes only. Clinical decisions should not be based solely on these projections. Consult with qualified healthcare professionals and consider local epidemiological data when implementing screening programs.