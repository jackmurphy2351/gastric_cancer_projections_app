# Gastric Cancer Mortality Reduction Calculator - Shiny App
# Based on SEER data and projections for 2030-2034

library(shiny)
library(shinymanager)
library(shinydashboard)
library(DT)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(tidyr)

# Username and password setup
credentials <- data.frame(
  user = c("cool_coauthor"),
  password = c("IM_begone2025"),
  stringsAsFactors = FALSE
)
# Source the calculation functions
source("R/gastric_mortality_calculator.R")

# UI Definition
ui <- dashboardPage(
  dashboardHeader(
    title = "Gastric Cancer Mortality Reduction Calculator",
    titleWidth = 450
  ),

  dashboardSidebar(
    width = 350,

    # Cancer type selection
    h4("Analysis Parameters", style = "padding-left: 15px; color: #605ca8;"),

    radioButtons(
      "cancer_type",
      "Gastric Cancer Type:",
      choices = list(
        "Total Gastric Cancer" = "total",
        "Non-cardia Adenocarcinoma" = "non-cardia adenocarcinoma"
      ),
      selected = "non-cardia adenocarcinoma"
    ),

    hr(),

    # Racial/Ethnic groups selection
    h4("Population Groups", style = "padding-left: 15px; color: #605ca8;"),

    # Select/Deselect all buttons for racial groups
    div(style = "padding-left: 15px; margin-bottom: 10px;",
        actionButton("select_all_racial", "Select All",
                     class = "btn-xs", style = "margin-right: 5px;"),
        actionButton("deselect_all_racial", "Deselect All",
                     class = "btn-xs")
    ),

    checkboxGroupInput(
      "racial_groups",
      "Racial/Ethnic Groups:",
      choices = list(
        "Hispanic" = "Hispanic",
        "Non-Hispanic American Indian/Alaska Native" = "Non-Hispanic American Indian/Alaska Native",
        "Non-Hispanic Asian American/Pacific Islander" = "Non-Hispanic Asian American/Pacific Islander",
        "Non-Hispanic Black" = "Non-Hispanic Black",
        "Non-Hispanic White" = "Non-Hispanic White"
      ),
      selected = c("Hispanic", "Non-Hispanic American Indian/Alaska Native",
                   "Non-Hispanic Asian American/Pacific Islander",
                   "Non-Hispanic Black", "Non-Hispanic White")
    ),

    # Age groups selection
    # Select/Deselect all buttons for age groups
    div(style = "padding-left: 15px; margin-bottom: 10px;",
        actionButton("select_all_age", "Select All",
                     class = "btn-xs", style = "margin-right: 5px;"),
        actionButton("deselect_all_age", "Deselect All",
                     class = "btn-xs")
    ),

    checkboxGroupInput(
      "age_groups",
      "Age Groups:",
      choices = list(
        "<40" = "<40",
        "40-44" = "40-44",
        "45-49" = "45-49",
        "50-54" = "50-54",
        "55-59" = "55-59",
        "60-64" = "60-64",
        "65-69" = "65-69",
        "70-74" = "70-74",
        "75-79" = "75-79",
        "80-84" = "80-84",
        "85+" = "85+"
      ),
      selected = c("<40", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")
    ),

    hr(),

    # Intervention parameters
    h4("Intervention Parameters", style = "padding-left: 15px; color: #605ca8;"),

    sliderInput(
      "chi_hp",
      HTML("H. pylori Treatment Compliance (χ<sub>hp</sub>):"),
      min = 0,
      max = 1,
      value = 0.9,
      step = 0.05
    ),

    sliderInput(
      "chi_screen",
      HTML("Screening Compliance (χ<sub>screen</sub>):"),
      min = 0,
      max = 1,
      value = 0.9,
      step = 0.05
    ),

    sliderInput(
      "p_hp_effectiveness",
      HTML("H. pylori Treatment Effectiveness:"),
      min = 0,
      max = 1,
      value = 0.9,
      step = 0.05
    ),

    br(),
    br(),

    # Download buttons
    conditionalPanel(
      condition = "output.results_available",
      downloadButton(
        "download_csv",
        "Download Results (CSV)",
        style = "width: 90%; margin-left: 15px; margin-bottom: 10px;"
      ),

      downloadButton(
        "download_summary",
        "Download Summary (CSV)",
        style = "width: 90%; margin-left: 15px;"
      )
    )
  ),

  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-radius: 5px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
        }
        .small-box {
          border-radius: 5px;
        }
        .info-box {
          min-height: 60px;
        }
        .info-box-content {
          padding-top: 5px;
          padding-bottom: 5px;
        }
        .btn-warning-custom {
          background-color: #f39c12 !important;
          border-color: #e08e0b !important;
          color: white !important;
          font-weight: bold;
          font-size: 18px;
          padding: 15px 40px;
          margin-top: 50px;
        }
        .btn-warning-custom:hover {
          background-color: #e08e0b !important;
          border-color: #d68910 !important;
        }
        .center-button-container {
          display: flex;
          justify-content: center;
          align-items: center;
          min-height: 400px;
        }
      "))
    ),

    # Main content area
    fluidRow(
      column(12,
             h2("Gastric Cancer Mortality Reduction Projections (2030-2034)"),
             p("This calculator estimates the potential reduction in gastric cancer mortality through screening and H. pylori eradication interventions, based on SEER data and Census projections."),
             p("Select the parameters on the left and click 'Calculate Mortality Reduction' to see results."),
             hr()
      )
    ),

    # Conditional display: Show button when no results, show results when calculated
    conditionalPanel(
      condition = "!output.results_available",
      fluidRow(
        column(12,
               div(class = "center-button-container",
                   actionButton(
                     "run_analysis_main",
                     "Calculate Mortality Reduction",
                     class = "btn btn-warning-custom",
                     icon = icon("calculator")
                   )
               )
        )
      )
    ),

    # Results section (only shown when results are available)
    conditionalPanel(
      condition = "output.results_available",

      # Summary boxes
      fluidRow(
        infoBoxOutput("total_groups_box"),
        infoBoxOutput("mean_reduction_box"),
        infoBoxOutput("max_reduction_box")
      ),

      # Results tabs with Recalculate button
      fluidRow(
        column(9,
               h3("Results", style = "margin-top: 0;")
        ),
        column(3,
               div(style = "text-align: right; padding-top: 5px;",
                   actionButton(
                     "recalculate",
                     "Recalculate",
                     class = "btn btn-info",
                     icon = icon("refresh")
                   )
               )
        )
      ),

      fluidRow(
        column(12,
               tabBox(
                 width = 12,

                 # Detailed results tab
                 tabPanel(
                   "Detailed Results",
                   icon = icon("table"),
                   br(),
                   DT::dataTableOutput("results_table")
                 ),

                 # Summary statistics tab
                 tabPanel(
                   "Summary by Group",
                   icon = icon("chart-bar"),
                   br(),
                   DT::dataTableOutput("summary_table")
                 ),

                 # Visualization tab
                 tabPanel(
                   "Visualizations",
                   icon = icon("chart-line"),
                   br(),
                   fluidRow(
                     column(12,
                            box(
                              title = "Mortality Reduction by Age and Race/Ethnicity",
                              status = "primary",
                              solidHeader = TRUE,
                              width = 12,
                              plotlyOutput("reduction_plot", height = "500px")
                            )
                     )
                   ),
                   fluidRow(
                     column(6,
                            box(
                              title = "Baseline vs. Post-Intervention Mortality",
                              status = "info",
                              solidHeader = TRUE,
                              width = 12,
                              plotlyOutput("comparison_plot", height = "400px")
                            )
                     ),
                     column(6,
                            box(
                              title = "Percent Reduction Distribution",
                              status = "success",
                              solidHeader = TRUE,
                              width = 12,
                              plotlyOutput("distribution_plot", height = "400px")
                            )
                     )
                   )
                 ),

                 # Methods tab
                 tabPanel(
                   "Methods",
                   icon = icon("info-circle"),
                   br(),
                   box(
                     title = "Calculation Methodology",
                     status = "warning",
                     solidHeader = TRUE,
                     width = 12,
                     h4("Overview"),
                     p("This calculator implements an algorithm for projecting reductions in gastric cancer mortality through two interventions:"),
                     tags$ul(
                       tags$li("Endoscopic screening for high-risk intestinal metaplasia (IM)"),
                       tags$li("H. pylori eradication therapy")
                     ),

                     h4("Key Parameters"),
                     tags$ul(
                       tags$li(HTML("<b>χ<sub>hp</sub></b>: Compliance rate with H. pylori eradication therapy (0-1)")),
                       tags$li(HTML("<b>χ<sub>screen</sub></b>: Compliance rate with endoscopic screening (0-1)")),
                       tags$li(HTML("<b>p<sub>hp</sub></b>: Effectiveness of H. pylori eradication therapy (0-1)"))
                     ),

                     h4("Data Sources"),
                     p("Baseline mortality projections are derived from:"),
                     tags$ul(
                       tags$li("SEER Program data (2015-2019 baseline)"),
                       tags$li("U.S. Census population projections"),
                       tags$li("Nordpred R package for trend projections to 2030-2034")
                     ),

                     h4("Assumptions"),
                     tags$ul(
                       tags$li("Relative risk reduction from screening with full compliance (r_screen0): 0.6"),
                       tags$li("Relative risk reduction from H. pylori treatment with full compliance (r_hp0): 0.667"),
                       tags$li("Interventions act multiplicatively when combined"),
                       tags$li("Effects are stratified by IM status and H. pylori status")
                     )
                   )
                 )
               )
        )
      )
    ),

    # Footer
    fluidRow(
      column(12,
             br(),
             hr(),
             p("© 2025 - Gastric Cancer Mortality Calculator | Based on SEER and U.S. Census Data",
               style = "text-align: center; color: #777;")
      )
    )
  )
)

# Wrap UI with the security layer
ui <- secure_app(ui)

# Server Definition
server <- function(input, output, session) {

  # Check credentials at the very top
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )

  # Reactive value to store results
  results <- reactiveValues(
    data = NULL,
    summary = NULL,
    available = FALSE
  )

  # Select/Deselect All functionality for racial groups
  observeEvent(input$select_all_racial, {
    updateCheckboxGroupInput(session, "racial_groups",
                             selected = c("Hispanic",
                                          "Non-Hispanic American Indian/Alaska Native",
                                          "Non-Hispanic Asian American/Pacific Islander",
                                          "Non-Hispanic Black",
                                          "Non-Hispanic White"))
  })

  observeEvent(input$deselect_all_racial, {
    updateCheckboxGroupInput(session, "racial_groups", selected = character(0))
  })

  # Select/Deselect All functionality for age groups
  observeEvent(input$select_all_age, {
    updateCheckboxGroupInput(session, "age_groups",
                             selected = c("<40", "40-44", "45-49", "50-54", "55-59",
                                          "60-64", "65-69", "70-74", "75-79", "80-84", "85+"))
  })

  observeEvent(input$deselect_all_age, {
    updateCheckboxGroupInput(session, "age_groups", selected = character(0))
  })

  # Function to perform the analysis
  perform_analysis <- function() {
    # Show progress
    withProgress(message = 'Calculating mortality reductions...', value = 0, {

      incProgress(0.3, detail = "Loading data...")

      # Check if any groups are selected
      if (length(input$racial_groups) == 0) {
        showNotification("Please select at least one racial/ethnic group", type = "error")
        return()
      }

      if (length(input$age_groups) == 0) {
        showNotification("Please select at least one age group", type = "error")
        return()
      }

      incProgress(0.3, detail = "Running calculations...")

      # Run the calculation
      tryCatch({
        calc_results <- calculate_gastric_mortality_reduction(
          data_file = "data/gastric_cancer_parameters.csv",
          gastric_cancer_type = input$cancer_type,
          racial_ethnic_groups = input$racial_groups,
          age_groups = input$age_groups,
          chi_hp = input$chi_hp,
          chi_screen = input$chi_screen,
          p_hp_effectiveness = input$p_hp_effectiveness
        )

        incProgress(0.3, detail = "Generating summary...")

        # Calculate summary
        summary_results <- calc_results %>%
          group_by(racial_ethnic_group) %>%
          summarise(
            n_age_groups = n(),
            mean_baseline = mean(baseline_mortality_per_100k, na.rm = TRUE),
            total_reduction = sum(mortality_reduction_per_100k, na.rm = TRUE),
            mean_reduction = mean(mortality_reduction_per_100k, na.rm = TRUE),
            mean_percent_reduction = mean(percent_reduction, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          arrange(desc(mean_percent_reduction))

        # Store results
        results$data <- calc_results
        results$summary <- summary_results
        results$available <- TRUE

        incProgress(0.1, detail = "Complete!")

        showNotification("Analysis completed successfully!", type = "message", duration = 3)

      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 10)
      })
    })
  }

  # Run analysis when main button is clicked
  observeEvent(input$run_analysis_main, {
    perform_analysis()
  })

  # Run analysis when recalculate button is clicked
  observeEvent(input$recalculate, {
    perform_analysis()
  })

  # Output for results availability
  output$results_available <- reactive({
    results$available
  })
  outputOptions(output, "results_available", suspendWhenHidden = FALSE)

  # Info boxes
  output$total_groups_box <- renderInfoBox({
    infoBox("Groups Analyzed",
            ifelse(results$available, nrow(results$data), "—"),
            icon = icon("users"),
            color = "blue")
  })

  output$mean_reduction_box <- renderInfoBox({
    if (!results$available) {
      infoBox("Mean Reduction", "—", icon = icon("arrow-down"), color = "green")
    } else {
      mean_val <- mean(results$data$percent_reduction, na.rm = TRUE)
      infoBox("Mean Reduction", sprintf("%.1f%%", mean_val),
              icon = icon("arrow-down"), color = "green")
    }
  })

  output$max_reduction_box <- renderInfoBox({
    if (!results$available) {
      infoBox("Maximum Reduction", "—", icon = icon("trophy"), color = "yellow")
    } else {
      max_val <- max(results$data$percent_reduction, na.rm = TRUE)
      max_group <- results$data[which.max(results$data$percent_reduction), ]
      infoBox("Maximum Reduction", sprintf("%.1f%%", max_val),
              subtitle = paste(max_group$racial_ethnic_group, max_group$age_group),
              icon = icon("trophy"), color = "yellow")
    }
  })

  # Detailed results table
  output$results_table <- DT::renderDataTable({
    if (!results$available) return(NULL)

    results$data %>%
      select(racial_ethnic_group, age_group,
             baseline_mortality_per_100k, mortality_reduction_per_100k,
             percent_reduction) %>%
      DT::datatable(
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        rownames = FALSE,
        colnames = c("Race/Ethnicity", "Age Group",
                     "Baseline Mortality (per 100k)",
                     "Mortality Reduction (per 100k)",
                     "Percent Reduction (%)"),
        class = 'cell-border stripe'
      ) %>%
      formatRound(c("baseline_mortality_per_100k", "mortality_reduction_per_100k"), 2) %>%
      formatRound("percent_reduction", 1) %>%
      formatStyle("percent_reduction",
                  background = styleColorBar(range(results$data$percent_reduction, na.rm = TRUE), 'lightblue'),
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')
  })

  # Summary table
  output$summary_table <- DT::renderDataTable({
    if (!results$available) return(NULL)

    results$summary %>%
      DT::datatable(
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip'
        ),
        rownames = FALSE,
        colnames = c("Race/Ethnicity", "Age Groups",
                     "Mean Baseline (per 100k)",
                     "Total Reduction (per 100k)",
                     "Mean Reduction (per 100k)",
                     "Mean % Reduction"),
        class = 'cell-border stripe'
      ) %>%
      formatRound(c("mean_baseline", "total_reduction", "mean_reduction"), 2) %>%
      formatRound("mean_percent_reduction", 1)
  })

  # Reduction plot
  output$reduction_plot <- renderPlotly({
    if (!results$available) return(NULL)

    p <- ggplot(results$data, aes(x = age_group, y = percent_reduction,
                                  fill = racial_ethnic_group,
                                  text = paste("Race/Ethnicity:", racial_ethnic_group,
                                               "<br>Age:", age_group,
                                               "<br>Reduction:", round(percent_reduction, 1), "%"))) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom") +
      labs(x = "Age Group", y = "Mortality Reduction (%)",
           fill = "Race/Ethnicity") +
      scale_fill_brewer(palette = "Set2")

    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.3))
  })

  # Comparison plot
  output$comparison_plot <- renderPlotly({
    if (!results$available) return(NULL)

    comparison_data <- results$data %>%
      group_by(racial_ethnic_group) %>%
      summarise(
        Baseline = mean(baseline_mortality_per_100k, na.rm = TRUE),
        `Post-Intervention` = mean(baseline_mortality_per_100k - mortality_reduction_per_100k, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_longer(cols = c("Baseline", "Post-Intervention"),
                   names_to = "Type", values_to = "Mortality")

    p <- ggplot(comparison_data, aes(x = racial_ethnic_group, y = Mortality,
                                     fill = Type,
                                     text = paste("Group:", racial_ethnic_group,
                                                  "<br>Type:", Type,
                                                  "<br>Rate:", round(Mortality, 1)))) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "", y = "Mortality Rate (per 100,000)", fill = "") +
      scale_fill_manual(values = c("Baseline" = "#e74c3c", "Post-Intervention" = "#27ae60"))

    ggplotly(p, tooltip = "text")
  })

  # Distribution plot
  output$distribution_plot <- renderPlotly({
    if (!results$available) return(NULL)

    p <- ggplot(results$data, aes(x = percent_reduction, fill = racial_ethnic_group,
                                  text = paste("Race/Ethnicity:", racial_ethnic_group,
                                               "<br>Reduction:", round(percent_reduction, 1), "%"))) +
      geom_histogram(bins = 20, alpha = 0.7, position = "identity") +
      theme_minimal() +
      labs(x = "Percent Reduction (%)", y = "Count",
           fill = "Race/Ethnicity") +
      scale_fill_brewer(palette = "Set2")

    ggplotly(p, tooltip = "text")
  })

  # Download handlers
  output$download_csv <- downloadHandler(
    filename = function() {
      cancer_type_clean <- gsub(" ", "_", tolower(input$cancer_type))
      paste0("gastric_cancer_", cancer_type_clean, "_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(results$data, file)
    }
  )

  output$download_summary <- downloadHandler(
    filename = function() {
      cancer_type_clean <- gsub(" ", "_", tolower(input$cancer_type))
      paste0("gastric_cancer_", cancer_type_clean, "_summary_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(results$summary, file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)