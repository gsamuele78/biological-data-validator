#' app_shiny.R # nolint: commented_code_linter.

# Purpose:
# This script defines a Shiny application for validating ecological data stored in CSV files.
# It provides a user-friendly interface for uploading files, validating data, managing database records,
# and generating reports. The app integrates with the SQLite database and uses modular R6 classes for validation.

# Documentation:
# - Shiny: https://shiny.rstudio.com/
# - R6 Classes: https://cran.r-project.org/web/packages/R6/vignettes/Introduction.html

# Load necessary libraries using renv
if (!require("renv")) install.packages("renv")
renv::restore()

library(shiny)  # For building the Shiny app
library(DT)     # For interactive tables
library(tools)   # For file_ext function

# Source R functions and classes
# These files contain the definitions of classes and functions used in the app.
source("R/data_classes.R")          # Defines DataSource, Sheet1Data, and Sheet2Data classes
source("R/validation_classes.R")    # Defines the Validator class for applying validation rules
source("R/report_class.R")          # Defines the Report class for generating validation reports
source("R/path_generation.R")       # Defines the PathGenerator class for managing file paths
source("R/email_class.R")           # Defines the EmailSender class for sending emails
source("R/db_interaction_class.R")  # Defines the DatabaseHandler class for interacting with the SQLite database
source("R/utils.R")                 # Contains utility functions
source("R/csv_mapping.R")           # Contains CSV mapping logic

# UI
# The user interface is divided into a sidebar for inputs and a main panel for outputs.
ui <- fluidPage(
  titlePanel("Biological Environment Data Validation"),
  sidebarLayout(
    sidebarPanel(
      # File upload inputs
      fileInput("data_file", "Choose CSV Data File", accept = c(".csv")),
      fileInput("image_files", "Choose Image Files (Max 4)", accept = c("image/jpeg", "image/png")),
      
      # Path and validation controls
      textInput("base_path", "Customizable Path", value = "/default/path"),
      actionButton("validate_button", "Validate Data"),
      downloadButton("download_report", "Download Report"),
      
      hr(),  # Horizontal line separator
      
      # Search and filter plot history
      h4("Search and Filter Plot History"),
      textInput("search_plot_code", "Plot Code"),
      dateInput("search_from_date", "From Date"),
      dateInput("search_to_date", "To Date"),
      actionButton("search_button", "Search"),
      
      hr(),
      
      # Plot history table
      h4("Plot History"),
      DT::dataTableOutput("history_table"),
      
      # Edit and delete records
      h4("Edit Record"),
      numericInput("edit_record_id", "Record ID to Edit", value = NA),
      actionButton("load_record_button", "Load Record"),
      textInput("edit_filepath", "Filepath", value = ""),
      textInput("edit_plot_code", "Plot Code", value = ""),
      dateInput("edit_sample_date", "Sample Date", value = NULL),
      textInput("edit_detector", "Detector", value = ""),
      textInput("edit_region", "Region", value = ""),
      textInput("edit_validation_status", "Validation Status", value = ""),
      textInput("edit_report_path", "Report Path", value = ""),
      actionButton("update_record_button", "Update Record"),
      
      h4("Delete Record"),
      numericInput("delete_record_id", "Record ID to Delete", value = NA),
      actionButton("delete_record_button", "Delete Record"),
      
      hr(),
      h3("Use Case Examples"),
      verbatimTextOutput("use_case_examples")
    ),
    mainPanel(
      verbatimTextOutput("validation_output"),
      uiOutput("report_link"),  # For online report viewing
      tableOutput("plot_history_table")  # Display plot history table
    )
  )
)

# Server
# The server logic handles user interactions, data validation, and database operations.
server <- function(input, output, session) {
  # Initialize DatabaseHandler
  db_handler <- DatabaseHandler$new("validation_history.db")  # Class defined in R/db_interaction_class.R
  
  # Initialize PathGenerator with a reactive value
  path_generator <- reactive({
    PathGenerator$new(input$base_path)  # Class defined in R/path_generation.R
  })
  
  # Initialize Validator with the PathGenerator instance
  validator <- reactive({
    Validator$new(path_generator())  # Class defined in R/validation_classes.R
  })
  
  # Initialize EmailSender
  email_sender <- EmailSender$new()  # Class defined in R/email_class.R
  
  # Reactive values to store data and errors
  data <- reactiveValues(data_source = NULL, errors = NULL)
  
  # Load data when a file is selected
  observeEvent(input$data_file, {
    req(input$data_file)
    tryCatch({
      data$data_source <- DataSource$new(input$data_file$datapath)  # Class defined in R/data_classes.R
      showNotification("Loaded CSV file successfully.", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading CSV file:", e$message), type = "error")
    })
  })
  
  # Validate data when the button is clicked
  observeEvent(input$validate_button, {
    req(data$data_source)
    validation_results <- validator()$validate(data$data_source)
    data$errors <- validation_results
    
    # Display validation results
    output$validation_output <- renderPrint({
      req(data$errors)
      if (nrow(data$errors) > 0) {
        paste(apply(data$errors, 1, function(x) paste(x, collapse = " | ")), collapse = "\n")
      } else {
        "Validation successful. No errors found."
      }
    })
    
    # Generate paths for saving data and report
    if (nrow(data$errors) == 0) {
      sample_row <- data$data_source$sheet1_data[[1]]
      data_paths <- path_generator()$generate_csv_paths(sample_row)
      report_path <- file.path(dirname(data_paths$main_path), "report-validation.html")
      
      # Save the original data file
      data$data_source$export_data(data_paths$main_path)
      
      # Generate report using the Report class
      report <- Report$new(data$data_source, data$errors)  # Class defined in R/report_class.R
      project_root <- rprojroot::find_root(rprojroot::has_file(".Rprofile"))
      report$generate(dirname(data_paths$main_path), project_root)
      
      # Add data to database
      db_handler$add_plot_data(
        filepath = input$data_file$datapath,
        plot_code = sample_row$Plot.code,
        sample_date = as.character(sample_row$Sample.date),
        detector = sample_row$Detector,
        region = sample_row$Region,
        validation_status = "Success",
        report_path = report_path
      )
      
      # Provide a link to the generated report
      output$report_link <- renderUI({
        tags$a(href = report_path, "View Generated Report", target = "_blank")
      })
    }
  })
  
  # Handle report download
  output$download_report <- downloadHandler(
    filename = "report-validation.html",
    content = function(file) {
      req(data$data_source)
      sample_row <- data$data_source$sheet1_data[[1]]
      data_paths <- path_generator()$generate_csv_paths(sample_row)
      report_path <- file.path(dirname(data_paths$main_path), "report-validation.html")
      if (file.exists(report_path)) {
        file.copy(report_path, file)
      }
    }
  )
  
  # Search plot history
  observeEvent(input$search_button, {
    filtered_data <- db_handler$get_plot_history(
      plot_code = input$search_plot_code,
      from_date = input$search_from_date,
      to_date = input$search_to_date
    )
    output$history_table <- DT::renderDataTable({
      DT::datatable(filtered_data, options = list(pageLength = 10, scrollX = TRUE))
    })
  })
  
  # Load record for editing
  observeEvent(input$load_record_button, {
    req(input$edit_record_id)
    record <- db_handler$get_plot_history()
    record <- record[record$id == input$edit_record_id, ]
    if (nrow(record) > 0) {
      updateTextInput(session, "edit_filepath", value = record$filepath)
      updateTextInput(session, "edit_plot_code", value = record$plot_code)
      updateDateInput(session, "edit_sample_date", value = as.Date(record$sample_date))
      updateTextInput(session, "edit_detector", value = record$detector)
      updateTextInput(session, "edit_region", value = record$region)
      updateTextInput(session, "edit_validation_status", value = record$validation_status)
      updateTextInput(session, "edit_report_path", value = record$report_path)
    }
  })
  
  # Update record
  observeEvent(input$update_record_button, {
    req(input$edit_record_id)
    db_handler$update_plot_data(
      id = input$edit_record_id,
      filepath = input$edit_filepath,
      plot_code = input$edit_plot_code,
      sample_date = as.character(input$edit_sample_date),
      detector = input$edit_detector,
      region = input$edit_region,
      validation_status = input$edit_validation_status,
      report_path = input$edit_report_path
    )
    output$history_table <- DT::renderDataTable({
      DT::datatable(db_handler$get_plot_history(), options = list(pageLength = 10, scrollX = TRUE))
    })
  })
  
  # Delete record
  observeEvent(input$delete_record_button, {
    req(input$delete_record_id)
    db_handler$delete_plot_data(input$delete_record_id)
    output$history_table <- DT::renderDataTable({
      DT::datatable(db_handler$get_plot_history(), options = list(pageLength = 10, scrollX = TRUE))
    })
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)