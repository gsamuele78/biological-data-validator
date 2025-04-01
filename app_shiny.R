#' app_shiny.R

# Load necessary libraries using renv
if (!require("renv")) install.packages("renv")
renv::restore()

library(shiny)
library(DT)
library(tools) # For file_ext function

# Source R functions and classes
source("R/data_classes.R")
source("R/validation_classes.R")
source("R/report_class.R")
source("R/path_generation.R")
source("R/email_class.R")
source("R/db_interaction_class.R")
source("R/utils.R")
source("R/csv_mapping.R")

# UI
ui <- fluidPage(
  titlePanel("Biological Environment Data Validation"),
  sidebarLayout(
    sidebarPanel(
      fileInput("data_file", "Choose CSV Data File", 
                accept = c(".csv")),
      fileInput("image_files", "Choose Image Files (Max 4)", 
                multiple = TRUE, 
                accept = c("image/jpeg", "image/png")),
      textInput("base_path", "Customizable Path", value = "/default/path"),
      actionButton("validate_button", "Validate Data"),
      downloadButton("download_report", "Download Report"),
            
      hr(), # Horizontal line separator
            
      h4("Search and Filter Plot History"),
      textInput("search_plot_code", "Plot Code"),
      dateInput("search_from_date", "From Date"),
      dateInput("search_to_date", "To Date"),
      actionButton("search_button", "Search"),
            
      hr(),
            
      h4("Plot History"),
      DT::dataTableOutput("history_table"), # Use DT package for interactive tables
            
      # Add more UI elements for viewing, modifying, deleting data here
      h4("Edit Record"),
      numericInput("edit_record_id", "Record ID to Edit", value = NA),
      actionButton("load_record_button", "Load Record"),
            
      # Add fields for editing data (make sure to handle NULL values appropriately)
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
      uiOutput("report_link"), # For online report viewing
      # Output for displaying the plot history table
      tableOutput("plot_history_table")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Initialize DatabaseHandler 
  db_handler <- DatabaseHandler$new("validation_history.db")
    
  # Initialize PathGenerator with a reactive value
  path_generator <- reactive({
    PathGenerator$new(input$base_path)
  })
    
  # Initialize Validator with the PathGenerator instance
  validator <- reactive({
    Validator$new(path_generator())
  })
    
  # Initialize EmailSender
  email_sender <- EmailSender$new()
    
  # Reactive values to store data and errors
  data <- reactiveValues(data_source = NULL, errors = NULL)
    
  # Load data when a file is selected
  observeEvent(input$data_file, {
    req(input$data_file)
        
    # Create DataSource object
    tryCatch({
      data$data_source <- DataSource$new(input$data_file$datapath)
      showNotification("Loaded CSV file successfully.", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading CSV file:", e$message), type = "error")
    })
  })
    
  # Validate data when the button is clicked
  observeEvent(input$validate_button, {
    req(data$data_source)
        
    # Extract sheet1 and sheet2 data from the DataSource object
    sheet1_data <- data$data_source$sheet1_data
    sheet2_data <- data$data_source$sheet2_data
        
    # Create temporary data object with the format expected by the validator
    validation_data <- list(
      sheet1_data = sheet1_data,
      sheet2_data = sheet2_data
    )
    class(validation_data) <- "DataForValidation" # Assuming the validator checks for this class
        
    # Perform validation - Fix: Ensure reactive values are accessed within reactive contexts
    # Create a reactive expression for validation results
    validation_results <- reactive({
      req(data$data_source)
      validator()$validate(data$data_source)
    })
    
    # Store the results in the reactive values object
    observe({
      data$errors <- validation_results()
    })
        
    # Display validation results
    output$validation_output <- renderPrint({
      req(data$errors)
      if (nrow(data$errors) > 0) {
        paste(apply(data$errors, 1, function(x) paste(x, collapse = " | ")), collapse = "\n")
      } else {
        "Validation successful!"
      }
    })
        
    # Generate paths for saving data and report
    if (nrow(data$errors) == 0) {
      # Use the first row for path generation
      sample_row <- sheet1_data[[1]] 
      data_paths <- path_generator()$generate_csv_paths(sample_row)
      report_path <- file.path(dirname(data_paths$main_path), "report-validation.html")
            
      # Create directory if it doesn't exist
      if (!dir.exists(dirname(data_paths$main_path))) {
        dir.create(dirname(data_paths$main_path), recursive = TRUE)
      }
            
      # Save the original data file
      data$data_source$export_data(data_paths$main_path)
            
      # Handle image uploads
      if (!is.null(input$image_files)) {
        handle_image_uploads(input$image_files, dirname(data_paths$main_path))
      }
            
      # Generate report using the Report class
      report <- Report$new(data$data_source, data$errors)
      project_root <- rprojroot::find_root(rprojroot::has_file(".Rprofile"))
      report$generate(dirname(data_paths$main_path), project_root)
            
      # Add data to database
      plot_data_id <- db_handler$add_plot_data(
        input$data_file$datapath, 
        sample_row$Plot.code, 
        as.character(sample_row$Sample.date), 
        sample_row$Detector, 
        sample_row$Region, 
        "Success", 
        report_path
      )
            
      # If images were uploaded, add them to the database as well
      if (!is.null(input$image_files)) {
        for (i in seq_len(nrow(input$image_files))) {
          db_handler$add_image_data(plot_data_id, file.path(dirname(data_paths$main_path), input$image_files$name[i]))
        }
      }
            
      # Update plot history table
      output$plot_history_table <- renderTable({
        db_handler$get_plot_history()
      })
            
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
      # Determine the correct report path
      req(data$data_source)
      sample_row <- data$data_source$sheet1_data[[1]]
      data_paths <- path_generator()$generate_csv_paths(sample_row)
      report_path <- file.path(dirname(data_paths$main_path), "report-validation.html")
            
      if (file.exists(report_path)) {
        file.copy(report_path, file)
      } else {
        showNotification("Report file not found.", type = "error")
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
      DT::datatable(filtered_data, options = list(pageLength = 10, scrollX = TRUE)) # Make table scrollable
    })
  })
    
  # Load record for editing
  observeEvent(input$load_record_button, {
    req(input$edit_record_id)
    record <- db_handler$get_plot_history() # Assuming you have a function to get a record by ID
    record <- record[record$id == input$edit_record_id, ]
        
    if (nrow(record) > 0) {
      updateTextInput(session, "edit_filepath", value = record$filepath)
      updateTextInput(session, "edit_plot_code", value = record$plot_code)
      updateDateInput(session, "edit_sample_date", value = as.Date(record$sample_date))
      updateTextInput(session, "edit_detector", value = record$detector)
      updateTextInput(session, "edit_region", value = record$region)
      updateTextInput(session, "edit_validation_status", value = record$validation_status)
      updateTextInput(session, "edit_report_path", value = record$report_path)
    } else {
      # Handle case where no record is found
      showModal(modalDialog(
        title = "Error",
        "No record found with the specified ID.",
        easyClose = TRUE
      ))
    }
  })
    
  # Update record
  observeEvent(input$update_record_button, {
    req(input$edit_record_id)
    db_handler$update_plot_data(
      input$edit_record_id,
      input$edit_filepath,
      input$edit_plot_code,
      as.character(input$edit_sample_date),
      input$edit_detector,
      input$edit_region,
      input$edit_validation_status,
      input$edit_report_path
    )
        
    # Refresh the history table
    output$history_table <- DT::renderDataTable({
      DT::datatable(db_handler$get_plot_history(), options = list(pageLength = 10, scrollX = TRUE))
    })
        
    # Show a success message
    showModal(modalDialog(
      title = "Success",
      "Record updated successfully.",
      easyClose = TRUE
    ))
  })
    
  # Delete record
  observeEvent(input$delete_record_button, {
    req(input$delete_record_id)
        
    showModal(modalDialog(
      title = "Confirm Delete",
      paste("Are you sure you want to delete record ID", input$delete_record_id, "?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete", "Delete")
      ),
      easyClose = TRUE
    ))
  })
    
  # Confirm delete action
  observeEvent(input$confirm_delete, {
    db_handler$delete_plot_data(input$delete_record_id)
        
    # Refresh the history table
    output$history_table <- DT::renderDataTable({
      DT::datatable(db_handler$get_plot_history(), options = list(pageLength = 10, scrollX = TRUE))
    })
        
    removeModal() # Close the confirmation modal
        
    # Show a success message
    showModal(modalDialog(
      title = "Success",
      "Record deleted successfully.",
      easyClose = TRUE
    ))
  })
    
  # Display plot history
  output$plot_history_table <- renderTable({
    db_handler$get_plot_history() # Initially show all history
  })
    
  # Display plot history with interactive features from DT package
  output$history_table <- DT::renderDataTable({
    DT::datatable(db_handler$get_plot_history(), options = list(pageLength = 10, scrollX = TRUE))
  })
    
  # Helper function to handle image uploads
  handle_image_uploads <- function(image_files, destination_path) {
    if (!dir.exists(destination_path)) {
      dir.create(destination_path, recursive = TRUE)
    }
        
    for (i in seq_len(nrow(image_files))) {
      file.copy(
        image_files$datapath[i],
        file.path(destination_path, image_files$name[i]),
        overwrite = TRUE
      )
    }
  }
    
  # Use Case Examples - consolidated implementation
  output$use_case_examples <- renderPrint({
    # Example with valid data
    valid_sheet1_data <- list(
      Sheet1Data$new(list(Plot.code = "Plot1", SU = 1, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 1")), # nolint: line_length_linter.
      Sheet1Data$new(list(Plot.code = "Plot1", SU = 2, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 2")), # nolint: line_length_linter.
      Sheet1Data$new(list(Plot.code = "Plot1", SU = 3, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 3")), # nolint: line_length_linter.
      Sheet1Data$new(list(Plot.code = "Plot1", SU = 4, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 4")) # nolint: line_length_linter.
    )
        
    valid_sheet2_data <- list(
      Sheet2Data$new(list(Plot.code = "Plot1", Subplot = 1, Species = "Species A", species_abb = "Sp. A", cover = 50, Layer = "Tree", Notes = "Note A")), # nolint: line_length_linter.
      Sheet2Data$new(list(Plot.code = "Plot1", Subplot = 2, Species = "Species B", species_abb = "Sp. B", cover = 60, Layer = "Herb", Notes = "Note B")), # nolint: line_length_linter.
      Sheet2Data$new(list(Plot.code = "Plot1", Subplot = 3, Species = "Species C", species_abb = "Sp. C", cover = 70, Layer = "Shrub", Notes = "Note C")), # nolint: line_length_linter.
      Sheet2Data$new(list(Plot.code = "Plot1", Subplot = 4, Species = "Species D", species_abb = "Sp. D", cover = 80, Layer = "Moss", Notes = "Note D")) # nolint: line_length_linter.
    )
        
    # Create a mock DataSource object
    valid_data_source <- list(
      sheet1_data = valid_sheet1_data,
      sheet2_data = valid_sheet2_data
    )
    class(valid_data_source) <- "DataForValidation"
        
    # Example with invalid data
    invalid_sheet1_data <- list(
      Sheet1Data$new(list(Plot.code = "Plot2", SU = 1, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 1")), # nolint: line_length_linter.
      Sheet1Data$new(list(Plot.code = "Plot2", SU = 1, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 2")), # Duplicate SU # nolint: line_length_linter.
      Sheet1Data$new(list(Plot.code = "Plot2", SU = 3, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 3")), # nolint: line_length_linter.
      Sheet1Data$new(list(Plot.code = "Plot2", SU = 4, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 4")) # nolint: line_length_linter.
    )
        
    invalid_sheet2_data <- list(
      Sheet2Data$new(list(Plot.code = "Plot2", Subplot = 1, Species = "Species A", species_abb = "Sp. A", cover = 50, Layer = "Tree", Notes = "Note A")), # nolint: line_length_linter.
      Sheet2Data$new(list(Plot.code = "Plot2", Subplot = 2, Species = "Species B", species_abb = "Sp. B", cover = 60, Layer = "Herb", Notes = "Note B")), # nolint: line_length_linter.
      Sheet2Data$new(list(Plot.code = "Plot2", Subplot = 3, Species = "Species C", species_abb = "Sp. C", cover = 70, Layer = "Shrub", Notes = "Note C")) # nolint: line_length_linter.
      # Missing Subplot 4
    )
        
    # Create a mock DataSource object for invalid data
    invalid_data_source <- list(
      sheet1_data = invalid_sheet1_data,
      sheet2_data = invalid_sheet2_data
    )
    class(invalid_data_source) <- "DataForValidation"
        
    # Initialize PathGenerator with a default base path for the examples
    example_path_generator <- PathGenerator$new("/example/path")
        
    # Initialize Validator with the PathGenerator
    example_validator <- Validator$new(example_path_generator)
        
    # Perform validation and print results
    cat("Use Case Example 1: Valid Data\n")
    cat("Validation Errors:\n")
    print(example_validator$validate(valid_data_source))
        
    cat("\n\nUse Case Example 2: Invalid Data\n")
    cat("Validation Errors:\n")
    print(example_validator$validate(invalid_data_source))
        
    # Example of how to use the DataSource class
    cat("\n\nExample of DataSource Class Usage:\n")
    cat("1. Loading an Excel file:\n")
    cat("   data_source <- DataSource$new('path/to/file.xlsx')\n")
    cat("   # Access data:\n")
    cat("   sheet1_data <- data_source$sheet1_data\n")
    cat("   sheet2_data <- data_source$sheet2_data\n\n")
        
    cat("2. Loading a CSV file:\n")
    cat("   data_source <- DataSource$new('path/to/file.csv')\n")
    cat("   # For CSV, it will automatically look for a second file named 'path/to/file_species.csv'\n\n")
        
    cat("3. Exporting data:\n")
    cat("   # To Excel:\n")
    cat("   data_source$export_to_excel('path/to/output.xlsx')\n\n")
    cat("   # To CSV:\n")
    cat("   data_source$export_to_csv('path/to/output.csv')\n")
    cat("   # This will create two files: output.csv and output_species.csv\n")
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)