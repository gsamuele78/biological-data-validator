# app_shiny.R

# Load necessary libraries using renv
if (!require("renv")) install.packages("renv")
renv::restore()

library(shiny)
library(DT)

# Source R functions and classes
source("R/data_classes.R")
source("R/validation_classes.R")
source("R/report_class.R")
source("R/path_generation_class.R")
source("R/email_class.R")
source("R/db_interaction_class.R")
source("R/utils.R")

# UI
ui <- fluidPage(
    titlePanel("Biological Environment Data Validation"),
    sidebarLayout(
        sidebarPanel(
            fileInput("excel_file", "Choose Excel File", accept = c(".xlsx", ".xls")),
            fileInput("image_files", "Choose Image Files (Max 4)", multiple = TRUE, accept = c("image/jpeg", "image/png")),
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
    db_handler <- DatabaseHandler$new()
    
    # Initialize PathGenerator with the default base path
    path_generator <- PathGenerator$new(input$base_path)  # Use a default path or a user-specified path
    
    # Initialize Validator with the PathGenerator instance
    validator <- Validator$new(path_generator)  # Pass the PathGenerator instance to Validator
    
    # Initialize EmailSender
    email_sender <- EmailSender$new()
    
    # Reactive values to store data and errors
    data <- reactiveValues(excel_data = NULL, errors = NULL)
    
    # Load Excel data when a file is selected
    observeEvent(input$excel_file, {
        req(input$excel_file)
        data$excel_data <- ExcelData$new(input$excel_file$datapath)
        # Since we don't have separate sheet1 and sheet2 reactive values, 
        # you can directly use data$excel_data$sheet1_data and data$excel_data$sheet2_data
    })
    
    # Validate data when the button is clicked
    observeEvent(input$validate_button, {
        req(data$excel_data)
        
        # Perform validation
        data$errors <- validator$validate(data$excel_data)
        
        # Display validation results
        output$validation_output <- renderPrint({
            if (nrow(data$errors) > 0) {
                paste(apply(data$errors, 1, function(x) paste(x, collapse = " | ")), collapse = "\n")
            } else {
                "Validation successful!"
            }
        })
        
        # Generate paths for saving data and report
        if (nrow(data$errors) == 0) {
            # Assuming validation is successful, generate paths
            sample_row <- data$excel_data$sheet1_data[[1]] # Example: Use the first row for path generation
            data_path <- path_generator$generate(
                sample_row$Plot.code,
                sample_row$Sample.date,
                sample_row$Detector,
                sample_row$Region
            )
            report_path <- file.path(data_path, "report-validation.html")
            
            # Save data, images, and generate report (replace with your actual logic)
            
            # Save the Excel file
            file.copy(input$excel_file$datapath, file.path(data_path, input$excel_file$name))
            
            # Handle image uploads
            if (!is.null(input$image_files)) {
                handle_image_uploads(input$image_files, data_path)
            }
            
            # Generate report using the Report class
            report <- Report$new(input$excel_file$datapath, data$errors, data$excel_data$sheet1_data, data$excel_data$sheet2_data)
            #report$generate(data_path)
            project_root <- rprojroot::find_root(rprojroot::has_file(".Rprofile"))
            report$generate(data_path, project_root)
            
            # Send email (optional)
            # email_sender$send(report_path)
            
            # Add data to database
            plot_data_id <- db_handler$add_plot_data(
                input$excel_file$datapath, 
                sample_row$Plot.code, 
                as.character(sample_row$Sample.date), 
                sample_row$Detector, 
                sample_row$Region, 
                "Success", 
                report_path
            )
            
            # If images were uploaded, add them to the database as well
            if (!is.null(input$image_files)) {
                for (i in 1:nrow(input$image_files)) {
                    db_handler$add_image_data(plot_data_id, file.path(data_path, input$image_files$name[i]))
                }
            }
            
            # Update plot history table
            output$plot_history_table <- renderTable({
                db_handler$get_plot_history()
            })
        }
    })
    
    # Handle report download
    output$download_report <- downloadHandler(
        filename = "report-validation.html",
        content = function(file) {
            # Determine the correct report path
            # This could be stored in a reactive variable or determined from data$excel_data
            # For example, using the first row of sheet1 data for path generation
            sample_row <- data$excel_data$sheet1_data[[1]]
            data_path <- path_generator$generate(
                sample_row$Plot.code,
                sample_row$Sample.date,
                sample_row$Detector,
                sample_row$Region
            )
            report_path <- file.path(data_path, "report-validation.html")
            
            file.copy(report_path, file)
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
    
    # Use Case Examples
    output$use_case_examples <- renderPrint({
        # Example with valid data
        valid_sheet1_data <- list(
            Sheet1Data$new(list(Plot.code = "Plot1", SU = 1, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 1")),
            Sheet1Data$new(list(Plot.code = "Plot1", SU = 2, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 2")),
            Sheet1Data$new(list(Plot.code = "Plot1", SU = 3, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 3")),
            Sheet1Data$new(list(Plot.code = "Plot1", SU = 4, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 4"))
        )
        
        valid_sheet2_data <- list(
            Sheet2Data$new(list(Plot.code = "Plot1", Subplot = 1, Species = "Species A", species_abb = "Sp. A", cover = 50, Layer = "Tree", Notes = "Note A")),
            Sheet2Data$new(list(Plot.code = "Plot1", Subplot = 2, Species = "Species B", species_abb = "Sp. B", cover = 60, Layer = "Herb", Notes = "Note B")),
            Sheet2Data$new(list(Plot.code = "Plot1", Subplot = 3, Species = "Species C", species_abb = "Sp. C", cover = 70, Layer = "Shrub", Notes = "Note C")),
            Sheet2Data$new(list(Plot.code = "Plot1", Subplot = 4, Species = "Species D", species_abb = "Sp. D", cover = 80, Layer = "Moss", Notes = "Note D"))
        )
        
        valid_excel_data <- ExcelData$new("dummy_path")  # Provide a dummy path
        valid_excel_data$sheet1_data <- valid_sheet1_data
        valid_excel_data$sheet2_data <- valid_sheet2_data
        
        # Example with invalid data
        invalid_sheet1_data <- list(
            Sheet1Data$new(list(Plot.code = "Plot2", SU = 1, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 1")),
            Sheet1Data$new(list(Plot.code = "Plot2", SU = 1, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 2")), # Duplicate SU
            Sheet1Data$new(list(Plot.code = "Plot2", SU = 3, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 3")),
            Sheet1Data$new(list(Plot.code = "Plot2", SU = 4, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 4"))
        )
        
        invalid_sheet2_data <- list(
            Sheet2Data$new(list(Plot.code = "Plot2", Subplot = 1, Species = "Species A", species_abb = "Sp. A", cover = 50, Layer = "Tree", Notes = "Note A")),
            Sheet2Data$new(list(Plot.code = "Plot2", Subplot = 2, Species = "Species B", species_abb = "Sp. B", cover = 60, Layer = "Herb", Notes = "Note B")),
            Sheet2Data$new(list(Plot.code = "Plot2", Subplot = 3, Species = "Species C", species_abb = "Sp. C", cover = 70, Layer = "Shrub", Notes = "Note C"))
            # Missing Subplot 4
        )
        
        invalid_excel_data <- ExcelData$new("dummy_path")  # Provide a dummy path
        invalid_excel_data$sheet1_data <- invalid_sheet1_data
        invalid_excel_data$sheet2_data <- invalid_sheet2_data
        
        # Initialize PathGenerator with a default base path for the examples
        example_path_generator <- PathGenerator$new("/example/path")
        
        # Initialize Validator with the PathGenerator
        example_validator <- Validator$new(example_path_generator)
        
        # Perform validation and print results
        cat("Use Case Example 1: Valid Data\n")
        cat("Validation Errors:\n")
        print(example_validator$validate(valid_excel_data))
        
        cat("\n\nUse Case Example 2: Invalid Data\n")
        cat("Validation Errors:\n")
        print(example_validator$validate(invalid_excel_data))
    })
}

shinyApp(ui, server)
