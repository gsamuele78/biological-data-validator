# In app.R - Server
# ... (Other server logic)

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
