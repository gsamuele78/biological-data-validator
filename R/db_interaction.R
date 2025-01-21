# In R/db_interaction.R

# Database interaction
DatabaseHandler <- R6Class("DatabaseHandler",
  # ... (initialize, create_tables, add_plot_data, add_image_data remain the same)

    # Add filtering to get_plot_history
    get_plot_history = function(plot_code = NULL, from_date = NULL, to_date = NULL) {
        query <- "SELECT * FROM plot_data WHERE 1=1"
        params <- list()
        
        if (!is.null(plot_code) && plot_code != "") {
            query <- paste(query, "AND plot_code LIKE ?")
            params <- c(params, paste0("%", plot_code, "%"))
        }
        
        if (!is.null(from_date)) {
            query <- paste(query, "AND sample_date >= ?")
            params <- c(params, as.character(from_date))
        }
        
        if (!is.null(to_date)) {
            query <- paste(query, "AND sample_date <= ?")
            params <- c(params, as.character(to_date))
        }
        
        query <- paste(query, "ORDER BY timestamp DESC")
        
        if (length(params) > 0) {
            dbGetQuery(self$db, query, params = params)
        } else {
            dbGetQuery(self$db, query)
        }
    },

    # Update a record
    update_plot_data = function(id, filepath, plot_code, sample_date, detector, region, validation_status, report_path) {
      dbExecute(self$db, "UPDATE plot_data SET filepath = ?, plot_code = ?, sample_date = ?, detector = ?, region = ?, validation_status = ?, report_path = ? WHERE id = ?",
                params = list(filepath, plot_code, sample_date, detector, region, validation_status, report_path, id))
    },

    # Delete a record
    delete_plot_data = function(id) {
      dbExecute(self$db, "DELETE FROM plot_data WHERE id = ?", params = list(id))
      dbExecute(self$db, "DELETE FROM images WHERE plot_data_id = ?", params = list(id)) # Delete associated images
    }
  )
)
