# tests/testthat/test_reactive_values.R # nolint: commented_code_linter.
library(testthat)
library(shiny)
library(R6)

# Load required files
source("../../R/data_classes.R")
source("../../R/validation_classes.R")

context("Reactive Values Usage")

test_that("Reactive values are accessed within reactive contexts", {
  # Skip if not in interactive mode
  skip_if_not(interactive(), "Skipping reactive tests in non-interactive mode")
  
  # Create a test Shiny app environment
  testServer({
    # Create reactive values
    data <- reactiveValues(data_source = NULL, errors = NULL)
    
    # Set up a reactive expression
    validation_results <- reactive({
      # This should work because we're accessing reactive values in a reactive context
      if (is.null(data$data_source)) {
        return(NULL)
      }
      return(data.frame(row = 1, column = "test", message = "test error"))
    })
    
    # Set up an observer
    observe({
      # This should work because we're accessing reactive values in a reactive context
      data$errors <- validation_results()
    })
    
    # Test setting values
    data$data_source <- list(sheet1_data = list(), sheet2_data = list())
    
    # Test that the reactive expression and observer work
    expect_equal(validation_results()$row, 1)
    expect_equal(validation_results()$column, "test")
    expect_equal(validation_results()$message, "test error")
    
    # Test that the observer has updated the reactive value
    expect_equal(data$errors$row, 1)
    expect_equal(data$errors$column, "test")
    expect_equal(data$errors$message, "test error")
  })
})

test_that("Reactive values are not accessed outside reactive contexts", {
  # Skip if not in interactive mode
  skip_if_not(interactive(), "Skipping reactive tests in non-interactive mode")
  
  # Create a test Shiny app environment
  testServer({
    # Create reactive values
    data <- reactiveValues(data_source = NULL, errors = NULL)
    
    # This should fail because we're accessing reactive values outside a reactive context
    expect_error({
      errors <- data$errors
    }, "Operation not allowed without an active reactive context")
  })
})
