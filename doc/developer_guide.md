# Developer's Guide

## Architecture

The project follows a modular architecture based on R6 classes. The main components are:

-   **Data Classes (`R/data_classes.R`):**
    -   `Sheet1Data`: Represents a single row of data from Sheet 1 of the Excel file.
    -   `Sheet2Data`: Represents a single row of data from Sheet 2 of the Excel file.
    -   `ExcelData`: Represents the entire Excel data, containing lists of `Sheet1Data` and `Sheet2Data` objects.
-   **Validation Classes (`R/validation_classes.R`):**
    -   `ValidationRule`: Base class for all validation rules.
    -   `DataTypeValidationRule`: Validates data types of columns.
    -   `MaxRowsValidationRule`: Checks for the maximum number of rows per plot code.
    -   `UniqueSUValidationRule`: Ensures unique SU values for each plot.
    -   `NotesValidationRule`: Validates the presence of notes when SU rows are empty.
    -   `Validator`: Applies all validation rules to the `ExcelData`.
-   **Report Class (`R/report_class.R`):**
    -   `Report`: Generates an HTML report summarizing the validation results.
-   **Path Generation Class (`R/path_generation_class.R`):**
    -   `PathGenerator`: Creates directory paths for saving reports and data based on plot code, sample date, detector, and region.
-   **Email Class (`R/email_class.R`):**
    -   `EmailSender`: Sends validation reports via email.
-   **Database Interaction Class (`R/db_interaction_class.R`):**
    -   `DatabaseHandler`: Handles interactions with the SQLite database (add, search, update, delete records).
-   **Utility Functions (`R/utils.R`):**
    -   `setup_logging()`: Configures logging for the application.
    -   `handle_image_uploads()`: Handles image uploads in the Shiny app.

## CLI (`app_cli.R`)

The CLI uses the `optparse` package to parse command-line arguments. The `main()` function in `app_cli.R` orchestrates the validation, report generation, and database operations based on the provided arguments.

### Extending the CLI

-   **Adding new commands:**
    1. Add new options to the `option_list` in `app_cli.R`.
    2. Handle the new options in the `main()` function, calling the appropriate functions/methods from the R6 classes.
-   **Adding new command-line arguments to existing commands:**
    1. Modify the relevant options in `option_list`.
    2. Update the corresponding logic in the `main()` function to use the new arguments.

## Shiny App (`app_shiny.R`)

The Shiny app provides a reactive user interface. It uses reactive values and observers to respond to user inputs and update the UI accordingly. The server function in `app_shiny.R` handles the logic for data validation, report generation, database interaction, and UI updates.

### Extending the Shiny App

-   **Adding new UI elements:**
    1. Modify the `ui` object in `app_shiny.R` to add new input elements (e.g., buttons, text inputs, select boxes) or output elements (e.g., tables, plots).
    2. Update the `server` function to handle the logic associated with the new UI elements.
-   **Adding new reactive logic:**
    1. Create new reactive expressions or observers using `reactive()` or `observeEvent()`.
    2. Use reactive values (`reactiveValues()`) to store and update data that is shared between different parts of the app.

## Examples (`examples/examples.R`)

This file contains examples demonstrating how to use the different classes and functions of the project. It is a good starting point for understanding how the components work together. It also serves as a basis for writing unit tests.

## Tests (`tests/testthat`)

Unit tests are written using the `testthat` package. Each test file focuses on testing a specific component (e.g., data classes, validation rules).

### Writing Tests

-   Create a new test file in `tests/testthat` (e.g., `test_my_new_feature.R`).
-   Use `test_that()` to define test cases.
-   Use `expect_` functions (e.g., `expect_equal()`, `expect_true()`, `expect_error()`) to make assertions about the expected behavior of the code.

### Running Tests

```R
testthat::test_dir("tests/testthat")
```
## Extending the Tool
## Adding Validation Rules
1.  **Create a new R6 class** that inherits from ValidationRule.
2.  **Implement** the check() **method** to perform the validation logic. The check() method should accept an ExcelData object as input and return a data frame of errors (if any).
3.  **(Optional) Implement** the get_error_level() **method** to specify the error level (e.g., "Warning", "Error") for the rule.
4.  **Add an instance of your new rule** to the Validator in both app_cli.R and app_shiny.R.

## Adding Data Fields
1. **Modify** the Sheet1Data and/or Sheet2Data **classes** to include the new fields.
2. **Update the validation rules** (in validation_classes.R) to handle the new fields (if necessary).
3. **Update the report generation logic** (in report_class.R and report.Rmd) to include the new fields in the report.
4. **Modify the database schema** (in db_interaction_class.R) if you want to store the new fields in the database.

## Adding Database Features
1. **Extend** the DatabaseHandler **class** with new methods for interacting with the database (e.g., new types of queries, data modification functions).
2. **Update the CLI and/or Shiny app** to provide an interface for the new features. This might involve adding new command-line options (in app_cli.R) or new UI elements (in app_shiny.R).

## Adding Email Functionality
**Update** the EmailSender **class** with new methods for interacting with the database (e.g., new types of queries, data modification functions).

## Documentation
-   **roxygen2 Comments:** Document all functions, classes, and methods using roxygen2 style comments.
-   **pkgdown Website (Optional):** If you choose to use pkgdown, update the website by running pkgdown::build_site().
-   **README.md:** Keep the main README.md up-to-date with any changes to the project's features, installation instructions, or usage examples.
-   **User Guide (doc/user_guide.md):** Update the user guide with instructions on how to use any new features.
-   **Developer's Guide (doc/developer_guide.md):** Update the developer's guide with information about the new code, changes to the architecture, or any other relevant details for developers.
