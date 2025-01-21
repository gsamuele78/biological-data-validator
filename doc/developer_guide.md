# Developer Guide for Biological Environment Data Validation Tool

This guide provides information for developers who want to contribute to or modify the Biological Environment Data Validation Tool.

## Table of Contents

1. [Project Structure](#project-structure)
2. [Code Style](#code-style)
3. [Adding Validation Rules](#adding-validation-rules)
4. [Modifying the UI](#modifying-the-ui)
5. [Testing](#testing)
6. [Contributing](#contributing)

## 1. Project Structure <a name="project-structure"></a>

The project has the following directory structure:

biological-data-validator/  
├── app.R # Shiny app (UI and server combined)  
├── R/ # R functions and classes  
│ ├── data_classes.R  
│ ├── validation_classes.R  
│ ├── report_class.R  
│ ├── path_generation_class.R  
│ ├── email_class.R  
│ └── db_interaction_class.R  
├── data/ # Sample data  
│ └── sample_data.xlsx  
├── report.Rmd # R Markdown report template  
├── LICENSE  
├── README.md  
└── doc/ # Documentation files  
├── user_guide.md  
├── examples/  
│ ├── valid_data_example.R  
│ └── invalid_data_example.R  
└── developer_guide.md


*   **`app.R`:**  Contains the Shiny application code (UI and server logic).
*   **`R/`:**  Contains R6 classes that define the data model, validation rules, report generation, path generation, email functionality, and database interactions.
*   **`data/`:**  Contains a sample Excel data file (`sample_data.xlsx`).
*   **`report.Rmd`:**  The R Markdown template for generating validation reports.
*   **`doc/`:** Contains documentation files, including the user guide, examples, and this developer guide.

## 2. Code Style <a name="code-style"></a>

*   Follow the [tidyverse style guide](https://style.tidyverse.org/) for R code.
*   Use meaningful variable and function names.
*   Comment your code to explain complex logic.
*   Keep functions short and focused on a single task.
*   Use R6 classes to organize related data and functions.

## 3. Adding Validation Rules <a name="adding-validation-rules"></a>

To add a new validation rule:

1. Create a new R6 class that inherits from the  `ValidationRule`  base class in  `R/validation_classes.R`.
2. Implement the  `check()`  method in your new class. The  `check()`  method should take an  `ExcelData`  object as input and return a data frame of errors found.
3. Add an instance of your new validation rule class to the  `Validator`  object in  `app.R`  (inside the server function).

**Example:**

```R
# In R/validation_classes.R

MyNewValidationRule <- R6Class("MyNewValidationRule",
  inherit = ValidationRule,
  public = list(
    check = function(excel_data) {
      errors <- data.frame(Sheet = character(), Row = integer(), Column = character(), Message = character(), stringsAsFactors = FALSE)
      # ... your validation logic here ...
      return(errors)
    }
  )
)

# In app.R (inside the server function)
validator <- Validator$new(path_generator)
validator$add_rule(MyNewValidationRule$new()) # Add the new rule
```
## 4. Modifying the UI <a name="modifying-the-ui"></a>
The UI is defined in the ui object within app.R. You can modify the UI by adding or removing Shiny input and output elements. Refer to the Shiny documentation for more information on creating Shiny UIs.

## 5. Testing <a name="testing"></a>
Manual Testing: Run the application and test the various features with different inputs (valid and invalid data).

Automated Testing (Recommended): Write unit tests using the testthat package to automatically test your validation rules and other functions.

## 6. Contributing <a name="contributing"></a>
If you want to contribute to the project, please follow these steps:

1. Fork the repository on GitHub.
2. Create a new branch for your changes: git checkout -b my-new-feature
3. Make your changes and commit them: git commit -m "Add some feature"
4. Push your branch to your fork: git push origin my-new-feature
5. Create a pull request on the main repository.

Please ensure your code follows the code style guidelines and includes appropriate tests.
