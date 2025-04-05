# Logic Explanation of the Biological Data Validator Application

## Overview
The **Biological Data Validator** is an R-based application designed to validate ecological data stored in CSV files. It provides a command-line interface (CLI) and integrates with a database to store validation results and associated metadata. The application uses object-oriented programming (OOP) principles with the `R6` package and supports modular validation rules, reporting, and database interactions.

---

## Logical Steps of the Application

1. **Input Data**:
   - Users provide CSV files containing ecological data (e.g., plot data and species data).
   - The application validates the file structure, content, and data types.

2. **Validation**:
   - Validation rules are applied to ensure data integrity (e.g., checking data types, unique values, and file naming conventions).
   - Errors and warnings are collected during validation.

3. **Reporting**:
   - Validation results are compiled into a report (HTML or CSV format).
   - Reports can be emailed to specified recipients.

4. **Database Interaction**:
   - Validated data and metadata are stored in an SQLite database.
   - Users can query, update, or delete records from the database.

5. **Output**:
   - Validation reports and processed data are saved to organized directories.
   - Users can retrieve validation history and associated images.

---

## File Descriptions and Purposes

### `app_cli.R`
- **Purpose**: Provides a command-line interface for validating data, managing the database, and generating reports.
- **Key Classes/Functions**:
  - `Validator`: Applies validation rules.
  - `DatabaseHandler`: Manages database interactions.
  - `PathGenerator`: Handles directory and file path generation.
  - `Report`: Generates validation reports.
  - `EmailSender`: Sends reports via email.
- **Logic**:
  1. Parse command-line arguments using `optparse`.
  2. Validate input files and apply validation rules.
  3. Generate reports and store results in the database.
  4. Provide options to query, update, or delete database records.

### `project_specific_rprofile.R`
- **Purpose**: Configures project-specific settings and ensures the `renv` environment is synchronized.
- **Key Functions**:
  - `check_and_update_renv`: Checks if the `renv` environment is synchronized and restores it if necessary.

### `.lintr`
- **Purpose**: Configures linting rules for the project to ensure code quality.
- **Key Settings**:
  - `linters`: Specifies linting rules (e.g., line length, object naming conventions).
  - `exclusions`: Lists directories or files to exclude from linting.

### `R/validation_classes.R`
- **Purpose**: Defines the `Validator` class, which applies validation rules to data.
- **Key Classes**:
  - `Validator`: Manages validation rules and applies them to data sources.

### `R/validation_rules.R`
- **Purpose**: Defines individual validation rules as R6 classes.
- **Key Classes**:
  - `ValidationRule`: Base class for all validation rules.
  - `DataTypeValidationRule`: Checks data types for specific fields.

### `R/report_class.R`
- **Purpose**: Defines the `Report` class for generating validation reports.
- **Key Classes**:
  - `Report`: Generates HTML or CSV reports based on validation results.

### `R/path_generation.R`
- **Purpose**: Defines the `PathGenerator` class for creating organized directory structures.
- **Key Classes**:
  - `PathGenerator`: Generates paths for saving data and reports.

### `R/email_class.R`
- **Purpose**: Defines the `EmailSender` class for sending validation reports via email.
- **Key Classes**:
  - `EmailSender`: Sends emails with attached reports.

### `R/db_interaction_class.R`
- **Purpose**: Defines the `DatabaseHandler` class for interacting with the SQLite database.
- **Key Classes**:
  - `DatabaseHandler`: Manages database connections and operations.

### `R/data_classes.R`
- **Purpose**: Defines classes for representing and handling data from CSV files.
- **Key Classes**:
  - `Sheet1Data`: Represents a single row in the main data sheet.
  - `Sheet2Data`: Represents a single row in the species data sheet.
  - `DataSource`: Manages loading and exporting data from CSV files.

### `R/csv_mapping.R`
- **Purpose**: Defines mappings between CSV columns and internal field names.
- **Key Variables**:
  - `SHEET1_CSV_MAPPING`: Maps main data sheet columns.
  - `SHEET2_CSV_MAPPING`: Maps species data sheet columns.

---

## Example Workflow

1. **Run CLI**:
   ```bash
   Rscript app_cli.R --file data/Plot_Template_INFI2023.csv --email user@example.com
   ```

2. **Validation**:
   - The `Validator` applies rules to the data.
   - Errors and warnings are collected.

3. **Report Generation**:
   - A validation report is generated and saved to a directory.

4. **Database Storage**:
   - Validated data and metadata are stored in the SQLite database.

5. **Email Report**:
   - The validation report is emailed to the specified recipient.

---

## References

- **`renv` Documentation**: [https://rstudio.github.io/renv/articles/renv.html](https://rstudio.github.io/renv/articles/renv.html)
- **R6 Classes**: [https://cran.r-project.org/web/packages/R6/vignettes/Introduction.html](https://cran.r-project.org/web/packages/R6/vignettes/Introduction.html)
- **`rmarkdown` Documentation**: [https://rmarkdown.rstudio.com/](https://rmarkdown.rstudio.com/)