# Biological Environment Data Validation Tool

[![R-CI (RSPM)](https://github.com/<your_github_username>/<your_repo_name>/actions/workflows/r-ci-rspm.yml/badge.svg)](https://github.com/<your_github_username>/<your_repo_name>/actions/workflows/r-ci-rspm.yml)
[![R-CI (BSPM)](https://github.com/<your_github_username>/<your_repo_name>/actions/workflows/r-ci-bspm.yml/badge.svg)](https://github.com/<your_github_username>/<your_repo_name>/actions/workflows/r-ci-bspm.yml)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

## Description

This R project provides a comprehensive solution for validating data collected in biological environment field studies. It is designed to ensure data quality, consistency, and integrity through a series of checks and validations. The project supports both a Command-Line Interface (CLI) and a Shiny web application interface, catering to a wide range of user preferences and technical expertise.


The tool performs the following key functions:

-   **Data Type Validation:** Ensures that the data entered in the Excel files conforms to the expected data types (e.g., numeric, character, date).
-   **Validation Rules:** Applies specific validation rules relevant to biological environment studies, such as:
    -   Maximum of 4 rows per plot code with SU (Sampling Unit) values of 1, 2, 3, and 4.
    -   Unique SU values for each plot.
    -   Presence of corresponding notes when SU rows are empty.
-   **Report Generation:** Creates detailed HTML reports summarizing the validation results, including error levels (Warning, Error) for each issue found.
-   **Data Management:** Stores validation history in an SQLite database, allowing users to track, search, update, and delete records.
-   **Image Handling:** (Optional) Associates image files (e.g., JPEG, PNG) with plot data records.
-   **Email Integration:** (Optional) Sends validation reports via email.

## Project Structure

The project is organized as follows:
```bash
biological-data-validator/
├── app_cli.R # Main CLI script
├── app_shiny.R # Main Shiny script
├── R/ # R functions and classes (shared)
│ ├── data_classes.R
│ ├── validation_classes.R
│ ├── report_class.R
│ ├── path_generation_class.R
│ ├── email_class.R
│ ├── db_interaction_class.R
│ └── utils.R
├── tests/ # Unit tests
│ └── testthat/
│ ├── test_data_classes.R
│ ├── test_validation_rules.R
│ └── ...
├── doc/ # Documentation files
│ ├── developer_guide.md
│ ├── user_guide.md
│ └── examples.md
├── examples/ # Usage examples
│ └── examples.R
├── report.Rmd # R Markdown report template
├── data/ # Sample data
│ └── sample_data.xlsx
├── .lintr # Linter configuration
├── .Rprofile # Project-specific R settings
├── project_specific_rprofile.R # Project-specific R settings template
├── .github/ # GitHub Actions workflows
│ └── workflows/
│ └── r-ci.yml
├── LICENSE
├── README.md
└── renv/ # renv environment files
└── renv.lock
```

## Installation

1. **Install R:** Download and install the latest version of R from [https://www.r-project.org/](https://www.r-project.org/).
2. **Install RStudio (Recommended):** Download and install RStudio Desktop from [https://www.rstudio.com/products/rstudio/download/](https://www.rstudio.com/products/rstudio/download/).
3. **Clone the Repository:**

    ```bash
    git clone https://github.com/gsamuele78/biological-data-validator.git
    cd biological-data-validator
    ```

4. **Install Dependencies:**

    ```R
    # (Open R console or RStudio in the project directory)
    install.packages("renv")  # If you don't have renv
    renv::restore()
    ```

## Usage

### Choosing the Interface

This project offers two interfaces:

1. **Command-Line Interface (CLI):** Suitable for users comfortable with the terminal or those who need to automate the validation process through scripts.
2. **Shiny Web Application:** Provides a user-friendly, interactive graphical interface for users who prefer a visual approach.

### Running the CLI Version

1. **Open your terminal or command prompt.**
2. **Navigate to the project directory:**

    ```bash
    cd /path/to/biological-data-validator
    ```

3. **Run the `app_cli.R` script with the desired options:**

    ```bash
    Rscript app_cli.R --file data/sample_data.xlsx --base_path output_data
    ```

    Refer to the CLI Usage section below for more details on available commands and options.

### Running the Shiny Version

1. **Open R or RStudio** in the project directory.
2. **Run the `run_shiny_app()` function** defined in the `.Rprofile` file (it will be added automatically if it doesn't exist):

    ```R
    run_shiny_app()
    ```

    This will launch the Shiny web application in your default browser.

### CLI Usage

The CLI provides a command-line interface for interacting with the tool. Here are the available commands:

#### Validate Data

```bash
Rscript app_cli.R --file data/sample_data.xlsx --base_path output_data
```
```bash
--file or -f: (Required) Path to the Excel data file.

--images or -i: (Optional) Comma-separated paths to image files.

--base_path or -b: (Optional) Base path for saving data and reports (default: /default/path).

--email or -e: (Optional) Email address to send the report to.

--database or -d: (Optional) Path to the SQLite database file (default: validation_history.db).
```
#### Search Records

```bash
Rscript app_cli.R --search --plot_code Plot1 --from_date 2023-01-01 --to_date 2023-12-31
```
```bash
--search or -s: (Required) Trigger a search operation.

--plot_code or -p: (Optional) Plot code to search for.

--from_date: (Optional) Start date for the search (YYYY-MM-DD).

--to_date: (Optional) End date for the search (YYYY-MM-DD).
```
#### Update Record

```bash
Rscript app_cli.R --update --update_id 1 --file updated_data.xlsx
```
```bash
--update or -u: (Required) Trigger an update operation.

--update_id: (Required) ID of the record to update.

--file: (Optional) You might need additional options to specify the updated values.

```
#### Delete Record

```bash
Rscript app_cli.R --delete --delete_id 1
```
```bash
--delete: (Required) Trigger a delete operation.

--delete_id: (Required) ID of the record to delete.
```
#### Get Help

```bash
Rscript app_cli.R --help
```
### Shiny Usage

1. **Upload Excel File:** Use the "Choose Excel File" button to upload your data file.

2. **Upload Images (Optional):** Use the "Choose Image Files" button to upload up to 4 JPEG images.

3. **Customize Path (Optional):** Modify the base path for saving data and reports.

4. **Validate Data:** Click the "Validate Data" button.

5. **View/Download Report:** View the report online or download it as an HTML file. The report includes error levels (Warning, Error) for each issue found.

6. **Search and Filter Plot History:**

    *   Enter a plot code in the "Plot Code" field to search for specific plots.

    *   Use the "From Date" and "To Date" fields to filter records by date range.

    *   Click the "Search" button to apply the filters.

7. **Edit Records:**

    *   Enter the ID of the record you want to edit in the "Record ID to Edit" field.

    *   Click "Load Record" to populate the edit fields.

    *   Modify the fields as needed.

    *   Click "Update Record" to save the changes.

8. **Delete Records:**

    *   Enter the ID of the record you want to delete in the "Record ID to Delete" field.

    *   Click "Delete Record".

    *   Confirm the deletion when prompted.
## Examples
See the [examples guide](https://www.google.com/url?sa=E&q=doc%2Fexamples.md) for detailed usage examples.  

## Project Maintenance
### Linting
**Purpose:** Identify potential code style, syntax, and logical errors.
* **Tool:** ``` lintr ``` package
* **Configuration:** ```  .lintr ``` file
* **How to Run:**
  ``` lintr::lint_pkg() ```
### Styling
**Purpose:** Automatically format code according to a consistent style guide (tidyverse style guide by default).
* **Tool:** styler package
* **How to Run:**
  ``` styler::style_pkg() ```
### Testing
**Purpose:** Ensure that individual components (functions, classes) of the code work as expected.
* **Tool:** testthat package
* **Structure:** Tests are located in the tests/testthat directory.
* **How to Run:**
  ``` testthat::test_dir("tests/testthat") ```
## Continuous Integration (CI)
**Purpose:** Automate linting, styling, and testing on every push and pull request to the main branch.

* **Platform:** GitHub Actions

* **Workflow File:** ``` .github/workflows/r-ci.yml ```

* **Actions:**
*    Checks out the repository code.
*    Sets up ``` R ```.
*    Installs dependencies using renv.
*    Runs ``` lintr ``` to check code style and potential errors.
*    Runs ``` testthat ``` to execute unit tests.
*    Runs ``` styler ``` to automatically format code.

## Logging
**Purpose:** Record important events, warnings, and errors during the application's execution for debugging and monitoring.
* **Tool:** logger package
* **Configuration:** ``` setup_logging() ``` function in R/utils.R
* **Usage:**
```bash  
log_info("Starting data validation...")
log_error("Data validation failed with {length(errors)} errors.")
 ```
## Package Management
**Purpose:** Create reproducible environments and manage project dependencies, ensuring that the correct versions of all required R packages are installed and used.
* **Tool:** ``` renv ```  package
* **Workflow:**
 1.  **Initialization:** ``` renv::init() ``` initializes a new project environment and creates an renv folder, renv.lock file, and modifies .Rprofile to automatically source renv/activate.R.
 2.  **Installation:** ``` renv::install("package_name") ``` installs the specified package and records the version in renv.lock.
 3.  **Snapshot:** ``` renv::snapshot() ``` updates the renv.lock file with the current state of the project's R library.
 4.  **Restore:** ``` renv::restore() ``` installs the packages and versions specified in the renv.lock file, ensuring a consistent environment.

## Documentation
**Purpose:** Provide clear and comprehensive documentation for users and developers.
* **Tools:**
* **roxygen2:** Generates documentation from specially formatted comments in the code (e.g., function descriptions, parameter details).
* **pkgdown:** Creates a static website from the R package's documentation, making it easy to browse and search (optional).

**Workflow:**
1. **Write roxygen2 comments:** Add roxygen2 style comments (lines starting with #') to your R code to document functions, classes, and data.
2. **Generate documentation:**
    ```bash
     # In the R console
    devtools::document()
     ```
3. **Build website (optional):**
   ```bash
   # In the R console
   install.packages("pkgdown") # If you don't have pkgdown
   pkgdown::build_site()
     ```
##   Contributing
Contributions to this project are welcome! If you would like to contribute, please follow these guidelines:
1. Fork the repository on GitHub.
2. Create a new branch for your feature or bug fix:
   ```bash
   git checkout -b feature/your-feature-name
   ```
   or
      ```bash
   git checkout -b bugfix/your-bug-fix
   ```
3. Make your changes and commit them with clear and descriptive commit messages.
4. Write unit tests for your changes using testthat.
5. Ensure your code passes linting with lintr.
6. Style your code with styler.
7. Update the documentation if necessary.
8. Push your branch to your forked repository.
9. Create a pull request to the main branch of the original repository.

## License
This project is licensed under the GNU General Public License v3.0 - see the [LICENSE](LICENSE) file for details.

      
      
   
