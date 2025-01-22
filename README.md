# Biological Environment Data Validation Tool

## Description

This R project validates data collected in the field for biological environment studies. It checks data type consistency, adherence to specific validation rules, generates reports, and manages historical data. The project supports both a Command-Line Interface (CLI) and a Shiny web application interface.

## Installation

1. **Install R:** Download and install the latest version of R from [https://www.r-project.org/](https://www.r-project.org/).
2. **Clone the Repository:**

    ```bash
    git clone <your_github_repo_url>
    cd biological-data-validator
    ```

3. **Install Dependencies:**

    ```R
    # (Open R console in the project directory)
    install.packages("renv")  # If you don't have renv
    renv::restore()
    ```

## Usage

### Choosing the Interface

This project offers two interfaces:

1. **Command-Line Interface (CLI):** For users who prefer the terminal or need to automate the validation process.
2. **Shiny Web Application:** For a more interactive, graphical user interface.

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

1. **Open RStudio** or start an R session in the project directory.
2. **Run the `run_shiny_app()` function** defined in the `.Rprofile` file:

    ```R
    run_shiny_app()
    ```

    This will launch the Shiny web application in your default browser.

### CLI Usage

The CLI is the main interface for interacting with the tool in command-line mode. Here are the commands:

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

### Project Maintenance

**Linting:** Run lintr::lint_pkg() in the R console to check for style and syntax issues.

**Styling:** Run styler::style_pkg() to automatically format code according to the style guide.

**Testing:** Run testthat::test_dir("tests/testthat") to execute unit tests.

**CI (GitHub Actions):** Linting, styling, and testing are automatically performed on pushes and pull requests to the main branch.

## License
This project is licensed under the GNU General Public License v3.0 - see the [LICENSE](LICENSE) file for details.
