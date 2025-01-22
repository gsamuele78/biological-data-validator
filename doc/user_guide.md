# User Guide

## Introduction

The Biological Environment Data Validation Tool is designed to help you validate and manage data collected from field studies. It provides two interfaces:

1. **Command-Line Interface (CLI):** A text-based interface for interacting with the tool using commands.
2. **Shiny Web Application:** A graphical user interface that allows you to interact with the tool through a web browser.

This guide will walk you through the steps of using both interfaces.

## Installation

Before you can use the tool, you need to install R and the required packages. Follow the instructions in the [Installation](README.md#installation) section of the main `README.md` file.

## Using the Command-Line Interface (CLI)

### Running the CLI

1. Open your terminal or command prompt.
2. Navigate to the project directory:

    ```bash
    cd /path/to/biological-data-validator
    ```

3. Execute the `app_cli.R` script using `Rscript`:

    ```bash
    Rscript app_cli.R [command] [options]
    ```

### CLI Commands

#### Validate Data

This command validates an Excel data file and generates a report.

```bash
Rscript app_cli.R --file <path_to_excel_file> --base_path <output_directory>
```
*   --file or -f: (Required) Path to the Excel data file (e.g., data/sample_data.xlsx).

*   --images or -i: (Optional) Comma-separated paths to image files to be associated with the data.

*   --base_path or -b: (Optional) The base directory where the validation report and any associated data will be saved (default: /default/path).

*   --email or -e: (Optional) Email address to send the validation report to.

*   --database or -d: (Optional) Path to the SQLite database file (default: validation_history.db).

**Example:**
```bash
Rscript app_cli.R --file data/sample_data.xlsx --base_path output_data --email test@example.com
```
#### Search Records
This command searches the database for plot data records based on specified criteria.
```bash
Rscript app_cli.R --search --plot_code <plot_code> --from_date <start_date> --to_date <end_date>
```
*   --search or -s: (Required) Indicates that you want to perform a search.

*   --plot_code or -p: (Optional) The plot code to search for.

*   --from_date: (Optional) The start date for the search (format: YYYY-MM-DD).

*   --to_date: (Optional) The end date for the search (format: YYYY-MM-DD).
**Example:**
```bash
Rscript app_cli.R --search --plot_code Plot1 --from_date 2023-01-01 --to_date 2023-12-31
```
#### Update Record
This command updates an existing record in the database. (Note: Currently, you need to manually update the values in the app_cli.R script.) 

```bash
Rscript app_cli.R --update --update_id <record_id>
```
*   --update or -u: (Required) Indicates that you want to update a record.

*   --update_id: (Required) The ID of the record you want to update.
**Example:**
```bash
Rscript app_cli.R --update --update_id 1
```
#### Delete Record
This command deletes a record from the database.
```bash
Rscript app_cli.R --delete --delete_id <record_id>
```
*   --delete: (Required) Indicates that you want to delete a record.

*   --delete_id: (Required) The ID of the record you want to delete.
**Example:**
```bash
Rscript app_cli.R --delete --delete_id 1
```
#### Get Help
This command displays help information about the available commands and options.
```bash
Rscript app_cli.R --help
```
## Using the Shiny Web Application
## Launching the Shiny App
1. Open R or RStudio in the project directory.
2. Run the run_shiny_app() function (defined in .Rprofile):
```bash
run_shiny_app()
```
This will open the Shiny app in your default web browser.

## Shiny App Features
#### Uploading Data
* **Excel File:** Click the "Choose Excel File" button and select your Excel data file (.xlsx or .xls).

* **Images (Optional):** Click the "Choose Image Files" button and select up to four image files (JPEG or PNG) that you want to associate with the data.

#### Customizing the Output Path
You can change the base path for saving data and reports by entering a new path in the "Customizable Path" field.

#### Validating Data
* Click the "Validate Data" button to start the validation process. The tool will check the data types, apply the validation rules, and generate a report.

#### Viewing and Downloading the Report
* The validation report will be displayed in the main panel of the Shiny app.

* You can download the report as an HTML file by clicking the "Download Report" button.

#### Searching and Filtering Plot History
* **Plot Code:** Enter a plot code in the "Plot Code" field to search for specific plot records.

* **Date Range:** Use the "From Date" and "To Date" fields to filter records based on the sample date.

* Click the "Search" button to apply the filters. The results will be displayed in the "Plot History" table.

#### Editing Records
1. **Load Record:** Enter the ID of the record you want to edit in the "Record ID to Edit" field and click "Load Record." The record's data will be loaded into the edit fields.
2. **Modify Fields:** Change the values in the fields as needed.
3. **Update Record:** Click the "Update Record" button to save the changes to the database.

#### Deleting Records
1. **Enter ID:** Enter the ID of the record you want to delete in the "Record ID to Delete" field.
2. **Delete:** Click the "Delete Record" button.
3. **Confirm:** Confirm the deletion when prompted.

#### Troubleshooting
* If you encounter any errors, check the R console for error messages.
* Make sure that all required packages are installed (renv::restore() should handle this).
* If you are using the CLI, ensure that you are providing the correct command-line arguments.
* If you are using the Shiny app, make sure that the necessary files (Excel data, images) are being uploaded correctly.
* If you find a bug or have a suggestion for improvement, please open an issue on the project's GitHub repository.
