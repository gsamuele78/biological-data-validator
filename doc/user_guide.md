# User Guide for Biological Environment Data Validation Tool

This guide provides instructions on how to use the Biological Environment Data Validation Tool.

## Table of Contents

1. [Introduction](#introduction)
2. [Installation](#installation)
3. [Running the Application](#running-the-application)
4. [Usage](#usage)
    *   [Uploading Data](#uploading-data)
    *   [Validation](#validation)
    *   [Report Generation](#report-generation)
    *   [Data Management](#data-management)
5. [Troubleshooting](#troubleshooting)

## 1. Introduction <a name="introduction"></a>

The Biological Environment Data Validation Tool is an R Shiny application designed to validate field-collected data for biological environment studies. It ensures data quality by checking data types, enforcing validation rules, and generating detailed reports.

## 2. Installation <a name="installation"></a>

Before running the application, ensure you have R and RStudio installed. Then, follow these steps:

1. **Clone the Repository:**

    ```bash
    git clone <your_github_repo_url>
    ```

2. **Install Required Packages:**

    Open RStudio and run the following command in the console:

    ```R
    install.packages(c("openxlsx", "readxl", "lubridate", "knitr", "rmarkdown", "mailR", "DBI", "RSQLite", "shiny", "shinyFiles", "DT"))
    ```

## 3. Running the Application <a name="running-the-application"></a>

### RStudio Desktop

1. Open the  `biological-data-validator.Rproj`  file in RStudio.
2. Open  `app.R`.
3. Click the "Run App" button in the top-right corner.

### RStudio Server

1. In RStudio Server, go to "New Project" -> "Version Control" -> "Git" and clone the repository using its URL.
2. Open  `app.R`.
3. Click the "Run App" button.

## 4. Usage <a name="usage"></a>

### 4.1 Uploading Data <a name="uploading-data"></a>

*   **Excel File:** Click the "Choose Excel File" button to upload your Excel data file (`.xlsx`  or  `.xls`). The file should have two sheets named "Sheet1" and "Sheet2" with the specified data fields.
*   **Images (Optional):** Click the "Choose Image Files" button to upload up to four JPEG images.
*   **Custom Path (Optional):** Modify the "Customizable Path" to specify where data and reports will be saved.

### 4.2 Validation <a name="validation"></a>

*   Click the "Validate Data" button to start the validation process.
*   The application will check the data types and apply the validation rules defined in the  `validation_classes.R`  file.
*   The results will be displayed in the "Validation Output" tab, indicating any errors or warnings found.

### 4.3 Report Generation <a name="report-generation"></a>

*   If no errors are found, a validation report will be generated automatically.
*   Click the "Download Report" button to download the report as an HTML file.
*   The report includes a summary of the validation results, error levels (Warning, Error), and details of any issues found.

### 4.4 Data Management <a name="data-management"></a>

*   **Search and Filter:** Use the "Plot Code", "From Date", and "To Date" fields to search and filter historical data. Click the "Search" button to apply the filters.
*   **Edit Records:**
    1. Enter the ID of the record to edit in the "Record ID to Edit" field.
    2. Click "Load Record".
    3. Modify the fields.
    4. Click "Update Record".
*   **Delete Records:**
    1. Enter the ID of the record to delete in the "Record ID to Delete" field.
    2. Click "Delete Record".
    3. Confirm the deletion.

## 5. Troubleshooting <a name="troubleshooting"></a>

*   **Error: "Package not found"**: Ensure all required R packages are installed (see the Installation section).
*   **Error: "Report not generated"**: Check the "Validation Output" tab for any errors that might have prevented report generation.
*   **Application not responding**: Try refreshing the web page or restarting the application in RStudio.

If you encounter any other issues, please refer to the R console for detailed error messages or contact the project developers for assistance.
