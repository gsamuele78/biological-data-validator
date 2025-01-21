# Biological Environment Data Validation Tool

## Description

This R Shiny application validates data collected in the field for biological environment studies. It checks for data type consistency, adherence to specific validation rules, generates reports, and manages historical data with search, filter, update, and delete capabilities.

## Installation

1. **Install R and RStudio:** Download and install the latest versions of R and RStudio from their respective websites.
2. **Clone the Repository:**

    ```bash
    git clone https://github.com/gsamuele78/biological-data-validator.git
    ```

3. **Install Required Packages:**

    ```R
    install.packages(c("openxlsx", "readxl", "lubridate", "knitr", "rmarkdown", "mailR", "DBI", "RSQLite", "shiny", "shinyFiles", "DT"))
    ```

## Running the Application

### RStudio Desktop

1. Open the `biological-data-validator.Rproj` file in RStudio.
2. Open `app.R`.
3. Click the "Run App" button in the top-right corner.

### RStudio Server

1. In RStudio Server, go to "New Project" -> "Version Control" -> "Git" and clone the repository using its URL.
2. Open `app.R`.
3. Click the "Run App" button.

## Usage

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

## License

This project is licensed under the GNU General Public License v3.0 - see the [LICENSE](LICENSE) file for details.
