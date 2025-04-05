#' R/email_class.R # nolint: commented_code_linter.
library(R6)
library(mailR)

# Purpose:
# This file defines the `EmailSender` class, which is responsible for sending validation reports via email.
# It uses the `mailR` package to send emails with attachments.

# Documentation:
# - R6 Classes: https://cran.r-project.org/web/packages/R6/vignettes/Introduction.html
# - mailR: https://cran.r-project.org/web/packages/mailR/index.html

#' @title EmailSender
#' @description Class for sending validation reports via email.
#' Example:
#' ```
#' email_sender <- EmailSender$new()
#' email_sender$send("path/to/report.html", "recipient@example.com")
#' ```
EmailSender <- R6Class("EmailSender",
  public = list(
    #' @description
    #' Send the validation report via email.
    #' @param report_path Path to the HTML report.
    #' @param recipient Recipient email address.
    #' Example:
    #' ```
    #' email_sender <- EmailSender$new()
    #' email_sender$send("path/to/report.html", "recipient@example.com")
    #' ```
    send = function(report_path, recipient = "pinco.pallino@gmail.com") {
      # Ensure the report file exists
      if (!file.exists(report_path)) {
        stop("The specified report file does not exist: ", report_path)
      }

      # Set up email server details (SMTP)
      # Replace with your actual email server configuration
      smtp <- list(
        host.name = "smtp.example.com",
        port = 587,
        user.name = "your_email@example.com",
        passwd = "your_password",
        ssl = TRUE
      )

      # Send the email
      tryCatch({
        send.mail(
          from = "your_email@example.com",
          to = recipient,
          subject = "Data Validation Report",
          body = "Please find the attached data validation report.",
          html = TRUE,  # If sending an HTML report
          attach.files = report_path,
          smtp = smtp,
          authenticate = TRUE,
          send = TRUE
        )
        message("Email sent successfully to: ", recipient)
      }, error = function(e) {
        stop("Failed to send email: ", e$message)
      })
    }
  )
)
