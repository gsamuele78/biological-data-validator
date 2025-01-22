# R/email_class.R
library(R6)
library(mailR)

#' EmailSender class for sending validation reports via email
EmailSender <- R6Class("EmailSender",
  public = list(
    #' @description
    #' Send the validation report via email
    #' @param report_path Path to the HTML report
    #' @param recipient Recipient email address
    send = function(report_path, recipient = "pinco.pallino@gmail.com") {
      # You'll need to set up your email server details here (SMTP)
      send.mail(from = "<your_email@example.com>",
                to = recipient,
                subject = "Data Validation Report",
                body = "Please find the attached data validation report.",
                html = TRUE, # If sending HTML report
                attach.files = report_path,
                smtp = list(host.name = "smtp.gmail.com", # Example: Gmail SMTP
                            port = 587,
                            user.name = "<your_email@example.com>",
                            passwd = "<your_password>",
                            ssl = TRUE),
                authenticate = TRUE,
                send = TRUE)
    }
  )
)
