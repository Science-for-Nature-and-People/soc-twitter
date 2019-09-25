# Attempting to send email notification when automat.R twitter API has an error

# mailR requires rJava. Follow answer on (https://stackoverflow.com/questions/50016207/unable-to-send-email-using-mailr-package) 
# to solve issues with mailR not loading properly and send.mail() not working. 
library(mailR)

send.mail(from = "sender@gmail.com",
          to = "jeremyknox@ucsb.edu",
          subject = "Subject of the email",
          body = "It works.",
          smtp = list(host.name = "aspmx.l.google.com", port = 25),
          authenticate = FALSE,
          send = TRUE)
