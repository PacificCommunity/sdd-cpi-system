##    Programme:  Make_Some_Emails.r
##
##    Objective:  
##    Make an email using Outlook Object Model  
##     https://msdn.microsoft.com/EN-US/library/office/ff869635.aspx
##     https://msdn.microsoft.com/en-us/library/office/ff869291.aspx
##               
##
   rm(list=ls(all=TRUE))
   source("R/themes.r")
   source("R/functions.r")
   

##
##    Make a dummy dataset which contains names, emails addresses and potentially anything else
##       we want to customise for the recipient - like a link to their specific unique files
##       which we're returning them as attachments.
##
   Sender_details_email <- data.frame(Name          = c("James Hogan",
                                                        'Dion Gamperle' ,
                                                        'Prince Siddharth' ,
                                                        'Michael Bealing' ,
                                                        'Laetitia Leroy' ,
                                                        'Eugene Isack' ,
                                                        'Mieke Welvaert' ,
                                                        'Milad Maralani' ,
                                                        'Eilya Torshizian', 
                                                        'Derek Gill', 
                                                        'John Yeabsley'),
                                      Email_Address = c("james.hogan@nzier.org.nz",
                                                        'dion.gamperle@nzier.org.nz' ,
                                                        'prince.siddharth@nzier.org.nz' ,
                                                        'michael.bealing@nzier.org.nz' ,
                                                        'laetitia.leroy@nzier.org.nz' ,
                                                        'eugene.isack@nzier.org.nz' ,
                                                        'mieke.welvaert@nzier.org.nz' ,
                                                        'milad.maralani@nzier.org.nz' ,
                                                        'eilya.torshizian@nzier.org.nz', 
                                                        'derek.gill@nzier.org.nz', 
                                                        'john.yeabsley@nzier.org.nz'),  stringsAsFactors = FALSE)

   ##
   ##    Initialise some RDCOMClient parameters
   ##

      #Create Microsoft dates
      Microsoft_Dates <- ymd("1899-12-30")
         
      #Create instance of Microsoft Outlook Application
      OutApp <- COMCreate("Outlook.Application")

      NameSpace <- OutApp$GetNameSpace("MAPI")

      #---Loop through all customers with an email address, create
      #---new email, attach their Survey xlsx file, and send to them

      for (i in 1:nrow(Sender_details_email))
         {
              
            Email_Address <- Sender_details_email[i,"Email_Address"]
              
            New_Email <- OutApp$CreateItem(0)
            New_Email$Display()
            New_Email[["To"]] <- Email_Address 
            New_Email[["Subject"]] <- "Automating Excel, Word and Outlook through R"
New_Email[["Body"]] <- paste("Hey", str_split_fixed(Sender_details_email$Name[i]," ",2)[,1],
",

So, here's a complete end-to-end productionised version of an R-based data reporting system which:

   (1)  Goes off to the Australian Bureau of Statistics and pulls down the latest version of their Household Expenditure Accounts and saves the information in the 'data_raw' directory.

   (2)  Reads in all the excel spreadsheets in 'data_raw' using RDCOMClient, and saves each tab as its own data frame in the 'data_intermediate' directory.  Unfortunately, because Statisics New Zealand has such rubbish systems, I had to pull New Zealand's Household Expendure Survey data manually out of NZ.Stat and save it in the data_raw directory.  But the code hoovers it up and saves it in the 'data_intermediate' directory.

   (3)  Runs some code which puts the NZ HES data in a 'Tidy' format, a la Hadley Wickham sense (http://vita.had.co.nz/papers/tidy-data.html)

   (4)  The next step is to read both the Australian City weekly expenditure and NZ HES by city, make them comparable to each other, and the connect them together so they can be compared.  This is the step where, in a more full system, we can go to down with the econometrics, seasonal adjustment or whatever we want to do to add value to the data.

   (5)  After we've added value, I then get R to:
      (i)   Make a picture (see attached).
      (ii)  Spark up Excel and use RDCOMClient to output the processed data from step 4 and make a printer-ready excel spreadsheet (see attached).  Go on, print it out.
      (iii) Spark up Word to output some text (based on my Housing Insight), and inset a graphic.  Word turns out to be more difficult than Excel to control.  Ideally, this is something which MarkDown can do better than RDCOMClient.

      Everything gets saved in the right place in a synced up directory with Sharepoint, so its automatically archived.

   (6)  Last, I get R to automate the sending a personalised email to you all, complete with including the above attachments.  In the future, these could be all of our members, where we return something back to them, and make it feel like, to them, we're haveing a personalised conversation with every single one of them, putting their unique characterised in context to our aggregated or modelled results.  For example, their QSBO submission in context to their industry, or thier region, or whatever, and return it back to them.

This is the bit where the smarts of modular programming really come into it.  For example, in the step 4 I've described above, we really can go to town in the modelling, and send their data back to them in a personalised email... which may also have some marketing... ie: 'We see you're in retail.  Did you know.. .blah blah blah?  See our insight or our latest puiblication blah blah blah'

Finally, this code it available for all of you to copy and adapt: 
https://nzier.sharepoint.com/:f:/g/Evj3w1md2-hNrWaKpx5q3OgBLp1-W57KkifS7Jnx7sC00A?e=6X5XPj

Go off and make beautiful things with it :)

Derek and John:  Hopefully, this doesn't look too much like spam for you.  But here's some new functionality which we can use to maintain a 'personal relationship' through connecting our members and customers to economic modelling and data, enabling them to receive individually tailored analysis through an efficient processing and communication channel.



Keep it real,
James
")
            Excel_file <- paste0(str_replace_all(getwd(), "\\/", "\\\\"), "\\Output\\Spreadsheet_Example.xlsx")
            Word_file  <- paste0(str_replace_all(getwd(), "\\/", "\\\\"), "\\Output\\Word_Example.doc")
            Graphic    <- paste0(str_replace_all(getwd(), "\\/", "\\\\"), "\\Graphics\\WeeklySpend.png")
            New_Email[["Attachments"]]$Add(Excel_file)
            New_Email[["Attachments"]]$Add(Word_file)
            New_Email[["Attachments"]]$Add(Graphic)
            New_Email$Send()
      }

      #OutApp$Quit()

