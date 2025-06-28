##    Programme:  Web_Data_Grab.R.r
##
##    Objective:  This code it going to go off to the ABS and grab their household expenditure data
##
##    Author:     James Hogan, Sector Trends, 2 November 2015
##

   Base_URL <- "http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/6530.02015-16?OpenDocument"

   ##
   ##    Open the URL and find the Price Index data page
   ##
      Web_Page <- getURL(Base_URL)
      Links <- getHTMLLinks(Web_Page)
      
      Links <- data.frame(Links = paste0("http://www.abs.gov.au", Links[str_detect(Links, "subscriber.nsf")]))

   ##
   ##    Delete all of the pre-existing files, download the new files, open and read them into R
   ##
   unlink("data_raw/*")         
   for(i in 1:nrow(Links))
      {
      download.file(url = str_replace_all(as.character(Links$Links[i]), " ", "\\%20"), 
                    mode = "wb",
                    destfile = paste0("data_raw/", str_split(str_replace_all(as.character(Links$Links[i]), " ", "\\%20"),"&")[[1]][2]))     
      }
   ##
   ##    Ditch the zipped stuff
   ##
   unlink("data_raw/*.zip")  
   
##
##    Done
##
