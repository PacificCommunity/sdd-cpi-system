##
##    Programme:  Create_Spreadsheet_Data_Collection_Template.r
##
##    Objective:  What is this programme designed to do?
##
##    Author:      James Hogan, Analysis & Insight Team, Statistics for Development Division, 16 June 2025
##    Peer Review: Geoffrey Wong, Analysis & Insight Team, Statistics for Development Division, 16 June 2025
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))
   ##
   ##    Load data from somewhere
   ##
      load('Data_Output/All_Divisions.rda')

   ##
   ##    Initialise some RDCOMClient parameters -
   ##
      ex <- COMCreate("Excel.Application")
      ex[["Visible"]] <- TRUE
      ex[["DisplayAlerts"]] <- FALSE
      ex[["AskToUpdateLinks"]] <- FALSE
      Workbook <- ex[["workbooks"]]

   ##
   ##    We could cut this different ways. Lets do it be all the things sourced from each
   ##       individual Price_Source - these can be the spreadsheet tabs. One spreadsheet for
   ##       each month
   ##
      Stores <- unique(All_Divisions$Price_Source)
      ##
      ##    Make a new workbook for this quarter
      ##
         current_workbook <- Workbook$Add()
         Sheets           <- current_workbook$Sheets()
      
         for(Each_Store in 1:length(Stores)) current_workbook$Sheets()$Add()
         
         for(Each_Store in 1:length(Stores)) 
         {
            current_worksheet           <- Sheets[[Each_Store]]
            current_worksheet[["Name"]] <- Stores[Each_Store]
            
            ##
            ##    Fill it up with data
            ##
            
               This_Store <- All_Divisions[All_Divisions$Price_Source ==  Stores[Each_Store],]
               
               This_Period <- max(This_Store$Period) + month(1)
               
               This_Store <- This_Store[This_Store$Period %in% c(max(This_Store$Period),
                                                                 max(This_Store$Period[This_Store$Period != max(This_Store$Period)])),]
                                                                 
               This_Store <- reshape2::dcast(This_Store,
                                             Division + Groups + Class + Subclass + COICOP + Item_nos + Weights + Price_Source ~ Period,
                                             value.var = "Measured_Price")
            ##
            ##    Headings
            ##
               current_range <- current_worksheet$Range("A1")
               current_range[["Value"]] <- "Kiribati Consumer Price Data Collection System"
               current_range[["Font"]][["size"]]  <- 20
               current_range[["Font"]][["Color"]] <- '0'
               current_range[["RowHeight"]] <- 30
               current_range[["WrapText"]] <- FALSE

               current_range <- current_worksheet$Cells(2, length(This_Store)-1)
               current_range[["Value"]] <- "Previous Periods"
               current_range <- current_worksheet$Cells(2, length(This_Store)+1)
               current_range[["Value"]] <- "Current Period"
               
               current_range <- current_worksheet$Range("I2:J2")
               current_range[["MergeCells"]] <- TRUE
               current_range[["HorizontalAlignment"]] <- TRUE
               
               
               for(col in 1:length(This_Store))
                  {
                     current_range <- current_worksheet$Cells(3, col)
                     current_range[["Value"]] <- names(This_Store)[col]
                  }
               current_range <- current_worksheet$Cells(3, (length(This_Store)+1))
               current_range[["Value"]] <- as.character(This_Period)
            
            ##
            ##    Fill it up with data
            ##
               for(row in 1:nrow(This_Store))
                  {
                     for(col in 1:length(This_Store))
                        {         
                           current_range <- current_worksheet$Cells((row+3), (col+0))
                           current_range[["Value"]] <- This_Store[row, col]
                        }
                  }               
               
         }      
         
         
         current_workbook$Sheets$Add()
      
current_workbook$Sheets()$count()
current_workbook$Sheets()$Add()
      
 Sheets <- current_workbook$Sheets()
 Sheets$Add()
              
      
       tic(paste("Reading Worksheet", Directories$Details[File]))
       current_file <- Workbook$Open(Directories$Details[File])
         for(j in 1:current_file$worksheets()$count())
         {
            Name <- current_file$worksheets(j)$name()
            ##
            ##  RDCOMClient keeps breaking, so we're going save each tab as CSV and read it in
            ##
             Current_Tab <- current_file$Worksheets(as.character(Name))
             if(Current_Tab[["ProtectionMode"]] == TRUE){Current_Tab[["ProtectionMode"]] <- FALSE}
#             Current_Tab$unprotect()
             unlink("Data_Intermediate/*.csv")
             result = tryCatch({ 
                                 ## Current_Tab$Range("A1:IV60000")$RemoveSubtotal()
                                 Current_Tab$SaveAs(FileName =  paste0(str_replace_all(getwd(), "\\/", "\\\\"), "\\Data_Intermediate\\test.csv"), FileFormat = 6)
                                 ##
                                 ##     Stick everything back in an appropriately named data frame and save
                                 ##
                                  X <- read.csv(paste0(str_replace_all(getwd(), "\\/", "\\\\\\\\"), "\\\\Data_Intermediate\\\\test.csv"), colClasses = c("character"),header = FALSE)
                                  X[] <- lapply(X, as.character)
                                  assign(paste0("RAWDATA_", Name, "XX", Directories$Save_Name[File]), X )
                                  save(list = paste0("RAWDATA_", Name, "XX", Directories$Save_Name[File]), 
                                       file = paste0("Data_Intermediate/RAWDATA_", Name, "XX", Directories$Save_Name[File], ".rda"))
                                  rm(list=paste0("RAWDATA_", Name, "XX", Directories$Save_Name[File]))
                               }, warning = function(w) {
                               }, error = function(e) {
                                   DidntRead <- rbind.fill( DidntRead , data.frame(File = Directories$Details[File],
                                                                                    Tab = Name))
                               }, finally = {
                               })  
         }
         ##
         ##     Close the RDCOMClient spreadsheet
         ##
         current_file$Close()
      toc()
     }
  ex$quit()
  unlink("Data_Intermediate/test.csv")

##
##    And we're done
##
