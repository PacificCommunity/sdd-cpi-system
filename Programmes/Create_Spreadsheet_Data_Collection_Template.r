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
            
               This_Store <- All_Divisions[(All_Divisions$Price_Source ==  Stores[Each_Store]) & !is.na(All_Divisions$Price_Source),]
               
               This_Period <- max(This_Store$Period) + month(1)
               
               This_Store <- This_Store[This_Store$Period %in% c(max(This_Store$Period),
                                                                 max(This_Store$Period[This_Store$Period != max(This_Store$Period)])),]
                                                                 
               This_Store <- reshape2::dcast(This_Store[!is.na(This_Store$Measured_Price),],
                                             Groups + Division + Class + Subclass + COICOP + Item_nos + Weights + Price_Source ~ Period,
                                             value.var = "Measured_Price")
               This_Store <- This_Store[order(This_Store$Groups, This_Store$Division, This_Store$Class, -This_Store$Weights),]
               
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
               
               current_range <- current_worksheet$Range("A2:K3")
               current_range[["Font"]][["Bold"]] <- TRUE
               current_range[["Font"]][["size"]] <- 14
               current_range[["Font"]][["Color"]] <- '0'

         
            ##
            ##    Align the colomns
            ##
               current_range <- current_worksheet$Range("A1")            
               current_range[["ColumnWidth"]] <- 54.43

               current_range <- current_worksheet$Range("B1")            
               current_range[["ColumnWidth"]] <- 32.71

               current_range <- current_worksheet$Range("C1")            
               current_range[["ColumnWidth"]] <- 40.86
         
               current_range <- current_worksheet$Range("D1")            
               current_range[["ColumnWidth"]] <- 25.86
         
               current_range <- current_worksheet$Range("E1")            
               current_range[["ColumnWidth"]] <- 10.71
         
               current_range <- current_worksheet$Range("F1")            
               current_range[["ColumnWidth"]] <- 13.86
         
               current_range <- current_worksheet$Range("G1:H1")            
               current_range[["ColumnWidth"]] <- 11.29
         
               current_range <- current_worksheet$Range("I1:K1")            
               current_range[["ColumnWidth"]] <- 16.14
                        
               
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
   ##
   ##    Save
   ## 
      current_workbook$SaveAs(FileName = paste0(str_replace_all(getwd(), "\\/", "\\\\"), "\\Data_Output\\Spreadsheet_Example.xlsx"))
      ex$quit()

##
##    And we're done
##
