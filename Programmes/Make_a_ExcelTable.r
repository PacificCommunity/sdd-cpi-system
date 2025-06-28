##
##    Programme:  Make_a_ExcelTable.r
##
##    Objective:  New Zealand and Australia are together.  Now graph them
##
##                Meet the Excel Object Model: https://docs.microsoft.com/en-us/office/vba/api/overview/excel/object-model
##              
##    Author:    James Hogan, NZIER, 8 April 2019
##
   rm(list=ls(all=TRUE))
   
   load("data_intermediate/Australia_and_NZ_Expenditure.rda")
   source("R/themes.r")
   source("R/functions.r")
   
   ##
   ##    Initialise some RDCOMClient parameters
   ##
      ex <- COMCreate("Excel.Application")
      ex[["Visible"]] <- TRUE
      ex[["DisplayAlerts"]] <- FALSE
      ex[["AskToUpdateLinks"]] <- FALSE
      Workbook <- ex[["workbooks"]]
      current_workbook <- Workbook$Add()
      ##
      ##    Make a new worksheet
      ##
      current_worksheet <- current_workbook$Worksheets(1)
      current_worksheet[["Name"]] <- "Sheet of Doom"
      
      Australian_Cities  <- unique(Australia_and_NZ_Expenditure$variable[Australia_and_NZ_Expenditure$Country == "Australia"])
      New_Zealand_Cities <- unique(Australia_and_NZ_Expenditure$variable[Australia_and_NZ_Expenditure$Country == "New Zealand"])
      Measures           <- unique(Australia_and_NZ_Expenditure$Measure[!(Australia_and_NZ_Expenditure$Measure %in% c("UNKNOWN", "Sales, trade-ins and refunds"))])
      Cities             <- c(New_Zealand_Cities, Australian_Cities)
      Countries          <- c("New Zealand", rep("", times = (length(New_Zealand_Cities)-1)), 
                              "Australia",   rep("", times = (length(Australian_Cities)-1)))
      ##
      ##    Headings
      ##
      current_range <- current_worksheet$Range("A1")
      current_range[["Value"]] <- "Household Expenditure - New Zealand versus Australian Cities"
      current_range[["Font"]][["size"]] <- 20
      current_range[["RowHeight"]] <- 30
      current_range[["WrapText"]] <- FALSE
      
      for(col in 1:length(Countries))
         {
            current_range <- current_worksheet$Cells(2, (col+1))
            current_range[["Value"]] <- Countries[col]
            
            current_range <- current_worksheet$Cells(3, (col+1))
            current_range[["Value"]] <- Cities[col]
         }
      ##
      ##    Rows
      ##
      for(row in 1:length(Measures))
         {
            current_range <- current_worksheet$Cells((row+3), 1)
            current_range[["Value"]] <- Measures[row]
         }
       
      ##
      ##   Drop some data
      ##
      for(row in 1:length(Measures))
         {
         for(col in 1:length(Cities))
            {         
               current_range <- current_worksheet$Cells((row+3), (col+1))
               current_range[["Value"]] <- Australia_and_NZ_Expenditure$Proportion[((Australia_and_NZ_Expenditure$Measure == Measures[row]) &
                                                                                    (Australia_and_NZ_Expenditure$variable == Cities[col]))]
            }
         }
      ##
      ##   Formating
      ##
         ##
         ##    Turn to percentages
         ## 
         TopRange    <- current_worksheet$Cells(4, 2)
         BottomRange <- current_worksheet$Cells((3+length(Measures)), (1+length(Cities)))
         
         current_range <- current_worksheet$Range(TopRange, BottomRange)
         current_range[["NumberFormat"]] <- "0.0%"

         ##
         ##    Bold the countries and merge the cells
         ## 
         TopRange    <- current_worksheet$Cells(2, 2)
         BottomRange <- current_worksheet$Cells(2, (1+length(Cities)))
         
         current_range <- current_worksheet$Range(TopRange, BottomRange)
         current_range[["Font"]][["Bold"]] <- TRUE
         current_range[["Font"]][["size"]] <- 14

         TopRange    <- current_worksheet$Cells(2, 2)
         BottomRange <- current_worksheet$Cells(2, 7)
         current_range <- current_worksheet$Range(TopRange, BottomRange)
         current_range[["MergeCells"]] <- TRUE

         TopRange    <- current_worksheet$Cells(2, 8)
         BottomRange <- current_worksheet$Cells(2, 15)
         current_range <- current_worksheet$Range(TopRange, BottomRange)
         current_range[["MergeCells"]] <- TRUE
         
         ##
         ##    Italise and wrap the cities
         ## 
         TopRange    <- current_worksheet$Cells(3, 2)
         BottomRange <- current_worksheet$Cells(3, (1+length(Cities)))
         
         current_range <- current_worksheet$Range(TopRange, BottomRange)
         current_range[["Font"]][["Italic"]] <- TRUE
         current_range[["WrapText"]] <- TRUE

         ##
         ##    resize the columns
         ## 
         
         current_range[["ColumnWidth"]] <- 12
         
         TopRange    <- current_worksheet$Cells(4, 1)
         BottomRange <- current_worksheet$Cells(100, 1)
         current_range <- current_worksheet$Range(TopRange, BottomRange)
         current_range[["Font"]][["size"]] <- 10
         current_range[["ColumnWidth"]] <- 30
         current_range[["WrapText"]] <- TRUE
         
         ##
         ##    Throw some borders around everything
         ## 
         
         TopRange    <- current_worksheet$Cells(2, 2)
         BottomRange <- current_worksheet$Cells((3+length(Measures)), (1+length(Cities)))
         current_range <- current_worksheet$Range(TopRange, BottomRange)
         current_range$BorderAround(LineStyle = 1,
                                    Weight    = 2)

         TopRange    <- current_worksheet$Cells(2, 1)
         BottomRange <- current_worksheet$Cells(3, (1+length(Cities)))
         current_range <- current_worksheet$Range(TopRange, BottomRange)
         current_range$BorderAround(LineStyle = 1,
                                    Weight    = 2)


         TopRange    <- current_worksheet$Cells(2, 1)
         BottomRange <- current_worksheet$Cells((3+length(Measures)), (1+length(New_Zealand_Cities)))
         current_range <- current_worksheet$Range(TopRange, BottomRange)
         current_range$BorderAround(LineStyle = 1,
                                    Weight    = 2)

         TopRange    <- current_worksheet$Cells(2, 1)
         BottomRange <- current_worksheet$Cells((3+length(Measures)), (1+length(Cities)))
         current_range <- current_worksheet$Range(TopRange, BottomRange)
         
         current_range$BorderAround(LineStyle = 1,
                                    Weight    = -4138)

         TopRange    <- current_worksheet$Cells(2, 2)
         BottomRange <- current_worksheet$Cells((3+length(Measures)), (1+length(Cities)))
         current_range <- current_worksheet$Range(TopRange, BottomRange)
         Style <- current_range$Style()
         Style[["HorizontalAlignment"]] <- -4108
         Style[["VerticalAlignment"]]   <- -4108
         
         current_range <- current_worksheet$Range("B2")
         Style <- current_range$Style()
         Style[["HorizontalAlignment"]] <- -4108

   ##
   ##    Add some notes
   ## 
      TopRange    <- current_worksheet$Cells((4+length(Measures)), 1)
      BottomRange <- current_worksheet$Cells((4+length(Measures)), 1)
      current_range <- current_worksheet$Range(TopRange, BottomRange)
      current_range[["Value"]] <- "Notes:"
      current_range[["Font"]][["size"]] <- 10
      current_range[["Font"]][["Italic"]] <- TRUE
      
      TopRange    <- current_worksheet$Cells((5+length(Measures)), 1)
      BottomRange <- current_worksheet$Cells((5+length(Measures)), 1)
      current_range <- current_worksheet$Range(TopRange, BottomRange)
      current_range[["Value"]] <- "(1) Analysis created by the Quant Team - NZIER"
      current_range[["Font"]][["size"]] <- 10
      current_range[["Font"]][["Italic"]] <- FALSE
      current_range[["WrapText"]] <- FALSE
      
      TopRange    <- current_worksheet$Cells((6+length(Measures)), 1)
      BottomRange <- current_worksheet$Cells((6+length(Measures)), 1)
      current_range <- current_worksheet$Range(TopRange, BottomRange)
      current_range[["Value"]] <- "(2) Data sourced from the Australian Bureau of Statistics and Statistics New Zealand"
      current_range[["Font"]][["size"]] <- 10
      current_range[["Font"]][["Italic"]] <- FALSE
      current_range[["WrapText"]] <- FALSE
      
   ##
   ##    Resize page and make it printer ready with NZIER stuff
   ## 
      PageSetup <- current_worksheet$PageSetup()
      PageSetup[["Orientation"]] <- 2

      TopRange    <- current_worksheet$Cells(1, 1)
      BottomRange <- current_worksheet$Cells((7+length(Measures)), (1+length(Cities)))
      current_range <- current_worksheet$Range(TopRange, BottomRange)
      current_range[["Font"]][["color"]] <- 0

      PageSetup[["PrintArea"]] <- "$A$1:$O$19"
      PageSetup[["Zoom"]] <- FALSE
      PageSetup[["FitToPagesWide"]] <- 1
      PageSetup[["FitToPagesTall"]] <- 1
      PageSetup[["CenterHeader"]] <- "An Example For How to Create a Spreadsheet Programmically in R"
      PageSetup[["CenterFooter"]] <- "Another Superb Output from the Quant Team"
      
      LogoGraphic <- PageSetup$RightFooterPicture()
      LogoGraphic[["FileName"]] <- paste0(str_replace_all(getwd(), "\\/", "\\\\"), "\\R\\NZIER.png")
      PageSetup[["RightFooter"]] <- "&G"

   ##
   ##    Save
   ## 
      current_workbook$SaveAs(FileName = paste0(str_replace_all(getwd(), "\\/", "\\\\"), "\\Output\\Spreadsheet_Example.xlsx"))
  ex$quit()
##
##    And we're done
##
