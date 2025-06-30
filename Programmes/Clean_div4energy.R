##
##    Programme:  Clean_div4energy.r
##
##    Objective:  What is this programme designed to do?
##
##    Author:      Geoffrey Wong, Analysis & Insight Team, Statistics for Development Division, 16 June 2025
##    Peer Review: James Hogan, Analysis & Insight Team, Statistics for Development Division, 16 June 2025
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))
   ##
   ##    Load some generic functions or colour palattes, depending on what you're doing.
   ##

   ##
   ##    Load data from somewhere
   ##
      load('Data_Intermediate/Regimen.rda')

      load("Data_Intermediate/RAWDATA_Div 4,5 energy & HHDXXCPI_calculation_Xmas_Test_Workbook_2025.rda")
      Div4Energy <- `RAWDATA_Div 4,5 energy & HHDXXCPI_calculation_Xmas_Test_Workbook_2025`
   ##
   ## Step 1: Fill in the blanks
   ## 
      Div4Energy$Subclass = ""
      Div4Energy$Code     = ""
      Div4Energy$Class = ""
      
      for (i in 1:nrow(Div4Energy)) 
      {
          Div4Energy$Subclass[i] <- ifelse(str_detect(Div4Energy$V1[i],"\\."),Div4Energy$V2[i],"") 
          Div4Energy$Code[i]     <- ifelse(str_detect(Div4Energy$V1[i],"\\."),Div4Energy$V1[i],"") 
          Div4Energy$Class[i]    <- ifelse(str_detect(Div4Energy$V1[i],"\\."),
                                           ifelse(Div4Energy$V2[(i-1)] == "", Div4Energy$V2[(i-2)], Div4Energy$V2[(i-1)]),"")
          Div4Energy$Class[i]    <- ifelse(Div4Energy$Class[i]=="Geomean", "",Div4Energy$Class[i])
      }
      
      for (i in 2:nrow(Div4Energy)) 
      {
        Div4Energy$Subclass[i] <- ifelse((Div4Energy$Subclass[i] == "") & (Div4Energy$Subclass[(i-1)] != ""),
                                        Div4Energy$Subclass[(i-1)],
                                        Div4Energy$Subclass[i]) 
        
        Div4Energy$Code[i]     <- ifelse((Div4Energy$Code[i] == "") & (Div4Energy$Code[(i-1)] != ""),
                                       Div4Energy$Code[(i-1)],
                                       Div4Energy$Code[i]) 
              
        Div4Energy$Class[i]    <- ifelse((Div4Energy$Class[i] == "") & (Div4Energy$Class[(i-1)] != ""),
                                       Div4Energy$Class[(i-1)],
                                       Div4Energy$Class[i])       
      }

   ##
   ## Step 2: Rename the Columns
   ##
      names(Div4Energy) <- c("ID", "Price_Source", Div4Energy[4,3:14], "Subclass", "Code", "Class")
   
   ##
   ## Step 3: Make Long
   ##
      Div4Energy <- reshape2::melt(Div4Energy,
                                 id.vars = c("ID", "Price_Source", "Subclass", "Code", "Class"),
                                 value.name = "Measured_Price",
                                 variable.name = "Period")
      Div4Energy <- Div4Energy[,]
      Div4Energy <- Div4Energy[Div4Energy$ID != "",]
      Div4Energy <- Div4Energy[Div4Energy$Price_Source != "",]
      Div4Energy <- Div4Energy[Div4Energy$Price_Source != "Geomean",]
      Div4Energy <- Div4Energy[Div4Energy$Subclass  != "",]
      Div4Energy <- Div4Energy[Div4Energy$Price_Source != Div4Energy$Subclass,]
      
      Categories <- unique(Div4Energy$Subclass)
      
      for(i in 1:nrow(Div4Energy))
      {
        Div4Energy$Code[i] <- paste0(Div4Energy$Code[i], ".", which(Categories == Div4Energy$Subclass[i]), ".", Div4Energy$ID[i])
        
      }
   ##
   ##    Clean up little odd ball stuff
   ##
      Div4Energy$Price_Source <- str_squish(Div4Energy$Price_Source)
      Div4Energy$Subclass  <- str_squish(Div4Energy$Subclass)
      Div4Energy$Code      <- str_squish(Div4Energy$Code)
      Div4Energy$Class     <- str_squish(Div4Energy$Class)

      Div4Energy$Period <- as.Date(paste0("15-",Div4Energy$Period ), "%d-%b-%y")
      Div4Energy$Measured_Price  <- as.numeric(Div4Energy$Measured_Price)

   ##
   ## Make sure this detail maps to the Regimen
   ##
      Regimen_Subclass <- unique(Regimen$Subclass[Regimen$Groups %in% c("04 Housing, water, electricty and other fuels", "05 Furnishings, household equipment & routine maintainance")])
      Regimen_Class    <- unique(Regimen$Class[Regimen$Groups %in% c("04 Housing, water, electricty and other fuels", "05 Furnishings, household equipment & routine maintainance")])
      
      Tab_Subclass     <- unique(Div4Energy$Subclass)
      Tab_Class        <- unique(Div4Energy$Class)

      Subclasses_Not_In_Collection <- Regimen_Subclass[!(Regimen_Subclass %in% Tab_Subclass)]
      Misspelt_Subclasses          <- Tab_Subclass[!(Tab_Subclass %in% Regimen_Subclass)]
      
      Class_Not_In_Collection <- Regimen_Class[!(Regimen_Class %in% Tab_Class)]
      Misspelt_Class          <- Tab_Class[!(Tab_Class %in% Regimen_Class)]

   ##
   ## Save files our produce some final output of something
   ##
      save(Div4Energy, file = 'Data_Intermediate/Div4Energy.rda')

##
##    And we're done
##
