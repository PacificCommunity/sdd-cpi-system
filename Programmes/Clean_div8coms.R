##
##    Programme:  Clean_div8coms.r
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

      load("Data_Intermediate/RAWDATA_Div 8, 9 Coms & RecreationXXCPI_calculation_Xmas_Test_Workbook_2025.rda")
      Div8Coms <- `RAWDATA_Div 8, 9 Coms & RecreationXXCPI_calculation_Xmas_Test_Workbook_2025`
      
   ##
   ## Step 1: Fill in the blanks
   ## 
      Div8Coms$Subclass = ""
      Div8Coms$Code     = ""
      Div8Coms$Class = ""
      
      for (i in 1:nrow(Div8Coms)) 
      {
          Div8Coms$Subclass[i] <- ifelse(str_detect(Div8Coms$V1[i],"\\."),Div8Coms$V2[i],"") 
          Div8Coms$Code[i]     <- ifelse(str_detect(Div8Coms$V1[i],"\\."),Div8Coms$V1[i],"") 
          Div8Coms$Class[i]    <- ifelse(str_detect(Div8Coms$V1[i],"\\."),
                                           ifelse(Div8Coms$V2[(i-1)] == "", Div8Coms$V2[(i-2)], Div8Coms$V2[(i-1)]),"")
          Div8Coms$Class[i]    <- ifelse(Div8Coms$Class[i]=="Geomean", "",Div8Coms$Class[i])
      }
      
      for (i in 2:nrow(Div8Coms)) 
      {
        Div8Coms$Subclass[i] <- ifelse((Div8Coms$Subclass[i] == "") & (Div8Coms$Subclass[(i-1)] != ""),
                                        Div8Coms$Subclass[(i-1)],
                                        Div8Coms$Subclass[i]) 
        
        Div8Coms$Code[i]     <- ifelse((Div8Coms$Code[i] == "") & (Div8Coms$Code[(i-1)] != ""),
                                       Div8Coms$Code[(i-1)],
                                       Div8Coms$Code[i]) 
              
        Div8Coms$Class[i]    <- ifelse((Div8Coms$Class[i] == "") & (Div8Coms$Class[(i-1)] != ""),
                                       Div8Coms$Class[(i-1)],
                                       Div8Coms$Class[i])       
      }

   ##
   ## Step 2: Rename the Columns
   ##
      names(Div8Coms) <- c("ID", "Price_Source", Div8Coms[4,3:14], "Subclass", "Code", "Class")
   
   ##
   ## Step 3: Make Long
   ##
      Div8Coms <- reshape2::melt(Div8Coms,
                                 id.vars = c("ID", "Price_Source", "Subclass", "Code", "Class"),
                                 value.name = "Measured_Price",
                                 variable.name = "Period")
      Div8Coms <- Div8Coms[,]
      Div8Coms <- Div8Coms[Div8Coms$ID != "",]
      Div8Coms <- Div8Coms[Div8Coms$Price_Source != "",]
      Div8Coms <- Div8Coms[Div8Coms$Price_Source != "Geomean",]
      Div8Coms <- Div8Coms[Div8Coms$Subclass  != "",]
      Div8Coms <- Div8Coms[Div8Coms$Price_Source != Div8Coms$Subclass,]
      
      Categories <- unique(Div8Coms$Subclass)
      
      for(i in 1:nrow(Div8Coms))
      {
        Div8Coms$Code[i] <- paste0(Div8Coms$Code[i], ".", which(Categories == Div8Coms$Subclass[i]), ".", Div8Coms$ID[i])
        
      }
   ##
   ##    Clean up little odd ball stuff
   ##
      Div8Coms$Price_Source <- str_squish(Div8Coms$Price_Source)
      Div8Coms$Subclass  <- str_squish(Div8Coms$Subclass)
      Div8Coms$Code      <- str_squish(Div8Coms$Code)
      Div8Coms$Class     <- str_squish(Div8Coms$Class)

      Div8Coms$Period <- as.Date(paste0("15-",Div8Coms$Period ), "%d-%b-%y")
      Div8Coms$Measured_Price  <- as.numeric(Div8Coms$Measured_Price)

   ##
   ## Make sure this detail maps to the Regimen
   ##
      Regimen_Subclass <- unique(Regimen$Subclass[Regimen$Groups %in% c("08 COMMUNICATION", "09 RECREATION AND CULTURE")])
      Regimen_Class    <- unique(Regimen$Class[Regimen$Groups %in% c("08 COMMUNICATION", "09 RECREATION AND CULTURE")])
      
      Tab_Subclass     <- unique(Div8Coms$Subclass)
      Tab_Class        <- unique(Div8Coms$Class)

      Subclasses_Not_In_Collection <- Regimen_Subclass[!(Regimen_Subclass %in% Tab_Subclass)]
      Misspelt_Subclasses          <- Tab_Subclass[!(Tab_Subclass %in% Regimen_Subclass)]
      
      Class_Not_In_Collection <- Regimen_Class[!(Regimen_Class %in% Tab_Class)]
      Misspelt_Class          <- Tab_Class[!(Tab_Class %in% Regimen_Class)]

   ##
   ## Save files our produce some final output of something
   ##
      save(Div8Coms, file = 'Data_Intermediate/Div8Coms.rda')

##
##    And we're done
##
