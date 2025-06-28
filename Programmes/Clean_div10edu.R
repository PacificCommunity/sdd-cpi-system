##
##    Programme:  Clean_Div10Edu.r
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

      load("Data_Intermediate/RAWDATA_Div 10, 11, 12, Ed. Rest. MicelXXCPI_calculation_Xmas_Test_Workbook_2025.rda")
      Div10Edu <- `RAWDATA_Div 10, 11, 12, Ed. Rest. MicelXXCPI_calculation_Xmas_Test_Workbook_2025`
      
   ##
   ## Step 1: Fill in the blanks
   ## 
      Div10Edu$Subclass = ""
      Div10Edu$Code     = ""
      Div10Edu$Class = ""
      
      for (i in 1:nrow(Div10Edu)) 
      {
          Div10Edu$Subclass[i] <- ifelse(str_detect(Div10Edu$V1[i],"\\."),Div10Edu$V2[i],"") 
          Div10Edu$Code[i]     <- ifelse(str_detect(Div10Edu$V1[i],"\\."),Div10Edu$V1[i],"") 
          Div10Edu$Class[i]    <- ifelse(str_detect(Div10Edu$V1[i],"\\."),
                                           ifelse(Div10Edu$V2[(i-1)] == "", Div10Edu$V2[(i-2)], Div10Edu$V2[(i-1)]),"")
          Div10Edu$Class[i]    <- ifelse(Div10Edu$Class[i]=="Geomean", "",Div10Edu$Class[i])
      }
      
      for (i in 2:nrow(Div10Edu)) 
      {
        Div10Edu$Subclass[i] <- ifelse((Div10Edu$Subclass[i] == "") & (Div10Edu$Subclass[(i-1)] != ""),
                                        Div10Edu$Subclass[(i-1)],
                                        Div10Edu$Subclass[i]) 
        
        Div10Edu$Code[i]     <- ifelse((Div10Edu$Code[i] == "") & (Div10Edu$Code[(i-1)] != ""),
                                       Div10Edu$Code[(i-1)],
                                       Div10Edu$Code[i]) 
              
        Div10Edu$Class[i]    <- ifelse((Div10Edu$Class[i] == "") & (Div10Edu$Class[(i-1)] != ""),
                                       Div10Edu$Class[(i-1)],
                                       Div10Edu$Class[i])       
      }

   ##
   ## Step 2: Rename the Columns
   ##
      names(Div10Edu) <- c("ID", "Price_Source", Div10Edu[4,3:14], "Subclass", "Code", "Class")
   
   ##
   ## Step 3: Make Long
   ##
      Div10Edu <- reshape2::melt(Div10Edu,
                                 id.vars = c("ID", "Price_Source", "Subclass", "Code", "Class"),
                                 value.name = "Measured_Price",
                                 variable.name = "Period")
      Div10Edu <- Div10Edu[,]
      Div10Edu <- Div10Edu[Div10Edu$ID != "",]
      Div10Edu <- Div10Edu[Div10Edu$Price_Source != "",]
      Div10Edu <- Div10Edu[Div10Edu$Price_Source != "Geomean",]
      Div10Edu <- Div10Edu[Div10Edu$Subclass  != "",]
      Div10Edu <- Div10Edu[Div10Edu$Price_Source != Div10Edu$Subclass,]
      
      Categories <- unique(Div10Edu$Subclass)
      
      for(i in 1:nrow(Div10Edu))
      {
        Div10Edu$Code[i] <- paste0(Div10Edu$Code[i], ".", which(Categories == Div10Edu$Subclass[i]), ".", Div10Edu$ID[i])
        
      }
   ##
   ##    Clean up little odd ball stuff
   ##
      Div10Edu$Price_Source <- str_squish(Div10Edu$Price_Source)
      Div10Edu$Subclass  <- str_squish(Div10Edu$Subclass)
      Div10Edu$Code      <- str_squish(Div10Edu$Code)
      Div10Edu$Class     <- str_squish(Div10Edu$Class)

      Div10Edu$Period <- as.Date(paste0("15-",Div10Edu$Period ), "%d-%b-%y")
      Div10Edu$Measured_Price  <- as.numeric(Div10Edu$Measured_Price)

   ##
   ## Make sure this detail maps to the Regimen
   ##
      Regimen_Subclass <- unique(Regimen$Subclass[Regimen$Groups %in% c("10 EDUCATION", "11 RESTAURANTS AND HOTELS", "12 MISCELANEOUS GOODS AND SERVICES")])
      Regimen_Class    <- unique(Regimen$Class[Regimen$Groups %in% c("10 EDUCATION", "11 RESTAURANTS AND HOTELS", "12 MISCELANEOUS GOODS AND SERVICES")])
      
      Tab_Subclass     <- unique(Div10Edu$Subclass)
      Tab_Class        <- unique(Div10Edu$Class)

      Subclasses_Not_In_Collection <- Regimen_Subclass[!(Regimen_Subclass %in% Tab_Subclass)]
      Misspelt_Subclasses          <- Tab_Subclass[!(Tab_Subclass %in% Regimen_Subclass)]

   ##
   ## Correct the ones that dont fit the Regimen
   ##
      Div10Edu$Subclass <- ifelse(str_detect(Div10Edu$Subclass, "Snacks Away From Home"),"Snacks Away from Home", 
                           ifelse(str_detect(Div10Edu$Subclass, "Restaurants and Bars onsale ALCOHOL"),"Restaurant and Bars onsale ALCOHOL", 
                           ifelse(str_detect(Div10Edu$Subclass, "Haircut, Beauty Salon"),"Haircut, Beauty salon", 
                           ifelse(str_detect(Div10Edu$Subclass, "Cosmetic Fragrance"),"Cosmetic Fragance", 
                           ifelse(str_detect(Div10Edu$Subclass, "Dental Product (Tooth Brush)"),"Dental Product (Tooth brush)", Div10Edu$Subclass)))))
 
 
 
   ##
   ## Make sure this detail maps to the Regimen
   ##
      Regimen_Subclass <- unique(Regimen$Subclass[Regimen$Groups %in% c("10 EDUCATION", "11 RESTAURANTS AND HOTELS", "12 MISCELANEOUS GOODS AND SERVICES")])
      Regimen_Class    <- unique(Regimen$Class[Regimen$Groups %in% c("10 EDUCATION", "11 RESTAURANTS AND HOTELS", "12 MISCELANEOUS GOODS AND SERVICES")])
      
      Tab_Subclass     <- unique(Div10Edu$Subclass)
      Tab_Class        <- unique(Div10Edu$Class)

      Subclasses_Not_In_Collection <- Regimen_Subclass[!(Regimen_Subclass %in% Tab_Subclass)]
      Misspelt_Subclasses          <- Tab_Subclass[!(Tab_Subclass %in% Regimen_Subclass)]
      
   ##
   ## Save files our produce some final output of something
   ##
      save(Div10Edu, file = 'Data_Intermediate/Div10Edu.rda')

##
##    And we're done
##
