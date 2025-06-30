##
##    Programme:  Clean_div1food.r
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

      load("Data_Intermediate/RAWDATA_Div 1 FoodXXCPI_calculation_Xmas_Test_Workbook_2025.rda")
      Div1food <- `RAWDATA_Div 1 FoodXXCPI_calculation_Xmas_Test_Workbook_2025`
   ##
   ## Step 1: Fill in the blanks
   ## 
      Div1food$Subclass = ""
      Div1food$Code     = ""
      Div1food$Class = ""
      
      for (i in 1:nrow(Div1food)) 
      {
          Div1food$Subclass[i] <- ifelse(str_detect(Div1food$V1[i],"\\."),Div1food$V2[i],"") 
          Div1food$Code[i]     <- ifelse(str_detect(Div1food$V1[i],"\\."),Div1food$V1[i],"") 
          Div1food$Class[i]    <- ifelse(str_detect(Div1food$V1[i],"\\."),
                                           ifelse(Div1food$V2[(i-1)] == "", Div1food$V2[(i-2)], Div1food$V2[(i-1)]),"")
          Div1food$Class[i]    <- ifelse(Div1food$Class[i]=="Geomean", "",Div1food$Class[i])
      }
      
      for (i in 2:nrow(Div1food)) 
      {
        Div1food$Subclass[i] <- ifelse((Div1food$Subclass[i] == "") & (Div1food$Subclass[(i-1)] != ""),
                                        Div1food$Subclass[(i-1)],
                                        Div1food$Subclass[i]) 
        
        Div1food$Code[i]     <- ifelse((Div1food$Code[i] == "") & (Div1food$Code[(i-1)] != ""),
                                       Div1food$Code[(i-1)],
                                       Div1food$Code[i]) 
              
        Div1food$Class[i]    <- ifelse((Div1food$Class[i] == "") & (Div1food$Class[(i-1)] != ""),
                                       Div1food$Class[(i-1)],
                                       Div1food$Class[i])       
      }

   ##
   ## Step 2: Rename the Columns
   ##
      names(Div1food) <- c("ID", "Price_Source", Div1food[4,3:14], "Subclass", "Code", "Class")
   
   ##
   ## Step 3: Make Long
   ##
      Div1food <- reshape2::melt(Div1food,
                                 id.vars = c("ID", "Price_Source", "Subclass", "Code", "Class"),
                                 value.name = "Measured_Price",
                                 variable.name = "Period")
      Div1food <- Div1food[,]
      Div1food <- Div1food[Div1food$ID != "",]
      Div1food <- Div1food[Div1food$Price_Source != "",]
      Div1food <- Div1food[Div1food$Price_Source != "Geomean",]
      Div1food <- Div1food[Div1food$Subclass  != "",]
      Div1food <- Div1food[Div1food$Price_Source != Div1food$Subclass,]
      
      Categories <- unique(Div1food$Subclass)
      
      for(i in 1:nrow(Div1food))
      {
        Div1food$Code[i] <- paste0(Div1food$Code[i], ".", which(Categories == Div1food$Subclass[i]), ".", Div1food$ID[i])
        
      }
   ##
   ##    Clean up little odd ball stuff
   ##
      Div1food$Price_Source <- str_squish(Div1food$Price_Source)
      Div1food$Subclass  <- str_squish(Div1food$Subclass)
      Div1food$Code      <- str_squish(Div1food$Code)
      Div1food$Class     <- str_squish(str_replace_all(Div1food$Class, "\\&", "and"))

      Div1food$Period <- as.Date(paste0("15-",Div1food$Period ), "%d-%b-%y")
      Div1food$Measured_Price  <- as.numeric(Div1food$Measured_Price)

   ##
   ## Make sure this detail maps to the Regimen
   ##
      Regimen_Subclass <- unique(Regimen$Subclass[Regimen$Groups %in% c("01 Food and Non-alcoholic beveages")])
      Regimen_Class    <- unique(Regimen$Class[Regimen$Groups %in% c("01 Food and Non-alcoholic beveages")])
      
      Tab_Subclass     <- unique(Div1food$Subclass)
      Tab_Class        <- unique(Div1food$Class)

      Subclasses_Not_In_Collection <- Regimen_Subclass[!(Regimen_Subclass %in% Tab_Subclass)]
      Misspelt_Subclasses          <- Tab_Subclass[!(Tab_Subclass %in% Regimen_Subclass)]

      Class_Not_In_Collection <- Regimen_Class[!(Regimen_Class %in% Tab_Class)]
      Misspelt_Class          <- Tab_Class[!(Tab_Class %in% Regimen_Class)]
   ##
   ## Save files our produce some final output of something
   ##
      save(Div1food, file = 'Data_Intermediate/Div1food.rda')

##
##    And we're done
##
