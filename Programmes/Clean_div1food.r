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
      load('Data_Intermediate/RAWDATA_XXComponent_Correction.rda')

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
      names(Div1food) <- c("ID", "Component", Div1food[4,3:14], "Subclass", "Code", "Class")
   
   ##
   ## Step 3: Make Long
   ##
      Div1food <- reshape2::melt(Div1food,
                                 id.vars = c("ID", "Component", "Subclass", "Code", "Class"),
                                 variable.name = "Period")
      Div1food <- Div1food[,]
      Div1food <- Div1food[Div1food$ID != "",]
      Div1food <- Div1food[Div1food$Component != "",]
      Div1food <- Div1food[Div1food$Component != "Geomean",]
      Div1food <- Div1food[Div1food$Subclass  != "",]
      Div1food <- Div1food[Div1food$Component != Div1food$Subclass,]
      
      Categories <- unique(Div1food$Subclass)
      
      for(i in 1:nrow(Div1food))
      {
        Div1food$Code[i] <- paste0(Div1food$Code[i], ".", which(Categories == Div1food$Subclass[i]), ".", Div1food$ID[i])
        
      }
   ##
   ##    Clean up little odd ball stuff
   ##
      Div1food$Component <- str_squish(Div1food$Component)
      Div1food$Subclass  <- str_squish(Div1food$Subclass)
      Div1food$Code      <- str_squish(Div1food$Code)
      Div1food$Class     <- str_squish(Div1food$Class)

      Div1food$Period <- as.Date(paste0("15-",Div1food$Period ), "%d-%b-%y")
      Div1food$value  <- as.numeric(Div1food$value)
      
   ##
   ## Save files our produce some final output of something
   ##
      save(Div1food, file = 'Data_Intermediate/Div1food.rda')

##
##    And we're done
##
