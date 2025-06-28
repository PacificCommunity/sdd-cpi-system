##
##    Programme:  Clean_div6health.r
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

      load("Data_Intermediate/RAWDATA_Div 6, 7 health & TransportXXCPI_calculation_Xmas_Test_Workbook_2025.rda")
      Div6Health <- `RAWDATA_Div 6, 7 health & TransportXXCPI_calculation_Xmas_Test_Workbook_2025`
      
   ##
   ## Step 1: Fill in the blanks
   ## 
      Div6Health$Subclass = ""
      Div6Health$Code     = ""
      Div6Health$Class = ""
      
      for (i in 1:nrow(Div6Health)) 
      {
          Div6Health$Subclass[i] <- ifelse(str_detect(Div6Health$V1[i],"\\."),Div6Health$V2[i],"") 
          Div6Health$Code[i]     <- ifelse(str_detect(Div6Health$V1[i],"\\."),Div6Health$V1[i],"") 
          Div6Health$Class[i]    <- ifelse(str_detect(Div6Health$V1[i],"\\."),
                                           ifelse(Div6Health$V2[(i-1)] == "", Div6Health$V2[(i-2)], Div6Health$V2[(i-1)]),"")
          Div6Health$Class[i]    <- ifelse(Div6Health$Class[i]=="Geomean", "",Div6Health$Class[i])
      }
      
      for (i in 2:nrow(Div6Health)) 
      {
        Div6Health$Subclass[i] <- ifelse((Div6Health$Subclass[i] == "") & (Div6Health$Subclass[(i-1)] != ""),
                                        Div6Health$Subclass[(i-1)],
                                        Div6Health$Subclass[i]) 
        
        Div6Health$Code[i]     <- ifelse((Div6Health$Code[i] == "") & (Div6Health$Code[(i-1)] != ""),
                                       Div6Health$Code[(i-1)],
                                       Div6Health$Code[i]) 
              
        Div6Health$Class[i]    <- ifelse((Div6Health$Class[i] == "") & (Div6Health$Class[(i-1)] != ""),
                                       Div6Health$Class[(i-1)],
                                       Div6Health$Class[i])       
      }

   ##
   ## Step 2: Rename the Columns
   ##
      names(Div6Health) <- c("ID", "Component", Div6Health[4,3:14], "Subclass", "Code", "Class")
   
   ##
   ## Step 3: Make Long
   ##
      Div6Health <- reshape2::melt(Div6Health,
                                 id.vars = c("ID", "Component", "Subclass", "Code", "Class"),
                                 variable.name = "Period")
      Div6Health <- Div6Health[,]
      Div6Health <- Div6Health[Div6Health$ID != "",]
      Div6Health <- Div6Health[Div6Health$Component != "",]
      Div6Health <- Div6Health[Div6Health$Component != "Geomean",]
      Div6Health <- Div6Health[Div6Health$Subclass  != "",]
      Div6Health <- Div6Health[Div6Health$Component != Div6Health$Subclass,]
      
      Categories <- unique(Div6Health$Subclass)
      
      for(i in 1:nrow(Div6Health))
      {
        Div6Health$Code[i] <- paste0(Div6Health$Code[i], ".", which(Categories == Div6Health$Subclass[i]), ".", Div6Health$ID[i])
        
      }
   ##
   ##    Clean up little odd ball stuff
   ##
      Div6Health$Component <- str_squish(Div6Health$Component)
      Div6Health$Subclass  <- str_squish(Div6Health$Subclass)
      Div6Health$Code      <- str_squish(Div6Health$Code)
      Div6Health$Class     <- str_squish(Div6Health$Class)

      Div6Health$Period <- as.Date(paste0("15-",Div6Health$Period ), "%d-%b-%y")
      Div6Health$value  <- as.numeric(Div6Health$value)

   ##
   ## Make sure this detail maps to the Regimen
   ##
      Regimen_Subclass <- unique(Regimen$Subclass[Regimen$Groups %in% c("06 HEALTH", "07 TRANSPORT")])
      Regimen_Class    <- unique(Regimen$Class[Regimen$Groups %in% c("06 HEALTH", "07 TRANSPORT")])
      
      Tab_Subclass     <- unique(Div6Health$Subclass)
      Tab_Class        <- unique(Div6Health$Class)

      Subclasses_Not_In_Collection <- Regimen_Subclass[!(Regimen_Subclass %in% Tab_Subclass)]
      Misspelt_Subclasses          <- Tab_Subclass[!(Tab_Subclass %in% Regimen_Subclass)]

   ##
   ## Correct the ones that doesnt
   ##
      Div6Health$Subclass <- ifelse(str_detect(Div6Health$Subclass, "2 Wheels, Motor Cycle"),"2 Wheels, motor Cycle" , 
                             ifelse(str_detect(Div6Health$Subclass, "Non-Prescription Medication"),"Non Prescription medication",Div6Health$Subclass))
      ##
      ##    Make a choice on the "Air Fare - Domestic", "Air Fare"  choices to code it to the "Air Fare - International" catagory
      ##
      ##    CONTROVERSIAL !!! But there's no  "Air Fare - Domestic" in the regimen
      ##
      Div6Health$Subclass <- ifelse(str_detect(Div6Health$Subclass, "Air Fare"),"Air Fare - International", Div6Health$Subclass)

   ##
   ## Make sure this detail maps to the Regimen
   ##
      Regimen_Subclass <- unique(Regimen$Subclass[Regimen$Groups %in% c("06 HEALTH", "07 TRANSPORT")])
      Regimen_Class    <- unique(Regimen$Class[Regimen$Groups %in% c("06 HEALTH", "07 TRANSPORT")])
      
      Tab_Subclass     <- unique(Div6Health$Subclass)
      Tab_Class        <- unique(Div6Health$Class)

      Subclasses_Not_In_Collection <- Regimen_Subclass[!(Regimen_Subclass %in% Tab_Subclass)]
      Misspelt_Subclasses          <- Tab_Subclass[!(Tab_Subclass %in% Regimen_Subclass)]
      
   ##
   ## Save files our produce some final output of something
   ##
      save(Div6Health, file = 'Data_Intermediate/Div6Health.rda')

##
##    And we're done
##
