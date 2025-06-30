##
##    Programme:  Clean_div2alc.r
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
   ##    Load data from somewhere
   ##
      load('Data_Intermediate/Regimen.rda')
   
      load("Data_Intermediate/RAWDATA_Div 2 ,3 Alc & Tobac, clothingXXCPI_calculation_Xmas_Test_Workbook_2025.rda")
      Div2alc <- `RAWDATA_Div 2 ,3 Alc & Tobac, clothingXXCPI_calculation_Xmas_Test_Workbook_2025`
   ##
   ## Step 1: Fill in the blanks
   ## 
      Div2alc$Subclass = ""
      Div2alc$Code     = ""
      Div2alc$Class    = ""


      for (i in 1:nrow(Div2alc)) 
      {
        Div2alc$Subclass[i] <- ifelse(str_detect(Div2alc$V1[i],"\\."),Div2alc$V2[i],"") 
        Div2alc$Code[i]     <- ifelse(str_detect(Div2alc$V1[i],"\\."),Div2alc$V1[i],"") 
        Div2alc$Class[i]    <- ifelse(str_detect(Div2alc$V1[i],"\\."),
                                         ifelse(Div2alc$V2[(i-1)] == "", Div2alc$V2[(i-2)], Div2alc$V2[(i-1)]),
                                         "") 
        Div2alc$Class[i]    <- ifelse(Div2alc$Class[i]=="Geomean", "",Div2alc$Class[i])
      }

      for (i in 2:nrow(Div2alc)) 
      {
        Div2alc$Subclass[i] <- ifelse((Div2alc$Subclass[i] == "") & (Div2alc$Subclass[(i-1)] != ""),
                                       Div2alc$Subclass[(i-1)],
                                       Div2alc$Subclass[i]) 
           
        Div2alc$Code[i]     <- ifelse((Div2alc$Code[i] == "") & (Div2alc$Code[(i-1)] != ""),
                                      Div2alc$Code[(i-1)],
                                      Div2alc$Code[i]) 
        
        Div2alc$Class[i]    <- ifelse((Div2alc$Class[i] == "") & (Div2alc$Class[(i-1)] != ""),
                                         Div2alc$Class[(i-1)],
                                         Div2alc$Class[i])       
      }

   ##
   ## Step 2: Rename the Columns
   ##
      names(Div2alc) <- c("ID", "Price_Source", Div2alc[4,3:14], "Subclass", "Code", "Class")

   ##
   ## Step 3: Make Long
   ##
      Div2alc <- reshape2::melt(Div2alc,
                                 id.vars = c("ID", "Price_Source", "Subclass", "Code", "Class"),
                                 value.name = "Measured_Price",
                                 variable.name = "Period")
      Div2alc <- Div2alc[,]
      Div2alc <- Div2alc[Div2alc$ID != "",]
      Div2alc <- Div2alc[Div2alc$Price_Source != "",]
      Div2alc <- Div2alc[Div2alc$Price_Source != "Geomean",]
      Div2alc <- Div2alc[Div2alc$Subclass  != "",]
      Div2alc <- Div2alc[Div2alc$Price_Source != Div2alc$Subclass,]

      Categories <- unique(Div2alc$Subclass)

      for(i in 1:nrow(Div2alc))
      {
        Div2alc$Code[i] <- paste0(Div2alc$Code[i], ".", which(Categories == Div2alc$Subclass[i]), ".", Div2alc$ID[i])
        
      }

      Div2alc$Price_Source <- str_squish(Div2alc$Price_Source)
      Div2alc$Subclass  <- str_squish(Div2alc$Subclass)
      Div2alc$Code      <- str_squish(Div2alc$Code)
      Div2alc$Class     <- str_squish(Div2alc$Class)

      Div2alc$Period <- as.Date(paste0("15-",Div2alc$Period ), "%d-%b-%y")
      Div2alc$Measured_Price  <- as.numeric(Div2alc$Measured_Price)

   ##
   ## Make sure this detail maps to the Regimen
   ##
      Regimen_Subclass <- unique(Regimen$Subclass[Regimen$Groups %in% c("02 Alcoholic beverages, tobbaco & Narcotics", "03 Clothing & footear")])
      Regimen_Class    <- unique(Regimen$Class[Regimen$Groups %in% c("02 Alcoholic beverages, tobbaco & Narcotics", "03 Clothing & footear")])
      
      Tab_Subclass     <- unique(Div2alc$Subclass)
      Tab_Class        <- unique(Div2alc$Class)

      Subclasses_Not_In_Collection <- Regimen_Subclass[!(Regimen_Subclass %in% Tab_Subclass)]
      Misspelt_Subclasses          <- Tab_Subclass[!(Tab_Subclass %in% Regimen_Subclass)]
      
      Class_Not_In_Collection <- Regimen_Class[!(Regimen_Class %in% Tab_Class)]
      Misspelt_Class          <- Tab_Class[!(Tab_Class %in% Regimen_Class)]
      
   ##
   ## Save files our produce some final output of something
   ##
   save(Div2alc, file = 'Data_Intermediate/Div2alc.rda')

##
##    And we're done
##
