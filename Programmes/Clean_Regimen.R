##
##    Programme:  Clean_Regimen.r
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
   ##    Load the uncleaned Regimen data
   ##
      load('Data_Intermediate/RAWDATA_Index_calculation_&aggregtionXXCPI_calculation_Xmas_Test_Workbook_2025.rda')
      
      Regimen <- `RAWDATA_Index_calculation_&aggregtionXXCPI_calculation_Xmas_Test_Workbook_2025`
     
      Regimen <- Regimen[,1:5]
   ##
   ## Step 1: Fill in the blanks
   ## 
      for (i in 2:nrow(Regimen)) 
      {
          Regimen$V1[i] <- ifelse((Regimen$V1[i] == "") & (Regimen$V1[(i-1)] != ""), Regimen$V1[(i-1)], Regimen$V1[i])
      }      
   ##
   ## Step 2: Carve out the Groups and the Classes
   ## 
      Groups <- Regimen[str_length(Regimen$V2) == 2,]
      Class  <- Regimen[str_length(Regimen$V2) == 4,]
      
   ##
   ## Step 3: Get the Regimen down to the lowest list
   ## 
      Regimen <- Regimen[Regimen$V4 != "",]
      names(Regimen) <- str_replace_all(Regimen[1,], " ", "_")
      Regimen <- Regimen[2:nrow(Regimen),]
      Regimen <- Regimen[Regimen$Item_name != "Weights check",]
      
   ##
   ## Step 4: Merge Groups and the Classes into lowest level Regimen
   ## 
      Regimen$Groups <- ""
      Regimen$Class  <- ""
      
      for (i in 1:nrow(Regimen)) 
      {
          Regimen$Groups[i] <- str_squish(Groups$V1[which(Groups$V2 %in% str_sub(Regimen$COICOP[i], start = 1, end = 2) )])
          Regimen$Class[i]  <- str_squish(Class$V1[ which( Class$V2 %in% str_sub(Regimen$COICOP[i], start = 1, end = 4) )])
      }      
      
      
   ##
   ## Save files our produce some final output of something
   ##
      names(Regimen) <- c('Class', 'COICOP', 'Item_nos', 'Subclass', 'Weights', 'Groups', 'Division')
      Regimen$Subclass <- str_squish(Regimen$Subclass)
      Regimen$Subclass <- ifelse(str_detect(Regimen$Subclass, "Condensed Milk with sugar"),"Condensed Milk with Sugar (Carnation)", Regimen$Subclass)
      
      save(Regimen, file = 'Data_Intermediate/Regimen.rda')
##
##    And we're done
##
