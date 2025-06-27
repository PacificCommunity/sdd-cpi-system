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
      Groups <- Regimen[str_length(Regimen$V2) == 2,]
      Class  <- Regimen[str_length(Regimen$V2) == 4,]
      
      Regimen <- Regimen[Regimen$V4 != "",]
      names(Regimen) <- str_replace_all(Regimen[1,], " ", "_")
      Regimen <- Regimen[2:nrow(Regimen),]
      


   ##
   ## Step 2: Make the names
   ## 
      Make_Group_Names <- reshape2::dcast(Regimen,
                                          ID ~ Size,
                                          value.var = "V1")
                                     
      for (i in 2:nrow(Make_Group_Names)) 
      {
          Make_Group_Names$`1`[i] <- ifelse(is.na(Make_Group_Names$`1`[i]) & !is.na(Make_Group_Names$`1`[(i-1)]), Make_Group_Names$`1`[(i-1)], Make_Group_Names$`1`[i])
          Make_Group_Names$`2`[i] <- ifelse(is.na(Make_Group_Names$`2`[i]) & !is.na(Make_Group_Names$`2`[(i-1)]), Make_Group_Names$`2`[(i-1)], Make_Group_Names$`2`[i])
          Make_Group_Names$`3`[i] <- ifelse(is.na(Make_Group_Names$`3`[i]) & !is.na(Make_Group_Names$`3`[(i-1)]), Make_Group_Names$`3`[(i-1)], Make_Group_Names$`3`[i])
          Make_Group_Names$`4`[i] <- ifelse(is.na(Make_Group_Names$`4`[i]) & !is.na(Make_Group_Names$`4`[(i-1)]), Make_Group_Names$`4`[(i-1)], Make_Group_Names$`4`[i])
          Make_Group_Names$`5`[i] <- ifelse(is.na(Make_Group_Names$`5`[i]) & !is.na(Make_Group_Names$`5`[(i-1)]), Make_Group_Names$`5`[(i-1)], Make_Group_Names$`5`[i])
          Make_Group_Names$`6`[i] <- ifelse(is.na(Make_Group_Names$`6`[i]) & !is.na(Make_Group_Names$`6`[(i-1)]), Make_Group_Names$`6`[(i-1)], Make_Group_Names$`6`[i])
      }      
      names(Make_Group_Names) <- c("ID", "A_Description", "B_Description", "C_Description", "D_Description", "E_Description")
      
   ##
   ## Step 2: Make the codes
   ## 
      Make_Group_Codes <- reshape2::dcast(Regimen,
                                          ID ~ Size,
                                          value.var = "V2")
      for (i in 2:nrow(Make_Group_Codes)) 
      {
          Make_Group_Codes$`1`[i] <- ifelse(is.na(Make_Group_Codes$`1`[i]) & !is.na(Make_Group_Codes$`1`[(i-1)]), Make_Group_Codes$`1`[(i-1)], Make_Group_Codes$`1`[i])
          Make_Group_Codes$`2`[i] <- ifelse(is.na(Make_Group_Codes$`2`[i]) & !is.na(Make_Group_Codes$`2`[(i-1)]), Make_Group_Codes$`2`[(i-1)], Make_Group_Codes$`2`[i])
          Make_Group_Codes$`3`[i] <- ifelse(is.na(Make_Group_Codes$`3`[i]) & !is.na(Make_Group_Codes$`3`[(i-1)]), Make_Group_Codes$`3`[(i-1)], Make_Group_Codes$`3`[i])
          Make_Group_Codes$`4`[i] <- ifelse(is.na(Make_Group_Codes$`4`[i]) & !is.na(Make_Group_Codes$`4`[(i-1)]), Make_Group_Codes$`4`[(i-1)], Make_Group_Codes$`4`[i])
          Make_Group_Codes$`5`[i] <- ifelse(is.na(Make_Group_Codes$`5`[i]) & !is.na(Make_Group_Codes$`5`[(i-1)]), Make_Group_Codes$`5`[(i-1)], Make_Group_Codes$`5`[i])
          Make_Group_Codes$`6`[i] <- ifelse(is.na(Make_Group_Codes$`6`[i]) & !is.na(Make_Group_Codes$`6`[(i-1)]), Make_Group_Codes$`6`[(i-1)], Make_Group_Codes$`6`[i])
      }      
      names(Make_Group_Codes) <- c("ID", "A_Code", "B_Code", "C_Code", "D_Code", "E_Code")

   ##
   ## Step 3: Bring them all together
   ## 
      All_Weights <- merge(Make_Group_Names,
                           Make_Group_Codes,
                           by = c("ID"))
      All_Weights <- merge(All_Weights,
                           Regimen[,c("ID","V5")],
                           by = c("ID"))




      
   ##
   ## Save files our produce some final output of something
   ##
      save(Regimen, file = 'Data_Intermediate/Regimen.rda')
##
##    And we're done
##
