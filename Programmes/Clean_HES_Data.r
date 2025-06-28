##
##    Programme:  Clean_HES_Data.r
##
##    Objective:  We're read HES in, but it needs to be made tidy before it proceeds further
##
##              
##    Author:    James Hogan, NZIER, 4 April 2019
##
   rm(list=ls(all=TRUE))
   
   load("data_intermediate/RAWDATA_NZ.Stat exportXXdb0caae7-24eb-4f41-bc7b-2aca1a4f9e8c.rda")
   HES_Data <- as.data.frame(`RAWDATA_NZ.Stat exportXXdb0caae7-24eb-4f41-bc7b-2aca1a4f9e8c`,stringsAsFactors = FALSE)
   names(HES_Data) <- c("Region", "Measure", "Expenditure", "Category", "Biff", "Year")	
   HES_Data[sapply(HES_Data, is.factor)] <- lapply(HES_Data[sapply(HES_Data, is.factor)], 
                                       as.character)
   for(i in 2:nrow(HES_Data))
      {
         HES_Data$Region[i] <- ifelse((HES_Data$Region[i] == "") & !(HES_Data$Region[(i-1)] == ""),
                                       str_trim(HES_Data$Region[(i-1)]),
                                       str_trim(HES_Data$Region[i]))
                                       
         HES_Data$Measure[i] <- ifelse((HES_Data$Measure[i] == "") & !(HES_Data$Measure[(i-1)] == ""),
                                       str_trim(HES_Data$Measure[(i-1)]),
                                       str_trim(HES_Data$Measure[i]))
                                       
         HES_Data$Expenditure[i] <- ifelse((HES_Data$Expenditure[i] == "") & !(HES_Data$Expenditure[(i-1)] == ""),
                                       str_trim(HES_Data$Expenditure[(i-1)]),
                                       str_trim(HES_Data$Expenditure[i]))
                                       
         HES_Data$Category[i] <- ifelse((HES_Data$Category[i] == "") & !(HES_Data$Category[(i-1)] == ""),
                                       str_trim(HES_Data$Category[(i-1)]),
                                       str_trim(HES_Data$Category[i]))
      }
    save(HES_Data, file = "data_intermediate/HES_Data.rda")
##
##    And we're done
##
   