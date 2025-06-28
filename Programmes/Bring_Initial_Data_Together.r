##
##    Programme:  Bring_Initial_Data_Together.r
##
##    Objective:  Bring all of the data Price_Sources together and check for consistency and any errors.
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
   ##    Load data from the individual spreadsheet tabs
   ##
      load('Data_Intermediate/Div1food.rda')
      load('Data_Intermediate/Div2alc.rda')
      load('Data_Intermediate/Div4Energy.rda')
      load('Data_Intermediate/Div6Health.rda')
      load('Data_Intermediate/Div8Coms.rda')
      load('Data_Intermediate/Div10Edu.rda')
      
      ##
      ##    Grab the Regimen
      ##
      load('Data_Intermediate/Regimen.rda')
      
   ##
   ## Step 1: Stick all of the divisions together
   ##
      All_Divisions <- rbind(Div1food,
                             Div2alc,
                             Div4Energy,
                             Div6Health,
                             Div8Coms,
                             Div10Edu)

   ##
   ## Step 2: Lets have a look at the uniformity of the classifications
   ##
      
      unique(All_Divisions$Price_Source)
      ##
      ##    Drop these into a CSV for standardisation - only run this code if you have new files which needs standardised
      ##       else it will overwrite your previous corrections.
      ##
           # write.csv(data.frame(Old_Name = unique(All_Divisions$Price_Source),
                                # New_Name = unique(All_Divisions$Price_Source)), 
                     # file = "Data_Raw/Component_Correction.csv",
                     # row.names = FALSE)
      ##
      ##    Read these from standardisation
      ##
          load('Data_Intermediate/RAWDATA_XXComponent_Correction.rda')

   ##
   ##    Merge the Price_Source (which is uncleaned) with the file RAWDATA_XXPrice_Source_Correction
   ##       which is generated through a recursive process. The first time this "Bring_Initial_Data_Together.r" Programme
   ##       was run, it created a csv file, which was read into Excel. The text file has two columns called "Old_Name" and "New_Name" 
   ##       which, when the programme was first run, were exactly the same. In excel, the analyst recorded the "New_Name" column
   ##       to be the preferred name of the Price_Source, creating two different columns "Old_Name" (which is the original name), and 
   ##       "New_Name" which is the recoded Price_Source name. 
   ##
   ##    The file got resaved as csv. Then, the Read_Spreadsheets and Read_CSVs programmes got re-run. That re-run process read in the recoded Price_Source
   ##       into an R dataframe which make the recoding dataset available to each of the tab cleaning programmes, where the recode values will
   ##       be applied to the old Price_Source variables derived from each dirty tab.
   ##
   ##    Finally, when the cleaned tab from these programmes are re-read into the "Bring_Initial_Data_Together" programme, then the code there 
   ##       will recreate the "Old_Name" and "New_Name" and the Price_Source_Correction.csv, but this time the old and the new will be exactly the
   ##       same - the corrected 
   ##
      All_Divisions <- merge(All_Divisions,
                             RAWDATA_XXComponent_Correction,
                             by.x = c("Price_Source"),
                             by.y = c("Old_Name"),
                             all.x = TRUE)
      All_Divisions <- All_Divisions[,names(All_Divisions) != "Price_Source"]              
      names(All_Divisions)[names(All_Divisions) == "New_Name"] <- "Price_Source"
      
   ##
   ## Step 3: Generate the geomeans at the Subclass level
   ##
      GeoMeans <- with(All_Divisions,
                             aggregate(list(Geometric_Mean = log(Measured_Price)),
                                       list(Subclass = Subclass,
                                            Period = Period),
                                     mean, 
                                     na.rm = TRUE))      
      GeoMeans$Geometric_Mean <- exp(GeoMeans$Geometric_Mean)

   ##
   ## Step 4: Generate price changes relative to the '2024-06-15' base period
   ##
      GeoMeans <- merge(GeoMeans,
                        GeoMeans[GeoMeans$Period == "2024-06-15",c("Subclass", "Geometric_Mean")],
                        by = "Subclass")
      names(GeoMeans) <- c("Subclass","Period","Current_Price","Base_Period_Price")
      GeoMeans$Price_Change <- with(GeoMeans, (Current_Price / Base_Period_Price))
   
   ##
   ## Step 5: Assign the Regimen Weights
   ##
      Weighted_Data <- merge(GeoMeans,
                             Regimen,
                             by = c("Subclass"))
      Weighted_Data[Weighted_Data$Subclass == "Rice",]
      Weighted_Data <- Weighted_Data[order(Weighted_Data$Division,
                                           Weighted_Data$Groups,
                                           Weighted_Data$Class,
                                           Weighted_Data$Subclass,
                                           Weighted_Data$Period),]

   ##
   ## Step 6: Generate the CPI
   ##
      CPI <- with(Weighted_Data,
                    aggregate(list(Weighted_Price_Change = (Price_Change * Weights)),
                              list(Subclass = Subclass,
                                   Period = Period),
                            sum, 
                            na.rm = TRUE)) 
      CPI[CPI$Subclass == "Rice",]

      CPI <- with(Weighted_Data,
                    aggregate(list(Weighted_Price_Change = (Price_Change * Weights)),
                              list(Period = Period,
                                   Groups = Groups),
                            sum, 
                            na.rm = TRUE)) 
      CPI <- CPI[order(CPI$Period),]

   ##
   ## Save files our produce some final output of something
   ##
      All_Divisions <- merge(All_Divisions,
                             Regimen,
                             by = c("Class", "Subclass"),
                             all.x = TRUE)
                             
      All_Divisions <- All_Divisions[,c("Division", "Groups", "Class", "Subclass", "COICOP", "Item_nos", "Weights", "Price_Source", "Period", "Measured_Price")]
      All_Divisions <- All_Divisions[order(All_Divisions$Division, 
                                           All_Divisions$Groups, 
                                           All_Divisions$Class, 
                                           All_Divisions$Subclass, 
                                           All_Divisions$COICOP, 
                                           All_Divisions$Item_nos, 
                                           All_Divisions$Weights, 
                                           All_Divisions$Price_Source, 
                                           All_Divisions$Period, 
                                           All_Divisions$Measured_Price),]
      
      save(All_Divisions, file = 'Data_Output/All_Divisions.rda')
      save(Weighted_Data, file = 'Data_Output/Weighted_Data.rda')
      save(CPI,           file = 'Data_Output/CPI.rda')

##
##    And we're done
##


      CPI <- with(Weighted_Data,
                    aggregate(list(Weighted_Price_Change = (Price_Change * Weights)),
                              list(Period = Period),
                            sum, 
                            na.rm = TRUE)) 
      CPI <- CPI[order(CPI$Period),]
      CPI$Base <- CPI$Weighted_Price_Change[CPI$Period == "2024-06-15"]
      CPI$Price_Change <- with(CPI, (Weighted_Price_Change / Base))



