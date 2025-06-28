##
##    Programme:  Bring_Initial_Data_Together.r
##
##    Objective:  Bring all of the data components together and check for consistency and any errors.
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
      
      unique(All_Divisions$Component)
      ##
      ##    Drop these into a CSV for standardisation - only run this code if you have new files which needs standardised
      ##       else it will overwrite your previous corrections.
      ##
           # write.csv(data.frame(Old_Name = unique(All_Divisions$Component),
                                # New_Name = unique(All_Divisions$Component)), 
                     # file = "Data_Raw/Component_Correction.csv",
                     # row.names = FALSE)
      ##
      ##    Read these from standardisation
      ##
          load('Data_Intermediate/RAWDATA_XXComponent_Correction.rda')

   ##
   ##    Merge the Component (which is uncleaned) with the file RAWDATA_XXComponent_Correction
   ##       which is generated through a recursive process. The first time this "Bring_Initial_Data_Together.r" Programme
   ##       was run, it created a csv file, which was read into Excel. The text file has two columns called "Old_Name" and "New_Name" 
   ##       which, when the programme was first run, were exactly the same. In excel, the analyst recorded the "New_Name" column
   ##       to be the preferred name of the component, creating two different columns "Old_Name" (which is the original name), and 
   ##       "New_Name" which is the recoded Component name. 
   ##
   ##    The file got resaved as csv. Then, the Read_Spreadsheets and Read_CSVs programmes got re-run. That re-run process read in the recoded component
   ##       into an R dataframe which make the recoding dataset available to each of the tab cleaning programmes, where the recode values will
   ##       be applied to the old component variables derived from each dirty tab.
   ##
   ##    Finally, when the cleaned tab from these programmes are re-read into the "Bring_Initial_Data_Together" programme, then the code there 
   ##       will recreate the "Old_Name" and "New_Name" and the Component_Correction.csv, but this time the old and the new will be exactly the
   ##       same - the corrected 
   ##
      All_Divisions <- merge(All_Divisions,
                             RAWDATA_XXComponent_Correction,
                             by.x = c("Component"),
                             by.y = c("Old_Name"),
                             all.x = TRUE)
      All_Divisions <- All_Divisions[,names(All_Divisions) != "Component"]              
      names(All_Divisions)[names(All_Divisions) == "New_Name"] <- "Component"
      
   ##
   ## Step 3: Generate the geomeans at the Subclass level
   ##
      GeoMeans <- with(All_Divisions,
                             aggregate(list(Geometric_Mean = log(value)),
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


   ##
   ## Step 6: Generate the CPI
   ##
      CPI <- with(Weighted_Data,
                    aggregate(list(Value = (Price_Change * Weights)),
                              list(Subclass = Subclass,
                                   Period = Period),
                            sum, 
                            na.rm = TRUE)) 
      CPI[CPI$Subclass == "Rice",]

      CPI <- with(Weighted_Data,
                    aggregate(list(Value = (Price_Change * Weights)),
                              list(Period = Period,
                                   Groups = Groups),
                            sum, 
                            na.rm = TRUE)) 
      CPI <- CPI[order(CPI$Period),]

   ##
   ## Save files our produce some final output of something
   ##
      save(Weighted_Data, file = 'Data_Output/Weighted_Data.rda')
      save(CPI,           file = 'Data_Output/CPI.rda')

##
##    And we're done
##


      CPI <- with(Weighted_Data,
                    aggregate(list(Value = (Price_Change * Weights)),
                              list(Period = Period),
                            sum, 
                            na.rm = TRUE)) 
      CPI <- CPI[order(CPI$Period),]
      CPI$Base <- CPI$Value[CPI$Period == "2024-06-15"]
      CPI$Price_Change <- with(CPI, (Value / Base))



