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
      
      ##
      ##    Grab the Regimen
      ##
      
   ##
   ## Step 1: Stick all of the divisions together
   ##
      All_Divisions <- rbind(Div1food,
                             Div2alc)

   ##
   ## Step 2: Lets have a look at the uniformity of the classifications
   ##
      
      unique(All_Divisions$Component)
      ##
      ##    Drop these into a CSV for standardisation - only run this code if you have new files which needs standardised
      ##       else it will overwrite your previous corrections.
      ##
         ## write.csv(data.frame(Old_Name = unique(All_Divisions$Component),
         ##                      New_Name = unique(All_Divisions$Component)), 
         ##           file = "Data_Raw/Component_Correction.csv",
         ##           row.names = FALSE)
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
      GeoMeans[GeoMeans$Subclass == "Rice",]


   ##
   ## Save files our produce some final output of something
   ##
      save(xxxx, file = 'Data_Intermediate/xxxxxxxxxxxxx.rda')
      save(xxxx, file = 'Data_Output/xxxxxxxxxxxxx.rda')
##
##    And we're done
##
