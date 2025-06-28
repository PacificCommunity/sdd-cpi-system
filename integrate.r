##    Programme:  NAME OF PROJECT IN HERE.r
##
##    Objective:  A brief description of what this project is trying to do.
##                This part of the documentation is about describing the big
##                picture purpose of the work. 
##
##    Plan of  :  What are the steps in this programme's execution? What does
##    Attack   :  each step do?
##
##                Be explicit about writing the programming steps here - how are
##                you going to start from nothing, and move through the stages 
##                that ultimately result in the final thing.  These steps are 
##                mirrored in the code down below.
##
##                Step 1:
##                Step 2:
##                Step 3:
##                Step 4:
##                Step xxx:
##
##    Important:  Does this programme make any important cross-project data 
##    Linkages :  connections? For example, does it read HIES data from PHD?
##                Or does it write data which affects other people's work areas?
##
##                The focus here is on the important data linkages between this
##                project and something else, so that if that something changes,
##                we can figure out the impact it has on this project.
##
##    Author   :  <PROGRAMMER>, <TEAM>, <DATE STARTED>
##
##    Peer     :  <PROGRAMMER>, <TEAM>, <PEER REVIEWED COMPLETED>
##    Reviewer :
##
   ##
   ##    Clear the decks and load up some functionality
   ##
      rm(list=ls(all=TRUE))
      options(scipen = 999)
   ##
   ##    Core libraries
   ##
      library(ggplot2)
      library(plyr)
      library(stringr)
      library(reshape2)
      library(lubridate)
      library(calibrate)
      library(Hmisc)
      library(RColorBrewer)
      library(stringi)
      library(sqldf)
      library(extrafont)
      library(scales)
      library(RDCOMClient)
      library(extrafont)
      library(tictoc)
   ##
   ##    Set working directory
   ##
      setwd("C:\\git\\pricing_system")
      setwd("C:\\GIT_Projects\\sdd-cpi-system")
      setwd("C:\\From BigDisk\\GIT\\sdd-cpi-system")
      
   ##
   ##   Modular-programmed code from here down. Each following programme
   ##      needs to be able to 'stand alone' in the sense that it starts of 
   ##       reading all the data it needs, it processes it, and it concludes
   ##       either writing data for a following programme, or creating a final
   ##       output. 
   ##
      ##
      ##    STEP 1:  Run some automatic readin scripts over the member supplied spreadsheets
      ##
         source("Programmes/Read_Spreadsheets.r")  # This does blah blah blah
         source("Programmes/Read_CSVs.r")          # This does blah blah blah
         
      ##
      ##    STEP 2:  Clean the regimen. This will be used to ensure all of the product descriptions are uniform
      ##
        source("Programmes/Clean_Regimen.r")       # This does blah blah blah
        
      ##
      ##    STEP 3:  Clean the spreadsheet data - do this only once
      ##
        source("Programmes/Clean_div1food.r")      # This does blah blah blah
        source("Programmes/Clean_div2alc.r")       # This does blah blah blah
        source("Programmes/Clean_div4energy.r")    # This does blah blah blah
        source("Programmes/Clean_div6health.r")    # This does blah blah blah
        source("Programmes/Clean_div8coms.r")      # This does blah blah blah
        source("Programmes/Clean_div10edu.r")      # This does blah blah blah

      ##
      ##    STEP 3:  Aggregate all of the cleaned tabs and generated the CPI
      ##
         source("Programmes/Bring_Initial_Data_Together.r") # This does blah blah blah
      ##
      ##    STEP 4: We now have a cleaned regimen and suite of prices. 
      ##
         source("Programmes/xxxxxxxx.r") # This does blah blah blah
      ##
      ##    STEP x: Final output
      ##
         source("Programmes/xxxxxxxx.r") # This does blah blah blah

      ##
      ##    Write up of results
      ##      
         rmarkdown::render("Programmes/SSAP_Value_Report.rmd", output_file = "C:\\From BigDisk\\GIT\\R_Fundamentals_Training\\Fundamentals Graphics and Reporting\\Product_Output\\Value of Good Science.docx")                


##
##   End of programme
##
