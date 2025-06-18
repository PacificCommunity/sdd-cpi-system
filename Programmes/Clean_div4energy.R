##
##    Programme:  SAMPLE_PROGRAMME.r
##
##    Objective:  What is this programme designed to do?
##
##    Author:     <PROGRAMMER>, <TEAM>, <DATE STARTED>
##    Peer Review:<PROGRAMMER>, <TEAM>, <DATE STARTED>
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

load("Data_Intermediate/RAWDATA_Div 4,5 energy & HHDXXCPI_calculation_Xmas_Test_Workbook_2025.rda")
div4energy <- `RAWDATA_Div 4,5 energy & HHDXXCPI_calculation_Xmas_Test_Workbook_2025`
##
## Step 1: Fill in the blanks
## 
div4energy$Subclass = ""
div4energy$Code     = ""
div4energy$Class = ""


for (i in 1:nrow(div4energy)) 
{
  div4energy$Subclass[i]=ifelse(str_detect(div4energy$V1[i],"\\."),div4energy$V2[i],"") 
  div4energy$Code[i]=ifelse(str_detect(div4energy$V1[i],"\\."),div4energy$V1[i],"") 
  div4energy$Class[i]=ifelse(str_detect(div4energy$V1[i],"\\."),
                          ifelse(div4energy$V2[(i-1)] == "", div4energy$V2[(i-2)], div4energy$V2[(i-1)]),
                          "") 
  div4energy$Class[i]=ifelse(div4energy$Class[i]=="Geomean", "",div4energy$Class[i])
}

for (i in 2:nrow(div4energy)) 
{
  div4energy$Subclass[i]=ifelse((div4energy$Subclass[i] == "") & (div4energy$Subclass[(i-1)] != ""),
                             div4energy$Subclass[(i-1)],
                             div4energy$Subclass[i]) 
  
  div4energy$Code[i]=ifelse((div4energy$Code[i] == "") & (div4energy$Code[(i-1)] != ""),
                         div4energy$Code[(i-1)],
                         div4energy$Code[i]) 
  
  div4energy$Class[i]=ifelse((div4energy$Class[i] == "") & (div4energy$Class[(i-1)] != ""),
                          div4energy$Class[(i-1)],
                          div4energy$Class[i])       }

##
## Step 2: Rename the Columns
##
names(div4energy) <- c("ID", "Component", div4energy[4,3:14], "Subclass", "Code", "Class")

##
## Step 3: Make Long
##
div4energy <- reshape2::melt(div4energy,
                          id.vars = c("ID", "Component", "Subclass", "Code", "Class"),
                          variable.name = "Period")
div4energy <- div4energy[,]
div4energy <- div4energy[div4energy$ID != "",]
div4energy <- div4energy[div4energy$Component != "",]
div4energy <- div4energy[div4energy$Component != "Geomean",]
div4energy <- div4energy[div4energy$Subclass  != "",]
div4energy <- div4energy[div4energy$Component != div4energy$Subclass,]

Categories <- unique(div4energy$Subclass)

for(i in 1:nrow(div4energy))
{
  div4energy$Code[i] <- paste0(div4energy$Code[i], ".", which(Categories == div4energy$Subclass[i]), ".", div4energy$ID[i])
  
}



div4energy$Period <- as.Date(paste0("15-",div4energy$Period ), "%d-%b-%y")

##
## Save files our produce some final output of something
##
save(div4energy, file = 'Data_Intermediate/div4energy.rda')

##
##    And we're done
##
