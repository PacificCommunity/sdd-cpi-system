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

load("Data_Intermediate/RAWDATA_Div 8, 9 Coms & RecreationXXCPI_calculation_Xmas_Test_Workbook_2025.rda")
div8coms <- `RAWDATA_Div 8, 9 Coms & RecreationXXCPI_calculation_Xmas_Test_Workbook_2025`
##
## Step 1: Fill in the blanks
## 
div8coms$Subclass = ""
div8coms$Code     = ""
div8coms$Class = ""


for (i in 1:nrow(div8coms)) 
{
  div8coms$Subclass[i]=ifelse(str_detect(div8coms$V1[i],"\\."),div8coms$V2[i],"") 
  div8coms$Code[i]=ifelse(str_detect(div8coms$V1[i],"\\."),div8coms$V1[i],"") 
  div8coms$Class[i]=ifelse(str_detect(div8coms$V1[i],"\\."),
                             ifelse(div8coms$V2[(i-1)] == "", div8coms$V2[(i-2)], div8coms$V2[(i-1)]),
                             "") 
  div8coms$Class[i]=ifelse(div8coms$Class[i]=="Geomean", "",div8coms$Class[i])
}

for (i in 2:nrow(div8coms)) 
{
  div8coms$Subclass[i]=ifelse((div8coms$Subclass[i] == "") & (div8coms$Subclass[(i-1)] != ""),
                                div8coms$Subclass[(i-1)],
                                div8coms$Subclass[i]) 
  
  div8coms$Code[i]=ifelse((div8coms$Code[i] == "") & (div8coms$Code[(i-1)] != ""),
                            div8coms$Code[(i-1)],
                            div8coms$Code[i]) 
  
  div8coms$Class[i]=ifelse((div8coms$Class[i] == "") & (div8coms$Class[(i-1)] != ""),
                             div8coms$Class[(i-1)],
                             div8coms$Class[i])       }

##
## Step 2: Rename the Columns
##
names(div8coms) <- c("ID", "Component", div8coms[4,3:14], "Subclass", "Code", "Class")

##
## Step 3: Make Long
##
div8coms <- reshape2::melt(div8coms,
                             id.vars = c("ID", "Component", "Subclass", "Code", "Class"),
                             variable.name = "Period")
div8coms <- div8coms[,]
div8coms <- div8coms[div8coms$ID != "",]
div8coms <- div8coms[div8coms$Component != "",]
div8coms <- div8coms[div8coms$Component != "Geomean",]
div8coms <- div8coms[div8coms$Subclass  != "",]
div8coms <- div8coms[div8coms$Component != div8coms$Subclass,]

Categories <- unique(div8coms$Subclass)

for(i in 1:nrow(div8coms))
{
  div8coms$Code[i] <- paste0(div8coms$Code[i], ".", which(Categories == div8coms$Subclass[i]), ".", div8coms$ID[i])
  
}



div8coms$Period <- as.Date(paste0("15-",div8coms$Period ), "%d-%b-%y")

##
## Save files our produce some final output of something
##
save(div8coms, file = 'Data_Intermediate/div8coms.rda')

##
##    And we're done
##
