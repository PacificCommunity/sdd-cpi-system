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

load("Data_Intermediate/RAWDATA_Div 6, 7 health & TransportXXCPI_calculation_Xmas_Test_Workbook_2025.rda")
div6health <- `RAWDATA_Div 6, 7 health & TransportXXCPI_calculation_Xmas_Test_Workbook_2025`
##
## Step 1: Fill in the blanks
## 
div6health$Subclass = ""
div6health$Code     = ""
div6health$Class = ""


for (i in 1:nrow(div6health)) 
{
  div6health$Subclass[i]=ifelse(str_detect(div6health$V1[i],"\\."),div6health$V2[i],"") 
  div6health$Code[i]=ifelse(str_detect(div6health$V1[i],"\\."),div6health$V1[i],"") 
  div6health$Class[i]=ifelse(str_detect(div6health$V1[i],"\\."),
                             ifelse(div6health$V2[(i-1)] == "", div6health$V2[(i-2)], div6health$V2[(i-1)]),
                             "") 
  div6health$Class[i]=ifelse(div6health$Class[i]=="Geomean", "",div6health$Class[i])
}

for (i in 2:nrow(div6health)) 
{
  div6health$Subclass[i]=ifelse((div6health$Subclass[i] == "") & (div6health$Subclass[(i-1)] != ""),
                                div6health$Subclass[(i-1)],
                                div6health$Subclass[i]) 
  
  div6health$Code[i]=ifelse((div6health$Code[i] == "") & (div6health$Code[(i-1)] != ""),
                            div6health$Code[(i-1)],
                            div6health$Code[i]) 
  
  div6health$Class[i]=ifelse((div6health$Class[i] == "") & (div6health$Class[(i-1)] != ""),
                             div6health$Class[(i-1)],
                             div6health$Class[i])       }

##
## Step 2: Rename the Columns
##
names(div6health) <- c("ID", "Component", div6health[4,3:14], "Subclass", "Code", "Class")

##
## Step 3: Make Long
##
div6health <- reshape2::melt(div6health,
                             id.vars = c("ID", "Component", "Subclass", "Code", "Class"),
                             variable.name = "Period")
div6health <- div6health[,]
div6health <- div6health[div6health$ID != "",]
div6health <- div6health[div6health$Component != "",]
div6health <- div6health[div6health$Component != "Geomean",]
div6health <- div6health[div6health$Subclass  != "",]
div6health <- div6health[div6health$Component != div6health$Subclass,]

Categories <- unique(div6health$Subclass)

for(i in 1:nrow(div6health))
{
  div6health$Code[i] <- paste0(div6health$Code[i], ".", which(Categories == div6health$Subclass[i]), ".", div6health$ID[i])
  
}



div6health$Period <- as.Date(paste0("15-",div6health$Period ), "%d-%b-%y")

##
## Save files our produce some final output of something
##
save(div6health, file = 'Data_Intermediate/div6health.rda')

##
##    And we're done
##
