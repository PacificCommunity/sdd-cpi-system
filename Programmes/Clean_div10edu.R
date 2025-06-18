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

load("Data_Intermediate/RAWDATA_Div 10, 11, 12, Ed. Rest. MicelXXCPI_calculation_Xmas_Test_Workbook_2025.rda")
div10edu <- `RAWDATA_Div 10, 11, 12, Ed. Rest. MicelXXCPI_calculation_Xmas_Test_Workbook_2025`
##
## Step 1: Fill in the blanks
## 
div10edu$Subclass = ""
div10edu$Code     = ""
div10edu$Class = ""


for (i in 1:nrow(div10edu)) 
{
  div10edu$Subclass[i]=ifelse(str_detect(div10edu$V1[i],"\\."),div10edu$V2[i],"") 
  div10edu$Code[i]=ifelse(str_detect(div10edu$V1[i],"\\."),div10edu$V1[i],"") 
  div10edu$Class[i]=ifelse(str_detect(div10edu$V1[i],"\\."),
                           ifelse(div10edu$V2[(i-1)] == "", div10edu$V2[(i-2)], div10edu$V2[(i-1)]),
                           "") 
  div10edu$Class[i]=ifelse(div10edu$Class[i]=="Geomean", "",div10edu$Class[i])
}

for (i in 2:nrow(div10edu)) 
{
  div10edu$Subclass[i]=ifelse((div10edu$Subclass[i] == "") & (div10edu$Subclass[(i-1)] != ""),
                              div10edu$Subclass[(i-1)],
                              div10edu$Subclass[i]) 
  
  div10edu$Code[i]=ifelse((div10edu$Code[i] == "") & (div10edu$Code[(i-1)] != ""),
                          div10edu$Code[(i-1)],
                          div10edu$Code[i]) 
  
  div10edu$Class[i]=ifelse((div10edu$Class[i] == "") & (div10edu$Class[(i-1)] != ""),
                           div10edu$Class[(i-1)],
                           div10edu$Class[i])       }

##
## Step 2: Rename the Columns
##
names(div10edu) <- c("ID", "Component", div10edu[4,3:14], "Subclass", "Code", "Class")

##
## Step 3: Make Long
##
div10edu <- reshape2::melt(div10edu,
                           id.vars = c("ID", "Component", "Subclass", "Code", "Class"),
                           variable.name = "Period")
div10edu <- div10edu[,]
div10edu <- div10edu[div10edu$ID != "",]
div10edu <- div10edu[div10edu$Component != "",]
div10edu <- div10edu[div10edu$Component != "Geomean",]
div10edu <- div10edu[div10edu$Subclass  != "",]
div10edu <- div10edu[div10edu$Component != div10edu$Subclass,]

Categories <- unique(div10edu$Subclass)

for(i in 1:nrow(div10edu))
{
  div10edu$Code[i] <- paste0(div10edu$Code[i], ".", which(Categories == div10edu$Subclass[i]), ".", div10edu$ID[i])
  
}



div10edu$Period <- as.Date(paste0("15-",div10edu$Period ), "%d-%b-%y")

##
## Save files our produce some final output of something
##
save(div10edu, file = 'Data_Intermediate/div10edu.rda')

##
##    And we're done
##
