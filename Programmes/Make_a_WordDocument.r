##
##    Programme:  Make_a_WordDocument.r
##
##    Objective:  New Zealand and Australia are together.  Now graph them
##
##                Meet the Word Object Model: https://docs.microsoft.com/en-us/office/vba/api/overview/word/object-model
##              https://stackoverflow.com/questions/47182134/vba-word-add-caption
##    Author:    James Hogan, NZIER, 9 April 2019
##
   rm(list=ls(all=TRUE))
   
   load("data_intermediate/Australia_and_NZ_Expenditure.rda")
   source("R/themes.r")
   source("R/functions.r")
   
  ##
  ##    Reorder the Employee Sizes
  ##

   ##
   ##    Initialise some RDCOMClient parameters
   ##
      Word <- COMCreate("Word.Application")
      Word[["Visible"]] <- TRUE
      Document <- Word[["Documents"]]$Add()

      Range <- Document$Paragraphs(1)$Range()
      Range[["Text"]] <- "New Zealand’s housing conversation has always been a metaphor for different things.
When our country was young, and our towns and cities were new, we used to talk about building communities and creating spaces for families. Housing was synonymous with employment, and industry.  Governments were building New Zealand incrementally through schemes. We were building roads, towns, suburbs, schools and homes.  The state housing period focused on creating suburbs of homes fit for a modern technological age.
But since the 21st century, those progressive housing conversations has been replaced by something that is almost exclusively financial.  It’s like houses have lost whatever character made them reflect where New Zealand was at different stages in time – expansion, industry, and quality of life.
Now houses are strictly investments. We have an obsession about them.  Some of us want to own more and more of them for a number of reasons.   We no longer wax lyrically about houses as new communities or making jobs and promoting industry.  High-end home ownership is a waterfront mansion with its own boat shed, helicopter pad, pool overlooking the sea and private access to the beach.  Conspicuous consumption is alive and well through a media interested in publishing the owner’s name.   
Meanwhile, in 2013, Lorde strikes a chord with a younger predominately renting generation who “[Are] not proud of my address, in the torn-up town. No post code envy."

      Graphic <- Document$InlineShapes()
      Chart <- Graphic$AddPicture(Filename = paste0(str_replace_all(getwd(), "\\/", "\\\\"), "\\R\\NZIER.png"))
      Chart[["ScaleHeight"]] <- 50
      Chart[["ScaleWidth"]] <- 50
      Chart <- Chart$ConvertToShape()
      Shape <- Document$Shapes(1)
      Shape[["Top"]] <- 250
      Wrappit <- Shape$WrapFormat()
      Wrappit[["Type"]] = 1
      Caption <- Shape$ShapeRange(1)
      Caption$InsertCaption(Label="Table")

   ##
   ##    Save
   ## 
      Document$SaveAs(FileName = paste0(str_replace_all(getwd(), "\\/", "\\\\"), "\\Output\\Word_Example.doc"))
  Word$quit()
##
##    And we're done
##
