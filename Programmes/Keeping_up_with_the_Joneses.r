##
##    Programme:  Keeping_up_with_the_Joneses.r
##
##    Objective:  NZ HES is in the right shape as the Australian expenditure data.
##                Is the grass greener on the other side of the fence?  What's the
##                average household spend in the NZ and different Australian cities?
##              
##    Author:    James Hogan, NZIER, 7 April 2019
##
   rm(list=ls(all=TRUE))
   
   load("data_intermediate/HES_Data.rda")
   load("data_intermediate/RAWDATA_Table 13.1XX65300DO013_201516.rda")
   OZ <- `RAWDATA_Table 13.1XX65300DO013_201516`
   OZ[sapply(OZ, is.factor)] <- lapply(OZ[sapply(OZ, is.factor)], as.character)   
   NZ <- HES_Data
##
##    Remap the NZ HES Data codes onto the Australian data codes
##
   OZ_Expenditure <- OZ[5:22,1:10]
   names(OZ_Expenditure) <- c("Goods and services", OZ[4,3:10])
   OZ_Expenditure <- melt(OZ_Expenditure,
                          id.var = c("Goods and services"))
   OZ_Expenditure <-  OZ_Expenditure[ OZ_Expenditure$value != "",]
   OZ_Expenditure$value <- str_replace_all(OZ_Expenditure$value, "\\*","")
   OZ_Expenditure$value <- as.numeric(str_replace_all(OZ_Expenditure$value, ",",""))
   ##
   ##    Make an "unknown" commodity to reflect the difference between total spend and
   ##       the sum of its components
   ##
   Missing <- with(OZ_Expenditure[OZ_Expenditure$`Goods and services` != "Total goods and services expenditure",],
                aggregate(list(value = value),
                          list(variable = variable),
                          sum, 
                          na.rm = TRUE)
                          )   
   Totals <- OZ_Expenditure[OZ_Expenditure$`Goods and services` == "Total goods and services expenditure",]
   Missing <- merge(Missing,
                    Totals,
                    by = c("variable"))
   Missing$value <- with(Missing, (value.y - value.x))
   Missing$`Goods and services` = "UNKNOWN"
   OZ_Expenditure <- rbind(OZ_Expenditure,
                           Missing[,c("variable","Goods and services","value")])
##
##    Now for the actual remap
##
   OZ_Expenditure$Measure <- ifelse((OZ_Expenditure$`Goods and services`  == 'Total goods and services expenditure'),      'Total',
                             ifelse((OZ_Expenditure$`Goods and services`  == 'Food and non-alcoholic beverages'),          'Food',
                             ifelse((OZ_Expenditure$`Goods and services`  == 'Alcoholic beverages'),                       'Alcoholic beverages, tobacco and illicit drugs',
                             ifelse((OZ_Expenditure$`Goods and services`  == 'Tobacco products'),                          'Alcoholic beverages, tobacco and illicit drugs',
                             ifelse((OZ_Expenditure$`Goods and services`  == 'Clothing and footwear'),                     'Clothing and footwear',
                             ifelse((OZ_Expenditure$`Goods and services`  == 'Current housing costs (selected dwelling)'), 'Housing and household utilities',
                             ifelse((OZ_Expenditure$`Goods and services`  == 'Domestic fuel and power'),                   'Housing and household utilities',
                             ifelse((OZ_Expenditure$`Goods and services`  == 'Household furnishings and equipment'),       'Household contents and services',
                             ifelse((OZ_Expenditure$`Goods and services`  == 'Household services and operation'),          'Household contents and services',
                             ifelse((OZ_Expenditure$`Goods and services`  == 'Medical care and health expenses'),          'Health',
                             ifelse((OZ_Expenditure$`Goods and services`  == 'Transport'),                                 'Transport',
                             ifelse((OZ_Expenditure$`Goods and services`  == 'Communication'),                             'Communication',
                             ifelse((OZ_Expenditure$`Goods and services`  == 'Recreation'),                                'Recreation and culture',
                             ifelse((OZ_Expenditure$`Goods and services`  == 'Education'),                                 'Education',
                             ifelse((OZ_Expenditure$`Goods and services`  == 'Miscellaneous goods and services'),          'Miscellaneous goods and services',
                             ifelse((OZ_Expenditure$`Goods and services`  == 'Personal care'),                             'Miscellaneous goods and services',
                             'UNKNOWN'))))))))))))))))

   OZ_Expenditure <- with(OZ_Expenditure,
                      aggregate(list(value = value),
                                list(Measure = Measure,
                                    variable = variable),
                                sum, 
                                na.rm = TRUE)
                                )   
   OZ_Expenditure$Country = "Australia"

##
##    Remap the NZ HES Data codes onto the Australian data codes
##
   NZ_Expenditure <- NZ[((NZ$Category == '2016') &
                         (NZ$Measure  == 'Average weekly household expenditure') &
                         (NZ$Expenditure %in% c('Total',
                                                'Food',
                                                'Alcoholic beverages, tobacco and illicit drugs',
                                                'Clothing and footwear',
                                                'Housing and household utilities',
                                                'Household contents and services',
                                                'Health',
                                                'Transport',
                                                'Communication',
                                                'Recreation and culture',
#                                                'Contributions to savings',
                                                'Education',
                                                'Miscellaneous goods and services',
                                                'Other expenditure',
                                                'Sales, trade-ins and refunds'))),]
   NZ_Expenditure <-NZ_Expenditure[,c("Region", "Expenditure", "Year")]
   names(NZ_Expenditure) = c("variable", "Measure", "value")
   NZ_Expenditure$value <- as.numeric(NZ_Expenditure$value)
   NZ_Expenditure$variable[NZ_Expenditure$variable == "All regions"] = "New Zealand"   
   Missing <- with(NZ_Expenditure[NZ_Expenditure$Measure != "Total",],
                aggregate(list(value = value),
                          list(variable = variable),
                          sum, 
                          na.rm = TRUE)
                          )   
   Totals <- NZ_Expenditure[NZ_Expenditure$Measure == "Total",]
   Missing <- merge(Missing,
                    Totals,
                    by = c("variable"))
   Missing$value <- with(Missing, (value.y - value.x))
   Missing$`Goods and services` = "UNKNOWN"
   NZ_Expenditure$Country = "New Zealand"
##
##    Stick both countries together and derive proportions
##   
   Together <- rbind.fill(OZ_Expenditure,
                          NZ_Expenditure)

   Australia_and_NZ_Expenditure <- sqldf("Select A.Country,
                                               A.Measure,
                                               A.variable,
                                               A.value,
                                               B.Total,
                                               A.value /B.Total as Proportion 
                                         from Together A,
                                              ( Select Country,
                                                       Measure,
                                                       variable,
                                                       value as Total
                                                  from Together
                                                  where Measure = 'Total') b
                                         where (A.Country = b.Country)
                                           and (A.variable = b.variable)")

   save(Australia_and_NZ_Expenditure, file = "data_intermediate/Australia_and_NZ_Expenditure.rda")
##
##    And we're done
##
   