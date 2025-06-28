##
##    Programme:  Make_a_Picture.r
##
##    Objective:  New Zealand and Australia are together.  Now graph them
##              
##    Author:    James Hogan, NZIER, 8 April 2019
##
   rm(list=ls(all=TRUE))
   
   load("data_intermediate/Australia_and_NZ_Expenditure.rda")
   source("R/themes.r")
   source("R/functions.r")
   
  ##
  ##    Reorder the Employee Sizes
  ##
      Expenditure_Proportions <- Australia_and_NZ_Expenditure[(!(Australia_and_NZ_Expenditure$Measure   %in% c('UNKNOWN', 'Total', 'Other expenditure')) &
                                                              (!(Australia_and_NZ_Expenditure$variable  %in% c('Aust.', 'New Zealand', 'Rest of North Island', 'Rest of South Island')))),]    
      Expenditure_Proportions$variable <- factor(Expenditure_Proportions$variable, levels = c("Melbourne", "Brisbane","Adelaide","Perth","Hobart","Darwin","Canberra(b)","Auckland","Wellington","Canterbury"),
                                                                                 labels = c("Melbourne", "Brisbane","Adelaide","Perth","Hobart","Darwin","Canberra","Auckland","Wellington","Canterbury"))
      Expenditure_Proportions$Country <- factor(Expenditure_Proportions$Country, levels = c("Australia", "New Zealand"),
                                                                                 labels = c("Australia", "New Zealand"))   
                                                                                 
      png("graphics/WeeklySpend.png", w = 15.7, h = 8.3, res = 1200, units = "in")
      ggplot(Expenditure_Proportions[Expenditure_Proportions$value >0,], 
               aes(x=variable, weight=Proportion, fill=Country)) +
            geom_bar() + 
            coord_flip() +
            xlab("") +
            ylab("Percentage of weekly expenditure") +
            scale_fill_manual(values =nzier.cols(), name="Proportion of Weekly               \nIncome by City               \n") +
            facet_grid(wrap(Measure) ~ .) +
            scale_y_continuous(labels= percent, breaks=seq(-0,.3,.025)) +
            theme_NZIER2() %+replace%
             theme(legend.title.align=0.5,
                plot.margin = unit(c(1,3,1,1),"mm"),
                legend.text  = element_text(size=12),
                axis.text.x  = element_text(angle=00, size=8),
                axis.text.y  = element_text(angle=00, size=7),
                axis.title.y  = element_text(angle=90, size=7.5),
                strip.text.y  = element_text(angle=360, size=7.5),
                plot.title = element_text(size = 12),
                legend.key.width = unit(1, "cm"),
                legend.spacing.y = unit(0, "cm"),
                legend.margin = margin(0, 0, 0, 0),
                legend.position  = "bottom")               
      dev.off() 
##
##    And we're done
##
