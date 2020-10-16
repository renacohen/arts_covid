#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyverse)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$histogram <- renderPlot({

        ggplot(afta_covid, aes(y = lost_revenue_total, fill = 
                                   fct_reorder(budget, lost_revenue_total, .fun = "mean", na.rm = T)))+
            geom_boxplot(na.rm = T) +
            ylim(0, 1000000) +
            labs(title = "Lost Revenue due to COVID in Arts Organizations of Different Sizes",
                 x = "", y = "Total Lost Revenue") +
            scale_fill_discrete(name = "Organization Budget") 

    })

})
