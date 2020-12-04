#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distPlot <- renderPlot({

        plot1 <- mtcars %>% 
            ggplot(aes(x = mpg, y = hp)) +
            xlim(input$x_axis_range)
        
        if (input$color == T) 
            plot1 +
            geom_point(color = "red")
        if(input$color == F)
            plot1 +
            geom_point(color = input$change_color)

    })

})
