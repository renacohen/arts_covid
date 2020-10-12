
#

library(shiny)
library(tidyverse)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        mtcars %>%
            ggplot(aes(x = mpg, y = hp)) +
            geom_point()
    })

})
