
#

library(shiny)
library(tidyverse)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
      ggplot(afta_covid, aes(y = lost_revenue_total, fill = fct_reorder(budget, lost_revenue_total, .fun = "mean", na.rm = T)))+
        geom_boxplot() +
        ylim(0, 1000000) +
        labs(title = "Lost Revenue due to COVID in Arts Organizations of Different Sizes",
             x = "", y = "Total Lost Revenue") +
        scale_fill_discrete(name = "Organization Budget") 
    })

})
