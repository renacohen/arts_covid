#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

afta_covid <- readRDS(file = "processed_data/afta_covid.RDS")

library(shiny)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)

shinyServer(function(input, output) {

    output$plot1 <- renderPlot({
      data1 <- afta_covid %>%
        filter(received_financial_assistance == input$variable)

        ggplot(data1, aes(y = severity_financial_impact))+
          geom_bar(fill = "lightblue", color = "black")+
          labs(title = "Considering Federal Aid and Financial Impact", 
               subtitle = "Organizations who recieved federal aid reported higher
               financial impact due to the pandemic, on average",
               x = "Number of Organizations", y = "Severity of Financial Impact") +
          theme_classic() 

    })
    
    output$plot2 <- renderPlot({
      data2 <- afta_covid %>%
        filter(state == input$var2) 
      
      ggplot(data2, aes(x = budget, fill = budget)) +
        geom_bar() +
        labs(title = "Survey Responses by Organizational Budget", x = "Budget",
             y = "Number of Organizations") +
        theme(axis.text.x = element_text(size = 5)) +
        theme_classic() +
        scale_fill_brewer(palette = "YlOrRd", name = "Budget") +
        theme(axis.text.x = element_blank()) 
        
        
    })
    
    output$plot3 <- renderPlot ({
      data2 <- afta_covid %>%
        filter(state == input$var2)
      ggplot(data2, aes(x = legal_status, fill = legal_status))+
        geom_bar() +
        theme(axis.text.x = element_text(size = 5)) +
        theme_classic() +
        labs(title = "Survey Responses by Legal Status", x = "Legal Status", 
             y = "Number of Organizations") +
        scale_fill_brewer(palette = "BuGn", name = "Legal Status") +
        theme(axis.text.x = element_blank()) 
      
    })
    
    output$plot4 <- renderPlot ({
      data2 <- afta_covid %>%
        filter(state == input$var2)
      ggplot(data2, aes(x = purpose, fill = purpose))+
        geom_bar() +
        theme(axis.text.x = element_text(size = 5)) +
        theme_classic() +
        labs(title = "Survey Responses by Organizational Purpose", x = "Organizational Purpose",
             y = "Number of Organizations") +
        scale_fill_brewer(palette = "Set3", name = "Organizational Purpose") +
        theme(axis.text.x = element_blank())
    })
    
    output$text <- renderText({
      data2 <- afta_covid %>%
        filter(state == input$var2)
      paste("There are", (nrow(data2)), "survey responses from", input$var2) 
      
    })

})
