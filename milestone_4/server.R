#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

afta_covid <- readRDS(file = "processed_data/afta_covid.RDS")
afta_covid_2 <- readRDS(file = "processed_data/afta_covid_2.RDS")
map_data <- readRDS(file = "processed_data/map_data.RDS")
word_data <- readRDS("~/Desktop/GOV 50/final_project/milestone_4/processed_data/word_data.RDS")
responses_coded <- readRDS("~/Desktop/GOV 50/final_project/milestone_4/processed_data/responses_coded.RDS")

library(shiny)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(ggmap)
library(gt)

shinyServer(function(input, output) {
  
  cute_pal <- c("thistle", "lavenderblush2", "lightblue1", "thistle2", 
                "lightblue3", "azure3", "darkmagenta", "maroon","plum4")
  
  output$num_org <- renderPlot({
    num_per_day <- afta_covid %>%
      group_by(date) %>%
      summarise(per_day = n(), .groups = "drop") %>%
      filter(date >= input$dates[1] & date <= input$dates[2])
    
    ggplot(num_per_day, aes(x = date, y = per_day)) +
      geom_segment(aes(x = date, xend = date, y = 0, yend = per_day), 
                   color = "thistle") +
      geom_point(size = 1.5, color = "maroon", fill = alpha("orchid", 0.3)) +
      theme_classic() +
      labs(title = "How Many Organizations Answered 
       the Survey Each Day?", x = "Date Survey was Taken", 
           y = "Number of Responses")
    
    
  })
  
  output$plot2 <- renderPlot({
    afta_covid_2 %>%
      filter(state == input$var2) %>%
      ggplot(aes(x = purpose)) + 
      geom_bar(color = "plum4", fill = "lavenderblush2") +
      theme_classic() +
      scale_x_discrete(labels = c("Literary \nart", "Local/regional/\nstate arts agency", "Media arts", "Other",
                                  "Performing Arts", "School/\n College / University", "Visual arts/\n Museum")) + 
      theme(axis.text.x = element_text(size = 8)) +
      scale_fill_manual(values = cute_pal, name = "Legal Status") +
      labs(title = "Organizations by Type",
           subtitle = "Plurality of Respondents are Performing Arts Organizations",
           x = "Organization Purpose", y = "Number of Organizations")

    })
    
    output$plot1 <- renderPlot ({
      afta_covid_2 %>%
        filter(state == input$var2) %>%
        ggplot(aes(x = budget, fill = legal_status))  + 
        geom_bar(color = "plum4") +
        theme_classic() +
        theme(axis.text.x = element_text(size = 5, angle = 45, vjust = 0.6)) +
        scale_fill_manual(values = cute_pal, name = "Legal Status") +
        labs(title = "Organizations by Budget and Legal Status",
             subtitle = "Most respondents are nonprofits with smaller budgets",
             x = "Budget", y = "Number of Organizations")
      
    })
    
    output$text <- renderText({
      data2 <- afta_covid %>%
        filter(state == input$var2)
      paste("There are", (nrow(data2)), "survey responses from", input$var2) 
      
    })
    
    output$plot_rev <- renderPlot({
      
      state_and_size <- afta_covid %>%
        filter(date <= "2020-06-01") %>%
        group_by(state, budget) %>%
        summarise(av_rev_loss = mean(lost_revenue_total, na.rm = T)) %>%
        filter(budget == input$orgsize)
      
      ggplot(data = state_and_size, aes(x = av_rev_loss)) +
        geom_histogram(fill = "lightblue", color = "black", na.rm = T)+
        theme_classic() +
        labs(title = "Average Lost Revenue", 
             subtitle = "Choose an organization size",
             x = "Average Revenue Loss (USD)", y = "Number of States") +
        geom_vline(xintercept = mean(state_and_size$av_rev_loss,
                                     na.rm = T), color = "maroon", lwd = 1.5, 
                   lty = 4) +
        geom_text(x = 1.25*mean(state_and_size$av_rev_loss,
                                na.rm = T), y = 6, label = "Mean lost revenue",
                  color = "maroon") +
        scale_y_continuous()
    })
    
    output$map <- renderLeaflet({
      
      leaflet(map_data) %>% 
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lat = 39.8283, lng = -98.5795, zoom = 4) %>%
        addCircleMarkers(lng = ~lng, lat = ~lat, weight = 1, 
                         color = "darkmagenta", fillColor = "thistle", 
                         radius = ~n, fillOpacity = 0.5, label = ~n)
    })
    
    output$survival_budget <- renderPlot ({
      
      afta_covid_2 %>%
        filter(budget != "No budget / all volunteer", budget != "NA") %>%
        ggplot(aes(x= survival_chances, fill = budget))+
        geom_bar(na.rm = T, color = "plum4") +
        
        # Scales = free keeps us from always having to use the same thing on the Y axis 
        
        facet_wrap(~budget, scales = "free") +
        theme_classic() + 
        scale_fill_manual(values = c(cute_pal[1:6], "lightblue2")) +
        # Hiding the legend, it's really not necessary since the fill is just different 
        # colors
        theme(legend.position = "none") + 
        labs(title = "Likelihood of Survival by Organizational Budget",
             subtitle = "5 is most confident of survival, 1 is least confident", 
             x = "Self-Reported Likelihood of Organization Survival", y = "Number of Organizations")
      
    })
    
    output$cloud <- renderWordcloud2({
      wordcloud2(data = word_data, size = 1, color = c(rep(c("thistle", "darkmagenta", 
                                                             "lightsteelblue", 	"#DDA0DD", 
                                                             "#d9d9d9"), 100)), shape = "circle")
      
    })
    
    output$response_table <- renderTable({
      responses_coded %>%
        filter(Sentiment == input$sentiment) %>%
        select(Comment) %>%
        gt()
      
      
    })

})
