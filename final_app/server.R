#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

afta_covid <- readRDS(file = "processed_data/afta_covid.RDS")
afta_covid_2 <- readRDS(file = "processed_data/afta_covid_2.RDS")
map_data <- readRDS(file = "processed_data/map_data.RDS")
word_data <- readRDS("processed_data/word_data.RDS")
responses_coded <- readRDS("processed_data/responses_coded.RDS")
unemployment_data <- readRDS("processed_data/unemployment_data.RDS")
model_final <- readRDS("processed_data/model_final.RDS")
covid_rates <- readRDS("processed_data/covid_rates.RDS")
model_comparison <- readRDS("processed_data/model_comparison.RDS")
model_vars <- readRDS("processed_data/model_vars.RDS")
model_final_tbl <- readRDS("processed_data/model_final_tbl.RDS")

library(shiny)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(ggmap)
library(gt)
library(treemap)
library(rstanarm)
library(tidymodels)
library(shinythemes)
library(leaflet)
library(wordcloud)
library(tm)
library(wordcloud2)
library(gganimate)
library(broom.mixed)
library(gtsummary)
library(rstanarm)
library(markdown)
library(Rcpp)


#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



shinyServer(function(input, output) {
  
  cute_pal <- c("thistle", "lavenderblush2", "lightblue1", "thistle2", 
                "lightblue3", "azure3", "darkmagenta", "maroon","plum4")
  
  
  cute_pal_hex <- c("#D8BFD8", "#EEE0E5", "#BFEFFF", "#EED2EE", "#9AC0CD", 
                    "#C1CDCD", "#8B008B", "#B03060", "#8B668B")
  
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
      select(purpose) %>%
      group_by(purpose) %>%
      summarise(value = n(), .groups = "drop") %>%
      treemap(index = "purpose", vSize = "value", palette = cute_pal_hex,
              border.col = "white", 
              title = "Organization Types")
    
  })
  
  output$plot1 <- renderPlot ({
    afta_covid_2 %>%
      ggplot(aes(x = budget, fill = legal_status))  + 
      geom_bar(color = "plum4") +
      theme_classic() +
      theme(axis.text.x = element_text(size = 5, angle = 45, vjust = 0.6)) +
      scale_fill_manual(values = cute_pal, name = "Legal Status") +
      labs(title = "Organizations by Budget and Legal Status",
           subtitle = "Most respondents are nonprofits with smaller budgets",
           x = "Budget", y = "Number of Organizations")
    
  })
  
  output$map <- renderLeaflet({
    
    leaflet(map_data) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lat = 39.8283, lng = -98.5795, zoom = 4) %>%
      addCircleMarkers(lng = ~lng, lat = ~lat, weight = 1, 
                       color = "darkmagenta", fillColor = "thistle", 
                       radius = ~n, fillOpacity = 0.5, label = ~n)
  })
  
  output$cloud <- renderWordcloud2({
    wordcloud2(data = word_data, size = 1, shape = "square",color = c(rep(c("thistle", "darkmagenta", 
                                                                            "lightsteelblue", 	"#DDA0DD", 
                                                                            "#d9d9d9"), 100)))
  })
  
  output$response_table <- renderTable({
    responses_coded %>%
      filter(Sentiment == input$sentiment) %>%
      select(Comment) %>%
      gt()
    
  })
  
  output$survival_budget <- renderPlot ({
    
    afta_covid_2 %>%
      filter(budget != "No budget / all volunteer", budget != "NA") %>%
      ggplot(aes(x= survival_chances, fill = budget))+
      geom_bar(na.rm = T, color = "plum4") +
      
      # Scales = free keeps us from always having to use the same thing on the Y axis 
      
      facet_wrap(~budget, scales = "free", ncol = 2) +
      theme_classic() + 
      scale_fill_manual(values = c(cute_pal[1:6], "lightblue2")) +
      # Hiding the legend, it's really not necessary since the fill is just different 
      # colors
      theme(legend.position = "none") + 
      labs(title = "Likelihood of Survival by Organizational Budget",
           subtitle = "5 is most confident of survival, 1 is least confident", 
           x = "Self-Reported Likelihood of Organization Survival", y = "Number of Organizations")
    
  })
  
  output$animation <- renderUI({
    includeHTML("animation.html")
    
  })
  
  output$states_1 <- renderPlot({
    
    ggplot(covid_rates, aes(x = pos_rate, y = financial_severity)) +
      geom_point(alpha = 0) + 
      geom_text(aes(label = abbreviation), color = "plum4") +
      labs(title = "") +
      theme_bw() +
      geom_smooth(method = "lm", formula = y~x, color = "darkmagenta") +
      theme_bw() +
      labs(title = "Average Financial Severity vs. Covid Rates",
           subtitle = "5 = Most Severe, 1 = Least Severe", 
           x = "Population Covid Rate",
           y = "Average Financial Severity")
    
  })
  
  
  output$states_2 <- renderPlot({
    
    ggplot(covid_rates, aes(x = pos_rate, y = likelihood_staff_reductions)) +
      geom_point(alpha = 0, na.rm = T) + 
      geom_text(aes(label = abbreviation), color = "lightblue3") +
      geom_smooth(method = "lm", formula = y~x, color = "lightblue4") +
      theme_bw() +
      labs(title = "Average Likelihood of Staff Reduction vs. Covid Rates",
           subtitle = "5 = Most Likely, 1 = Least Likely", 
           x = "Population Covid Rate",
           y = "Average Likelihood of Staff Reduction")
    
  })
  
  output$states_3 <- renderPlot({
    
    covid_rates %>%
      ggplot(aes(x = pos_rate, y = survival_chances)) +
      geom_point(alpha = 0, na.rm = T) + 
      geom_text(aes(label = abbreviation), color = "thistle3") +
      geom_smooth(method = "lm",formula = y~x,  color = "thistle4") +
      theme_bw() +
      labs(title = "Average Chance of Organization Survival vs. Covid Rates",
           subtitle = "5 = Most Likely to Survive, 1 = Least Like", 
           x = "Population Covid Rate",
           y = "Average Chance of Organization Survival")
  })
  
  output$models_table <- render_gt({
    
    model_comparison %>%
      gt() %>%
      tab_header("Comparing Metrics for 3 Models")
  })
  
  output$models <- renderUI ({
    
    withMathJax(
      'Model 1: $x_{revenue,i} = \beta_1x_{lost attendees,i}$')
    
  })
  
  output$var_table <- render_gt({
    
    model_vars %>%
      gt() %>%
      tab_header("Explanation of Variables Included in 3 Models")
  })
  
  output$final_mod <- render_gt({
    
    model_final_tbl %>%
      gt() %>%
      tab_header("Output for the Final Model")
  })
  
  output$pred_plot <- renderPlot({
    
    new_obs <- tibble(lost_attendees = input$attendees)
    
    individual <- posterior_predict(model_final, newdata = new_obs) %>%
      as_tibble() %>%
      rename("Individual Predictions" = `1`) %>%
      mutate_all(as.numeric)
    average <- posterior_epred(model_final, newdata = new_obs) %>%
      as_tibble() %>%
      rename("Average Predictions" = `1`) %>%
      mutate_all(as.numeric)
    
    model_predictions <- cbind(individual, average) %>%
      pivot_longer(cols = everything(), names_to = "Type", values_to = "lost_rev") 
    
    
    ggplot(model_predictions, aes(x = lost_rev, fill = Type)) +
      geom_histogram(bins = 50, alpha = 0.7, color = "plum4",
                     position = "identity",
                     aes(y = after_stat(count/sum(count)))) +
      scale_x_continuous(labels = scales::dollar_format()) +
      scale_y_continuous(labels = scales::percent_format())+
      labs(title = "Predictions") +
      scale_fill_manual(values = cute_pal) + 
      theme_bw() +
      labs(title = "Posterior Probability Distribution",
           subtitle = "Making individual and average predictions using our chosen model", 
           x = "Predicted Lost Revenue", y = "Probability")
    
    
  })
  
})
