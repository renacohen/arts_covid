#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Loading in all of the data that I need (so many sets!)
# They're all in a processed data folder within the final app directory

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

# Loading all the libraries I need (also a lot!)

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

# Every good shiny app starts with a server input output!

shinyServer(function(input, output) {
  
  # This is the color palette that I used all throughout. I went online
  # and picked colors that I thought would look cute together, hence the name
  # "cute_pal"
  
  cute_pal <- c("thistle", "lavenderblush2", "lightblue1", "thistle2", 
                "lightblue3", "azure3", "darkmagenta", "maroon","plum4")
  
  # And here's that same palette in hex, because some of the outputs didn't
  # like words
  
  cute_pal_hex <- c("#D8BFD8", "#EEE0E5", "#BFEFFF", "#EED2EE", "#9AC0CD", 
                    "#C1CDCD", "#8B008B", "#B03060", "#8B668B")
  
  # This plot shows the number of organizations that answered the survey per
  # day. It allows the user to choose the range of dates it displays. Got 
  # the idea for the lollipop plot from the R graph gallery
  
  output$num_org <- renderPlot({
    
    # First making a new data frame with a column that has the number of 
    # organizations that answered the survey per day. 
    
    num_per_day <- afta_covid %>%
      group_by(date) %>%
      
      # This creates a new column that counts the number of entries in
      # on each date
      
      summarise(per_day = n(), .groups = "drop") %>%
      
      # Now filtering this data frame so that the "minimum" date is the 
      # lower end of the input$dates slider and the "maximum" date is the
      # higher end
      
      filter(date >= input$dates[1] & date <= input$dates[2])
    
    # Using this new data frame, I made a lollipop plot to show the number
    # of organizations that responded per day
    
    ggplot(num_per_day, aes(x = date, y = per_day)) +
      
      # geom_segment allows for the lollipop plot. This is entirely stolen
      # off of r graph gallery, but basically you set x and xend to your
      # x variable, y to 0, and yend to your y variable
      
      geom_segment(aes(x = date, xend = date, y = 0, yend = per_day), 
                   color = "thistle") +
      
      # Just picking more cute colors that worked with the theme
      
      geom_point(size = 1.5, color = "maroon", fill = alpha("orchid", 0.3)) +
      theme_classic() +
      labs(title = "How Many Organizations Answered 
       the Survey Each Day?", x = "Date Survey was Taken", 
           y = "Number of Responses")
    
  })
  
  # This is a treemap that shows the breakdown of organization purpose
  # for whatever state the user chooses
  
  output$plot2 <- renderPlot({
    afta_covid_2 %>%
      
      # Filtering the data by the state the user chooses
      
      filter(state == input$var2) %>%
      select(purpose) %>%
      group_by(purpose) %>%
      
      # Counting the number of organizations with each type of purpose 
      # in each type of state. Treemap needs this value column in order
      # to do its thing
      
      summarise(value = n(), .groups = "drop") %>%
      
      # Index argument maps to the thing we want to be displayed in each 
      # category. vSize maps to how many of that thing there is. Palette
      # is one of the palettes I created earlier (but it had to be in hex)
      
      treemap(index = "purpose", vSize = "value", palette = cute_pal_hex,
              border.col = "white", 
              title = "Organization Types")
    
  })
  
  # This is just a basic barplot meant to give the user a sense of the budget
  # and legal status breakdown of the organizations who answered the survey
  
  output$plot1 <- renderPlot ({
    afta_covid_2 %>%
      
      # Mapped legal_status to fill so that each bar would be broken down
      # into legal status by color. I thought this was a pretty good way
      # to figure out a lot of info about hte data on one plot
      
      ggplot(aes(x = budget, fill = legal_status))  + 
      geom_bar(color = "plum4") +
      theme_classic() +
      
      # The budget names were super long, so I had to make them small, turn
      # them on their side, and bring them down from the graph a bit
      # in order to make the graph look ok
      
      theme(axis.text.x = element_text(size = 5, angle = 45, vjust = 0.6)) +
      
      # Scale fill manual allows me to change the color of the budget fill
      # I mapped it to cute_pal from earlier
      
      scale_fill_manual(values = cute_pal, name = "Legal Status") +
      labs(title = "Organizations by Budget and Legal Status",
           subtitle = "Most respondents are nonprofits with smaller budgets",
           x = "Budget", y = "Number of Organizations")
    
  })
  
  # Ahh the map! My pride and joy. So little code but it took so long to 
  # figure out. Note the renderLeaflet and not renderPlot
  
  output$map <- renderLeaflet({
    
    # The great thing about leaflet is you can pipe onto it! So I started
    # out with the map_data that I created in my Plots and Things Rmd and 
    # went from there.
    
    leaflet(map_data) %>% 
      
      # This is basically equivalent to ggthemes but for maps. In my case
      # it's a simple map with basic lines and a grey-white color scheme
      
      addProviderTiles(providers$CartoDB.Positron) %>%
      
      # I wanted the map to show the US in its entirety by default. 
      # Wyatt had the bright idea to google the center of the US (which is
      # what's inputted for lat and lng here) and then we played around with
      # the level of zoom until we found something that worked
      
      setView(lat = 39.8283, lng = -98.5795, zoom = 4) %>%
      
      # This adds circle markers to the map based on the rows in my map-ready
      # data set. It also changes how big the circle is based on how many
      # observations there are in that zip code (shown in the n column of 
      # map_data). The label also reflects this measurement
      
      addCircleMarkers(lng = ~lng, lat = ~lat, weight = 1, 
                       color = "darkmagenta", fillColor = "thistle", 
                       radius = ~n, fillOpacity = 0.5, label = ~n)
  })
  
  # Ahh my beautiful word cloud based on survey responses! Note the 
  # renderWordcloud2 function
  
  output$cloud <- renderWordcloud2({
    
    # I'm still not quite sure if specifying the size and shape actually
    # made a difference
    
    wordcloud2(data = word_data, size = 1, 
               shape = "square",
               
               # For this color argument, if I wanted to customize I needed
               # a vector where the order of colors corresponded to the order
               # that I wanted those colors assigned to words. In order to 
               # make this look pretty and random, I took a short list of 
               # colors and repeated it 100 times (more than the total)
               # number of words in the dataset
               
               color = c(rep(c("thistle", "darkmagenta", 
                                                "lightsteelblue", 	"#DDA0DD", 
                                                "#d9d9d9"), 100)))
  })
  
  # This is a table that shows responses to the open-ended survey questions
  # based on a sentiment chosen by the user
  
  output$response_table <- renderTable({
    
    # Using my responses_coded data from earlier
    
    responses_coded %>%
      
      # Filtering by the sentiment that the reader chooses
      
      filter(Sentiment == input$sentiment) %>%
      
      # Selecting the comment itself to be displayed (the other column is
      # sentiment, which we don't care about since the user has already chosen
      # it)
      
      select(Comment) %>%
      
      # Gt() gets us oour table
      
      gt()
    
  })
  
  # This plot shows the distribution of survival likelihoods by organization
  # size. It's a barplot with facet_wrap
  
  output$survival_budget <- renderPlot ({
    
    afta_covid_2 %>%
      
      # I don't really know what it would mean for an organization with no
      # budget not to survive the pandemic, but likely it wouldn't be for 
      # financial reasons. In any case, it seemed silly to include them in
      # this visualization
      
      filter(budget != "No budget / all volunteer", budget != "NA") %>%
      
      # Mapping survival_chances to the x aesthetic and fill to budget so that
      # when we facet later, each graph will be a different color
      
      ggplot(aes(x= survival_chances, fill = budget)) +
      geom_bar(na.rm = T, color = "plum4") +
      
      # Scales = free keeps us from always having to use the same thing 
      # on the Y axis. ncol = 2 just worked best for the formatting
      
      facet_wrap(~budget, scales = "free", ncol = 2) +
      theme_classic() + 
      
      # I wanted the first six colors from cute_pal and then one more color 
      
      scale_fill_manual(values = c(cute_pal[1:6], "lightblue2")) +
      
      # Hiding the legend, it's really not necessary since the fill is 
      # just different colors
      
      theme(legend.position = "none") + 
      
      # Adding labels
      
      labs(title = "Likelihood of Survival by Organizational Budget",
           subtitle = "5 is most confident of survival, 1 is least confident", 
           x = "Self-Reported Likelihood of Organization Survival", 
           y = "Number of Organizations")
    
  })
  
  # This animation was created in animation.RMD and saved as an html
  # as animation.html. It shows the unemployment rates in various sectors 
  # over the first 9 months of the pandemic
  
  output$animation <- renderUI({
    includeHTML("animation.html")
    
  })
  
  # This plot shows the relationship (or in this case, lack thereof) between
  # COVID-19 rates and lost revenue in arts organizations by state for small
  # organizations
  
  output$states_1 <- renderPlot({
    
    covid_rates %>%
      filter(abbreviation != "NY") %>%
    
    # The x axis of this scatterplot will show the positive COVID rate
    # and the y-axis the average financial severity within that state on a  
    # one to five scale. Note that the data is already at state level from
    # before when I saved it as an RDS
    
    ggplot(aes(x = pos_rate, y = financial_severity)) +
      
      # Rather than points, I wanted the labels to be the states. So first 
      # I had to get rid of the points
      
      geom_point(alpha = 0) + 
      
      # This allows each state's point to be its abbreviation. I actually got
      # this idea from one of the problem sets!
      
      geom_text(aes(label = abbreviation), color = "plum4") +
      
      # Adding a trendline (it's basically flat) to prove my point that
      # there's no relationship between COVID rate and financial impact
      # on a statewide level
      
      geom_smooth(method = "lm", formula = y~x, color = "darkmagenta") +
      
      # Adding a theme
      
      theme_bw() +
      
      # Adding labels
      
      labs(title = "Average Financial Severity vs. Covid Rates",
           subtitle = "5 = Most Severe, 1 = Least Severe", 
           x = "Population Covid Rate",
           y = "Average Financial Severity")
    
  })
  
  # This is the exact same graph as above but with a different color and with a 
  # slightly different different y variable (average likelihood of staff
  # reductions as opposed to severity of financial impact)
  
  output$states_2 <- renderPlot({
    
    covid_rates %>%
      filter(abbreviation != "NY") %>%
    
    ggplot(aes(x = pos_rate, y = likelihood_staff_reductions)) +
      geom_point(alpha = 0, na.rm = T) + 
      geom_text(aes(label = abbreviation), color = "lightblue3") +
      geom_smooth(method = "lm", formula = y~x, color = "lightblue4") +
      theme_bw() +
      labs(title = "Average Likelihood of Staff Reduction vs. Covid Rates",
           subtitle = "5 = Most Likely, 1 = Least Likely", 
           x = "Population Covid Rate",
           y = "Average Likelihood of Staff Reduction")
    
  })
  
  # Same deal but a different y variable/color again: in this case, average
  # chance of organization survival
  
  output$states_3 <- renderPlot({
    
    covid_rates %>%
      filter(abbreviation != "NY") %>%
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
  
  # This table shows the RMSE for the three different models I tried
  
  output$models_table <- render_gt({
    
    model_comparison %>%
      gt() %>%
      
      # Only real formatting I needed to do was add a title, which could happen
      # through tab_header
      
      tab_header("Comparing Metrics for 3 Models")
  })
  

  # This table shows the variables included in each of my three models...
  # again, not much to do since the heavy lifting is in my Model.Rmd
  
  output$var_table <- render_gt({
    
    model_vars %>%
      gt() %>%
      tab_header("Explanation of Variables Included in 3 Models")
  })
  
  # Same deal as before. This table gives the coefficients for my final model
  # See model.Rmd for more information on how this was compiled
  
  output$final_mod <- render_gt({
    
    model_final_tbl %>%
      gt() %>%
      tab_header("Output for the Final Model")
  })
  
  # This plot allows the user to pick a number of lost attendees and then
  # calculates a posterior distribution for lost revenue based on my 
  # chosen model
  
  output$pred_plot <- renderPlot({
    
    # Creating a "new observation" based on the number of attendees the 
    # user selects from a slider
    
    new_obs <- tibble(lost_attendees = input$attendees)
    
    # Creating a dataset for predictions for an individual organization. 
    # For individuals, we use posterior_predict
    
    individual <- posterior_predict(model_final, newdata = new_obs) %>%
      
      # Putting this output in tibble form so that it's usable
      
      as_tibble() %>%
      
      # Renaming the sole column into something meaningful
      rename("Individual Predictions" = `1`) %>%
      
      # Making the column numeric so that ggplot won't get mad
      
      mutate_all(as.numeric)
    
    # Now I will repeat the exact same process, but using posterior_epred
    # instead of posterior_predict, which will give us predictions for 
    # the true average lost revenue of organizations at a particular
    # number of lost attendees
    
    average <- posterior_epred(model_final, newdata = new_obs) %>%
      as_tibble() %>%
      rename("Average Predictions" = `1`) %>%
      mutate_all(as.numeric)
    
    # Model_predictions will be the data we pass into our ggplot. First
    # I combined the individual and average tibbles
    
    model_predictions <- cbind(individual, average) %>%
      
      # In order to be able to make the plot the way I wanted, I needed 
      # all the predictions in one column and then another column to say
      # whether they were for an individual or an average. Pivot_longer 
      # accomplishes this
      
      pivot_longer(cols = everything(), names_to = "Type", 
                   values_to = "lost_rev") 
    
    
    # Finally time to make the plot! I mapped fill to type so they'd each
    # be different colors
    
    ggplot(model_predictions, aes(x = lost_rev, fill = Type)) +
      
      # Position = identity allows us to more easily see the two different
      # types of predictions, as does the alpha = 0.5. Bins was chosen fairly
      # arbitrarily. Color here corresponds to the outline of the bins
      
      geom_histogram(bins = 50, alpha = 0.7, color = "plum4",
                     position = "identity",
                     aes(y = after_stat(count/sum(count)))) +
      
      # Our x is lost revenue so it should be in dollars
      
      scale_x_continuous(labels = scales::dollar_format()) +
      
      # This is a probability distribution, so the y's should be formatted as 
      # percentages
      
      scale_y_continuous(labels = scales::percent_format()) +
      
      # As always, gotta bring in cute_pal and the classic theme_bw
      
      scale_fill_manual(values = cute_pal) + 
      theme_bw() +
      
      # Titling the graph
      
      labs(title = "Posterior Probability Distribution",
           subtitle = "Making individual and average predictions using our chosen model", 
           x = "Predicted Lost Revenue", y = "Probability")
    
    
  })
  
})
