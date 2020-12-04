#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(leaflet)
library(wordcloud)
library(tm)
library(wordcloud2)
library(gganimate)
library(broom.mixed)
library(gtsummary)
#library(rstanarm)
library(markdown)

ui <- navbarPage(theme = shinytheme("yeti"),
    "The Impact of Coronavirus On Arts Organizations",
    tabPanel("Explore Data by State",
             fluidPage(titlePanel("Background and Motivation"),
                       fluidRow(
                         
                         column(6, plotOutput("plot1")),
                         column(6, plotOutput("plot2"))
                       ))),
    tabPanel("Explore Data by State",
                 fluidPage(titlePanel("Lost Revenue"),
                 sidebarPanel(selectInput("var2", "State", c("Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut",
                                                          "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana",
                                                          "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
                                                          "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", 
                                                          "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico",
                                                          "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania",
                                                          "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont",
                                                          "Virginia", "West Virginia", "Washington", "Wisconsin"))),
             
                           textOutput("text"),
             br(),
             br(),
             br(),
             mainPanel(
      
             
                           plotOutput("plot1"),
             br(),
             br(),
             br(),
             
             
                           plotOutput("plot2"),
             br(),
             br(),
             br(),

             
             sliderInput("dates", "Choose a Range of Dates",
                         min = as.Date("2020-03-13", "%Y-%m-%d"),
                         max = as.Date("2020-09-02", "%Y-%m-%d"), 
                         value = c(as.Date("2020-03-13", timeFormat = "%Y-%m-%d"),
                                   as.Date("2020-09-02", "%Y-%m-%d"))),
             plotOutput("num_org"),
             leafletOutput("map"),

                               
             ))),
    tabPanel("Modeling Economic Impact",
             titlePanel("Modeling lost revenue"),
             h3("Why I modeled small orgs"),
             plotOutput("survival_budget"),
             mainPanel(
             h3("Here's a model")),
             sliderInput("attendees", "Number of Attendees",
                         min = 0, max = 40000,
                         value = 0, step = 200,
             )),
             #plotOutput("predplot")),
    
    tabPanel("Broadening to Pandemic",
             titlePanel("Relating Arts Organizations to COVID status"),
             #mainPanel(htmlOutput("animation"))),
    tabPanel("Exploring Textual Data",
             titlePanel("Here's some text"),
             wordcloud2Output("cloud"),
             br(),
             br(),
             br(),
             selectInput("sentiment", "Sentiment of Message", c("Fear", "Loss", "Resilience",
                                                                "Exhaustion")),
             tableOutput("response_table")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("This summer, I worked as an intern at Americans for the Arts (AFTA), a national arts advocacy organization.
               Since March, AFTA has been running a survey for arts organizations to self-report the impact that 
               coronavirus has had on their activities, economic status, and future plans, which has now garnered over 18,000
               responses. This project aims to combine this data with data about the spread of coronavirus in different states
               to determine what sort of variation exists in the impact of coronavirus on arts organizations."),
             p(a("Here is a link to my Github repo!", href = "https://github.com/renacohen/final_project/")),
             h3("About Me"),
             p("My name is Rena Cohen and I study WGS with a secondary in Statistics. 
             You can reach me at renacohen@college.harvard.edu."))))

