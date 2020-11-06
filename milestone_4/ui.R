#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


ui <- navbarPage(
    "The Impact of Coronavirus On Arts Organizations",
    tabPanel("Data from Your State",
                 titlePanel("Lost Revenue"),
                 mainPanel(selectInput("var2", "State", c("Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut",
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
      
             
                           plotOutput("plot2"),
             br(),
             br(),
             br(),
             
                           plotOutput("plot3"),
             br(),
             br(),
             br(),
             
                           plotOutput("plot4"),
              
                               
             ),
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
             You can reach me at renacohen@college.harvard.edu.")))


