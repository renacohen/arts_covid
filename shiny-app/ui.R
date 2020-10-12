#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- navbarPage(
  "Final Project Title",
  tabPanel("Model",
           ggplot(afta_covid, aes(y = lost_revenue_total, fill = fct_reorder(budget, lost_revenue_total, .fun = "mean", na.rm = T)))+
             geom_boxplot() +
             ylim(0, 1000000) +
             labs(title = "Lost Revenue due to COVID in Arts Organizations of Different Sizes",
                  x = "", y = "Total Lost Revenue") +
             scale_fill_discrete(name = "Organization Budget"))
  tabPanel("Discussion",
           titlePanel("Discussion Title"),
           p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
  tabPanel("About", 
           titlePanel("About"),
           h3("Project Background and Motivations"),
           p("Hello, this is where I talk about my project."),
           h3("About Me"),
           p("My name is ______ and I study ______. 
             You can reach me at ______@college.harvard.edu.")))
