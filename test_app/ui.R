#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("MT Cars Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("x_axis_range", # the name of our input
                        "X-axis Range:", # what the user will see
                        min = 0,
                        max = 40,
                        value = c(10,30)),
            selectInput("change_color", "Change the color of the plot", 
                          choices = c("red", "palegreen", "blue")),
            checkboxInput("color", "Check for Pink", F)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
))
