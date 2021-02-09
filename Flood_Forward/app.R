#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cerulean"),
                
    navbarPage("Flood Forward",
               tabPanel("Home",
                        titlePanel("Flood Forward")),
               tabPanel("About the Project"),
               tabPanel("Data"),
               tabPanel("Study Area Overview",
                        sidebarLayout(
                            sidebarPanel(
                                checkboxGroupInput("overview_map",
                                                   "Select layer(s)",
                                                   choices = c("County", "Subbasins", "Watersheds", "Riparian"),
                                                   selected = NULL)
                            ),
                            mainPanel()
                        )),
               tabPanel("Flood Risk Analysis",
                        sidebarLayout(
                            sidebarPanel(
                                checkboxGroupInput("flood_risk_map",
                                                   "Select Flood Risk result(s)",
                                                   choices = c("Final Priority Ranking", "FEMA Flood Hazard", "Catchment Area", "Fire Hazard", "Disadvantaged Communities", "Levee Failures"),
                                                   selected = "Final Priority Ranking")
                            ),
                            mainPanel()
                        )),
               tabPanel("Ecosystem Enhancement Analysis",
                        sidebarLayout(
                            sidebarPanel(
                                checkboxGroupInput("ecosystem_priority_map",
                                                   "Select Ecosystem Priority result(s)",
                                                   choices = c("Final Priority Ranking", "Groundwater-Dependent Ecosystems", "Critical Habitats", "Native Fish Species"),
                                                   selected = "Final Priority Ranking")
                            ),
                            mainPanel()
                        )),
               tabPanel("Multiple Benefit Weighting",
                        # Sidebar with a slider input for number of bins 
                        sidebarLayout(
                            sidebarPanel(
                                sliderInput("Multi-benefit Priority",
                                            "Select preferences:",
                                            min = 1,
                                            max = 3,
                                            value = 2)
                            ),
                            
                            # Show a plot of the generated distribution
                            mainPanel()
                        )),
               tabPanel("Contact Us")),           

)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
