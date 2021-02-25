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
library(here)
library(sf)
library(tmap)
library(janitor)

### read in shapefiles
county <- read_sf(here("Flood_Forward", "transfer_for_shiny", "county_boundary_project", "county_boundary_project.shp")) %>% 
    clean_names()

chowchilla <- read_sf(here("Flood_Forward", "transfer_for_shiny", "chowchilla_project", "chowchilla_project.shp"))

madera <- read_sf(here("Flood_Forward", "transfer_for_shiny", "madera_project", "madera_project.shp"))

subbasins <- rbind(chowchilla, madera) %>% 
    rename("name" = "Basin_Su_1")

nhd_flowlines <- read_sf(here("Flood_Forward", "transfer_for_shiny", "nhd_flowlines", "nhd_flowlines.shp"))

watersheds <- read_sf(here("Flood_Forward", "transfer_for_shiny", "watersheds_dissolve", "watersheds_dissolve.shp")) %>% 
    mutate(final_priority_ranking = case_when(
        watersheds$Name == "Ash Slough" ~ 2,
        watersheds$Name == "Berenda Slough" ~ 7,
        watersheds$Name == "Berenda Creek" ~ 8,
        watersheds$Name == "Buttonwillow Slough" ~ 3,
        watersheds$Name == "Chowchilla River" ~ 9,
        watersheds$Name == "Cottonwood Creek" ~ 4,
        watersheds$Name == "Dry Creek" ~ 6,
        watersheds$Name == "Fresno River" ~ 1,
        watersheds$Name == "San Joaquin River" ~ 5,
    ))

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cerulean"),
                
    navbarPage("Flood Forward",
               tabPanel("Home",
                        titlePanel("Flood Forward")),
               tabPanel("About the Project"),
               tabPanel("Data"),
               tabPanel("Study Area Overview",
                        tmapOutput("madera_overview_tmap")),
               tabPanel("Flood Risk Analysis",
                        sidebarLayout(
                            sidebarPanel(
                                radioButtons("flood_risk",
                                             "Select Flood Risk result(s)",
                                             choices = c("Final Priority Ranking" = "final_priority_ranking", "FEMA Flood Hazard" = "flood_area", "Catchment Area" = "AreaAcres", "Fire Hazard" = "fire_area_", "Disadvantaged Communities" = "area_DAC_a", "Levee Failures" = "Levee_fail"),
                                             selected = ("Final Priority Ranking" = "final_priority_ranking"))
                            ),
                            mainPanel("Flood Risk Analysis Results",
                                      plotOutput("floods_plot"))
                        )),
               tabPanel("Ecosystem Enhancement Analysis",
                        sidebarLayout(
                            sidebarPanel(
                                radioButtons("ecosystem_priority_map",
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
    ### 1) Study Area Overview tmap
    output$madera_overview_tmap = renderTmap({
        tmap_mode("view")
        
        tm_shape(county)+
            tm_polygons("name")+
            tm_shape(subbasins)+
            tm_polygons("name")+
            tm_shape(watersheds)+
            tm_polygons("Name")+
            tm_shape(nhd_flowlines)+
            tm_lines(col = "blue")
    })
    
    ### 2) flood risk analysis reactive map
    floods_select <- reactive({
        watersheds %>%
            select(c(Name, input$flood_risk)) %>% 
            rename("column_name" = input$flood_risk)
    })
    
    output$floods_plot <- renderPlot({
        
        ggplot(data = floods_select()) +
            geom_sf(aes(fill = column_name))
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
