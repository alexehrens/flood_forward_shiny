#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(raster)
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

watersheds <- read_sf(here("Flood_Forward", "transfer_for_shiny", "watersheds_dissolve", "watersheds_dissolve.shp"))

watershed_ranks <- watersheds %>% 
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

### read in ecosystems rasters
ecosystem_priority_rast <- raster(here("Flood_Forward", "transfer_for_shiny", "ecosystem_priority_rank", "ecosystem_priority_rank.tif"))

gde_score_rast <- raster(here("Flood_Forward", "transfer_for_shiny", "gde_score", "gde_score.tif"))

critical_habitats_rast <- raster(here("Flood_Forward", "transfer_for_shiny", "critical_habitats", "crit_habitat_score.tif"))

native_fish_rast <- raster(here("Flood_Forward", "transfer_for_shiny", "native_fish", "native_fish_reclass.tif"))

# make data frames for each raster for plotting
ecosystem_priority_df <- rasterToPoints(ecosystem_priority_rast) %>% 
    as.data.frame() %>% 
    mutate(coords = paste(ecosystem_priority_df$x, ", ", ecosystem_priority_df$y))

gde_score_df <- rasterToPoints(gde_score_rast) %>% 
    as.data.frame() %>% 
    mutate(coords = paste(gde_score_df$x, ", ", gde_score_df$y))

critical_habitats_df <- rasterToPoints(critical_habitats_rast) %>% 
    as.data.frame() %>% 
    mutate(coords = paste(critical_habitats_df$x, ", ", critical_habitats_df$y))

native_fish_df <- rasterToPoints(native_fish_rast) %>% 
    as.data.frame() %>% 
    mutate(coords = paste(native_fish_df$x, ", ", native_fish_df$y))

# merge ecosystems data frames
ecosystems_join1 <- native_fish_df %>% 
    left_join(ecosystem_priority_df, by = "coords") ###### having trouble joining/stacking our rasters because the cell coordinates don't match with each other ----> WHAT TO DO

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cerulean"),
                
    navbarPage("Flood Forward",
               tabPanel("Home",
                        titlePanel("Flood Forward")),
               tabPanel("About the Project",
                        titlePanel("The Project"),
                        "This project looks at the flood risk reduction, and ecosystem enhancement benefits of floodplain restoration from flood managed aquifer recharge in Madera County, California. Flood Managed Aquifer Recharge (Flood-MAR) is an integrated water resource management strategy that utilizes floodwaters, from or in anticipation of, precipitation events for managed aquifer recharge. Flood-MAR provides an opportunity to simultaneously reduce flood risk during storm events and recharge underlying aquifers to increase the state’s drought-resilient water supply, while enhancing natural ecosystems.",
                        mainPanel(imageOutput("flood_mar")),
                        titlePanel("Motivation"),
                        "California water resource managers must adapt their strategies in storing, transferring, and managing fresh water supplies. Climate change is projected to cause highly variable weather that will make old water management strategies obsolete. New management strategies are required to adapt to intensified flood risk events.  Droughts and more extreme precipitation events are projected to occur. Additionally, precipitation is expected to fall more as rain instead of snow, reducing the reliance on the Sierra snowpack as a reliable source of freshwater storage. 
                        
                        Another motivation for this project is to advocate for the sustainable management of groundwater. Groundwater accounts for 43% of the average water supply in California during normal years. In years of drought, Central Valley farmers and residents rely on groundwater for as much as 70% of water supply. Extreme drought coupled with the demand for irrigating crops has led to the unsustainable extraction of groundwater causing aquifers to deteriorate."),
               tabPanel("Data"),
               tabPanel("Study Area Overview",
                        tmapOutput("madera_overview_tmap"),
                        titlePanel("About Madera County"),
                        "The project focuses specifically in Madera County, which includes the Madera Basin and a large portion of the Chowchilla Basin. Groundwater is an essential source of water for agricultural, domestic, municipal, industrial, and environmental sectors of Madera County, which has a population of over 156,000 people. The largest economic sector in Madera is agriculture, which utilizes 500,000 acre feet of groundwater each year, providing commodities to the nation and generating at least $2 billion annually for the county. Madera is vulnerable to flood risk. There were 6-7 major flooding events from 1978 - 2009.  Climate change is likely to increase this number of flooding disasters in Madera, as precipitation will fall more as rain, and in more intense bursts. Additionally, Madera has lost over 90% of its floodplain habitat. Restoring this critical resource will help reduce flood risk throughout the county. Madera Basin is identified as critically overdrafted. To comply with SGMA, groundwater sustainability agencies (GSAs) in the Basin were required to develop a groundwater sustainability plan (GSP)."),
               tabPanel("Flood Risk Analysis",
                        sidebarLayout(
                            sidebarPanel(
                                radioButtons("flood_risk",
                                             "Select Flood Risk result(s)",
                                             choices = c("Final Priority Ranking" = "final_priority_ranking", "FEMA Flood Hazard" = "flood_area", "Catchment Area" = "AreaAcres", "Fire Hazard" = "fire_area_", "Disadvantaged Communities" = "area_DAC_a", "Levee Failures" = "Levee_fail"),
                                             selected = ("Final Priority Ranking" = "final_priority_ranking")),
                                "Our analysis looked at 5 variables that influence flood risk in Madera County: FEMA Flood Hazard, Flood Catchment Area, Fire Hazard, Disadvantaged Communities, and Levee Failures. Click on the tabs to see which catchment areas prioritise each of the variables."),
                            mainPanel("Flood Risk Analysis Results",
                                      plotOutput("floods_plot"))
                        ),
                        titlePanel("Results"),
                        "The results from the flood risk assessment, which combines all 5 variables, indicate that the greatest priority watersheds for achieving flood risk reduction with groundwater recharge projects are Fresno River and Ash Slough. These watersheds were among the highest priorities for each of the 5 parameters. The next highest priority watersheds are Buttonwillow Slough, Cottonwood Creek, and San Joaquin River, which all display similar final flood risk scores. The watersheds listed here should be given the highest priority for achieving flood risk reduction benefits with Flood-MAR projects in Madera County."),
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
        
        tmap_options(basemaps = c("Esri.WorldTopoMap", "Esri.WorldGrayCanvas", "OpenStreetMap"))
        
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
        watershed_ranks %>%
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
