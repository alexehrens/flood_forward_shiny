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
library(rnaturalearth)
library(rnaturalearthdata)

### read in shapefiles
### read in earth background layer
earth <- ne_countries(scale = "medium", returnclass = "sf") %>%
    st_as_sf()

county <-
    st_read(
        here(
            "Flood_Forward",
            "transfer_for_shiny",
            "county_boundary_project",
            "county_boundary_project.shp"
        )
    ) %>%
    clean_names() %>% 
    st_transform(crs = crs(earth))

chowchilla <-
    st_read(
        here(
            "Flood_Forward",
            "transfer_for_shiny",
            "chowchilla_project",
            "chowchilla_project.shp"
        )
    ) %>% 
    st_transform(crs = crs(earth))

madera <-
    st_read(
        here(
            "Flood_Forward",
            "transfer_for_shiny",
            "madera_project",
            "madera_project.shp"
        )
    ) %>% 
    st_transform(crs = crs(earth))

subbasins <- rbind(chowchilla, madera) %>%
    rename("name" = "Basin_Su_1")

nhd_flowlines <-
    st_read(here(
        "Flood_Forward",
        "transfer_for_shiny",
        "nhd_flowlines",
        "nhd_flowlines.shp"
    )) %>% 
    st_transform(crs = crs(earth))

watersheds <-
    st_read(
        here(
            "Flood_Forward",
            "transfer_for_shiny",
            "watersheds_dissolve",
            "watersheds_dissolve.shp"
        )
    ) %>% 
    st_transform(crs = crs(earth))

# flood risk shapefile
watershed_ranks <- watersheds %>%
    mutate(
        final_priority_ranking = case_when(
            watersheds$Name == "Ash Slough" ~ 2,
            watersheds$Name == "Berenda Slough" ~ 7,
            watersheds$Name == "Berenda Creek" ~ 8,
            watersheds$Name == "Buttonwillow Slough" ~ 3,
            watersheds$Name == "Chowchilla River" ~ 9,
            watersheds$Name == "Cottonwood Creek" ~ 4,
            watersheds$Name == "Dry Creek" ~ 6,
            watersheds$Name == "Fresno River" ~ 1,
            watersheds$Name == "San Joaquin River" ~ 5,
        )
    )

### read in earth background layer
earth <- ne_countries(scale = "medium", returnclass = "sf") %>%
    st_as_sf()

### read in basemap
#basemap <- get_map(location = c(-121, 36, -118, 40), maptype = "terrain", source = "stamen", zoom = 8)

### read in ecosystems rasters
ecosystem_priority_rast <-
    raster(
        here(
            "Flood_Forward",
            "transfer_for_shiny",
            "ecosystem_priority_rank",
            "ecosystem_priority_rank.tif"
        )
    ) %>%
    projectRaster(crs = crs(earth))

gde_score_rast <-
    raster(here(
        "Flood_Forward",
        "transfer_for_shiny",
        "gde_score",
        "gde_score.tif"
    )) %>%
    projectRaster(crs = crs(earth))

critical_habitats_rast <-
    raster(
        here(
            "Flood_Forward",
            "transfer_for_shiny",
            "critical_habitats",
            "crit_habitat_score.tif"
        )
    ) %>%
    projectRaster(crs = crs(earth))

native_fish_rast <-
    raster(
        here(
            "Flood_Forward",
            "transfer_for_shiny",
            "native_fish",
            "native_fish_reclass.tif"
        )
    ) %>%
    projectRaster(crs = crs(earth))

# reproject each raster to match extent and parameters of ecosystem_priority_df
gde_score_reproj <- gde_score_rast %>%
    projectRaster(to = ecosystem_priority_rast)

critical_habitats_reproj <- critical_habitats_rast %>%
    projectRaster(to = ecosystem_priority_rast)

native_fish_reproj <- native_fish_rast %>%
    projectRaster(to = ecosystem_priority_rast)

# make data frames for each raster for plotting
ecosystem_priority_df <-
    rasterToPoints(ecosystem_priority_rast) %>%
    as.data.frame()

colnames(ecosystem_priority_df) <-
    c("x", "y", "ecosystem_priority_rank")

ecosystem_priority_coords <- ecosystem_priority_df %>%
    mutate(coords = paste(ecosystem_priority_df$x, ", ", ecosystem_priority_df$y))

gde_score_df <- rasterToPoints(gde_score_reproj) %>%
    as.data.frame()

colnames(gde_score_df) <- c("x", "y", "gde_score")

gde_score_coords <- gde_score_df %>%
    mutate(coords = paste(gde_score_df$x, ", ", gde_score_df$y))

critical_habitats_df <-
    rasterToPoints(critical_habitats_reproj) %>%
    as.data.frame()

colnames(critical_habitats_df) <- c("x", "y", "crit_habitat_score")

critical_habitats_coords <- critical_habitats_df %>%
    mutate(coords = paste(critical_habitats_df$x, ", ", critical_habitats_df$y))

native_fish_df <- rasterToPoints(native_fish_reproj) %>%
    as.data.frame()

colnames(native_fish_df) <- c("x", "y", "native_fish_reclass")

native_fish_coords <- native_fish_df %>%
    mutate(coords = paste(native_fish_df$x, ", ", native_fish_df$y))

# merge ecosystems data frames
ecosystems_join1 <- native_fish_coords %>%
    left_join(ecosystem_priority_coords, by = "coords")

ecosystems_join2 <- ecosystems_join1 %>%
    left_join(critical_habitats_coords, by = "coords")

ecosystems_join3 <- ecosystems_join2 %>%
    left_join(gde_score_coords, by = "coords")

# tidy combined raster data frame
ecosystems_all <- ecosystems_join3 %>%
    rename("x" = "x.x", "y" = "y.x") %>%
    dplyr::select(x,
                  y,
                  ecosystem_priority_rank,
                  gde_score,
                  crit_habitat_score,
                  native_fish_reclass)

#### 4) sensitivity analysis
sa_rast <- raster(here("Flood_Forward", "transfer_for_shiny", "sensitivity_analysis_results.tif"))

sa_df <- rasterToPoints(sa_rast) %>% 
    as.data.frame()

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("flatly"),
    
    navbarPage(
        "Flood Forward",
        tabPanel(
            "Home",
            titlePanel(h1("Flood Forward", align = "center")),
            img(
                src = "hensley_lake.jpg",
                style = "display: block; margin-left: auto; margin-right: auto;",
                width = "100%",
                height = "650"
            ),
            "Hensley Lake, Madera County. Image Credit: Michael J. Nevins"
        ),
        tabPanel(
            "About the Project",
            titlePanel("The Project"),
            "This project looks at the flood risk reduction, and ecosystem enhancement benefits of floodplain restoration from flood managed aquifer recharge in Madera County, California. Flood Managed Aquifer Recharge (Flood-MAR) is an integrated water resource management strategy that utilizes floodwaters, from or in anticipation of, precipitation events for managed aquifer recharge. Flood-MAR provides an opportunity to simultaneously reduce flood risk during storm events and recharge underlying aquifers to increase the state’s drought-resilient water supply, while enhancing natural ecosystems.",
            img(src = "floodMAR_diagram.jpg",
                width = 400,
                align = "left"),
            titlePanel("Motivation"),
            "California water resource managers must adapt their strategies in storing, transferring, and managing fresh water supplies. Climate change is projected to cause highly variable weather that will make old water management strategies obsolete. New management strategies are required to adapt to intensified flood risk events. Droughts and more extreme precipitation events are projected to occur. Additionally, precipitation is expected to fall more as rain instead of snow, reducing the reliance on the Sierra snowpack as a reliable source of freshwater storage. Another motivation for this project is to advocate for the sustainable management of groundwater. Groundwater accounts for 43% of the average water supply in California during normal years. In years of drought, Central Valley farmers and residents rely on groundwater for as much as 70% of water supply. Extreme drought coupled with the demand for irrigating crops has led to the unsustainable extraction of groundwater causing aquifers to deteriorate."
        ),
        tabPanel("Data"),
        tabPanel(
            "Study Area Overview",
            titlePanel("About Madera County"),
            "The project focuses specifically in Madera County, which includes the Madera Basin and a large portion of the Chowchilla Basin. Groundwater is an essential source of water for agricultural, domestic, municipal, industrial, and environmental sectors of Madera County, which has a population of over 156,000 people. The largest economic sector in Madera is agriculture, which utilizes 500,000 acre feet of groundwater each year, providing commodities to the nation and generating at least $2 billion annually for the county. Madera is vulnerable to flood risk. There were 6-7 major flooding events from 1978 - 2009. Climate change is likely to increase this number of flooding disasters in Madera, as precipitation will fall more as rain, and in more intense bursts. Additionally, Madera has lost over 90% of its floodplain habitat. Restoring this critical resource will help reduce flood risk throughout the county. Madera Basin is identified as critically overdrafted. To comply with SGMA, groundwater sustainability agencies (GSAs) in the Basin were required to develop a groundwater sustainability plan (GSP).",
        sidebarLayout(
            sidebarPanel(
                selectInput("watersheds",
                            "Select Watershed of Interest",
                            choices = c(
                                "Ash Slough",
                                "Berenda Slough",
                                "Berenda Creek",
                                "Buttonwillow Slough",
                                "Chowchilla River",
                                "Cottonwood Creek",
                                "Dry Creek",
                                "Fresno River",
                                "San Joaquin River"
                            ),
                            selected = "San Joaquin River")
            ),
        mainPanel("Madera County Watersheds",
                  tmapOutput("madera_overview_tmap"))
        )),
        tabPanel(
            "Flood Risk Analysis",
            sidebarLayout(
                sidebarPanel(
                    radioButtons(
                        "flood_risk",
                        "Select Flood Risk result(s)",
                        choices = c(
                            "Flood Hazard Ranking" = "final_priority_ranking",
                            "FEMA Flood Hazard" = "flood_area",
                            "Catchment Area" = "AreaAcres",
                            "Fire Hazard" = "fire_area_",
                            "Disadvantaged Communities" = "area_DAC_a",
                            "Levee Failures" = "Levee_fail"
                        ),
                        selected = ("Final Priority Ranking" = "final_priority_ranking")
                    ),
                    "Our analysis looked at 5 variables that influence flood risk in Madera County: FEMA Flood Hazard, Flood Catchment Area, Fire Hazard, Disadvantaged Communities, and Levee Failures. Click on the tabs to see which catchment areas prioritise each of the variables."
                ),
                mainPanel("Flood Risk Analysis Results",
                          plotOutput("floods_plot"))
            ),
            titlePanel("Results"),
            "The results from the flood risk assessment, which combines all 5 variables, indicate that the greatest priority watersheds for achieving flood risk reduction with groundwater recharge projects are Fresno River and Ash Slough. These watersheds were among the highest priorities for each of the 5 parameters. The next highest priority watersheds are Buttonwillow Slough, Cottonwood Creek, and San Joaquin River, which all display similar final flood risk scores. The watersheds listed here should be given the highest priority for achieving flood risk reduction benefits with Flood-MAR projects in Madera County."
        ),
        tabPanel(
            "Ecosystem Enhancement Analysis",
            sidebarLayout(
                sidebarPanel(
                    radioButtons(
                        "ecosystems",
                        "Select Ecosystem Priority result(s)",
                        choices = c(
                            "Ecosystem Priority Ranking" = "ecosystem_priority_rank",
                            "Groundwater-Dependent Ecosystems" = "gde_score",
                            "Critical Habitats" = "crit_habitat_score",
                            "Native Fish Species" = "native_fish_reclass"
                        ),
                        selected = ("Ecosystem Priority Ranking" = "ecosystem_priority_rank")
                    ),
                    "The ecosystem enhancement results provide insight into which sites within Madera and Chowchilla Sub Basins have significant ecosystems that could be enhanced through floodplain restoration and groundwater recharge projects. Ecosystem enhancement priorities were determined using a combination of three parameters: proximity to groundwater-dependent ecosystems, proximity to critical habitats for endangered or threatened species, and the richness of native fish species in each HUC12 watershed. Click on the tabs to see the results from each of these parameters as well as an overall ecosystem prioritization score."
                ),
                mainPanel(
                    "Ecosystem Enhancement Analysis Results",
                    plotOutput("ecosystems_plot")
                )
            ),
            titlePanel("Results"),
            "Areas with the closest proximity to GDEs are along the major stream channels that cut from northeast to southwest across the Madera and Chowchilla Subbasins. This includes significant portions of Berenda Slough, Berenda Creek, Dry Creek, Fresno River, and Cottonwood Creek. Some of these areas even have distances of 0 meters from GDEs, which would make them the highest priority for locating groundwater recharge projects with the ecosystem enhancement benefit. 

The critical habitats for endangered or threatened species from the US Fish and Wildlife Service dataset were mostly located along the north and eastern boundaries of the study area. High priority areas were distributed across each of the major streams in the study area, all of which showed the same pattern of high priority in the east and low priority in the west.

The highest fish species richness are located along the San Joaquin River at the southern boundary of Madera Subbasin. The major streams in the Chowchilla Subbasin (Chowchilla River, Ash Slough, Berenda Slough) all show high species richness of native fish. 

Much of the highest priority sites are on the northeastern end of the Chowchilla Subbasin on Berenda Slough and the northernmost end of the Chowchilla River and Ash Slough. Some areas of high priority can be seen in Madera Subbasin as well, on sections of the Fresno River, Cottonwood Creek, and the San Joaquin River. These locations represent the best options for achieving ecosystem enhancement benefits from groundwater recharge projects."
        ),
        tabPanel(
            "Recommended Sites",
            sidebarLayout(
                sidebarPanel(
                    numericInput(
                        "top_sites",
                        "Select % of the best sites you want to see:",
                        min = 1,
                        max = 100,
                        value = 100
                    )
                ),
            
            # Show a plot of the generated distribution
            mainPanel("Sensitivity Analysis Results",
                      plotOutput("sa_plot"))
            )
        ),
        tabPanel(
            "Contact Us",
            titlePanel(h1("The Team", align = "center")),
            img(
                src = "squad.png",
                style = "display: block; margin-left: auto; margin-right: auto;",
                width = "75%",
                height = "75%"
            )
        )
    ),
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    ### 1) Study Area Overview tmap
    watershed_select <- reactive({
        watersheds %>% 
            filter(Name == input$watersheds)
    })
    
    nhd_select <- reactive({
        st_zm(nhd_flowlines) %>% 
            st_filter(st_zm(watershed_select()),
                      join = st_within)
    })
    
    output$madera_overview_tmap = renderTmap({
        tmap_mode("view")
        
        tmap_options(basemaps = c(
            "Esri.WorldTopoMap",
            "Esri.WorldGrayCanvas",
            "OpenStreetMap"
        ))
        
        tm_shape(watershed_select()) +
            tm_polygons("Name") +
            tm_shape(nhd_select()) +
            tm_lines(col = "blue")
    })
    
    ### 2) flood risk analysis reactive map
    floods_select <- reactive({
        watershed_ranks %>%
            dplyr::select(c(Name, input$flood_risk)) %>%
            rename("column_name" = input$flood_risk)
    })
    
    floods_map <- reactive({
        ggplot(data = floods_select()) +
            geom_sf(aes(fill = column_name)) +
            labs(fill = case_when(
                input$flood_risk == "final_priority_ranking" ~ "Flood Hazard Priority Ranking",
                input$flood_risk == "flood_area" ~ "FEMA Flood Hazard Area (ac)",
                input$flood_risk == "AreaAcres" ~ "Total Catchment Area (ac)",
                input$flood_risk == "fire_area_" ~ "Fire Hazard Area (ac)",
                input$flood_risk == "area_DAC_a" ~ "Disadvantaged Communitieis Area (ac)",
                input$flood_risk == "Levee_fail" ~ "Number of Levee Failures")) +
            theme_void()
    })
    
    output$floods_plot <- renderPlot({
        floods_map()
    })
    
    ### 3) ecosystem priority analysis reactive map
    ecosystems_select <- reactive({
        ecosystems_all %>%
            dplyr::select(c(x, y, input$ecosystems)) %>%
            rename("column_name" = input$ecosystems)
    })
    
    output$ecosystems_plot <- renderPlot({
        ggplot() +
            geom_sf(data = subbasins, fill = "darkgray")  +
            coord_sf(
                xlim = c(-120.56, -119.7),
                ylim = c(36.79, 37.2),
                expand = 0
            ) +
            geom_tile(data = ecosystems_select(), aes(
                x = x,
                y = y,
                fill = column_name
            )) +
            scale_fill_gradientn(colors = c(
                "firebrick",
                "orange",
                "gold",
                "lightgreen",
                "darkgreen"
            )) +
            theme_void()+
            labs(fill = "Priority Ranking")
    })
    
    ######## 4) sensitivity analysis results
    prop <- reactive({
        input$top_sites / 100
    })
    
    sa_top <- reactive({
        sa_df %>% 
            filter(sensitivity_analysis_results > 0) %>% 
            slice_max(order_by = sensitivity_analysis_results, prop = prop())
    })
    
    output$sa_plot <- renderPlot({
        ggplot() +
            geom_sf(data = subbasins, fill = "darkgray")  +
            coord_sf(
                xlim = c(-120.56, -119.7),
                ylim = c(36.79, 37.2),
                expand = 0
            ) +
            geom_tile(data = sa_top(),
                      aes(x = x, y = y, fill = sensitivity_analysis_results)) +
            scale_fill_gradientn(colors = c(
                "firebrick",
                "orange",
                "gold",
                "lightgreen",
                "darkgreen"
            ))+
            theme_void()+
            labs(fill = "Sensitivity Analysis Score")
    })

}

# Run the application
shinyApp(ui = ui, server = server)
