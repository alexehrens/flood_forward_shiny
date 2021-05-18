#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# attach packages ###################################################
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
library(RColorBrewer)

### read in shapefiles #############################################

### read in earth background layer
earth <- ne_countries(scale = "medium", returnclass = "sf") %>%
    st_as_sf()

### read in overview tab shapefiles -------------------------------
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

#######################################################################

# flood risk shapefile -----------------------------------------------
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
##########################################################################

### read in ecosystems rasters -------------------------------------------
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

################################################################################################

#### sensitivity analysis ######################################################################
sa_rast <- raster(here("Flood_Forward", "transfer_for_shiny", "sensitivity_analysis_results.tif"))

sa_df <- rasterToPoints(sa_rast) %>% 
    as.data.frame()

################################################################################################

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
            ################# About the Project page #########################################
            "About the Project",
            titlePanel("The Project"),
            "This project looks at the flood risk reduction, and ecosystem enhancement benefits of floodplain restoration from flood managed aquifer recharge in Madera County, California. Flood Managed Aquifer Recharge (Flood-MAR) is an integrated water resource management strategy that utilizes floodwaters, from or in anticipation of, precipitation events for managed aquifer recharge. Flood-MAR provides an opportunity to simultaneously reduce flood risk during storm events and recharge underlying aquifers to increase the state’s drought-resilient water supply, while enhancing natural ecosystems.",
            img(src = "floodMAR_diagram.jpg",
                width = 600,
                align = "left"),
            titlePanel("Motivation"),
            "California water resource managers must adapt their strategies in storing, transferring, and managing fresh water supplies. Climate change is projected to cause highly variable weather that will make old water management strategies obsolete. New management strategies are required to adapt to intensified flood risk events. Droughts and more extreme precipitation events are projected to occur. Additionally, precipitation is expected to fall more as rain instead of snow, reducing the reliance on the Sierra snowpack as a reliable source of freshwater storage. Another motivation for this project is to advocate for the sustainable management of groundwater. Groundwater accounts for 43% of the average water supply in California during normal years. In years of drought, Central Valley farmers and residents rely on groundwater for as much as 70% of water supply. Extreme drought coupled with the demand for irrigating crops has led to the unsustainable extraction of groundwater causing aquifers to deteriorate."
        ),
        tabPanel(
            ###################### Data page ###################################################
            "Data",
            titlePanel("Data Sources"),
            h4(strong("General:")),
            p("California Department of Water Recourses (DWR) Madera and Chowchilla Subbasin Boundaries"),
            p("Recharge for Resilience Decision Support Tool. https://waterresilience.wixsite.com/waterresilienceca/download-the-tool"),
            p("United States Geological Survey (USGS) 10m Digital Elevation Model (DEM)"),
            p("United States Geological Survey (USGS) HUC 12 boundaries. https://www.usgs.gov/core-science-systems/ngp/national-hydrography/watershed-boundary-dataset?qt-science_support_page_related_con=4#qt-science_support_page_related_con"),
            p("United States Geological Survey (USGS) National Hydrography Dataset (NHD). https://www.usgs.gov/core-science-systems/ngp/national-hydrography/national-hydrography-dataset?qt-science_support_page_related_con=0#qt-science_support_page_related_con"),
            p("United States Geological Survey (USGS) National Land Cover Database (NLCD, 2016). https://www.usgs.gov/centers/eros/science/national-land-cover-database?qt-science_center_objects=0#qt-science_center_objects"),
            p(h4(strong("Flood Risk Analysis"))),
            p("CalEnviroScreen 3.0 Disadvantaged Communities Dataset. https://oehha.ca.gov/calenviroscreen/report/calenviroscreen-30"),
            p("CALFIRE California Fire Hazard Severity Zone (FHSZ) Dataset. https://gis.data.ca.gov/datasets/789d5286736248f69c4515c04f58f414
"),
            p("California Natural Resources Agencies California Protected Areas Database  (CPAD). https://www.calands.org"),
            p("Federal Emergency Management (FEMA) National Flood Hazard Layer (NFHL). https://www.fema.gov/flood-maps/national-flood-hazard-layer"),
            p("Madera County Historic Levee Data"),
            p(h4(strong("Ecosystem Enhancement Analysis"))),
            p("The Nature Conservancy (TNC) Indicators of Groundwater Dependent Ecosystems Database (iGDE). https://groundwaterresourcehub.org/sgma-tools/mapping-indicators-of-gdes/"),
            p("UC Davis Center for Watershed Sciences PISCES Native Fish Species Database. https://pisces.ucdavis.edu"),
            p("United States Fish and Wildlife Service (USFWS) Critical Habitat Database. https://ecos.fws.gov/ecp/report/table/critical-habitat.html")),
        tabPanel(
            ####################### Study Area Overview page ###################################
            "Study Area Overview",
            titlePanel("About Madera County"),
            "The project focuses specifically in Madera County, which includes the Madera Basin and a large portion of the Chowchilla Basin. Groundwater is an essential source of water for agricultural, domestic, municipal, industrial, and environmental sectors of Madera County, which has a population of over 156,000 people. The largest economic sector in Madera is agriculture, which utilizes 500,000 acre feet of groundwater each year, providing commodities to the nation and generating at least $2 billion annually for the county. Madera is vulnerable to flood risk. There were 6-7 major flooding events from 1978 - 2009. Climate change is likely to increase this number of flooding disasters in Madera, as precipitation will fall more as rain, and in more intense bursts. Additionally, Madera has lost over 90% of its floodplain habitat. Restoring this critical resource will help reduce flood risk throughout the county. Madera Basin is identified as critically overdrafted. To comply with SGMA, groundwater sustainability agencies (GSAs) in the Basin were required to develop a groundwater sustainability plan (GSP). The figure below shows the location of Madera County within the state of California, and shows the locations of its two groundwater subbasins, Chowchilla and Madera, within the county.",
            img(src = "area_of_interest.png",
                width = 800,
                style = "display: block; margin-left: auto; margin-right: auto;"),
            p(h2("Watersheds")),
            p("Below are each of the watersheds that the Flood Forward project delineated for this region, and the network of streams that intersect each watershed (from the NHD Flowlines dataset). The 9 watersheds are named corresponding to the name of their primary stream channel that drains the watershed: Ash Slough, Berenda Creek, Berenda Slough, Buttonwillow Slough, Chowchilla River, Cottonwood Creek, Dry Creek, Fresno River, and San Joaquin River. To view each of the 9 watersheds in more detail, use the dropdown menu widget below. To turn the NHD or watershed layers off, use the interactive features on the map itself (in the upper left hand corner of the map)."),
        sidebarLayout(
            sidebarPanel(
                selectInput("watersheds",
                            "Select Watershed of Interest",
                            choices = c(
                                "Ash Slough",
                                "Berenda Creek",
                                "Berenda Slough",
                                "Buttonwillow Slough",
                                "Chowchilla River",
                                "Cottonwood Creek",
                                "Dry Creek",
                                "Fresno River",
                                "San Joaquin River"
                            ),
                            selected = "Cottonwood Creek"),
                "Selecting a watershed will display the boundary of that watershed and the streams and tributaries that intersect its boundary."
            ),
        mainPanel(tmapOutput("madera_overview_tmap"))
        ),
        p(" "),
        p(" "),
        p(" "),
        p(" "),
        p(" "),
        p(" "),
        p(" "),
        p(" "),
        ),
        tabPanel(
            ############################## Flood Risk Analysis ##################################################
            "Flood Risk Analysis",
            titlePanel("Flood Risk Analysis"),
            p("Our analysis looked at 5 variables that influence flood risk in Madera County: FEMA flood hazard layer, total watershed catchment area, fire hazard severity zones, disadvantaged communities, and levee failures. Assigned priority rankings for the 5 variables were combined using weighted sums into a final priority ranking for flood risk (the button labelled 'Flood Hazard Ranking'). A ranking of 1 corresponds to the greatest flood risk and greatest priority for achieving flood risk reduction benefits through groundwater recharge projects."),
            p(h4(strong("The Variables"))),
            p(strong("FEMA Flood Hazard Layer: "), "The FEMA flood hazard layer provides the extent of land area that is within the 100-year flood zone, or the area of land which would be inundated by a flood event with a 1% likelihood of occurring. Watersheds were ranked based on the area of FEMA flood hazard layer that fell within that watershed's boundaries. Watersheds with greater flood hazard areas were given a higher rank for flood risk priority."),
            p(strong("Total catchment area: "), "Total catchment area serves as a proxy for a watershed's capacity to capture precipitation and contribute to flooding. Greater catchment areas were given higher flood risk priority."),
            p(strong("Fire hazard zones: "), "There is a strong relationship between fire burn areas and post-fire flooding and debris flows. We used a Fire Hazard Severity Zone dataset to find the area of high fire hazard that fell within each watershed. Watersheds with the greatest fire hazard area were assigned greater flood risk priority ranks."),
            p(strong("Disadvantaged communities: "), "It is important to include exposure to disadvantaged communities (DACs) as a metric for flood risk, as these communities are more vulnerable to catastrophic damage caused by flooding. In this analysis, we gave watersheds with the greatest areas of DACs higher rankings for flood risk priority."),
            p(strong("Levee failures: "), "Finally, failures in Madera County's levee system (including erosion, seepage, or breaks) are indicators of areas where the water system is stressed. Watersheds with more levee failures are at greater flood risk and are given higher priority so that levee system stress can be reduced."),
            titlePanel("Flood Risk Analysis Results"),
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
                        selected = ("Final Priority Ranking" = "final_priority_ranking"),
                    ),
                    "Click on the tabs to see which catchment areas are given the greatest priority for each of the parameters. Darker red colors indicate greater flood risk and therefore higher priority."
                ),
                mainPanel(
                          plotOutput("floods_plot"))
            ),
            titlePanel("Results"),
            "The results from the flood risk assessment, which combines all 5 variables, indicate that the greatest priority watersheds for achieving flood risk reduction with groundwater recharge projects are Fresno River and Ash Slough. These watersheds were among the highest priorities for each of the 5 parameters. The next highest priority watersheds are Buttonwillow Slough, Cottonwood Creek, and San Joaquin River, which all display similar final flood risk scores. The watersheds listed here should be given the highest priority for achieving flood risk reduction benefits with Flood-MAR projects in Madera County."
        ),
        tabPanel(
            #################### Ecosystem Enhancement Analysis ####################################
            "Ecosystem Enhancement Analysis",
            titlePanel("Ecosystem Enhancement Analysis"),
            p("The ecosystem enhancement results provide insight into which sites within Madera and Chowchilla Sub Basins have significant ecosystems that could be enhanced through floodplain restoration and groundwater recharge projects. Ecosystem enhancement priorities were determined using a combination of three parameters: proximity to groundwater-dependent ecosystems, proximity to critical habitats for endangered or threatened species, and the richness of native fish species in each HUC12 watershed. Results from each of these parameters were combined to produce the Final Ecosystem Priority Ranking, which ranks riparian sites that are within a 50-meter floodplain from major stream channels."),
            p(h4(strong("The Variables"))),
            p(strong("Groundwater-dependent ecosystems: "), "Groundwater-dependent ecosystems (GDEs) are ecological communities or species that require groundwater to meet some or all of their water needs. These generally follow major stream corridors. For our analysis, we ranked our delineated riparian areas based on their proximity to these ecosystems - the closest areas are given higher priority than those further away."),
            p(strong("Critical habitats: "), "Critical habitats for federally endangered or threatened species are another important ecosystem to protect and enhance. We prioritized riparian areas that were closer to these critical habitats over areas that were further away."),
            p(strong("Native fish species: "), "Finally we wanted to rank riparian areas by the richness of native fish species that are found in their respective watersheds. We ranked areas with greater fish species richness higher than those with fewer native fish species."),
            titlePanel("Ecosystem Enhancement Analysis Results"),
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
                    "Click on the tabs to see the results from each of these parameters as well as an overall ecosystem prioritization score. Darker blue colors indicate higher priority areas for enhancing ecosystems with groundwater recharge projects."
                ),
                mainPanel(
                    plotOutput("ecosystems_plot")
                )
            ),
            titlePanel("Results"),
            p("Areas with the closest proximity to GDEs are along the major stream channels that cut from northeast to southwest across the Madera and Chowchilla Subbasins. This includes significant portions of Berenda Slough, Berenda Creek, Dry Creek, Fresno River, and Cottonwood Creek. Some of these areas even have distances of 0 meters from GDEs, which would make them the highest priority for locating groundwater recharge projects with the ecosystem enhancement benefit."),
            p("The critical habitats for endangered or threatened species from the US Fish and Wildlife Service dataset were mostly located along the north and eastern boundaries of the study area. High priority areas were distributed across each of the major streams in the study area, all of which showed the same pattern of high priority in the east and low priority in the west."),
            p("The highest fish species richness are located along the San Joaquin River at the southern boundary of Madera Subbasin. The major streams in the Chowchilla Subbasin (Chowchilla River, Ash Slough, Berenda Slough) all show high species richness of native fish."),
            p("Much of the highest priority sites are on the northeastern end of the Chowchilla Subbasin on Berenda Slough and the northernmost end of the Chowchilla River and Ash Slough. Some areas of high priority can be seen in Madera Subbasin as well, on sections of the Fresno River, Cottonwood Creek, and the San Joaquin River. These locations represent the best options for achieving ecosystem enhancement benefits from groundwater recharge projects.")
        ),
        tabPanel(
            ############### Tradeoff Analysis ######################################
            "Tradeoff Analysis",
            titlePanel("Tradeoff Analysis - Multiple-Benefit Weighting"),
          #  p(h4(strong("Multiple-Benefit Weighting"))),
            p("After completing our flood risk and ecosystem analyses, results were combined with an analysis of site suitability to determine  areas most suitable for achieving groundwater recharge with flood risk reduction or ecosystem enhancement benefits. For our final output, we combined the results for site-suitable flood risk or ecosystems benefit using a multiple-benefit weighted sum that can be changed to reflect differences in user preferences. For example, a flood control agency may have no interest in enhancing ecosystems and choose to only look at flood risk reduction as their desired co-benefit when making decisions on groundwater recharge project siting. For the purposes of this app, the scenario in which flood risk reduction and ecosystem enhancement benefits are given equal (50-50) weighting."),
          titlePanel("Tradeoff Analysis Results"),
          sidebarLayout(
              sidebarPanel(
                 sliderInput(
                     "tradeoff",
                     "Select desired multiple-benefit tradeoff weighting",
                     min = 0,
                     max = 100,
                     value = 50,
                     step = 25
                 ), 
                 "The number selected corresponds to the percentage of weighting assigned to the ecosystems enhancement benefit relative to the flood risk reduction benefit. For example, an selection of 0 means 0% ecosystems and therefore 100% flood risk weighting, whereas a selection of 75 means 75% ecosystems and 25% flood risk weighting."
              ),
              mainPanel(
                  
              )
          )),
        tabPanel(
            ############## Sensitivity Analysis ###################################
            "Sensitivity Analysis",
            titlePanel("Sensitivity Analysis"),
           # p(h4(strong("Sensitivity Analysis"))),
            p("The last step of our project was to perform a sensitivity analysis to determine how the spatial distribution of priority sites would change with changes in each of the analyses variables. In this analysis, each variable for both flood risk and ecosystem analyses were set to either 0% or 100% weighting to test how that change impacted final site recommendation results. Since there were 5 flood risk and 3 ecosystem variables, each tested at 0% and 100% weighting, this analysis was performed a total of 16 times. Using raster analysis in Rstudio, the 16 outcomes were analyzed to determine how many times each raster cell appeared as a high priority site. The number reported in the figure below are the total number of times a given cell resulted in a high priority score, ranging from 0 to 16. Sites with scores of 16 consistently showed up as the 'best' sites for multiple-benefit groundwater recharge projects no matter how any of the input variables were changed. Those sites are considered the top and most recommended sites in Madera County for groundwater recharge projects with flood risk reduction and ecosystem enhancement benefits."),
            titlePanel("Sensitivity Analysis Results - Top Recommended Sites"),
            sidebarLayout(
                sidebarPanel(
                    numericInput(
                        "top_sites",
                        "Select the percentage (%) of best sites to display:",
                        min = 1,
                        max = 100,
                        value = 100
                    ),
                    "The number selected corresponds to the percentage of the top sites that will display in the figure to the right. For example, entering 20 will result in the top 20% of sites being displayed."
                ),
                mainPanel(
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
#######################################################################################

# Define server logic required to draw a histogram
server <- function(input, output) {
    ### 1) Study Area Overview tmap ##################################################
    # select watershed using reactive element
    watershed_select <- reactive({
        watersheds %>% 
            filter(Name == input$watersheds)
    })
    
    # select streams based on watershed selection
    nhd_select <- reactive({
        st_zm(nhd_flowlines) %>% 
            st_filter(st_zm(watershed_select()),
                      join = st_within)
    })
    
    # plot tmap
    output$madera_overview_tmap = renderTmap({
        tmap_mode("view")
        
        tmap_options(basemaps = c(
            "Esri.WorldTopoMap",
            "Esri.WorldGrayCanvas",
            "OpenStreetMap"
        ))
        
        tm_shape(watershed_select(), name = "Watersheds") +
            tm_polygons("Name") +
            tm_shape(nhd_select(), name = "NHD Stream Layer") +
            tm_lines(col = "blue")
    })
    
    ############################################################################
    
    ### 2) flood risk analysis reactive map ####################################
    # select using reactive element
    floods_select <- reactive({
        watershed_ranks %>%
            dplyr::select(c(Name, input$flood_risk)) %>%
            rename("column_name" = input$flood_risk)
    })
    
    # plot
    floods_map <- reactive({
        ggplot(data = floods_select()) +
            geom_sf(aes(fill = column_name)) +
            scale_fill_gradientn(colours = case_when(
                input$flood_risk == "final_priority_ranking" ~ c("darkred", "white"),
                input$flood_risk %in% c("flood_area", "AreaAcres", "fire_area_", "area_DAC_a", "Levee_fail") ~ c("white", "darkred"))) +
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
    ######################################################################################
    
    ### 3) ecosystem priority analysis reactive map ######################################
    ##### select using reactive element
    ecosystems_select <- reactive({
        ecosystems_all %>%
            dplyr::select(c(x, y, input$ecosystems)) %>%
            rename("column_name" = input$ecosystems)
    })
    
    #### plot
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
            scale_fill_gradientn(colors = c("lightblue1", "blue")) +
            theme_void()+
            labs(fill = "Priority Ranking")
    })
    
    ####################################################################################
    
    ######## 4) sensitivity analysis results ###########################################
    # set reactive element turning input number into proportion
    prop <- reactive({
        input$top_sites / 100
    })
    
    # reactive element to select top proportion of sites
    sa_top <- reactive({
        sa_df %>% 
            filter(sensitivity_analysis_results > 0) %>% 
            slice_max(order_by = sensitivity_analysis_results, prop = prop())
    })
    
    # plot
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
            scale_fill_gradientn(colors = case_when(
            #    sa_top()$sensitivity_analysis_results >= 12 ~ "firebrick",
            #    sa_top()$sensitivity_analysis_results < 12 & sa_top()$sensitivity_analysis_results >= 8 ~ "orange",
            #    sa_top()$sensitivity_analysis_results < 8 & sa_top()$sensitivity_analysis_results >= 4 ~ "limegreen",
            #    sa_top()$sensitivity_analysis_results < 4 ~ "darkgreen"))+
                prop() >= 0.8 ~ c("firebrick","orange","gold","limegreen","darkgreen"),
                prop() < 0.8 & prop() >= 0.6 ~ c("orange", "gold", "gold", "limegreen", "darkgreen"),
                prop() < 0.6 & prop() >= 0.4 ~ c("gold", "gold", "limegreen", "darkgreen", "darkgreen"),
                prop() < 0.4 & prop() >= 0.2 ~ c("limegreen", "limegreen", "darkgreen", "darkgreen", "darkgreen"),
                prop() < 0.2 ~ c("darkgreen", "darkgreen", "darkgreen", "darkgreen", "darkgreen")
                ))+
            theme_void()+
            labs(fill = "Sensitivity Analysis Score")
    })

}
##################################################################################

# Run the application
shinyApp(ui = ui, server = server)
