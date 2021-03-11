ggplot() +
  geom_sf(data = county) +
  geom_sf(data = subbasins) +
  geom_sf(data = watersheds) +
  geom_sf(data = nhd_flowlines)


tmap_mode("view")

tm_shape(county)+
  tm_polygons("name")+
tm_shape(subbasins)+
  tm_polygons("name")+
tm_shape(watersheds)+
  tm_polygons("Name")+
tm_shape(nhd_flowlines)+
  tm_lines(col = "blue")



# flood risk reactive map
floods_select <- watersheds %>%
    select(c(Name, final_priority_ranking))

  ggplot(data = floods_select) +
    geom_sf(aes(fill = final_priority_ranking))

  #### for client version of shiny app:
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
  
  
  ### 1) Study Area Overview tmap - test
  watershed_select <-watersheds %>% 
      filter(Name == "Fresno River")
  
  nhd_select <- st_zm(nhd_flowlines) %>% 
      st_filter(st_zm(watershed_select),
                join = st_within)

  ggplot() +
    geom_sf(data = watershed_select)+
    geom_sf(data = nhd_select)
  
  watershed_mask <- extent(watershed_select)
  
  sa_select <- sa_rast %>% 
    setExtent(ext = watershed_mask)
  
  sa_select_df <- rasterToPoints(sa_select) %>% 
    as.data.frame()
  
  ggplot() +
    geom_sf(data = watershed_select)+
    geom_raster(data = sa_select)
  
  
  sa_top <- sa_df %>% 
    filter(sensitivity_analysis_results > 0) %>% 
      slice_max(order_by = sensitivity_analysis_results, prop = 0.99)
  
  output$sa_plot <- renderPlot({
    ggplot(data = sa_top()) +
      geom_tile(aes(x = x, y = y, fill = sensitivity_analysis_results))
  })
  
  
  leaflet() %>% 
    # Base layers
    addProviderTiles(providers$Esri.WorldTopoMap, group = "Basemap") %>% 
    # Polygon layers
    addPolygons(data = subbasins, color = "black", weight = 0.5, fillOpacity = 0) %>% 
    # Raster layers
    addRasterImage(data = ecosystems_select())
  
  
  ggplot() +
      geom_sf(data = subbasins, fill = "darkgray")  +
      coord_sf(
        xlim = c(-120.56, -119.7),
        ylim = c(36.79, 37.2),
        expand = 0
      )+theme_void()# +
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
      theme_minimal()