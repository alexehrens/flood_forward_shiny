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