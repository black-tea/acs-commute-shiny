#####                                                          #####
##### This app displays census tract-level commute mode splits #####
#####                                                          #####

##### Setup #####
library(leaflet) # For interactive mapping
library(sf) # For manipulating spatial data
<<<<<<< HEAD:server.R
library(tigris)

### Load data from prep.R
la_boundary <- readRDS('la_boundary.rds')
la_tract_modesplit <- readRDS('la_tract_modesplit.rds')
tracts <- readRDS('la_tracts.rds')
=======
#api.key.install(key="00a2d85a7c7b1346879d0e355cc0361f30188f28") # Personal Email Census API Key
#api.key.install(key="9890281ff4c9bdd8980fc6ee6692b00769853229") # City Email Census API Key
options(tigris_use_cache = TRUE)

##### Download / Prep Data #####
### LA City Boundary
la_boundary <- rgdal::readOGR('data/LACityBoundary/CityBoundary.shp')
la_boundary <- st_as_sf(la_boundary)
la_boundary <- st_transform(la_boundary, 4326)

### Tigris: ACS Boundaries
options(tigris_class = "sf")
tracts <- tracts(state = 'CA', county = "037", cb=TRUE) # If cb is set to TRUE, download a generalized (1:500k) tracts file. Defaults to FALSE (the most detailed TIGER/Line file)
tracts <- st_transform(tracts, 4326)

### ACS: Means of Transportation for all census tracts in the Los Angeles County
years <- c(2010,2011,2012,2013,2014,2015)
la_tracts <- geo.make(state = "CA", county = "Los Angeles", tract = "*")

# Lapply function to download ACS data for selected years
la_tract_modesplit <- lapply(years, function(x) {
  
  print(x)
  
  # Use the ACS API to query for the data
  acs_result <- acs.fetch(key="00a2d85a7c7b1346879d0e355cc0361f30188f28",
                          endyear = x, 
                          span = 5,
                          geography = la_tracts,
                          table.number = "B08301",
                          col.names = "pretty")
  
  # Convert to data.frame for merging
  year_df <- data.frame(paste0(str_pad(acs_result@geography$state, 2, "left", pad="0"), 
                               str_pad(acs_result@geography$county, 3, "left", pad="0"), 
                               str_pad(acs_result@geography$tract, 6, "left", pad="0")), 
                        acs_result@estimate[,c(
                          "Means of Transportation to Work: Total:",
                          "Means of Transportation to Work: Car, truck, or van:",
                          "Means of Transportation to Work: Public transportation (excluding taxicab):",
                          "Means of Transportation to Work: Bicycle",
                          "Means of Transportation to Work: Walked")], 
                        stringsAsFactors = FALSE)
  
  # Clean & Format df
  rownames(year_df)<-1:nrow(year_df)
  names(year_df)<-c("GEOID","total", "car", "transit", "bicycle", "walk")
  
  # Calculate & Add Mode Splits
  year_df$car_pct <- 100*(year_df$car/year_df$total)
  year_df$transit_pct <- 100*(year_df$transit/year_df$total)
  year_df$bicycle_pct <- 100*(year_df$bicycle/year_df$total)
  year_df$walk_pct <- 100*(year_df$walk/year_df$total)
  
  # Subset out the perentages, then rename with year
  pct_names <- c("car_pct", "transit_pct", "bicycle_pct", "walk_pct")
  year_pct <- data.frame(year_df[,c("GEOID","car_pct", "transit_pct", "bicycle_pct", "walk_pct")])
  
  return(year_pct)
})
names(la_tract_modesplit) <- years
>>>>>>> 187530fe8bcbdc9b2a6c17b18d2ef0df921f10eb:acs_commute/server.R

##### Interactive Map #####
function(input, output, session) {
  
  ### Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      # Add polygons ahead of time to set initial zoom level
      addPolygons(data = la_boundary,
                  fill = FALSE)
  })
  
  ### Calculate the values that will be mapped, based on UI Input
  commute_merged <- reactive({
    if(input$maptype == '5yr_est'){
      
      # If 5-yr estimate is selected, return modesplits for that year
      year_selected <- toString(input$yearview)
      result_df <- geo_join(tracts, la_tract_modesplit[[year_selected]], "GEOID", "GEOID")
      
    } else if(input$maptype == 'time_change'){
      
      # Grab first / second year
      yr1 <- toString(input$yearRange[1])
      yr2 <- toString(input$yearRange[2])
      
      diff_df <- within(merge(la_tract_modesplit[[yr2]],la_tract_modesplit[[yr1]],by="GEOID"), {
        car_pct <- car_pct.x - car_pct.y
        transit_pct <- transit_pct.x - transit_pct.y
        bicycle_pct <- bicycle_pct.x - bicycle_pct.y
        walk_pct <- walk_pct.x - walk_pct.y
      })[,c("GEOID","car_pct","transit_pct","bicycle_pct","walk_pct")]
      
      result_df <- geo_join(tracts,diff_df, "GEOID", "GEOID")
    }
    
    return(result_df)
  })
  
  ### Clip the geography, if needed, based on UI Input
  commute_data <- reactive({
    geography <- input$geography
    if (geography == "city_tract"){
      clip_result <- st_intersection(commute_merged(), la_boundary)
    } else{
      clip_result <- commute_merged()
    }
    return(clip_result)
  })
  
  ### Subset out the mode, based on UI Input
  mode_data <- reactive({
    mode <- input$mode
    return(commute_data()[[mode]])
  })

  
  ### Update Map Symbology & Legend based on UI Input
  observe({
    
    # Formatting Mode for Legend & Popup
    modevars <- list('car_pct' = 'Private Vehicle',
                     'transit_pct' = 'Transit',
                     'bicycle_pct' = 'Bicycle',
                     'walk_pct' = 'Foot')
    
    # Legend  & Popup Title
    if(input$maptype == '5yr_est'){
      legend_title <- paste0("Percent Commuting<br>by ", modevars[input$mode])
      popup_title <- paste0("Percent Commuting by ", modevars[input$mode])
    } else if (input$maptype == 'time_change'){
      legend_title <- paste0("Change in Percent<br>Commuting by<br>", modevars[input$mode])
      popup_title <- paste0("Change in Percent Commuting by ", modevars[input$mode])
    }  

    # Update mode, popup, and color scales based on mode input
    popup <- paste0("GEOID: ", commute_data()$GEOID, "<br>", popup_title, ": ", round(mode_data(),2))
    pal <- colorNumeric(
      palette = "YlGnBu",
      domain = mode_data()
    )
    
    # Update the map
    leafletProxy("map") %>%
      clearControls() %>%
      clearShapes() %>%
      addPolygons(data = commute_data(),
                  fillColor = ~pal(mode_data()),
                  color = "#b2aeae", # you need to use hex colors
                  fillOpacity = 0.7,
                  weight = 1,
                  smoothFactor = 0.2,
                  stroke = FALSE,
                  popup = popup) %>%
      addLegend(pal = pal,
                values = mode_data(),
                position = "bottomleft",
                title = legend_title,
                labFormat = labelFormat(suffix = "%"))
  })
  
  
}