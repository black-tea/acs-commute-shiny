#####
##### This app displays census tract-level commute mode splits #####
##### I am interested in ACS 5-year estimates                  #####

##### Next Steps
##### 1. Small bar chart on right showing past five years
##### 2. Option: Latest Estimate vs. Changes 
##### w/ dynamic fields allowing them to select which years to compare
##### 3. Format "mode" more nicely on server side

##### Setup
library(acs) # For downloading ACS Data
library(tigris) # For census boundary data
library(leaflet) # For interactive mapping
library(rgdal) # For importing shapefiles
library(sf) # For manipulating spatial data
api.key.install(key="00a2d85a7c7b1346879d0e355cc0361f30188f28") # ACS Data API Key
options(tigris_use_cache = TRUE)
#setwd("~/GitHub/acs-commute-shiny/acs_commute/data/LACityBoundary")

##### Download / Prep Data
# LA City Boundary
la_boundary <- rgdal::readOGR('data/LACityBoundary/CityBoundary.shp')
la_boundary <- st_as_sf(la_boundary)
la_boundary <- st_transform(la_boundary, 4326)

# Tigris: ACS Boundaries
options(tigris_class = "sf")
tracts <- tracts(state = 'CA', county = "037", cb=TRUE) # If cb is set to TRUE, download a generalized (1:500k) tracts file. Defaults to FALSE (the most detailed TIGER/Line file)

# ACS: Means of Transportation for all census tracts in the Los Angeles County
la_tracts <- geo.make(state = "CA", county = "Los Angeles", tract = "*")
la_tract_modesplit <- acs.fetch(endyear = 2015, span = 5, geography = la_tracts, table.number = "B08301", col.names = "pretty")

# Convert to data.frame for merging
commute_df <- data.frame(paste0(str_pad(la_tract_modesplit@geography$state, 2, "left", pad="0"), 
                                str_pad(la_tract_modesplit@geography$county, 3, "left", pad="0"), 
                                str_pad(la_tract_modesplit@geography$tract, 6, "left", pad="0")), 
                         la_tract_modesplit@estimate[,c(
                           "Means of Transportation to Work: Total:",
                           "Means of Transportation to Work: Car, truck, or van:",
                           "Means of Transportation to Work: Public transportation (excluding taxicab):",
                           "Means of Transportation to Work: Bicycle",
                           "Means of Transportation to Work: Walked")], 
                         stringsAsFactors = FALSE)

# Clean & Format df
rownames(commute_df)<-1:nrow(commute_df)
names(commute_df)<-c("GEOID", "total", "car", "transit", "bicycle", "walk")

# Calculate & Add Mode Splits
commute_df$car_pct <- 100*(commute_df$car/commute_df$total)
commute_df$transit_pct <- 100*(commute_df$transit/commute_df$total)
commute_df$bicycle_pct <- 100*(commute_df$bicycle/commute_df$total)
commute_df$walk_pct <- 100*(commute_df$walk/commute_df$total)

# Examine resulting df
head(commute_df)

# Merge ACS df with Tigris Spatial data
commute_merged <- geo_join(tracts, commute_df, "GEOID", "GEOID")
commute_merged <- st_transform(commute_merged, 4326)

##### Interactive Map
function(input, output, session) {
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = la_boundary,
                  fill = FALSE)
  })
  
  # This observer is responsible for maintaining the geography
  # Here I am going to need to do the sf filter
  
  # This observer is responsible for maintaining the symbology and legend,
  # according to the variables the user has chosen to map.
  observe({
    
    # Update Geography based on geography input
    geography <- input$geography
    if (geography == "city_tract"){
      commute_data <- st_intersection(commute_merged, la_boundary)
    } else {
      commute_data <- commute_merged
    }
    
    # Update mode, popup, and color scales based on mode input
    mode <- input$mode
    mode_data <- commute_data[[mode]]
    popup <- paste0("GEOID: ", commute_data$GEOID, "<br>", "Percent of Households Commuting by ", mode, ": ", round(mode_data,2))
    pal <- colorNumeric(
      palette = "YlGnBu",
      domain = mode_data
    )
  
    # Update the map
    leafletProxy("map", data = commute_data) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(fillColor = ~pal(mode_data), 
                  color = "#b2aeae", # you need to use hex colors
                  fillOpacity = 0.7, 
                  weight = 1, 
                  smoothFactor = 0.2,
                  stroke = FALSE,
                  popup = popup) %>%
      addLegend(pal = pal, 
                values = mode_data, 
                position = "bottomleft", 
                title = paste0("Pct Commuting<br>by ", mode),
                labFormat = labelFormat(suffix = "%"))
  })
  
}