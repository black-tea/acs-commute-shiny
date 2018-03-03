#####                                                          #####
##### This app displays census tract-level commute mode splits #####
#####                                                          #####

##### Setup #####
library(leaflet) # For interactive mapping
library(sf) # For manipulating spatial data
library(tigris)

### Load data from prep.R
la_boundary <- readRDS('data/la_boundary.rds')
la_tract_modesplit <- readRDS('data/la_tract_modesplit.rds')
tracts <- readRDS('data/la_tracts.rds')

options(tigris_use_cache = TRUE)

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
      legend_title <- paste0("People Commuting<br>by ", modevars[input$mode])
      popup_title <- paste0("Percent Commuting by ", modevars[input$mode])
    } else if (input$maptype == 'time_change'){
      legend_title <- paste0("Change in<br>Commuting by<br>", modevars[input$mode])
      popup_title <- paste0("Change in Percent Commuting by ", modevars[input$mode])
    }  

    # Update mode, popup, and color scales based on mode input
    popup <- paste0(round(mode_data(),2),"%")
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