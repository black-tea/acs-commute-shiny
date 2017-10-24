
library(leaflet)

# Choices for drop-downs
mode_vars <- c(
  "Car, Truck, Van" = "car_pct",
  "Public Transit" = "transit_pct",
  "Bicycle" = "bicycle_pct",
  "Walked" = "walk_pct"
)
geo_vars <- c(
  "LA City - Tract" = "city_tract",
  "LA County - Tract" = "county_tract"
)

navbarPage("ACS Means of Transportation to Work", id="nav",
           
           tabPanel("Interactive map",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h2("Map explorer"),
                                      
                                      selectInput("geography","Geography",geo_vars, selected = "county_tract"),
                                      selectInput("mode", "Mode", mode_vars, selected = "car_pct")
                                      
                                      #plotOutput("histCentile", height = 200),
                                      #plotOutput("scatterCollegeIncome", height = 250)
                        )
                        
                    )
           ),
           
           conditionalPanel("false", icon("crosshair"))
)
