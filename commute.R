#####
##### This app displays census tract-level commute mode splits #####
##### I am interested in ACS 5-year estimates                  #####

# page 1: citywide 1 year estimates
# page 2: census tract 5 year estimates
# Also, look into LA Times neighborhood boundaries (link below for summary)
# https://losangeles2b.wordpress.com/2012/03/27/commuter-mode-choice-2/#more-412

# A listing of available datasets
# https://www.socialexplorer.com/data/metadata/

# R tutorial of ACS data manipulation
# http://zevross.com/blog/2015/10/14/manipulating-and-mapping-us-census-data-in-r-using-the-acs-tigris-and-leaflet-packages-3/


##### Setup
library(acs) # For downloading ACS Data
library(tigris) # For census boundary data
library(leaflet) # For interactive mapping
api.key.install(key="00a2d85a7c7b1346879d0e355cc0361f30188f28") # ACS Data API Key

##### Download / Prep Data
# Tigris: ACS Boundaries
options(tigris_class = "sf")
tracts <- tracts(state = 'CA', county = "Los Angeles", cb=TRUE) # If cb is set to TRUE, download a generalized (1:500k) tracts file. Defaults to FALSE (the most detailed TIGER/Line file)

# ACS: Means of Transportation for all census tracts in the Los Angeles County
years <- c(2010,2011,2012,2013,2014,2015)
la_tracts <- geo.make(state = "CA", county = "Los Angeles", tract = "*")
la_tract_modesplit <- lapply(years, )
la_tract_modesplit <- acs.fetch(endyear = 2015, geography = la_tracts, table.number = "B08301", col.names = "pretty")

# Print data column names
attr(la_tract_modesplit,'acs.colnames')

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
commute_df <- select(commute_df, 1:6)
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

##### Leaflet Map
popup <- paste0("GEOID: ", commute_merged$GEOID, "<br>", "Percent of Households Commuting by Private Vehicle: ", round(commute_merged$car_pct,2))
pal <- colorNumeric(
  palette = "YlGnBu",
  domain = commute_merged$car_pct
)

map<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = commute_merged, 
              fillColor = ~pal(car_pct), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              stroke = FALSE,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = commute_merged$car_pct, 
            position = "bottomright", 
            title = "Percent of Households<br>Commuting by Private Veh",
            labFormat = labelFormat(suffix = "%")) 
map