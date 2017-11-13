##### Setup
library(acs)
library(tigris)

##### LA City Boundary
la_boundary <- st_read('data/la_boundary/CityBoundary.shp')
la_boundary <- st_transform(la_boundary, 4326)

##### Tigris: ACS Boundaries
options(tigris_class = "sf")
# If cb is set to TRUE, download a generalized (1:500k) tracts file, 
# defaults to FALSE (the most detailed TIGER/Line file)
tracts <- tracts(state = 'CA', county = "Los Angeles", cb=TRUE)
tracts <- st_transform(tracts, 4326)

##### ACS: Means of Transportation for all census tracts in the Los Angeles County
api.key.install(key="9890281ff4c9bdd8980fc6ee6692b00769853229") # ACS Data API Key
years <- c(2010,2011,2012,2013,2014,2015)
la_tracts <- geo.make(state = "CA", county = "Los Angeles", tract = "*")

# Lapply function to download ACS data for selected years, resulting in a list of dfs
la_tract_modesplit <- lapply(years, function(x) {
  
  # For tracking
  print(x)
  
  # Use the ACS API to query for the data
  acs_result <- acs.fetch(endyear = x, 
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
  
  # Return final df
  return(year_pct)
  
})

# Name the dfs from the years 
names(la_tract_modesplit) <- years

# Output to .rds
saveRDS(tracts, 'la_tracts.rds')
saveRDS(la_tract_modesplit, 'la_tract_modesplit.rds')
saveRDS(la_boundary, 'la_boundary.rds')


