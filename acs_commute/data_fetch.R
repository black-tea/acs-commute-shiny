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

# Lapply function to download ACS data for selected years
la_tract_modesplit <- lapply(years, function(x) {
  
  # Get the year of the query
  yr <- str(x)
  
  # Use the ACS API to query for the data
  acs_result <- acs.fetch(endyear = x, 
                      span = 5,
                      geography = la_tracts,
                      table.number = "B08301",
                      col.names = "pretty")
  
  # Convert to data.frame for merging
  year_df <- data.frame(acs_result@estimate[,c(
                             "Means of Transportation to Work: Total:",
                             "Means of Transportation to Work: Car, truck, or van:",
                             "Means of Transportation to Work: Public transportation (excluding taxicab):",
                             "Means of Transportation to Work: Bicycle",
                             "Means of Transportation to Work: Walked")], 
                           stringsAsFactors = FALSE)
  
  # Clean & Format df
  rownames(year_df)<-1:nrow(year_df)
  names(year_df)<-c("total", "car", "transit", "bicycle", "walk")
  
  # Calculate & Add Mode Splits
  year_df$car_pct <- 100*(year_df$car/year_df$total)
  year_df$transit_pct <- 100*(year_df$transit/year_df$total)
  year_df$bicycle_pct <- 100*(year_df$bicycle/year_df$total)
  year_df$walk_pct <- 100*(year_df$walk/year_df$total)
  
  # Subset out the perentages, then rename with year
  year_pct <- data.frame(year_df[,c("car_pct", "transit_pct", "bicycle_pct", "walk_pct")])
  names(year_pct) <- lapply()
})

# Create commute_df (similar to before)
# Use do.call and cbind to then combine the list of dfs into the commute_df