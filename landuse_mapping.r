library(FedData)
library(sf)
library(terra)
library(dplyr)

# 1. Read CAPLTER sites file (with lat/long)
sites <- read.csv("Caplter/arthros_temporal.csv", stringsAsFactors = FALSE)

# columns are 'lat' and 'long'
sites_sf <- st_as_sf(
  sites,
  coords = c("long", "lat"),
  crs = 4326
)

# 2. Get NLCD data for the sites
nlcd <- get_nlcd(
  template = sites_sf,
  label = "cap_lter",
  year = 2019,     
  dataset = "landcover",
  landmass = "L48"
)

# 3. Extract NLCD value at each site
vals <- terra::extract(nlcd, vect(sites_sf))
# vals[,2] is currently a factor/character with labels like "Shrub/Scrub"
sites$nlcd_label <- as.character(vals[, 2])

# 4. Collapse NLCD labels to 3 regions
sites$landuse <- case_when(
  # Developed classes --> Urban
  sites$nlcd_label %in% c(
    "Developed, Open Space",
    "Developed, Low Intensity",
    "Developed, Medium Intensity",
    "Developed, High Intensity"
  ) ~ "Urban",

  # Desert-like classes
  sites$nlcd_label %in% c(
    "Shrub/Scrub",
    "Barren Land"
  ) ~ "Desert",

  # Agricultural classes
  sites$nlcd_label %in% c(
    "Pasture/Hay",
    "Cultivated Crops"
  ) ~ "Agricultural",

  TRUE ~ "Other"
)

# sanity check
print(table(sites$landuse, useNA = "ifany"))

# 5. Save a simple mapping for Python
write.csv(
  sites[, c("site_code", "landuse", "nlcd_label")],
  "Caplter/site_landuse_from_nlcd.csv",
  row.names = FALSE
)
