############################################
### 0. Shrimp Analysis -- Gulf of Mexico ###
############################################

# Clear environment
rm(list = ls())

# Calculate start time of code (determine how long it takes to complete all code)
start <- Sys.time()

#####################################
#####################################

# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(docxtractr,
               dplyr,
               elsa,
               fasterize,
               fs,
               geosphere,
               ggplot2,
               janitor,
               microbenchmark,
               ncf,
               paletteer,
               pdftools,
               plyr,
               purrr,
               raster,
               RColorBrewer,
               reshape2,
               rgdal,
               rgeoda,
               rgeos,
               rmapshaper,
               rnaturalearth, # use devtools::install_github("ropenscilabs/rnaturalearth") if packages does not install properly
               RSelenium,
               sf,
               shadowr,
               sp,
               stringr,
               terra, # is replacing the raster package
               tidyr,
               tidyverse)

# Resources
## geosphere package Github: https://github.com/rspatial/geosphere
## geosphere reference manual: https://cran.r-project.org/web/packages/geosphere/geosphere.pdf

#####################################
#####################################

# Create directories
dir.create("data/shrimp_annual")
dir.create("data/a_raw_data")

#####################################

# Directories
## shrimp directory
data_dir <- "data/a_raw_data"

## land directory
land_dir <- "data/a_raw_data/USGSEsriWCMC_GlobalIslands_v3/v108/globalislandsfix.gdb"

## Expoort directory
shrimp_gpkg <- "data/shrimp_annual/shrimp.gpkg"

#####################################

# View layer names within geodatabase
## ****Note: should notice 4 layers
sf::st_layers(dsn = land_dir,
              do_count = TRUE)

#####################################
#####################################

# Function to create clean land feature data
## The function will take the input (land data) and then return a single feature

land_function <- function(land_data){
  land_layer <- land_data %>%
    # rectify any issues
    sf::st_make_valid() %>%
    # create field called "land"
    dplyr::mutate(land = "land") %>%
    # select the "land" field
    dplyr::select(land) %>%
    # reproject the coordinate reference system
    sf::st_transform("EPSG:4326") %>%
    # group all rows by the different elements with "land" field -- this will create a row for the grouped data
    dplyr::group_by(land) %>%
    # summarise all those grouped elements together -- in effect this will create a single feature
    dplyr::summarise()
  return(land_layer)
}

#####################################
#####################################

# load data
## Effort
# load(file.path(data_dir, "effort_2014_2021.RData"))

## Pings
load(file.path(data_dir, "pings_2014_2021.RData"))

### Load continental land data
continents <- sf::st_read(dsn = land_dir, layer = "USGSEsriWCMC_GlobalIslandsv2_Continents") %>%
  # use the land function to clean the data for later use
  land_function()

### Load big island land data
big_islands <- sf::st_read(dsn = land_dir, layer = "USGSEsriWCMC_GlobalIslandsv2_BigIslands") %>%
  # make all features valid as an error may be generated otherwise
  sf::st_make_valid() %>%
  # use the land function to clean the data for later use
  land_function()

### Load small island land data
small_islands <- sf::st_read(dsn = land_dir, layer = "USGSEsriWCMC_GlobalIslandsv2_SmallIslands") %>%
  # use the land function to clean the data for later use
  land_function()

### Load very small island land data
very_small_islands <- sf::st_read(dsn = land_dir, layer = "USGSEsriWCMC_GlobalIslandsv2_VerySmallIslands") %>%
  # use the land function to clean the data for later use
  land_function()

#####################################

# Examine data
head(pings_2014_2021)
names(pings_2014_2021)

## get years
ping_years <- pings_2014_2021 %>%
  dplyr::select(STAMP) %>%
  dplyr::mutate(start_date = format(as.Date(STAMP, format="%Y/%m/%d"),"%Y"))

years <- list(unique(ping_years$start_date))

#####################################
#####################################

# run analysis
i <- 1
for(i in 1:length(years)){
  start_time2 <- Sys.time()
  
  shrimp_ping_year <- paste0("shrimp_pings", years[[1]][i])
  shrimp_ping_ocean_year <- paste0("shrimp_pings_ocean", years[[1]][i])
  shrimp_transect_year <- paste0("shrimp_transects", years[[1]][i])
  
  shrimp_pings <- pings_2014_2021 %>%
    # limit to fishing activity
    dplyr::filter(vessel_state == "fishing") %>%
    # filter for tows that start in year of interest
    ## ***Note: faster to do as separate filter than combining with activity filter
    dplyr::filter(.,
                  stringr::str_detect(string = .$STAMP,
                                      pattern = years[[1]][i])) %>%
    # remove bad coordinates
    filter(between(LATITUDE, -90, 90), between(LONGITUDE, -180, 180)) %>%
    # remove duplicates 
    distinct(VSBN, SERIAL, STAMP, LONGITUDE, LATITUDE) %>%
    # sort by time stamp within vessels
    arrange(VSBN, STAMP) %>%
    group_by(VSBN, SERIAL) %>%
    # calculate distances and times
    ## ***Note: geopackage was used originally by Kyle Dettloff (kyle.dettloff@noaa.gov) when creating the original dataset
    ##          The methods noted that the previous analysis used Vincenty ellipsoid method given it took the curvature
    ##          of the earth into consideration (https://sedarweb.org/documents/sedar-87-dw-01-estimation-of-commercial-shrimp-effort-in-the-gulf-of-mexico/)
    ##          Original methods: https://github.com/kyledettloff-NOAA/GOMshrimpEffort/blob/main/effort_scaled.R
    
    ## To learn more about the methods see here: http://www.movable-type.co.uk/scripts/latlong-vincenty.html
    ## and here: https://github.com/rspatial/geosphere/blob/master/R/distVincentyEllipsoid.R
    
    ## ***Note: distances will need to be under 1 nautical mile (1 nautical mile = 1852 meters)
    mutate(nm = geosphere::distVincentyEllipsoid(cbind(LONGITUDE, LATITUDE),
                                                 cbind(lag(LONGITUDE), lag(LATITUDE))) / 1852,
           start_date = format(as.POSIXct(STAMP), format="%Y/%m/%d"),
           start_time = format(as.POSIXct(STAMP), format = "%H:%M:%S"),
           mins = as.numeric(STAMP - lag(STAMP), units = "mins")) %>%
    # move the "nm" to be before the "mins" field
    dplyr::relocate(nm,
                    .before = mins) %>%
    # convert to sf feature
    sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"),
                 # set the coordinate reference system to WGS84
                 crs = 4326, # EPSG 4326 (https://epsg.io/4326)
                 # keep longitude and latitude fields
                 remove = FALSE) %>%
    # create a transect field for each new vessel
    ## transect is defined as when the distance between points is under 1 nautical mile and within 30 minutes of the previous points
    ## ***note: a new transect will begin if the nautical mile distance is great than 1 or minutes difference is greater than 30
    ## ***note: new vessels will have NA for both the "nm" and "mins" fields and the transect will start at 0 again
    dplyr::mutate(transect = ifelse(test = (nm <= 1.0 & mins <= 30) | is.na(nm),
                                    yes = 0,
                                    no = 1) %>% cumsum(),
                  vessel_trans = paste(VSBN, transect, sep = "_"))
  
  assign(shrimp_ping_year, shrimp_pings)
  
  total_time <- Sys.time() - start_time2
  print(total_time)
  
  # Export data
  sf::st_write(obj = shrimp_ping_year, dsn = export_dir, layer = paste0("shrimp_pings", years[[1]][i]), append = F)
}



#####################################

# # Troubleshooting
# test <- shrimp_pings_2014 %>%
#   # dplyr::filter(VSBN %in% c("1022072", "1038803", "1050624")) %>%
#   # dplyr::mutate(transect = ifelse(test = (nm <= 1.0 & mins >= 0 & mins <= 30) | is.na(nm),
#   #                                 yes = 0,
#   #                                 no = 1) %>% cumsum,
#   #               vessel_trans = paste(VSBN, transect, sep = "_")) %>%
#   dplyr::filter((nm > 1.0 & mins < 30) | (nm < 1.0 & mins > 30))
# View(test)

## Examine top of Shrimp pinged data for 2014
head(shrimp_pings)

list(unique(shrimp_pings$VSBN))
list(unique(shrimp_pings$SERIAL))

#####################################
#####################################

# remove any pings that fall on land
start_time2 <- Sys.time()
shrimp_pings_ocean <- shrimp_pings %>%
  sf::st_make_valid() %>%
  # Remove continental land
  sf::st_difference(continents) %>%
  # Remove big island land
  sf::st_difference(big_islands) %>%
  # Remove small island land
  sf::st_difference(small_islands) %>%
  # Remove very small island land
  sf::st_difference(very_small_islands)

assign(shrimp_ping_ocean_year, shrimp_pings_ocean)

total_time <- Sys.time() - start_time2
print(total_time)

#####################################
#####################################

start_time <- Sys.time()
shrimp_transects <- shrimp_pings_ocean %>%
  dplyr::group_by(vessel_trans) %>%
  dplyr::summarise() %>%
  sf::st_cast(x = .,
              to = "MULTIPOINT") %>%
  sf::st_cast(x = .,
              to = "LINESTRING")

assign(shrimp_transect_year, shrimp_transects)

total_time <- Sys.time() - start_time
print(total_time)

#####################################
#####################################

# Export data
## Shrimp
sf::st_write(obj = shrimp_pings_2014, dsn = export_dir, layer = "shrimp_pings", append = F)
sf::st_write(obj = shrimp_2014_transects, dsn = export_dir, layer = "shrimp_transects", append = F)

## land data
sf::st_write(obj = continents, dsn = export_dir, layer = "continents", append = F)
sf::st_write(obj = big_islands, dsn = export_dir, layer = "big_islands", append = F)
sf::st_write(obj = small_islands, dsn = export_dir, layer = "small_islands", append = F)
sf::st_write(obj = very_small_islands, dsn = export_dir, layer = "very_small_islands", append = F)
