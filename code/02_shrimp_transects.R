###############################################
### 02. Shrimp Analysis -- Shrimp Transects ###
###############################################

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
               stringi,
               stringr,
               terra, # is replacing the raster package
               tidyr,
               tidyverse)

# Resources
## geosphere package Github: https://github.com/rspatial/geosphere
## geosphere reference manual: https://cran.r-project.org/web/packages/geosphere/geosphere.pdf

#####################################
#####################################

# Directories
## shrimp directory
data_dir <- "data/a_raw_data"

## big islands directory
big_islands_dir <- "data/shrimp_annual"

## land directory
land_dir <- "data/shrimp_annual/land.gpkg"

## Export directory
shrimp_gpkg <- "data/shrimp_annual/shrimp.gpkg"
shapefile_dir <- "data/shrimp_annual/shapefiles"

#####################################

# View layer names within geopackage
## ****Note: should notice 3 layers
sf::st_layers(dsn = land_dir,
              do_count = TRUE)

#####################################
#####################################

# Set parameters
## Coordinate reference system
ping_crs <- 4326 # WGS84 --> EPSG 4326 (https://epsg.io/4326)

#####################################
#####################################

load_start <- Sys.time()

# load data
## Effort
# load(file.path(data_dir, "effort_2014_2021.RData"))

## Pings
load(file.path(data_dir, "pings_2014_2021.RData"))
shrimp_time <- Sys.time()

### Load continental land data
continents <- sf::st_read(dsn = land_dir, layer = "continents")
continents_time <- Sys.time()

### Load big island land data
big_islands <- terra::readRDS(file = file.path(big_islands_dir, "big_islands.RData"))
big_islands_time <- Sys.time()

### Load small island land data
small_islands <- sf::st_read(dsn = land_dir, layer = "small_islands")
small_islands_time <- Sys.time()

### Load very small island land data
very_small_islands <- sf::st_read(dsn = land_dir, layer = "very_small_islands")
very_small_islands_time <- Sys.time()

load_end <- Sys.time()
paste("Time to take load shrimp data:", shrimp_time - load_start, units(shrimp_time - load_start),
      "Time to take load continents data:", continents_time - shrimp_time, units(continents_time - shrimp_time),
      "Time to take load big islands data:", big_islands_time - continents_time, units(big_islands_time - continents_time),
      "Time to take load small islands data:", small_islands_time - big_islands_time, units(small_islands_time - big_islands_time),
      "Time to take load very small islands data:", very_small_islands_time - small_islands_time, units(very_small_islands_time - small_islands_time),
      "Time to take load shrimp and land data:", load_end - load_start, units(load_end - load_start))

#####################################

# Examine data
head(pings_2014_2021)
names(pings_2014_2021)

## get years
ping_year <- pings_2014_2021 %>%
  dplyr::select(STAMP) %>%
  dplyr::mutate(start_date = format(as.Date(STAMP, format="%Y/%m/%d"),"%Y"))

year_list <- as.vector((unique(ping_year$start_date)))
print(year_list)

#####################################
#####################################

# run analysis
i <- 1
for(i in 1:length(year_list)){
  
  # designate loop start time
  start_time <- Sys.time()
  
  # designate fishing analysis start time
  fishing_pings <- Sys.time()
  
  #####################################
  
  # define annual object names for shrimp data
  ## shrimp ping data by year
  shrimp_ping_year <- paste0("shrimp_pings", year_list[i])
  
  ## shrimp ping data only in ocean by year
  shrimp_ping_ocean_year <- paste0("shrimp_pings_ocean", year_list[i])
  
  ## shrimp transect data by year
  shrimp_transect_year <- paste0("shrimp_transects", year_list[i])
  
  #####################################
  #####################################
  
  # create shrimp ping data for a particular year
  shrimp_pings <- pings_2014_2021 %>%
    # limit to fishing activity
    dplyr::filter(vessel_state == "fishing") %>%
    # filter for tows that start in year of interest
    ## ***Note: faster to do as separate filter than combining with activity filter
    dplyr::filter(.,
                  # search for the year within the STAMP field
                  stringr::str_detect(string = .$STAMP,
                                      pattern = year_list[i])) %>%
    # remove bad coordinates
    ## any latitudes below or above -90 and 90 or longitudes below and above -180 and 180 are not real
    filter(between(LATITUDE, -90, 90), between(LONGITUDE, -180, 180)) %>%
    # remove duplicates 
    distinct(VSBN, SERIAL, STAMP, LONGITUDE, LATITUDE) %>%
    
    # # sort by time stamp within vessels
    # arrange(VSBN, STAMP) %>%
    # group by vessel
    group_by(VSBN, SERIAL) %>%
    
    # sort by time stamp within vessels and serial numbers
    arrange(VSBN, SERIAL, STAMP) %>%
    
    # calculate distances and times
    ## ***Note: geopackage was used originally by Kyle Dettloff (kyle.dettloff@noaa.gov) when creating the original dataset
    ##          The methods noted that the previous analysis used Vincenty ellipsoid method given it took the curvature
    ##          of the earth into consideration (https://sedarweb.org/documents/sedar-87-dw-01-estimation-of-commercial-shrimp-effort-in-the-gulf-of-mexico/)
    ##          Original methods: https://github.com/kyledettloff-NOAA/GOMshrimpEffort/blob/main/effort_scaled.R
    
    ## To learn more about the methods see here: http://www.movable-type.co.uk/scripts/latlong-vincenty.html
    ## and here: https://github.com/rspatial/geosphere/blob/master/R/distVincentyEllipsoid.R
    
    ## ***note: distances will need to be under 1 nautical mile (1 nautical mile = 1852 meters)
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
                 crs = ping_crs, # EPSG 4326 (https://epsg.io/4326)
                 # keep longitude and latitude fields
                 remove = FALSE) %>%
    
    # create a transect field for each new vessel
    ## transect is defined as when the distance between points is under 1 nautical mile and within 30 minutes of the previous points
    ## ***note: a new transect will begin if the nautical mile distance is great than 1 or minutes difference is greater than 30
    ## ***note: new vessels will have NA for both the "nm" and "mins" fields and the transect will start at 0 again
    dplyr::mutate(transect = ifelse(test = (nm <= 1.0 & mins <= 30) | is.na(nm),
                                    yes = 0,
                                    no = 1) %>% cumsum(),
                  vessel_trans = paste(VSBN, SERIAL, transect, sep = "_"))
  
  # assign the shrimp pings data looped to templated annual data object
  assign(shrimp_ping_year, shrimp_pings)
  
  # Export data
  sf::st_write(obj = shrimp_pings, dsn = shrimp_gpkg, layer = paste0("shrimp_pings", year_list[i]), append = F)
  
  # calculate time to create annual shrimp fishing data
  fishing_total <- Sys.time() - fishing_pings
  print(fishing_total)
  
  #####################################
  #####################################
  
  # start time for ocean ping data analysis
  ocean_time <- Sys.time()
  
  # create shrimp ping data by year that do not fall on land
  shrimp_pings_ocean <- shrimp_pings %>%
    # check validity or fix any invalid geometry
    sf::st_make_valid() %>%
    
    # Remove continental land
    sf::st_difference(continents) %>%
    # Remove big island land
    sf::st_difference(big_islands) %>%
    # Remove small island land
    sf::st_difference(small_islands) %>%
    # Remove very small island land
    sf::st_difference(very_small_islands)
  
  # assign the shrimp pings data looped to templated annual data object
  assign(shrimp_ping_ocean_year, shrimp_pings_ocean)
  
  # Export data
  sf::st_write(obj = shrimp_pings_ocean, dsn = shrimp_gpkg, layer = paste0("shrimp_pings_ocean", year_list[i]), append = F)
  
  # calculate time to create annual shrimp fishing data in only ocean areas
  ocean_total <- Sys.time() - ocean_time
  print(ocean_total)
  
  # print the time it takes to complete the first two parts (fishing and now ocean pings by year)
  second_phase <- Sys.time() - start_time
  print(paste("Time to complete first two analyses took:", second_phase, units(Sys.time() - start_time)))
  
  #####################################
  #####################################
  
  transect_time <- Sys.time()
  
  # create transects for marine only shrimp data by year
  shrimp_transects <- shrimp_pings_ocean %>%
    # group by the vessel transect
    dplyr::group_by(VSBN, vessel_trans) %>%
    # summarise all the associated points along the transect
    ## do_union = FALSE will make points get added in order for the transect
    dplyr::summarise(do_union = FALSE) %>%
    # add year back as a field
    dplyr::mutate(year = year_list[i]) %>%
    # relocate year before vessel and vessel transect
    dplyr::relocate(year,
                    .before = VSBN) %>%
    # set as simple feature (sf)
    sf::st_sf() %>%
    # ensure all points are multipoint
    ## ***note: this is for any transect that is a single point
    sf::st_cast(x = .,
                to = "MULTIPOINT") %>%
    # change all the points to linestring to make them a single line feature
    sf::st_cast(x = .,
                to = "LINESTRING") %>%
    # reproject the coordinate reference system to match BOEM call areas
    sf::st_transform("EPSG:5070") # EPSG 5070 (https://epsg.io/5070)
  
  # assign the shrimp transects data looped to templated annual data object
  assign(shrimp_transect_year, shrimp_transects)
  
  # Export data
  ## Geopackage
  sf::st_write(obj = shrimp_transects, dsn = shrimp_gpkg, layer = paste0("shrimp_transects", year_list[i]), append = F)
  
  ## Shapefile (***note: this is so the year transects can get used by terra to create a raster dataset)
  # sf::st_write(obj = shrimp_transects, dsn = file.path(paste(shapefile_dir, paste0("shrimp_transects_", year_list[i], ".shp"), sep = "/")), append = F)
  
  # calculate time to create annual shrimp fishing transect data in only ocean areas
  transect_time <- Sys.time() - transect_time
  print(transect_time)
  
  #####################################
  #####################################
  
  # calculate total time to finish the three components (fishing, ocean, and transect)
  total_time <- Sys.time() - start_time
  print(paste0("Time to run the whole analysis for ", year_list[i], ": ", total_time, units(Sys.time() - start_time)))
}

#####################################
#####################################

# # Troubleshooting
# test <- shrimp_pings_2014 %>%
#   # dplyr::filter(VSBN %in% c("1022072", "1038803", "1050624")) %>%
#   # dplyr::mutate(transect = ifelse(test = (nm <= 1.0 & mins >= 0 & mins <= 30) | is.na(nm),
#   #                                 yes = 0,
#   #                                 no = 1) %>% cumsum(),
#   #               vessel_trans = paste(VSBN, transect, sep = "_")) %>%
#   dplyr::filter((nm > 1.0 & mins < 30) | (nm < 1.0 & mins > 30))
# View(test)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
