###############################################
### 03. Shrimp Analysis -- Shrimp Transects ###
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
## shrimp geopackage
shrimp_gpkg <- "data/shrimp_annual/shrimp.gpkg"

#####################################

# View layer names within geopackage
transect_list <- c(sf::st_layers(dsn = shrimp_gpkg,
                                 do_count = TRUE)[[1]]) %>%
  # get only the layers that are for transects
  stringr::str_subset(string = .,
                      pattern = "transects") %>%
  sort(decreasing = T) %>%
  # remove the last element (NA) that comes from the dataset
  head(., -1) %>%
  # switch again so annual transects are ascending
  sort(decreasing = F)

# get years list
year_list <- transect_list %>%
  ## substitute nothing ("") in place of the "shrimp_transects" that is at the beginning of each layer name
  ## this will give only the year
  sub("shrimp_transects", "", .)

#####################################
#####################################

# create reference table
shrimp_transects <- sf::st_sf(vessel_trans = "",
                              # make geometry a linestring
                              geom = st_sfc(lapply(1, function(x) st_linestring())),
                              # set coordinate reference system as EPSG:5070
                              crs = "EPSG:5070") %>%
  # remove first blank observation
  dplyr::slice(-1)

#####################################

# load and clean shrimp transect data
# i <- 1
for(i in 1:length(transect_list)){
  
  # designate loop start time
  start_time <- Sys.time()
  
  #####################################
  
  # create placeholder raster name
  transect_year <- paste("shrimp_transect", year_list[i], sep = "_")
  
  #####################################
  
  # load data
  shrimp_year <- sf::st_read(dsn = shrimp_gpkg, layer = transect_list[i])
  
  #####################################
  
  # assign the shrimp pings data looped to templated annual data object
  assign(transect_year, shrimp_year)
  
  # add years to reference sf
  shrimp_transects <- rbind(shrimp_transects, shrimp_year)
  
  # remove shrimp_year from environment
  rm(shrimp_year)
  
  print(paste("Time to load shrimp transect data for ", year_list[i], ": ", Sys.time() - start_time, units(Sys.time() - start_time)))
}

#####################################
#####################################

# Export data
## Vector
sf::st_write(obj = shrimp_transects, dsn = shrimp_gpkg, layer = "shrimp_transects", sep = "/", append = F)

#####################################
#####################################

# calculate end time and print time difference
paste(Sys.time() - start) # print how long it takes to calculate