############################################
### 01. Shrimp Analysis -- Land Cleaning ###
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

# Create directories
dir.create("data/a_raw_data")

#####################################

# Directories
## shrimp directory
data_dir <- "data/a_raw_data"

## land directory
land_dir <- "data/a_raw_data/USGSEsriWCMC_GlobalIslands_v3/v108/globalislandsfix.gdb"

## Export directory
land_gpkg <- "data/shrimp_annual/land.gpkg"
export_dir <- "data/shrimp_annual"

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

load_start <- Sys.time()

### Load continental land data
continents <- sf::st_read(dsn = land_dir, layer = "USGSEsriWCMC_GlobalIslandsv2_Continents") %>%
  # use the land function to clean the data for later use
  land_function()
continents_time <- Sys.time()

paste("Time to take load continents data:", continents_time - load_start, units(continents_time - load_start))

### Load big island land data
big_islands <- sf::st_read(dsn = land_dir, layer = "USGSEsriWCMC_GlobalIslandsv2_BigIslands") %>%
  # make all features valid as an error may be generated otherwise
  sf::st_make_valid() %>%
  # use the land function to clean the data for later use
  land_function() %>%
  sf::st_make_valid()
big_islands_time <- Sys.time()

paste("Time to take load big islands data:", big_islands_time - continents_time, units(big_islands_time - continents_time))

### Load small island land data
small_islands <- sf::st_read(dsn = land_dir, layer = "USGSEsriWCMC_GlobalIslandsv2_SmallIslands") %>%
  # use the land function to clean the data for later use
  land_function()
small_islands_time <- Sys.time()

paste("Time to take load big islands data:", small_islands_time - big_islands_time, units(small_islands_time - big_islands_time))

### Load very small island land data
very_small_islands <- sf::st_read(dsn = land_dir, layer = "USGSEsriWCMC_GlobalIslandsv2_VerySmallIslands") %>%
  # use the land function to clean the data for later use
  land_function()
very_small_islands_time <- Sys.time()

paste("Time to take load big islands data:", very_small_islands_time - small_islands_time, units(very_small_islands_time - small_islands_time))

load_end <- Sys.time()
paste("Time to take load land data:", load_end - load_start, units(load_end - load_start))

#####################################
#####################################

# Export data
## land data
sf::st_write(obj = continents, dsn = land_gpkg, layer = "continents", append = F)
sf::st_write(obj = small_islands, dsn = land_gpkg, layer = "small_islands", append = F)
sf::st_write(obj = very_small_islands, dsn = land_gpkg, layer = "very_small_islands", append = F)

### ***Note: big islands cannot get saved to the geopackage so instead gets exported as an RDS (.RData) file
### This is likely as when it becomes a multipolygon it has more than 999 columns
### See more here: https://github.com/r-spatial/sf/issues/1506
terra::saveRDS(object = big_islands, file = file.path(paste(export_dir, "big_islands.RData", sep = "/")))

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
