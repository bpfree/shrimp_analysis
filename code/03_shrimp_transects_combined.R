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

## Export directory
shapefile_dir <- "data/shrimp_annual/shapefiles"

#####################################

# View layer names within geopackage
sf::st_layers(dsn = shrimp_gpkg,
              do_count = TRUE)

#####################################
#####################################

load_start <- Sys.time()

# load shrimp transect data
## 2014 transects
shrimp_transects2014 <- sf::st_read(dsn = shrimp_gpkg, layer = "shrimp_transects2014")
time2014 <- Sys.time()

## 2015 transects
shrimp_transects2015 <- sf::st_read(dsn = shrimp_gpkg, layer = "shrimp_transects2015")
time2015 <- Sys.time()

## 2016 transects
shrimp_transects2016 <- sf::st_read(dsn = shrimp_gpkg, layer = "shrimp_transects2016")
time2016 <- Sys.time()

## 2017 transects
shrimp_transects2017 <- sf::st_read(dsn = shrimp_gpkg, layer = "shrimp_transects2017")
time2017 <- Sys.time()

## 2018 transects
shrimp_transects2018 <- sf::st_read(dsn = shrimp_gpkg, layer = "shrimp_transects2018")
time2018 <- Sys.time()

## 2019 transects
shrimp_transects2019 <- sf::st_read(dsn = shrimp_gpkg, layer = "shrimp_transects2019")
time2019 <- Sys.time()

## 2020 transects
shrimp_transects2020 <- sf::st_read(dsn = shrimp_gpkg, layer = "shrimp_transects2020")
time2020 <- Sys.time()

## 2021 transects
shrimp_transects2021 <- sf::st_read(dsn = shrimp_gpkg, layer = "shrimp_transects2021")
time2021 <- Sys.time()

load_end <- Sys.time()
paste("Time to take load 2014 data:", time2014 - load_start, units(time2014 - load_start),
      "Time to take load 2015 data:", time2015 - load_start, units(time2015 - load_start),
      "Time to take load 2016 data:", time2016 - load_start, units(time2016 - load_start),
      "Time to take load 2017 data:", time2017 - load_start, units(time2017 - load_start),
      "Time to take load 2018 data:", time2018 - load_start, units(time2018 - load_start),
      "Time to take load 2019 data:", time2019 - load_start, units(time2019 - load_start),
      "Time to take load 2020 data:", time2020 - load_start, units(time2020 - load_start),
      "Time to take load 2021 data:", time2021 - load_start, units(time2021 - load_start),
      "Time to load all shrimp transect data:", load_end - load_start, units(load_end - load_start))

#####################################
#####################################

# combine all transects
## combine all rows of transects across years 
shrimp_transects <- rbind(shrimp_transects2014,
                          shrimp_transects2015,
                          shrimp_transects2016,
                          shrimp_transects2017,
                          shrimp_transects2018,
                          shrimp_transects2019,
                          shrimp_transects2020,
                          shrimp_transects2021) %>%
  # reproject the coordinate reference system to match BOEM call areas
  sf::st_transform("EPSG:5070") # EPSG 5070 (https://epsg.io/5070)

#####################################
#####################################

# Export data
## Vector
sf::st_write(obj = shrimp_transects, dsn = shrimp_gpkg, layer = "shrimp_transects", sep = "/", append = F)
sf::st_write(obj = shrimp_transects, dsn = file.path(paste(shapefile_dir, "shrimp_transects.shp", sep = "/")), append = F)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start, units(Sys.time() - start)) # print how long it takes to calculate