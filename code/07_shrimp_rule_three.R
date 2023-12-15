###########################################################
### 07. Shrimp Analysis -- Shrimp Vessel Rule of Three  ###
###########################################################

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
## shrimp shapefiles
shrimp_gpkg <- "data/shrimp_annual/shrimp.gpkg"

## raster dir
raster_dir <- "data/shrimp_annual/rasters"

#####################################

# View layer names within geopackage
sf::st_layers(dsn = shrimp_gpkg,
              do_count = TRUE)

#####################################
#####################################

# View layer names within geopackage
year_list <- c(sf::st_layers(dsn = shrimp_gpkg,
                             do_count = TRUE)[[1]]) %>%
  # get only the layers that are for transects
  stringr::str_subset(string = .,
                      pattern = "transects") %>%
  ## substitute nothing ("") in place of the "shrimp_transects" that is at the beginning of each layer name
  ## this will give only the year
  sub("shrimp_transects", "", .) %>%
  # remove any element that is empty ("")
  stringi::stri_remove_empty() %>%
  # sort the vector so it is ascending
  sort()

#####################################
#####################################
  
# load data
## transect data
### 2014 transects
i <- 1

shrimp_transects2014 <- sf::st_read(dsn = shrimp_gpkg, layer = paste0("shrimp_transects", year_list[i])) %>%
  as.data.frame() %>%
  dplyr::group_by(year, VSBN) %>%
  dplyr::summarise()
View(shrimp_transects2014)

