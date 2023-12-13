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

# list all shapefiles to later extract the years
# shapefiles <- list.files(shapefile_dir,
#                          # pattern for finding files that match the shapefiles
#                          pattern = ".shp")
# 
# # create species list of unique years of shrimp transects
# years_list <- unique(sapply(strsplit(x = shapefiles,
#                                      # split file names into elements by "_"
#                                      split = "_"),
#                             # function across all files is to return third element from the string
#                             ## in this case that is the year
#                             function(x) x[3])) %>%
#   # substitute nothing ("") in place of the .shp that is at the end of the string
#   ## this will give only the year
#   sub(".shp", "", .) %>%
#   # remove the last element (NA) that comes from the data set shrimp_transects.shp
#   head(., -1)

# load data
## transect data
### 2014 transects
shrimp_transects2014 <- sf::st_read(dsn = shrimp_gpkg, layer = "shrimp_transects2014")

#####################################

# ## Gulf of Mexico
# gulf_mexico_grid <- sf::st_read(dsn = shrimp_gpkg, layer = "gulf_mexico_polygon_grid")

## Raster grid
# raster_100m <- terra::rast(x = file.path(raster_dir, paste0("shrimp_raster_100m.grd")))

#####################################
#####################################

# create list of unique vessels
vessels <- shrimp_transects2014 %>%
  # count the number of points along the transect
  dplyr::mutate(vertices = mapview::npts(., by_feature = TRUE),
                layer = "shrimp lines") %>%
  # limit to transects that have more than a single point
  dplyr::filter(vertices >= 2) %>%
  # convert to multilinestring
  sf::st_cast(x = ., to = "MULTILINESTRING") %>%
  # create vessel field
  separate(data = ., col = vessel_trans, into = c("vessel_id", "serial", "transect"), remove = T) %>%
  dplyr::group_by(vessel_id) %>%
  dplyr::summarise()

length(unique(vessels$vessel_id))

vessel_list <- list(unique(vessels$vessel_id))
print(vessel_list)

vessel_example <- vessels %>%
  dplyr::filter(row(x = .) == 1)
shrimp_year <- terra::vect(x = vessels)

#####################################
#####################################

shrimp_year <- terra::vect(file.path(shapefile_dir, paste0("shrimp_transects_", years_list[[1]], ".shp")))

vessel_ids <- shrimp_year[[1]][1]

shrimvessel_loop <- vessel_ids[[1]][1]

#####################################

# create rasterized version of the shrimp transects data to the 100m resolution raster grid
shrimp_rast <- terra::rasterizeGeom(x = shrimp_year[[1]][1],
                                    # use the 100m raster grid
                                    y = rast_100m,
                                    # count number of occurrences in very cell
                                    fun = "count")

i <- 1
for(i in 1:length(years_list)){
  # designate loop start time
  start_time <- Sys.time()
  
  #####################################
  
  # load data
  shrimp_year <- terra::vect(file.path(shapefile_dir, paste0("shrimp_transects_", years_list[[i]], ".shp")))
  
  # define annual object names for shrimp data
  ## shrimp transect data by year
  raster_year <- paste("shrimp_transect_raster", years_list[i], sep = "_")
  
  # create rasterized version of the shrimp transects data to the 100m resolution raster grid
  shrimp_rast <- terra::rasterizeGeom(x = shrimp_year,
                                      # use the 100m raster grid
                                      y = rast_100m,
                                      # count number of occurrences in very cell
                                      fun = "count")
  print(paste("Time takes to create data:", Sys.time() - start_time))
  
  # assign the rasterized transect data to templated raster grid
  assign(raster_year, shrimp_rast)
  
  # plot shrimp transect raster
  plot(shrimp_rast)
  
  #####################################
  
  # Export data
  terra::writeRaster(shrimp_rast, filename = file.path(raster_dir, paste0(raster_year, ".grd")), overwrite = T)
  
  print(paste("Time to complete total task:", Sys.time() - start_time, units(Sys.time() - sum_start)))
}

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start, units(Sys.time() - sum_start)) # print how long it takes to calculate