####################################################
### 04. Shrimp Analysis -- Year transect rasters ###
####################################################

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
shapefile_dir <- "data/shrimp_annual/shapefiles"

## Export directory
raster_dir <- "data/shrimp_annual/rasters"

#####################################

# View layer names within geopackage
sf::st_layers(dsn = shrimp_gpkg,
              do_count = TRUE)

#####################################
#####################################

# list all shapefiles to later extract the years
shapefiles <- list.files(shapefile_dir,
                         # pattern for finding files that match the shapefiles
                         pattern = ".shp")

# create species list of unique years of shrimp transects
years_list <- unique(sapply(strsplit(x = shapefiles,
                                     # split file names into elements by "_"
                                     split = "_"),
                            # function across all files is to return third element from the string
                            ## in this case that is the year
                            function(x) x[3])) %>%
  # substitute nothing ("") in place of the .shp that is at the end of the string
  ## this will give only the year
  sub(".shp", "", .) %>%
  # remove the last element (NA) that comes from the data set shrimp_transects.shp
  head(., -1)

#####################################
#####################################

## load all years data
shrimp_2014_2021 <- sf::st_read(dsn = shrimp_gpkg, layer = "shrimp_transects")

#####################################
#####################################

# create study area
## get minimum and maximum values for all the years
xmin <- sf::st_bbox(shrimp_2014_2021)$xmin
xmax <- sf::st_bbox(shrimp_2014_2021)$xmax
ymin <- sf::st_bbox(shrimp_2014_2021)$ymin
ymax <- sf::st_bbox(shrimp_2014_2021)$ymax

## Create points for study area
### Add points as they need to be drawn (clockwise or counterclockwise)
aoi_points <- rbind(c("point", xmax, ymin), # southeastern point
                    c("point", xmax, ymax), # northeastern point
                    c("point", xmin, ymax), # northwestern point
                    c("point", xmin, ymin)) %>% # southwestern point
  # convert to data frame
  as.data.frame() %>%
  # rename column names
  dplyr::rename("point" = "V1",
                "lon" = "xmax",
                "lat" = "ymin") %>%
  # convert to simple feature
  sf::st_as_sf(coords = c("lon", "lat"),
               # set the coordinate reference system to to match BOEM call areas
               crs = 5070) # EPSG 5070 (https://epsg.io/5070)

#####################################

# Create polygon
aoi_poly <- aoi_points %>%
  # group by the points field
  dplyr::group_by(point) %>%
  # combine geometries without resolving borders to create multipoint feature
  dplyr::summarise(geometry = st_combine(geometry)) %>%
  # create a value field
  dplyr::mutate(value = 0) %>%
  # convert back to sf
  sf::st_as_sf() %>%
  # convert to polygon simple feature
  sf::st_cast("POLYGON") %>%
  # convert back to sf
  sf::st_as_sf()

## Check units for determining cellsize of grid (units will be in meters)
sf::st_crs(aoi_poly, parameters = TRUE)$units_gdal

## inspect boundary box
plot(aoi_poly)

#####################################
#####################################

# create raster grid
## Square
### Grid with 100 meter cell size
#### Create a template raster that has the extent of the study area
rast_temp <- terra::rast(aoi_poly,
                         # use the extent of the marine study area
                         extent = aoi_poly,
                         # give raster to have resolution of 100 meters
                         resolution = 100,
                         # have coordinate reference system as the study area (EPSG:5070)
                         crs = crs(aoi_poly))

# see dimensions
dim(rast_temp)

#### Create raster filed with the data from the study area
rast_100m <- terra::rasterize(x = aoi_poly,
                              y = rast_temp,
                              field = "value")

#####################################
#####################################

# i <- 1
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
  print(paste("Time takes to create data:", Sys.time() - start_time, units(Sys.time() - start_time)))
  
  # assign the rasterized transect data to templated raster grid
  assign(raster_year, shrimp_rast)
  
  # plot shrimp transect raster
  plot(shrimp_rast)
  
  #####################################
  
  # Export data
  terra::writeRaster(shrimp_rast, filename = file.path(raster_dir, paste0(raster_year, ".grd")), overwrite = T)
  
  print(paste("Time to complete total task:", Sys.time() - start_time, units(Sys.time() - start_time)))
}

#####################################
#####################################

# Export data
## raster
terra::writeRaster(rast_100m, filename = file.path(raster_dir, "shrimp_raster_100m.grd"), overwrite = T)

#####################################
#####################################

# random <- as.vector(round(runif(n = 10,
#                           min = 0,
#                           max = 100000), 0))
# 
# transect_test <- shrimp_transects[random]

# calculate end time and print time difference
print(Sys.time() - start, units(Sys.time() - start)) # print how long it takes to calculate