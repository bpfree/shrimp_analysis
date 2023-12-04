###############################################
### 04. Shrimp Analysis -- Shrimp Transects ###
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
data_dir <- "data/shrimp_annual"

## Export directory
raster_dir <- "data/shrimp_annual"

#####################################
#####################################

shrimp_transects2014 <- sf::st_read(dsn = shrimp_gpkg, layer = "shrimp_transects2014") %>%
  # reproject the coordinate reference system to match BOEM call areas
  sf::st_transform("EPSG:5070") # EPSG 5070 (https://epsg.io/5070)

shrimp_transects <- terra::vect(file.path(data_dir, "shrimp_transects.shp", sep = "/"))

#####################################
#####################################

# Study Area
## get minimum and maximum values for all the years
xmin <- sf::st_bbox(shrimp_transects2014)$xmin
xmax <- sf::st_bbox(shrimp_transects2014)$xmax
ymin <- sf::st_bbox(shrimp_transects2014)$ymin
ymax <- sf::st_bbox(shrimp_transects2014)$ymax

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

# Create grid
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

dim(rast_temp)

#### Create raster filed with the data from the study area
rast_100m <- terra::rasterize(x = aoi_poly,
                              y = rast_temp,
                              field = "value")

#####################################
#####################################

# random <- as.vector(round(runif(n = 10,
#                           min = 0,
#                           max = 100000), 0))
# 
# transect_test <- shrimp_transects[random]

raster_start <- Sys.time()
shrimp_rast <- terra::rasterizeGeom(x = shrimp_transects2014,
                                    # use the 100m raster grid
                                    y = rast_100m,
                                    # count number of occurrences in very cell
                                    fun = "count")
paste("Time takes to create data:", Sys.time() - raster_start)
plot(shrimp_rast)

#####################################
#####################################

## Raster
terra::writeRaster(shrimp_rast, filename = file.path(raster_dir, "shrimp_transect_2014.grd"), overwrite = T)

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate