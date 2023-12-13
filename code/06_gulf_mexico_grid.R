###############################################################
### 06. Shrimp Analysis -- Gulf of Mexico 100m polygon grid ###
###############################################################

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
               mapview,
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

## big islands directory
big_islands_dir <- "data/shrimp_annual"

## land directory
land_dir <- "data/shrimp_annual/land.gpkg"

## export directory
shrimp_gpkg <- "data/shrimp_annual/shrimp.gpkg"

#####################################

# View layer names within geopackage
sf::st_layers(dsn = shrimp_gpkg,
              do_count = TRUE)

#####################################
#####################################

# load data
## load all years data
shrimp_transects <- sf::st_read(dsn = shrimp_gpkg, layer = "shrimp_transects")

## land data
### continental data
continents <- sf::st_read(dsn = land_dir, layer = "continents") %>%
  # reproject the coordinate reference system to match BOEM call areas
  sf::st_transform("EPSG:5070") # EPSG 5070 (https://epsg.io/5070)

### big island land data
big_islands <- terra::readRDS(file = file.path(big_islands_dir, "big_islands.RData")) %>%
  # reproject the coordinate reference system to match BOEM call areas
  sf::st_transform("EPSG:5070") # EPSG 5070 (https://epsg.io/5070)

### small island land data
small_islands <- sf::st_read(dsn = land_dir, layer = "small_islands") %>%
  # reproject the coordinate reference system to match BOEM call areas
  sf::st_transform("EPSG:5070") # EPSG 5070 (https://epsg.io/5070)

### very small island land data
very_small_islands <- sf::st_read(dsn = land_dir, layer = "very_small_islands") %>%
  # reproject the coordinate reference system to match BOEM call areas
  sf::st_transform("EPSG:5070") # EPSG 5070 (https://epsg.io/5070)

#####################################

# create shrimp lines and transects
# ***note: this will be used to get areas only where transects exist
shrimp_lines <- shrimp_transects %>%
  # count the number of points along the transect
  dplyr::mutate(vertices = mapview::npts(shrimp_transects, by_feature = TRUE),
                layer = "shrimp lines") %>%
  # limit to transects that have more than a single point
  dplyr::filter(vertices >= 2) %>%
  # convert to multilinestring
  sf::st_cast(x = ., to = "MULTILINESTRING") %>%
  # add a buffer of 500 meters
  sf::st_buffer(dist = 500)

# make shrimp transects a single feature
## ***note: separating out so analysis does not time out
shrimp_lines <- shrimp_lines %>%
  # group by the layer field
  dplyr::group_by(layer) %>%
  # summarise by the layer field so 
  dplyr::summarise()

#####################################
#####################################

# create study area
## get minimum and maximum values for all the years
xmin <- sf::st_bbox(shrimp_transects)$xmin
xmax <- sf::st_bbox(shrimp_transects)$xmax
ymin <- sf::st_bbox(shrimp_transects)$ymin
ymax <- sf::st_bbox(shrimp_transects)$ymax

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

# limit to only transected areas
aoi_poly <- aoi_poly %>%
  # limit focus to areas within 500 meters of transects
  rmapshaper::ms_clip(shrimp_lines)

### Remove land areas (leave only marine / coastal areas)
aoi_marine <- aoi_poly %>%
  # Remove continental land
  sf::st_difference(continents) %>%
  # Remove big island land
  sf::st_difference(big_islands) %>%
  # Remove small island land
  sf::st_difference(small_islands) %>%
  # Remove very small island land
  sf::st_difference(very_small_islands) %>%
  # select fields of interest
  dplyr::select(point, value)

plot(aoi_marine)

#####################################
#####################################

# create raster grid
## Square
### Grid with 100 meter cell size
#### Create a template raster that has the extent of the study area
rast_temp <- terra::rast(aoi_marine,
                         # use the extent of the marine study area
                         extent = aoi_marine,
                         # give raster to have resolution of 100 meters
                         resolution = 100,
                         # have coordinate reference system as the study area (EPSG:5070)
                         crs = crs(aoi_marine))

# see dimensions
dim(rast_temp)

#### Create raster filed with the data from the study area
rast_100m <- terra::rasterize(x = aoi_marine,
                              y = rast_temp,
                              field = "value")

# plot raster grid to verify land has been removed
plot(rast_100m)

#####################################
#####################################

# create grid as polygons
gulf_polygon <- terra::as.polygons(x = rast_100m,
                                   aggregate = FALSE) %>%
  # convert to sf
  sf::st_as_sf()

#####################################
#####################################

# export data
## grid
sf::st_write(obj = gulf_polygon, dsn = shrimp_gpkg, layer = "gulf_mexico_polygon_grid", append = F)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start, units(Sys.time() - sum_start)) # print how long it takes to calculate