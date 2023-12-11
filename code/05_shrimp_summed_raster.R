################################################################
### 05. Shrimp Analysis -- Summarized shrimp transect raster ###
################################################################

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
## shrimp rasters
raster_dir <- "data/shrimp_annual/rasters"

#####################################

# inspect which years there are rasters for
years <- list.files(raster_dir,
                    # pattern for finding files that match the shapefiles
                    pattern = ".grd")

# create species list of unique years of shrimp transects
years_list <- unique(sapply(strsplit(x = years,
                                     # split file names into elements by "_"
                                     split = "_"),
                            # function across all files is to return third element from the string
                            ## in this case that is the year
                            function(x) x[4])) %>%
  # remove all elements that will have .grd.aux.xml patterns
  stringr::str_subset(string = ., pattern = ".xml", negate = TRUE) %>%
  # substitute nothing ("") in place of the .grd or .grd.aux.xml that is at the end of the string
  ## this will give only the year
  sub(pattern = ".grd", replace = "", x = .)

#####################################
#####################################

# load rasters
# i <- 1
for(i in 1:length(years_list)){
  
  # designate loop start time
  start_time <- Sys.time()
  
  #####################################
  
  # create placeholder raster name
  raster_year <- paste("shrimp_transect_raster", years_list[i], sep = "_")
  
  #####################################
  
  # load data
  shrimp_rast <- terra::rast(x = file.path(raster_dir, paste0("shrimp_transect_raster_", years_list[[i]], ".grd")))
  
  #####################################
  
  # assign the shrimp pings data looped to templated annual data object
  assign(raster_year, shrimp_rast)
  
  print(paste("Time to load shrimp raster data:", Sys.time() - start_time))
}

# insepct data
ext(shrimp_transect_raster_2014)
ext(shrimp_transect_raster_2015)
ext(shrimp_transect_raster_2016)
ext(shrimp_transect_raster_2017)
ext(shrimp_transect_raster_2018)
ext(shrimp_transect_raster_2019)
ext(shrimp_transect_raster_2020)
ext(shrimp_transect_raster_2021)

#####################################
#####################################

# sum all years into a single rasters

sum_start <- Sys.time()

shrimp_raster_2014_2021 <- c(shrimp_transect_raster_2014,
                             shrimp_transect_raster_2015,
                             shrimp_transect_raster_2016,
                             shrimp_transect_raster_2017,
                             shrimp_transect_raster_2018,
                             shrimp_transect_raster_2019,
                             shrimp_transect_raster_2020,
                             shrimp_transect_raster_2021) %>%
  terra::app(sum, na.rm = T)

print(paste("Time taken to summarize all shrimp transect rasters:", Sys.time() - sum_start, units(Sys.time() - sum_start)))

#####################################
#####################################

plot(shrimp_raster_2014_2021)

#####################################
#####################################

# Export raster
terra::writeRaster(shrimp_raster_2014_2021, filename = file.path(raster_dir, "shrimp_raster_2014_2021.grd"), overwrite = T)