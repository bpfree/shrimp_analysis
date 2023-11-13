rm(list = ls())

library(dplyr)
library(marmap)
library(sf)
library(geosphere)

# Resources
## geosphere package Github: https://github.com/rspatial/geosphere
## geosphere reference manual: https://cran.r-project.org/web/packages/geosphere/geosphere.pdf


dir.create("data/zz_miscellaneous_data")

data_dir <- "data"
export_dir <- "data/zz_miscellaneous_data"

# load data
## Effort
# load(file.path(data_dir, "effort_2014_2021.RData"))

## Pings
load(file.path(data_dir, "pings_2014_2021.RData"))

# Examine data
head(pings_2014_2021)
names(pings_2014_2021)

## get years
ping_years <- pings_2014_2021 %>%
  dplyr::select(STAMP) %>%
  dplyr::mutate(start_date = format(as.Date(.$STAMP, format="%Y/%m/%d"),"%Y"))

years <- list(unique(ping_years$start_date))


start_time <- Sys.time()
shrimp_pings_2014 <- pings_2014_2021 %>%
  # limit to fishing activity
  dplyr::filter(vessel_state == "fishing") %>%
  # filter for tows that start in year of interest
  dplyr::filter(.,
                stringr::str_detect(string = .$STAMP,
                                    pattern = "2014")) %>%
  dplyr::mutate(start_date = format(as.POSIXct(.$STAMP), format="%Y/%m/%d"),
                start_time = format(as.POSIXct(.$STAMP), format = "%H:%M:%S"))

total_time <- Sys.time() - start_time
print(total_time)

list(unique(shrimp_pings_2014$vessel_state))

## Examine top of Shrimp pinged data for 2014
head(shrimp_pings_2014)

list(unique(shrimp_pings_2014$VSBN))
list(unique(shrimp_pings_2014$SERIAL))


########################

vessel1 <- shrimp_pings_2014 %>%
  dplyr::select(VSBN,
                LONGITUDE,
                LATITUDE,
                start_date,
                start_time) %>%
  # filter for test vessels
  dplyr::filter(VSBN %in% c("1022072",
                            "1023214",
                            "1027077",
                            "1027078")) %>%
  # convert to sf feature
  sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"),
               # set the coordinate reference system to WGS84
               crs = 4326, # EPSG 4326 (https://epsg.io/4326)
               # keep longitude and latitude fields
               remove = FALSE)

list(unique(vessel1$VSBN))
View(vessel1)

# time change calculation
vessel1_diff <- vessel1 %>%
  # calculate distance between a point and the previous point
  ## ***Note: geopackage was used originally by Kyle Dettloff (kyle.dettloff@noaa.gov) when creating the original dataset
  ##          The methods noted that the previous analysis used Vincenty ellipsoid method given it took the curvature
  ##          of the earth into consideration (https://sedarweb.org/documents/sedar-87-dw-01-estimation-of-commercial-shrimp-effort-in-the-gulf-of-mexico/)
  ##          Original methods: https://github.com/kyledettloff-NOAA/GOMshrimpEffort/blob/main/effort_scaled.R
  
  ## To learn more about the methods see here: http://www.movable-type.co.uk/scripts/latlong-vincenty.html
  ## and here: https://github.com/rspatial/geosphere/blob/master/R/distVincentyEllipsoid.R
  
  ## ***Note: distances will need to be under 1 nautical mile (1 nautical mile = 1852 meters)
  mutate(nm = geosphere::distVincentyEllipsoid(p1 = cbind(LONGITUDE, LATITUDE),
                                               p2 = cbind(lag(LONGITUDE), lag(LATITUDE))) / 1852,
         # calculate the time between 
         mins_diff = as.numeric(STAMP - lag(STAMP), units = "mins")) %>%
  dplyr::select(-LONGITUDE,
                -LATITUDE)

vess_a <- vessel1_diff %>%
  dplyr::filter(VSBN == "1022072") %>%
  dplyr::slice(1:10)

vess_b <- vessel1_diff %>%
  dplyr::filter(VSBN == "1023214") %>%
  dplyr::slice(1:10)

vess_c <- vessel1_diff %>%
  dplyr::filter(VSBN == "1027077") %>%
  dplyr::slice(1:10)

vess_d <- vessel1_diff %>%
  dplyr::filter(VSBN == "1027078") %>%
  dplyr::slice(1:10)


vess_comb <- rbind(vess_a,
                   vess_b,
                   vess_c,
                   vess_d) %>%
  dplyr::select(VSBN,
                nm,
                mins_diff) %>%
  # use cumsum() to have transect go from 1 to 2 to ... (rolling calculation)
  # use RcppRoll::roll_sum(v)) [https://itsalocke.com/blog/understanding-rolling-calculations-in-r/]
  dplyr::mutate(transect_id = ifelse(
    lead(VSBN) == VSBN & 
      lead(lag(nm)) >= 1.0 & 
      lead(lag(mins_diff)) >= 30,
    paste0(VSBN, "_", cumsum(lead(VSBN) != lag(VSBN, default = first(VSBN)))), lag()))

View(vess_comb)

# Export data

