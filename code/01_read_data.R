rm(list = ls())

library(dplyr)
library(geosphere)
library(marmap)
library(Rcpp)
library(sf)
library(stringr)


# Resources
## geosphere package Github: https://github.com/rspatial/geosphere
## geosphere reference manual: https://cran.r-project.org/web/packages/geosphere/geosphere.pdf

# Create directories
dir.create("data/zz_miscellaneous_data")

data_dir <- "data"
export_dir <- "data/zz_miscellaneous_data/shrimp.gpkg"

############################################

# load data
## Effort
# load(file.path(data_dir, "effort_2014_2021.RData"))

## Pings
load(file.path(data_dir, "pings_2014_2021.RData"))

############################################

# Examine data
head(pings_2014_2021)
names(pings_2014_2021)

## get years
ping_years <- pings_2014_2021 %>%
  dplyr::select(STAMP) %>%
  dplyr::mutate(start_date = format(as.Date(STAMP, format="%Y/%m/%d"),"%Y"))

years <- list(unique(ping_years$start_date))


start_time <- Sys.time()
shrimp_pings_2014 <- pings_2014_2021 %>%
  # limit to fishing activity
  dplyr::filter(vessel_state == "fishing") %>%
  # filter for tows that start in year of interest
  ## ***Note: faster to do as separate filter than combining with activity filter
  dplyr::filter(.,
                stringr::str_detect(string = .$STAMP,
                                    pattern = "2014")) %>%
  # remove bad coordinates
  filter(between(LATITUDE, -90, 90), between(LONGITUDE, -180, 180)) %>%
  # remove duplicates 
  distinct(VSBN, SERIAL, STAMP, LONGITUDE, LATITUDE) %>%
  # sort by time stamp within vessels
  arrange(VSBN, STAMP) %>% group_by(VSBN, SERIAL) %>%
  # calculate distances and times
  mutate(nm = geosphere::distVincentyEllipsoid(cbind(LONGITUDE, LATITUDE),
                                               cbind(lag(LONGITUDE), lag(LATITUDE))) / 1852,
         start_date = format(as.POSIXct(STAMP), format="%Y/%m/%d"),
         start_time = format(as.POSIXct(STAMP), format = "%H:%M:%S"),
         mins = as.numeric(STAMP - lag(STAMP), units = "mins")) %>%
  filter(hours > 0 | is.na(hours)) %>%
  # calculate speeds based on time between pings
  mutate(knots = nm / hours,
         knots_calc = ifelse(test = hours > 0.5 | hours < 59/3600 | knots > 11.5,
                             yes = NA,
                             no = knots),
         brk = ifelse(test = hours < 8 | is.na(hours),
                      yes = 0,
                      no = 1) %>% 
           cumsum)
total_time <- Sys.time() - start_time
print(total_time)

list(unique(shrimp_pings_2014$vessel_state))

## Examine top of Shrimp pinged data for 2014
head(shrimp_pings_2014)

list(unique(shrimp_pings_2014$VSBN))
list(unique(shrimp_pings_2014$SERIAL))

########################

start_time2 <- Sys.time()
vessel1 <- shrimp_pings_2014 %>%
  dplyr::select(VSBN,
                LONGITUDE,
                LATITUDE,
                STAMP,
                start_date,
                start_time,
                nm,
                mins) %>%
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
total_time2 <- Sys.time() - start_time2
print(total_time2)

list(unique(vessel1$VSBN))
View(vessel1)

# # time change calculation
# vessel1_diff <- vessel1 %>%
#   # calculate distance between a point and the previous point
#   ## ***Note: geopackage was used originally by Kyle Dettloff (kyle.dettloff@noaa.gov) when creating the original dataset
#   ##          The methods noted that the previous analysis used Vincenty ellipsoid method given it took the curvature
#   ##          of the earth into consideration (https://sedarweb.org/documents/sedar-87-dw-01-estimation-of-commercial-shrimp-effort-in-the-gulf-of-mexico/)
#   ##          Original methods: https://github.com/kyledettloff-NOAA/GOMshrimpEffort/blob/main/effort_scaled.R
#   
#   ## To learn more about the methods see here: http://www.movable-type.co.uk/scripts/latlong-vincenty.html
#   ## and here: https://github.com/rspatial/geosphere/blob/master/R/distVincentyEllipsoid.R
#   
#   ## ***Note: distances will need to be under 1 nautical mile (1 nautical mile = 1852 meters)
#   mutate(nm = geosphere::distVincentyEllipsoid(p1 = cbind(LONGITUDE, LATITUDE),
#                                                p2 = cbind(lag(LONGITUDE), lag(LATITUDE))) / 1852,
#          # calculate the time between points
#          mins_diff = as.numeric(STAMP - lag(STAMP), units = "mins")) %>%
#   dplyr::select(-LONGITUDE,
#                 -LATITUDE,
#                 -STAMP) %>%
#   dplyr::arrange(VSBN,
#                  start_date)

vess_a <- vessel1 %>%
  dplyr::filter(VSBN == "1022072") %>%
  dplyr::slice(1:10)

vess_b <- vessel1 %>%
  dplyr::filter(VSBN == "1023214") %>%
  dplyr::slice(1:10)

vess_c <- vessel1 %>%
  dplyr::filter(VSBN == "1027077") %>%
  dplyr::slice(1:10)

vess_d <- vessel1 %>%
  dplyr::filter(VSBN == "1027078") %>%
  dplyr::slice(1:10)


vess_comb <- rbind(vess_a,
                   vess_b,
                   vess_c,
                   vess_d) %>%
  dplyr::mutate(transect = ifelse(test = nm <= 1.0 | is.na(nm),
                                  yes = 0,
                                  no = 1) %>%
                  cumsum,
                vessel_trans = paste(VSBN, transect, sep = "_"))

vess_transect <- vess_comb %>%
  dplyr::group_by(vessel_trans) %>%
  dplyr::summarise() %>%
  sf::st_cast(x = .,
              to = "MULTIPOINT") %>%
  sf::st_cast(x = .,
              to = "LINESTRING")

# Export data
sf::st_write()


  dplyr::select(VSBN,
                nm,
                mins) %>%
  dplyr::mutate(vessel = as.numeric(factor(VSBN)),
                nm_transect = ifelse(test = nm <= 1.0,
                                     yes = 1,
                                     no = 2),
                mins_transect = ifelse(test = mins_diff <= 30 & mins_diff >= 0,
                                       yes = 1,
                                       no = 2)) %>%
  # clean first row
  dplyr::mutate(nm_transect = ifelse(test = row_number() == 1,
                                     yes = 1,
                                     no = nm_transect),
                mins_transect = ifelse(test = row_number() == 1,
                                       yes = 1,
                                       no = mins_transect)) %>%
  # clean last row
  dplyr::mutate(nm_transect = ifelse(test = row_number() == dim(.)[1],
                                     yes = ifelse(test = nm <= 1.0,
                                                  yes = 1,
                                                  no = 2),
                                     no = nm_transect),
                mins_transect = ifelse(test = row_number() == dim(.)[1],
                                       yes = ifelse(test = mins_diff <= 30  & mins_diff >= 0,
                                                    yes = 1,
                                                    no = 2),
                                       no = mins_transect)) %>%
  # new ship
  dplyr::mutate(new_ship = ifelse(test = row_number() == 1,
                                  yes = TRUE,
                                  no = vessel == lag(vessel)),
                transect_label = ifelse(test = row_number() == 1,
                                        yes = "start",
                                        no = ifelse(test = new_ship == TRUE & nm_transect == 2,
                                                    yes = "start",
                                                    no = ifelse(test = new_ship == F,
                                                                yes = "start",
                                                                no = ifelse(test = new_ship == TRUE & lead(nm_transect) == 2,
                                                                            yes = "stop",
                                                                            no = ifelse(test = new_ship == TRUE & nm_transect == 1,
                                                                                        yes = "continue",
                                                                                        no = ifelse(test = row_number == dim(.)[1] & new_ship == TRUE & nm_transect == 1 & mins_transect == 1,
                                                                                                    yes = "continue",
                                                                                                    no = ifelse(test = new_ship == FALSE,
                                                                                                                yes = "start",
                                                                                                                no = NA))))))))


  
  
  
  
  
  
  
  

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
      test = row_number() == 1,
      yes = paste0(VSBN, "_", cumsum(VSBN != lag(VSBN, default = first(VSBN)))),
      no = ifelse(
        test = VSBN == lag(VSBN) & nm <= 1.0 & mins_diff <= 30,
        yes = paste0(VSBN, "_", cumsum(VSBN != lag(VSBN, default = first(VSBN))), "_", cumsum(abs(nm - lag(nm, default = last(nm))))),
        no = "new transect"
      ))) %>%
  dplyr::mutate(transect_id = ifelse(
    test = transect_id == "new transect",
    yes = lead(transect_id),
    no = transect_id
  ))


# Initialize a counter
counter <- 0

# Use sapply to iterate through each row of the data frame
vess_comb$transect_id <- sapply(1:nrow(vess_comb), function(i) {
  # Check the condition
  if (is.na(vess_comb$nm))  {
    # Increment the counter
    counter <<- counter + 1
    # Return the dynamic field name
    paste("test", counter, sep = "")
  } else {
    # If condition is not met, return NA or any other value
    NA
  }
})

View(vess_comb)
  
VSBN == lead(VSBN) & 
  lead(nm) <= 1 & 
  lead(mins_diff) <= 30
no = paste0(VSBN, "_", cumsum(VSBN != lag(VSBN, default = first(VSBN))))))




test = is.na(nm),
yes = paste0(VSBN, "_", RcppRoll::roll_sum(VSBN)),
no = ifelse(
  test = VSBN == lag(VSBN) & 
    nm <= 1.0 & 
    mins_diff <= 30,
  yes = paste0(VSBN, "_", cumsum(VSBN != lag(VSBN, default = first(VSBN)))),
  no = paste0(VSBN, "_", cumsum(VSBN != lag(VSBN, default = first(VSBN))))))



lead(VSBN) == VSBN & 
  between(lead(nm, default = nm[length(nm)]), nm - 1.0, nm + 1.0) & 
  between(lead(mins_diff, default = mins_diff[length(mins_diff)]), mins_diff - 30, mins_diff + 30),
lag(new_field),
paste("transect", row_number())

# Export data


create value to be vessel_0 (cumsum start)
if next value is not vessel or nm > 1 or mins_diff > 30, then paste value
if next value is same vessel, but nm > 1 or mins_diff > 30, then paste value + 1
if next value is same vessel and nm <= 1 and mins_diff <= 30, then paste previous value
