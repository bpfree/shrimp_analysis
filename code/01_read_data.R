rm(list = ls())

library(dplyr)

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
  dplyr::filter(VSBN == "1022072")
  
# Export data

