#################################################
### 0. Webscrape Data -- USGS Global Islands  ###
#################################################

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
               ggplot2,
               janitor,
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

#####################################
#####################################

# Set directories
## download directory
download_dir <- "~/downloads"

## output directory
data_dir <- "data/a_raw_data"

#####################################
#####################################

# Webscrape set-up
## Process uses RSelenium package (learn more about basics here: https://cran.r-project.org/web/packages/RSelenium/vignettes/basics.html)
### Another helpful tutorial: https://joshuamccrain.com/tutorials/web_scraping_R_selenium.html
### Firefox profile (based on this link: https://yizeng.me/2014/05/23/download-pdf-files-automatically-in-firefox-using-selenium-webdriver/)
fprof <- RSelenium::makeFirefoxProfile(list(
  # detail level for download (0 = Desktop, 1 = systems default downloads location, 2 = custom folder.)
  browser.download.folderList = 2,
  # location for data download
  browser.download.dir = data_dir,
  # stores a comma-separated list of MIME types to save to disk without asking what to use to open the file
  browser.helperApps.neverAsk.saveToDisk = "application/pdf",
  # disable PDF javascript so that PDFs are not displayed
  pdfjs.disabled = TRUE,
  # turn off scan and loading of any additionally added plug-ins
  plugin.scan.plid.all = FALSE,
  # high number defined for version of Adobe Acrobat
  plugin.scan.Acrobat = "99.0"))

#####################################

# Launch RSelenium server and driver
rD <- RSelenium::rsDriver(browser="firefox",
                          # set which version of browser
                          version = "latest",
                          # Chrome version (turn off as Firefox will be used)
                          chromever = NULL,
                          # set which version of Gecko to use
                          geckover = "latest",
                          # status messages
                          verbose = TRUE,
                          # populate with the Firefox profile
                          extraCapabilities = fprof)

## Remote driver
remDr <- rD[["client"]]
remDr$open(silent = TRUE)

#####################################
#####################################

# Navigates to USGS global islands dataset (source: https://www.sciencebase.gov/catalog/item/63bdf25dd34e92aad3cda273)
# and scrapes the site data for the states and regions of interest
## Global Islands Explorer: https://rmgsc.cr.usgs.gov/gie/
### Interactive Explorer: https://rmgsc.cr.usgs.gov/gie/gie.shtml
## Scientific manuscript: https://www.tandfonline.com/doi/full/10.1080/1755876X.2018.1529714

# Base URL
base_url <- "https://www.sciencebase.gov/catalog/item/63bdf25dd34e92aad3cda273"

# Navigate to page
remDr$navigate(base_url)
Sys.sleep(5)

#####################################

# Prepare window
remDr$maxWindowSize()
Sys.sleep(2)

#####################################

# Click link to download mappackage
download_toggle <-remDr$findElement(using = "css selector",
                                    value = "div.table-responsive:nth-child(3) > table:nth-child(1) > caption:nth-child(1) > span:nth-child(1)")
download_toggle$clickElement()

# Wait 40 minutes for the data to download (2400 seconds = 40 minutes)
Sys.sleep(2400)

#####################################
#####################################

# Close RSelenium servers
remDr$close()
rD$server$stop()

#####################################
#####################################

# View files in downloads folder to make sure file is there
list.files(download_dir)

file.rename(from=file.path(download_dir, "USGSEsriWCMC_GlobalIslands_v3_mpk.zip"),  # Make default download directory flexible
            # send to the raw data directory
            to=file.path(data_dir, "USGSEsriWCMC_GlobalIslands_v3_mpk.zip"))

#####################################

# Unzip the data
## grab text before ".zip" and keep only text before that
new_dir_name <- sub(".zip", "", "USGSEsriWCMC_GlobalIslands_v3_mpk.zip")

## create new directory for data
new_dir <- file.path(data_dir, new_dir_name)

## unzip the file
unzip(zipfile = file.path(data_dir, "USGSEsriWCMC_GlobalIslands_v3_mpk.zip"),
      # export file to the new data directory
      exdir = data_dir)

### relist the files
list.files(data_dir)

## rename the file so that the mappackage can get unzipped
### ***Note: will have to manually unzip the file
file.rename(from=file.path(data_dir, "USGSEsriWCMC_GlobalIslands_v3.mpk"),  # Make default download directory flexible
            # send to the raw data directory
            to=file.path(data_dir, "USGSEsriWCMC_GlobalIslands_v3.zip"))

## remove original zipped file
file.remove(file.path(data_dir, "USGSEsriWCMC_GlobalIslands_v3_mpk.zip"))

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
