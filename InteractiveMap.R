# load packages
library(dplyr)
library(ggplot2)
library(rjson)
library(jsonlite)
library(leaflet)
library(RCurl)
library(leaflet.extras2)
library(sf)
library(geojsonsf)
# set factors to false
options(stringsAsFactors = FALSE)
library(tiff)
library(geojsonlint)
library(leaftime)
library(htmltools)
library(leafem)

geotiffFile = "LC08_L1TP_015034_20190812_20200827_02_T1_VAA.TIF"
geotiffFile2 = "LC08_L1TP_015034_20190812_20200827_02_T1_B5.TIF"
geotiffFile3 = "secondTIFFFile.TIF"


leaflet()   %>% addTiles()  %>%
  addGeotiff(
    file = geotiffFile3
    , opacity = 0.9
    , colorOptions = colorOptions(
      palette = hcl.colors(256, palette = "viridis")
      , na.color = "transparent"
    )
  )

filenames <- Sys.glob("*.TIF")
cat(filenames, fill = getOption("width"))












