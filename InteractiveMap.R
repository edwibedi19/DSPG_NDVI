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
library(raster)
#geotiffFile = "LC08_L1TP_015034_20190812_20200827_02_T1_VAA.TIF"
#geotiffFile2 = "LC08_L1TP_015034_20190812_20200827_02_T1_B5.TIF"
geotiffFile3 = "C:/Users/victo/Documents/secondTIFFFile.TIF"


# clean the memory
rm(list=ls())

# load packages
library(leaflet)

# set factors to false
library(leafem)
library(raster)

geotiffFile = "C:/Users/victo/Downloads//LC08_L1TP_015034_20190812_20200827_02_T1_B5.TIF"
my_file = raster(geotiffFile)


m <- leaflet()   %>% 
  setView(lng = -80, lat=38, zoom=8) %>% 
  addTiles()  
m
addGeoRaster(m, my_file,
             colorOptions = colorOptions(
               palette = hcl.colors(256, palette = "viridis")
               , na.color = "transparent"
             ))
















leaflet()   %>% addTiles()  %>%
  addGeotiff(
    file = geotiffFile3
    , opacity = 0.9
    , colorOptions = colorOptions(
      palette = hcl.colors(256, palette = "viridis")
      , na.color = "transparent"
    )
  )



## outline of Floyd
virginiaCounty <- st_read("C:/Users/victo/Downloads/VirginiaAdministrativeBoundary.shp/VirginiaAdministrativeBoundary.shp/VirginiaCounty.shp")
f <- virginiaCounty[5,]%>%st_transform(crs = "+init=epsg:4326")

m <- leaflet(options = leafletOptions(minzoom = 19))   %>%
  setView(lng = -80.3, lat = 36.91, zoom = 9.5) %>%
  addProviderTiles("CartoDB") %>%
  addPolygons(data = f,
              stroke = FALSE) 




addGeoRaster(m2, my_file,
             colorOptions = colorOptions(
               palette = hcl.colors(256, palette = "viridis")
               , na.color = "transparent"
             ))
  
m2  %>%
  addPolygons(data = f,
              stroke = FALSE) 
m2



leaflet()   %>% addTiles() %>%
  addGeotiff(
    file = geotiffFile
    , opacity = 0.9
    , colorOptions = colorOptions(
      palette = hcl.colors(256, palette = "viridis")
      , na.color = "transparent"
    )
  )










