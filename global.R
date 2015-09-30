library(rgdal)
library(maptools)
library(leaflet)
library(dplyr)
library(magrittr)
library(ggplot2)
library(scales)
useCache = TRUE
if (useCache) load("init.RData") else {
  source("functions/gis.R")
  
  spPolygons <- getPolygons(lCommunes = list(Paris = 75101:75120), union = FALSE, cleanup = TRUE)
  spPolygons@data$insee <- as.numeric(as.character(spPolygons@data$insee))
  
  spAccidents <- readOGR("data", layer = "accidentologie-paris", stringsAsFactors = FALSE)
  save.image("init.RData")
}
dfDataAccidents <- spAccidents@data
dfDataAccidents$cp <-  as.numeric(dfDataAccidents$cp )
dfNbAccs <- dfDataAccidents %>% 
  mutate(year = substr(date, 1, 4)) %>% 
  filter(year == 2012) %>% 
  count(cp) %>% 
  mutate(insee = cp + 100,
         pop = paste0("Nombre d'accidents en 2012: ", n))
dfNbAccs$arrs <- paste0(1:20, c("er", rep("ème", 19)), " arrondissement")


pal = colorNumeric("RdBu", dfNbAccs$n)

spPolygons@data %<>% 
  inner_join(dfNbAccs %>% select(insee, n, pop, arrs))
# mainMap <- leaflet(spPolygons) %>% 
#   addTiles() %>% 
#   addProviderTiles("Acetate.terrain") %>% 
#   addPolygons()

# stroke = FALSE, color = ~pal(n), smoothFactor = 0.2, fillOpacity = 0.6, popup = ~pop)
# addMarkers(data = dfCoords, popup = ~pop)

