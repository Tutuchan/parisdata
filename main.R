library(rgdal)
library(maptools)
library(leaflet)
library(dtpdao)
library(dplyr)

wsIDF <- WebServiceIDF$new()
spPolygons <- wsIDF$GetPolygons(lCommunes = list(Paris = 75101:75120), union = FALSE, cleanup = TRUE)
spPolygons@data$insee <- as.numeric(as.character(spPolygons@data$insee))

spAccidents <- readOGR("data", layer = "accidentologie-paris", stringsAsFactors = FALSE)
dfDataAccidents <- spAccidents@data
dfDataAccidents$cp <-  as.numeric(dfDataAccidents$cp )
dfNbAccs <- dfDataAccidents %>% 
  mutate(year = substr(date, 1, 4)) %>% 
  filter(year == 2012) %>% 
  count(cp) %>% 
  mutate(insee = cp + 100,
         pop = paste0("Nombre d'accidents en 2012: ", n))


# dfCoords <- bind_rows(lapply(1:length(spPolygons@polygons), function(i) {
#   p <- spPolygons@polygons[[i]]
#   insee <- spPolygons@data$insee[i]
#   data.frame(lon = p@labpt[1], lat = p@labpt[2], nb = dfNbAccs$n[dfNbAccs$insee == insee], pop = paste0("Nombre d'accidents en 2012: ", dfNbAccs$n[dfNbAccs$insee == insee]))
#   }))

pal = colorNumeric("Blues", dfNbAccs$n)

spPolygons@data %<>% inner_join(dfNbAccs %>% select(insee, n, pop))
leaflet(spPolygons) %>% 
  addTiles() %>% 
  addProviderTiles("Acetate.terrain") %>% 
  addPolygons(stroke = FALSE, color = ~pal(n), smoothFactor = 0.2, fillOpacity = 1, popup = ~pop)
  # addMarkers(data = dfCoords, popup = ~pop)