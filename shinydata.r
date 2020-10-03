library(rgdal)
library(sp)
library(RColorBrewer)
library(leaflet)
source('main.r')

shp <- readOGR("Mapa\\.","MG_UF_2019", stringsAsFactors=FALSE, encoding="UTF-8")
teste <-  read_excel("Teste.xlsx")

#pg <- pg %>% group_by(Estado) %>% mutate(cumsum = cumsum(PG))
#pg <- pg %>%
#  group_by(Estado) %>%
#  summarise(Score= max(Score))

# <- as.data.frame(pg)
#
#ibge <- read.csv("Dados\\estadosibge.csv", header=T,sep=",")
#
#pg <- merge(pg,ibge, by.x = "Estado", by.y = "UF")

proj4string(shp) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

Encoding(shp$NM_UF) <- "UTF-8"

points <- eventReactive(input$recalc, {
  cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
}, ignoreNULL = FALSE)

pal <- colorBin("Blues",domain = NULL,n=5) #cores do mapa

state_popup <- paste0("<strong>Estado: </strong>", 
                      shp$NM_UF)
mapa <- leaflet(data = shp) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal(1), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = state_popup)%>%
              addMarkers(lng = teste$lon,lat= teste$lat)

class(mapa)
