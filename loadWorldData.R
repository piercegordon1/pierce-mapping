#load libraries
library(shiny)
library(leaflet)
library(RColorBrewer)
library(raster)
library(ggmap)
#load data using rgdal
library(rgdal)

#############################

countries <- readOGR('./world-shapefile', layer = 'TM_WORLD_BORDERS')

participatory <- read.csv('./data/ParticipatoryData.csv')
paperdata <- read.csv('./data/Data Scraping for Journal Articles.csv')
#removing South Sudan from my dataset
participatory <- subset(participatory, COUNTRY!="South Sudan")

#appending my data to the data in the shapefile
#countries@data <- cbind(participatory, countries@data)

#a <- merge(countries@data, participatory, by.x = "NAME" , by.y = "COUNTRY", sort = TRUE, all = TRUE)
#b <-merge(countries@data, participatory, by = "ISO2", sort = TRUE, all = TRUE)
#c <-merge(countries@data, participatory, by = "ISO3", sort = TRUE, all = TRUE)
#d <-merge(countries@data, participatory, by = "UNM49", sort = TRUE, all = TRUE)

countries@data$polyorder <- 1 : dim(countries@data)[1]

tmp <- merge(countries@data, participatory, by = "ISO3", sort = TRUE, all.x = TRUE)
tmp <- tmp[ order(tmp$polyorder), ]
countries@data <- tmp

#make map with color scale based on 'WORK'

countryColor <- colorFactor(topo.colors(10), countries@data$WORK)

leaflet(countries) %>% 
  addTiles() %>% 
  addPolygons(stroke=NULL, smoothFactor=0.5, 
              color = ~countryColor(WORK), 
              popup = ~paste("<strong>Areas of Study :</strong>",WORK, "<strong>Country:</strong>",NAME)) %>% 
  addLegend(pal=countryColor, values = ~WORK, position="bottomright")



