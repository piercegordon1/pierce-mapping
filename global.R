##load libraries
library(shiny)
library(leaflet)
library(RColorBrewer)
library(rgdal)
library(raster)
library(ggmap)
library(RColorBrewer)
source("./dataFilter.R")

#############################
countries <- readOGR('./world-shapefile', layer = 'world3')
articles <- read.csv('./data/Data Scraping for Journal Articles.csv')

participatory <- read.csv('./data/ParticipatoryData.csv')

#removing South Sudan from my dataset
participatory <- subset(participatory, COUNTRY!="South Sudan")
#participatory is the test dynamic list.
#It is zeroed out in all of the data, 
#so that is can be incremented dynamically.
participatory2 <- participatory
participatory2$WORK <- 0
participatory2$FIRSTPUB <- 0
participatory2$ALLPUB <- 0
participatory2$RESTPUB <- 0

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


#Generating unique list of countries
countryList <- unique(countries@data$NAME) %>% as.character() %>% sort()
countryList2 <- unique(countries@data$ISO2.x) %>% as.character() %>% sort()
AuthorList <- unique(articles$Authors) %>% as.character() %>% sort()
UniversityList <- unique(articles$Place.of.Publish..1st.author.) %>% as.character() %>% sort()

#Generating map Type List
MapTypeList <- c("","WORK", "FIRSTPUB", "RESTPUB", "ALLPUB")
