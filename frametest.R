##load libraries
library(shiny)
library(leaflet)
library(RColorBrewer)
library(rgdal)
library(raster)
library(ggmap)
library(RColorBrewer)

#############################
countries <- readOGR('./world-shapefile', layer = 'TM_WORLD_BORDERS')

#read .CSV data
participatory <- read.csv('./data/ParticipatoryData.csv')
articles <- read.csv('./data/Data Scraping for Journal Articles.csv')

participatory
participatory2 <- participatory
participatory2$FIRSTPUB <- NULL
participatory2$RESTPUB <- NULL
participatory2$ALLPUB <- NULL
participatory2$WORK <- NULL



num = nrow(participatory)
x = data.frame(matrix(0, nrow = num, ncol = 4))

for(i in 2:num) {
  countrylabel <- participatory$ISO2[i]
  #factor(articles$Country.of.Publication..1st.Author., levels=levels(countrylabel) )
  
  if(articles$Country.of.Publication..1st.Author.[i] == countrylabel)
  {
    x[i] <- x[i] + length(grep(CountryLabel, articles$Place.of.Work[i]))
  }

}
  








#removing South Sudan from my dataset
participatory <- subset(participatory, COUNTRY!="South Sudan")

participatory2 <- subset(participatory, COUNTRY!="South Sudan")



articles$Country.of.Publication..1st.Author.








if (participatory2 == participatory) {
  print("YES")
} else {
  print("NO")
}