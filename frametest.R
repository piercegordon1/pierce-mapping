##########################
#Pierce Gordon
#Energy and Resources Group
##########################

#This is the code for the section of material 
#that shoud read the initial list of articles 
#and be able to tally the amount of 'places of 
#work' (countries) that a specific country is 
#working in. Data used below.

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

#read .CSV data, botht the static  country list,
#and the article lists
participatory <- read.csv('./data/ParticipatoryData.csv')
articles <- read.csv('./data/Data Scraping for Journal Articles.csv')

#participatory is the test dynamic list.
#It is zeroed out in all of the data, 
#so that is can be incremented dynamically.
participatory2 <- participatory
participatory2$WORK <- 0
participatory2$FIRSTPUB <- 0
participatory2$ALLPUB <- 0
participatory2$RESTPUB <- 1

#X is a temporary dataframe that MIGHT be necessary. Right now, it is not used.
#x <- participatory[-c(1,3,4)]
#x[2:5] <- 0
#x <- matrix(0, nrow = num, ncol = 4)

#Incrmental variables.
i <- 1
j <- 1
k <- 1

#US is used as the test case 
#for two reasons: it works in the 
#largest collection of places, and it 
#makes the cod work the longest. If it 
#takes a short amount of time, then all other 
#countries should also work, and will take less time/
countrylabel <- "US"

#####################################

#Now, the meat. It searches in the 1st Author column 
#of the Article dataset for if the test country is 
#present. If it is, then it searches in the same row 
#(read:article) for all countries that are available 
#in the Place of Work column. It then should pass that 
#number to the participatory2 dataset (read: the 
#country list) and adds value to the respective country 
#being searched. It goes through all countries until 
#complete, then all articles as well. The intent is to 
#be able to use this code conversely: if one searches 
#for place of work, one can find the number of authors 
#from certain countries working in that specific country. 
#So, the final for loop should incrememnt every single 
#insance of a country code it sees.

#For example, many All Authors have strings like "GB, 
#GB, GB, GB, US, US, EG". It should be able to count 4 
#authors from Great Britian from this paper, 2 United 
#States, and one Egyptian, and put them into their 
#respective country categories.

####################################3

#Access all countries, to search in articles if the county is available
#for(i in 1:nrow(participatory2)) {
  #increment the articles being accessed
  for(j in 1:nrow(articles)) {
    #is the filtered country available in the 1st Author column?
    if(grepl(articles$Country.of.Publication..1st.Author.[j], countrylabel)) {
      #Loop the countries to search for in the same row, Place of Work column
      for(k in 1:nrow(participatory2))
        y <- participatory[k,2]
        #Is the current country available in the paper's row, Place of Work column?
        if (any(grepl(y, articles$Place.of.Work[j]))) {
          #Increment the coountry count, and pass it to the reactive country dataset 
          #x[k,2] <- x[k,2] + length(grep(y, articles$Place.of.Work[j]))
          participatory2$WORK[k] <- length(grep(y, articles$Place.of.Work[j]))
        }
    }
  }
#}
