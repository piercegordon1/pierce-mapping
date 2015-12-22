

##########################
#Pierce Gordon
#Energy and Resources Group
##########################

##load libraries
library(shiny)
library(leaflet)
library(RColorBrewer)
library(rgdal)
library(raster)
library(ggmap)
library(RColorBrewer)

#This function filters the raw data in the Articles .csv file for the reactive inputs in the Shiny countries file.
#countrylabel <- "US"

#These variables are placeholders made to debug the code before it turns into a function. Keep these a comment when working.
#data <- countries@data
#crossFilter <- "UK" 
#Authors <- ""
#University <- ""
#YearLow <- ""
#YearHigh <- ""
#Authors <- ""
#University <- "" 
#Publisher <- ""
#GSRank <- ""
#KeywordList <- ""

#These articles should be available in the Shiny Global 
#code when the code is actually running. Thus, it should 
#not be necessary to have these variables when the function
#actually runs.
articles <- read.csv('./data/Data Scraping for Journal Articles.csv')
participatory <- read.csv('./data/ParticipatoryData.csv')
countries <- readOGR('./world-shapefile', layer = 'world3')
countries@data$polyorder <- 1 : dim(countries@data)[1]
tmp <- merge(countries@data, participatory, by = "ISO3", sort = TRUE, all.x = TRUE)
tmp <- tmp[ order(tmp$polyorder), ]
countries@data <- tmp
participatory <- read.csv('./data/ParticipatoryData.csv')


dataFilter <- function(articlelist, countries, crossFilter, YearLow, YearHigh, Authors, University, Publisher, GSRank, KeywordList) {
  
  ###code goes here. ALL THIS DOES IS ONE THING: RETURNS A DATA SET 
  ###that filters based upon the inputs given.
  
  #There are two critical variable types here that the loops use: the labels, and the matches. 
  #~Labels are assigned the filters we currently care about in the data, and the ~matches see if the labels match the 
  #information in the current column we care about. Add here to add new filters first.
  articles <- articlelist
  yearlow <- YearLow
  yearhi <- YearHigh
  yearmatch <- FALSE
  authorlabel <- Authors
  authormatch <- FALSE
  universitylabel <- University
  universitymatch <- FALSE
  countrylabel <- crossFilter
  countrymatch <- FALSE
  publisherlabel <- Publisher
  publishermatch <- FALSE
  gsranklabel <- GSRank
  gsrankmatch <- FALSE
  keywordlabel <-KeywordList
  keywordmatch <- FALSE
  
  
  #These variables set the reactive country lists where the data is input. Participatory 
  #is the full list, made by a .csv file, and participatory2 is the reactive dataset, which 
  #changes based upon the filters chosen.
  participatory <- countries@data
  participatory2 <- countries@data
  participatory2$WORK <- 0
  participatory2$FIRSTPUB <- 0
  participatory2$ALLPUB <- 0
  participatory2$RESTPUB <- 0
  
  #Incrementing variables for each of the loops.
  i <- 1
  j <- 1
  k <- 1
  
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
  #First, the code asks what type of map it wants to generate: 
  #1 = a Place of Work map, 
  #2 = a 1st Author map, 
  #3 = a All Author map, 
  #4 = or a "Rest Author" Map.
  
  
  #These let the user of the function 
  #choose which map they want to use.
  #maptest 
  map <- function() {
    message("What type of map do you want to display? 1 = Work, 2 = 1stAuth, 3 = AllAuth, 4= RestAuth ");
    x <- as.numeric(readLines(n=1));
    return(x)
  }
  
  cross <- function(maptype) {
    if(maptype == 1){
      message("What type of Author Data do you want to cross with the data? 2 == 1stAuth, 3 == AllAuth, 4 == RestAuth ");
      x <- as.numeric(readLines(n=1));  
      return(x)
    } else {
      message("The crossed data will be the Place of Work data.");
      x <- 1
      return(x)
    }
  }
  
  maptype <- map()
  crosstype <- cross(maptype)
  #This is for debugging, to make sure the main parts of the code are working.
  #maptype = 1
  #crosstype = 2
  
  #This variable is used as the test variable to indicate if the 
  #country listed is located in the column of interest, and thus 
  #the list of second countries is worth adding up to the reactive 
  #country index.
  if(crosstype==1){
    crossarray<-articles$Place.of.Work
    }else if(crosstype==2){
    crossarray<-articles$Country.of.Publication..1st.Author.
    #Adding cells of dataframes...how?
    #}else if(crosstype==3){
    #  crossarray<-articles$Country.of.Publication..Rest.of.authors.+articles$Country.of.Publication..1st.Author.
  }else if(crosstype==4){
    crossarray<-articles$Country.of.Publication..Rest.of.authors.
  }
  
  
  #This variable is used as the array that is searched inside for instances of countries, 
  #to be finally incremented in the active Country index.
  if(maptype==1){
    maparray <- articles$Place.of.Work
  }else if(maptype==2){
    maparray <- articles$Country.of.Publication..1st.Author.
    #Adding cells of dataframes...how?
    #}else if(maptype==3){
    #  maparray<-articles$Country.of.Publication..Rest.of.authors.+articles$Country.of.Publication..1st.Author.
  }else if(maptype==4){
    maparray <- articles$Country.of.Publication..Rest.of.authors.
  }
  
  #############################################
  #Filter algorithm. Goes through raw data and filters papers that don't match the reactive values.
  #############################################
  
  cat("Working....")
  for(j in 2:nrow(articles)) {
    cat(".")
    if(articles$Year[j] <= yearhi && articles$Year[j] >= yearlow) {
      yearmatch <- TRUE
    }
    authormatch <- grepl(authorlabel, articles$Authors[j])
    universitymatch <- grepl(universitylabel, articles$Place.of.Publish..1st.author.[j])
    countrymatch <- grepl(countrylabel, crossarray[j])
    publishermatch <- grepl(publisherlabel, articles$Publisher[j])
    gsrankmatch <- grepl(gsranklabel, articles$GSRank)
    #How do we get keyword to match more than the first cell?
    #keywordmatch <- grepl(keywordlabel, articles$Second.Keyword) || grepl(keywordlabel, articles$X) ||  grepl(keywordlabel, articles$X.1) ||  grepl(keywordlabel, articles$X.2) ||  grepl(keywordlabel, articles$X.3) ||  grepl(keywordlabel, articles$X.4) ||  grepl(keywordlabel, articles$X.5) || grepl(keywordlabel, articles$X.6) || 
   
    #These matches control for blank spaces; 
    #if blank spaces exist, then these filters are excluded.
    if(yearhi == -1 || yearlow == -1){ #(1>0){ different checks
      yearmatch <- TRUE
    }
    if(authorlabel == ""){
      authormatch <- TRUE
    }
    if(universitylabel == ""){
      universitymatch <- TRUE
    }
    if(countrylabel == ""){
      countrymatch <- TRUE
    }
    if(publisherlabel == ""){
      publishermatch <- TRUE
    }
    if(gsranklabel == ""){
      gsrankmatch <- TRUE
    }
    if(keywordlabel == ""){
      keywordmatch <- TRUE
    }
    
    #Used for debugging. See what matching values are being passed to  
    #cat (j)
    #cat("YEAR " , yearmatch, "|", yearhi , "|" , yearlow,  "|")  
    #cat("PUBLISHER " , publishermatch, "|") 
    #cat("GS RANK " , gsrankmatch, "|") 
    #cat("KEYWORD" , keywordmatch, "|") 
    #cat("AUTHOR ", authormatch, "|")  
    #cat("UNIVERSITY ", universitymatch, "|") 
    #cat("COUNTRY ", countrymatch, "\n")
    
 
    #This is the filter algorithm.
    if(yearmatch && authormatch && universitymatch && countrymatch && publishermatch && gsrankmatch && keywordmatch) {
      #Loop the countries to search for in the same row, Place of Work column
      for(k in 1:nrow(participatory2)) {
        #if you see:
        #Error in grepl(y, maparray[j]) : invalid 'pattern' argument
        #Make sure participatory2$ISO matches the actual column name.
        y <- participatory2$ISO2.x[k]
        #Is the current country available in the paper's row, Place of Work column?
        if (any(grepl(y, maparray[j]))) {
          #Increment the country count, and pass it to the reactive country dataset 
          if(maptype==1){
             participatory2$WORK[k] <- participatory2$WORK[k] + length(grep(y, maparray[j]))
            }else if(maptype==2){
             participatory2$ALLPUB[k] <- participatory2$ALLPUB[k] + length(grep(y, maparray[j]))
             #Adding cells of dataframes...how?
             #}else if(maptype==3){
             #  maparray<-articles$Country.of.Publication..Rest.of.authors.+articles$Country.of.Publication..1st.Author.
            }else if(maptype==4){
             participatory2$RESTPUB[k] <- participatory2$RESTPUB[k] + length(grep(y, maparray[j]))
          }
        }
      }
    }
    
    #This refreshes the match variables to see if they match in the next row's articles.
    yearmatch <- FALSE
    authormatch <- FALSE
    universitymatch <- FALSE
    countrymatch <- FALSE
    publishermatch <- FALSE
    gsrankmatch <- FALSE
    keywordmatch <- FALSE
    
  }
  #This is necessary for the running of the function. 
  #Change from participatory to participatory2 to test function accuracy.
 cat("Done!")
 return(participatory2) 
}

#run this after aving a function each time to rerun the function:
#source("C:/Users/Pierce/Desktop/pierce-mapping/dataFilter.R")
#dataFilter(articles, countries, "", -1, -1, "", "", "", "", "")