##########################
#Pierce Gordon
#Energy and Resources Group
##########################

##load the necessary libraries 
library(shiny)
library(leaflet)
library(RColorBrewer)
library(rgdal)
library(raster)
library(ggmap)
library(RColorBrewer)
library(stringr)

#These lines load the necessary files to run the code: the article spreadsheet, the inital country list. and the country shapefiles. 
#These articles should be available in the Shiny Global 
#code when the code is actually running. Thus, it should 
#not be necessary to have these variables when the function
#actually runs.
articles <- read.csv('./data/Data Scraping for Journal Articles Final 041916.csv')
participatory <- read.csv('./data/ParticipatoryData.csv')
countries <- readOGR('./world-shapefile', layer = 'world3')
#These lines merge the participatory and countries dataset.
countries@data$polyorder <- 1 : dim(countries@data)[1]
tmp <- merge(countries@data, participatory, by = "ISO3", sort = TRUE, all.x = TRUE)
tmp <- tmp[ order(tmp$polyorder), ]
countries@data <- tmp
participatory <- read.csv('./data/ParticipatoryData.csv')

#Make sure to run the entire file so the adequate variables are available, before running the function at the bottom.

#here the function starts,
dataMiner <- function(articlelist, countries, crossFilter, YearLow, YearHigh, CiteLow, CiteHigh, GSRankLow, GSRankHigh, Authors, University, Publisher,  KeywordList) {
  
  #This version of DataMiner is used for active user input, for use with QGIS mapping software.
  
  ###For the entire code, all it does it take in the article list, filter it in the needed manner, and return the required country list based upon the inputs given.
  
  #There are two critical variable types I’ve created here 
  #that the filter loops use: the labels, and the matches. 
  #~Labels are assigned the inputs fed into DataFilter by 
  #the function’s user, and the ~matches test if there is 
  #this variable has been assigned a user input.If new 
  #filters need to be made. put them here first
  
  
  articles <- articlelist
  yearlow <- YearLow
  yearhi <- YearHigh
  yearmatch <- FALSE
  citelow <- CiteLow
  citehi <- CiteHigh
  citematch <- FALSE
  gsranklow <- GSRankLow
  gsrankhi <- GSRankHigh
  gsrankmatch <- FALSE
  authorlabel <- Authors
  universitylabel <- University
  countrylabel <- crossFilter
  publisherlabel <- Publisher
  keywordlabel <-KeywordList
  authormatch <- FALSE
  universitymatch <- FALSE
  countrymatch <- FALSE
  publishermatch <- FALSE
  keywordmatch <- FALSE

  
  if(Authors!="") {
    authorlabel <- unlist(strsplit(authorlabel, " "))
  }
  if(University!="") {
    universitylabel <- unlist(strsplit(universitylabel, " ")) 
  }
  if(crossFilter!="") {
    countrylabel <- unlist(strsplit(countrylabel, " "))
  }
  if(Publisher!="") {
    publisherlabel <- unlist(strsplit(publisherlabel, " "))
  }
  if(KeywordList!="") {
    keywordlabel <- unlist(strsplit(keywordlabel, " "))
  }
  
  
  #These variables set the reactive country lists where the data is input. Participatory 
  #is the full list, made by a .csv file, and the new participatory2 is the reactive dataset, which 
  #changes based upon the filters chosen.
  participatory <- countries@data
  participatory2 <- countries@data
  participatory2$WORK <- 0
  participatory2$FIRSTPUB <- 0
  participatory2$ALLPUB <- 0
  participatory2$RESTPUB <- 0
  
  #Incrementing variables for each of the loops.
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
  #being searched. It goes through all countries in the 
  #country list until complete, then the loop repeats for 
  # the next article, until all articles have been searched. 
  
  #For example, many All Authors have strings like "GB, 
  #GB, GB, GB, US, US, EG". If the filter finds and decides to count this column, It should be able to count 4 
  #authors from Great Britian from this paper, 2 United 
  #States, and one Egyptian, and put them into their 
  #respective country categories in the participatory2 dataframe.
  
  ####################################3
  #First, the code asks what type of map it wants to generate: 
  #1 = a Place of Work map, 
  #2 = an 1st Author map, 
  #3 = an All Author map, 
  #4 = or a "Rest Author" Map.
  
  
  #These temporary codes let the user of the function 
  #choose which map they want to use. The authors are meant only to cross with Places of Work: either the Place of work is the input, and thus author maps will show, or an author location is the input, and thus place of work maps will show.
  
  
  map <- function() {
    message("What type of map do you want to display? 1 = Work, 2 = 1stAuth, 3 = RestAuth, 4= AllAuth ");
    x <- as.numeric(readLines(n=1));
    return(x)
  }
  

  cross <- function(maptype) {

    if(maptype==1){
      if(crossFilter != "") {
      #if(1>0) {
      message("What type of Author Column do you want to be the filter? 2 == 1stAuth, 3 == AllAuth, 4 == RestAuth ");
      x <- as.numeric(readLines(n=1));  
      return(x)
      } else {
        x <- 2
      }
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
  
  #The variable cross is used as the test variable to indicate if the 
  #country listed is located in the column of interest, and thus 
  #the list of second countries is worth adding up to the reactive 
  #country index. The column assigned to the variable (Place of Work, 1st Author, etc.) is chosen here.
  if(crosstype==1){
    crossarray<-sapply(articles$Place.of.Work, as.character)
  }else if(crosstype==2){
    crossarray<-sapply(articles$Country.of.Publication..1st.Author., as.character)
  }else if(crosstype  ==3){
    crossarray<-sapply(articles$Country.of.Publication..Rest.of.authors., as.character)
  }else if(crosstype==4){
    s<-sapply(articles$Country.of.Publication..1st.Author., as.character)
    t<-sapply(articles$Country.of.Publication..Rest.of.authors., as.character)
    crossarray<-paste(s, t)
  }
  
  #This is where the sibling array to cross array is assigned its column: if crossarray is Places of Work, the maparray is 1st Authors. This variable is used as the array where the country counting occurs 
  if(maptype==1){
    maparray <- sapply(articles$Place.of.Work, as.character)
  }else if(maptype==2){
    maparray <- sapply(articles$Country.of.Publication..1st.Author., as.character)
  }else if(maptype==3){
    maparray <- sapply(articles$Country.of.Publication..Rest.of.authors., as.character)
  }else if(maptype==4){
    s<-sapply(articles$Country.of.Publication..1st.Author., as.character)
    t<-sapply(articles$Country.of.Publication..Rest.of.authors., as.character)
    maparray<-paste(s, t)
  }
  
  #############################################
  #Filter algorithm. Goes through raw data and filters papers that don't match the reactive values.
  #############################################
  
  
  cat("Working")
  for(j in 2:nrow(articles)) {
    cat(".")

    #These preliminary tests check if there is a match between the user-defined input and the current row.
    #This code searches in multiple columns for the available keywords.

    if(yearhi == -1 && yearlow == -1){
      yearmatch <- TRUE
    } else {
      if(articles$Year[j] <= yearhi && articles$Year[j] >= yearlow) {
        yearmatch <- TRUE
      }
    }
    if(citehi == -1 && citelow == -1){
      citematch <- TRUE
    } else {
      if(articles$Cites[j] <= citehi && articles$Cites[j] >= citelow) {
        citematch <- TRUE
      }
    }
    if(gsranklow == -1 && gsrankhi == -1){
      gsrankmatch <- TRUE
    } else {
      if(articles$GSRank[j] <=  gsrankhi && articles$GSRank[j] >= gsranklow) {
        gsrankmatch <- TRUE
      }
    }
    if(authorlabel[1] == ""){
      authormatch <- TRUE
    } else {
      for(l in 1:length(authorlabel)){
        authormatch <- grepl(authorlabel[l], articles$Authors[j])
        if (authormatch) {
          break
        }
      }
    }
    if(universitylabel[1] == ""){
      universitymatch <- TRUE
    } else {
      for(l in 1:length(universitylabel)){
        universitymatch <- grepl(universitylabel[l], articles$Place.of.Publish..1st.author.[j])
        if (universitymatch) {
          break
        }
      }
    }
    if(countrylabel[1] == ""){
      countrymatch <- TRUE
    } else {
      for(l in 1:length(countrylabel)){
        countrymatch <- grepl(countrylabel[l], crossarray[j])
        if (countrymatch) {
          break
        }
      }
    }
    if(publisherlabel[1] == ""){
      publishermatch <- TRUE
    } else {
      for(l in 1:length(publisherlabel)){
        publishermatch <- grepl(publisherlabel, articles$Publisher[j])
        if (publishermatch) {
          break
        }
      }
    }
    if(keywordlabel[1] == ""){
      keywordmatch <- TRUE
    } else {
      for(l in 1:length(keywordlabel)){
        for(i in 28:32){
          keywordmatch <- grepl(keywordlabel[l], articles[j,i])
          if (keywordmatch) {
            break
          }
        }
      }
    }
    
    #Used for debugging. We can see what matching values are being passed to different parts of the code.
    #cat (j)
    #cat("YEAR " , yearmatch, "|", yearhi , "|" , yearlow,  "|")  
    #cat("PUBLISHER " , publishermatch, "|") 
    #cat("GS RANK " , gsrankmatch, "|") 
    #cat("KEYWORD" , keywordmatch, "|") 
    #cat("AUTHOR ", authormatch, "|")  
    #cat("UNIVERSITY ", universitymatch, "|") 
    #cat("COUNTRY ", countrymatch, "\n")
    
    
    #This is the filter algorithm. First, it checks if all of the matches in this column are true. If so, it runs the rest of the code.
    if(yearmatch && citematch && authormatch && universitymatch && countrymatch && publishermatch && gsrankmatch && keywordmatch) {
      #Loops the countries to search in the desired row for the countries of interest, and prepares participatory2 to be modified by its code.
      z <- sapply(participatory2$ISO2.x, as.character)
      for(k in 1:nrow(participatory2)) {
        #if you see:
        #Error in grepl(y, maparray[j]) : invalid 'pattern' argument
        #Make sure participatory2$ISO matches the actual column name.
        y <- z[k]
        #Is the current country available in the paper's row, Place of Work column?
        if (any(grepl(y, maparray[j]))) {
          #Increment the country count, and pass it to the reactive country dataset 
          if(maptype==1){
            participatory2$WORK[k] <- participatory2$WORK[k] + str_count( maparray[j], pattern = y)
          }else if(maptype==2){
            participatory2$FIRSTPUB[k] <- participatory2$FIRSTPUB[k] + str_count( maparray[j], pattern = y)
          }else if(maptype==3){
            participatory2$RESTPUB[k] <- participatory2$RESTPUB[k] + str_count( maparray[j], pattern = y)
          }else if(maptype==4){
            participatory2$ALLPUB[k] <- participatory2$ALLPUB[k] + str_count( maparray[j], pattern = y)
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
    citematch <- FALSE
    
  }
  #This is necessary for the running of the function. 
  #Change from participatory to participatory2 to test function accuracy.
  cat("Done!")
  
  #This code gives the user the opportunity to write the new completed participatory2 data to an excel spreadsheet.
  writedata <- function() {
    message("Would you like to write your data to a .csv file for use in other programs? (1 for yes, 0 for no)");
    x <- as.numeric(readLines(n=1));  
    return(x)
  }
  
  
  e <- 1
  while(e==1){
    x = writedata()
    if(x==1){
      write.csv(participatory2, file = "test.csv")
      message("Written, ready, and available. Check here for your file: C:/Users/Pierce/Google Drive/URAP 2016/URAP 2016 Student Folders/Design Impact Literature Review/pierce-mapping/data/test.csv")
      e <- 0
    }else if(x==0){
      message("Okay. Your work is done here!")
      e <- 0
    }else{
      message("Oops, your input was incorrect. Choose a valid input.")
      e <- 1
    }
  }
  
  return(participatory2) 
}

########################################################
#run this after aving a function each time to rerun the function:
########################################################
#source("C:/Users/Pierce/Google Drive/URAP 2016/URAP 2016 Student Folders/Design Impact Literature Review/pierce-mapping/dataFilter.R")
#
#
########################################################
#This file runs the code. Make sure to run the entire file (select all the code, and Run) so the adequate variables are available,
#before running the function here at the bottom.
########################################################
#Variables corresponding with the function:
#(articlelist, countries, crossFilter, YearLow, YearHigh, CiteLow, CiteHigh, GSRankLow, GSRankHigh, Authors, University, Publisher,  KeywordList)
#dataMiner(articles, countries, "", -1, -1, -1, -1, -1, -1, "", "", "", "")
#
#
#
#
#
