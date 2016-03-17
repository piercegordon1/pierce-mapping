

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
Sys.setlocale('LC_ALL','C');

#These are placeholders for the function to see if it works when fed the input.
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

#These lines load the necessary files to run the code: the article spreadsheet, the inital country list. and the country shapefiles. 
#These articles should be available in the Shiny Global 
#code when the code is actually running. Thus, it should 
#not be necessary to have these variables when the function
#actually runs.
articles <- read.csv('./data/Data Scraping for Journal Articles.csv')
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
dataFilter <- function(articlelist, countries, crossFilter, YearLow, YearHigh, Authors, University, Publisher, GSRank, KeywordList) {
  
  ###For the entire code, all it does it take in the article list, filter it in the needed manner, and return the required country list based upon the inputs given.
  
  #There are two critical variable types I've created here 
  #that the filter loops use: the labels, and the matches. 
  #~Labels are assigned the inputs fed into DataFilter by 
  #the function's user, and the ~matches test if there is 
  #this variable has been assigned a user input.If new 
  #filters need to be made. put them here first
  
  
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
    d <- 1;
    return(d);
  }
  #map <- function() {
    #message("What type of map do you want to display? 1 = Work, 2 = 1stAuth, 3 = AllAuth, 4= RestAuth ");
    #x <- as.numeric(readLines(n=1));
    #return(x)
  #}
  
  maptype <- 0
  
  #cross <- function(maptype) {
    #if(maptype==1){
      #if(1>0) {
      #message("What type of Author Data do you want to cross with the data? 2 == 1stAuth, 3 == AllAuth, 4 == RestAuth ");
      #x <- as.numeric(readLines(n=1));  
      #return(x)
    #} else {
    #  message("The crossed data will be the Place of Work data.");
      #x <- 1
      #return(x)
    #}
  #}
  
  cross <- function(maptype) {
    d <- 2;
    return(d);
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
    crossarray<-articles$Place.of.Work
  }else if(crosstype==2){
    crossarray<-articles$Country.of.Publication..1st.Author.
    #BUG Adding cells of dataframes...how?
    #}else if(crosstype==3){
    #  crossarray<-articles$Country.of.Publication..Rest.of.authors.+articles$Country.of.Publication..1st.Author.
  } else if (crosstype==3){ # concatenation instead of adding 
    df <- data.frame(a = articles$Country.of.Publication..Rest.of.authors., 
    b = articles$Country.of.Publication..1st.Author.);
    crossarray <- rowSums(df)
  } else if(crosstype==4){
    crossarray<-articles$Country.of.Publication..Rest.of.authors.
  }
  
  #This is where the sibling array to cross array is assigned its column: if crossarray is Places of Work, the maparray is 1st Authors. This variable is used as the array where the country counting occurs 
  if(maptype==1){
    maparray <- articles$Place.of.Work
  }else if(maptype==2){
    maparray <- articles$Country.of.Publication..1st.Author.
    #BUG Adding cells of dataframes...how?
    #}else if(maptype==3){
    #  maparray<-articles$Country.of.Publication..Rest.of.authors.+articles$Country.of.Publication..1st.Author.
  }else if(maptype==4){
    maparray <- articles$Country.of.Publication..Rest.of.authors.
  }
  
  #############################################
  #Filter algorithm. Goes through raw data and filters papers that don't match the reactive values.
  #############################################
  
  
  #cat("Working....")
  for(j in 2:nrow(articles)) {
    #cat(".")
    if(articles$Year[j] <= yearhi && articles$Year[j] >= yearlow) {
      yearmatch <- TRUE
    }
    #print("dataFilter.R line 209")
    #These preliminary tests check if there is a match between the user-defined input and the current row.
    authormatch <- grepl(authorlabel, articles$Authors[j])
    universitymatch <- grepl(universitylabel, articles$Place.of.Publish..1st.author.[j])
    countrymatch <- grepl(countrylabel, crossarray[j])
    publishermatch <- grepl(publisherlabel, articles$Publisher[j])
    gsrankmatch <- grepl(gsranklabel, articles$GSRank)
    #BUG How do we get keyword to match more than the first cell? multiple columns 
    #keywordmatch <- grepl(keywordlabel, articles$Second.Keyword) || grepl(keywordlabel, articles$X) ||  grepl(keywordlabel, articles$X.1) ||  grepl(keywordlabel, articles$X.2) ||  grepl(keywordlabel, articles$X.3) ||  grepl(keywordlabel, articles$X.4) ||  grepl(keywordlabel, articles$X.5) || grepl(keywordlabel, articles$X.6) || 
    #
    #
    #
    #These matches control for blank spaces; 
    #if the user made one of the filters blank, then these filters are excluded as those which filter numbers from the program.
    #print("dataFilter.R line 223")
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
    if(yearmatch && authormatch && universitymatch && countrymatch && publishermatch && gsrankmatch && keywordmatch) {
      #Loops the countries to search in the desired row for the countries of interest, and prepares participatory2 to be modified by its code.
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
  
  #This code gives the user the opportunity to write the new completed participatory2 data to an excel spreadsheet.
  writedata <- function() {
    #message("Would you like to write your data to a .csv file for use in other programs? (1 for yes, 0 for no)");
    #x <- as.numeric(readLines(n=1));  
    return(1)
  }
  
  
  e <- 1
  while(e==1){
    x = writedata()
    if(x==1){
      write.csv(participatory2, file = "test.csv")
      message("Written, ready, and available. Check in the pierce-mapping folder for your file.")
      e <- 0
    }else if(x==0){
      message("Okay. Your work is done here!")
      e <- 0
    }else{
      message("Oops, your input was incorrect. Choose a valid input.")
      e <- 1
    }
  }
  
  return(participatory2) #normal data frame countries = large spatial 
  print("return line 315")
}

########################################################
#run this after aving a function each time to rerun the function:
########################################################
#source("./dataFilter.R")
#
#
########################################################
#This file runs the code. Make sure to run the entire file (select all the code, and Run) so the adequate variables are available, before running the function here at the bottom.
########################################################
dataFilter(articles, countries, "", -1, -1, "", "", "", "", "");
#
#source("URAP/pierce-mapping/dataFilter.R");

#### try ####