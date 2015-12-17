

##########################
#Pierce Gordon
#Energy and Resources Group
##########################

#This function filters the raw data in the Articles .csv file for the reactive inputs in the Shiny countries file.
#countrylabel <- "US"

#These variables are placeholders made to debug the code before it turns into a function. Keep these a comment when working.
#data <- countries@data
#crossFilter <- "UK" 
#Authors <- ""
#University <- ""

frametest <- function(data, crossFilter, Authors, University) {
  articles <- read.csv('./data/Data Scraping for Journal Articles.csv')
  ###code goes here. ALL THIS DOES IS ONE THING: RETURNS A DATA SET 
  ###that filters based upon the inputs given.
  
  #There are two critical variables here that the loops use: the labels, and the matches. 
  #Labels are assigned the Shiny inputs, and the matches see if the labels match the 
  #information in the current column we care about. Add here to add new filters first.

  authorlabel <- Authors
  authormatch <- FALSE
  universitylabel <- University
  universitymatch <- FALSE
  countrylabel <- crossFilter
  countrymatch <- FALSE
  
  #These variables set the reactive country lists where the data is input. Participatory 
  #is the full list, made by a .csv file, and participatory2 is the reactive dataset, which 
  #changes based upon the filters chosen.
  participatory <- data@data
  participatory2 <- data@data
  participatory2$WORK <- 0
  participatory2$FIRSTPUB <- 0
  participatory2$ALLPUB <- 0
  participatory2$RESTPUB <- 0
  
  #X is a temporary dataframe that MIGHT be necessary. Right now, it is not used.
  #x <- participatory[-c(1,3,4)]
  #x[2:5] <- 0
  #x <- matrix(0, nrow = num, ncol = 4)
  
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
  
  #Filter algorithm. Goes through raw data and filters papers that don't match the reactive values.
  
  #for(i in 1:nrow(participatory2)) {
  #increment the articles being accessed
  for(j in 2:nrow(articles)) {
    authormatch <- grepl(authorlabel, articles[[Authors[j]]])
    universitymatch <- grepl(universitylabel, articles[[Place.of.Publish..1st.author.[j]]])
    countrymatch <- grepl(countrylabel, articles[[Country.of.Publication..1st.Author.[j]]])
    
    if(authorlabel == ""){
      authormatch <- TRUE
    }
    if(universitylabel == ""){
      universitymatch <- TRUE
    }
    if(countrylabel == ""){
      countrymatch <- TRUE
    }
   
    #Used for debugging. See what matching values are being passed to  
    #cat("AUTHOR ", j,":", authormatch, " ")  
    #cat("UNIVERSITY ", j,":" , universitymatch, " ") 
    #cat("COUNTRY ", j,":", countrymatch, "\n")

    
   if(authormatch && universitymatch && countrymatch) {
       #Loop the countries to search for in the same row, Place of Work column
         for(k in 1:nrow(participatory2)) {
           y <- participatory2$ISO2.x[k]
           #Is the current country available in the paper's row, Place of Work column?
           if (any(grepl(y, articles$Place.of.Work[j]))) {
             #Increment the country count, and pass it to the reactive country dataset 
            #x[k,2] <- x[k,2] + length(grep(y, articles$Place.of.Work[j]))
             participatory2$WORK[k] <- participatory2$WORK[k] + length(grep(y, articles$Place.of.Work[j]))
           }
         }
      }
    
    authormatch <- FALSE
    universitymatch <- FALSE
    countrymatch <- FALSE
    
    
 }

#This is necessary for the running of the function
 return(participatory2) 
}
