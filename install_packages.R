##run this script only once, to make sure you have installed all the appropriate packages!
#only needed if installing outside RStudio
#options(repos="https://cran.rstudio.com") 

#install packages
install.packages("shiny")
install.packages("leaflet")
install.packages("RColorBrewer")
install.packages("rgdal")
install.packages("raster")
install.packages("ggmap")

#load data
source("loadData.R")
