  function(input, output, session) {
  
  #set the initial color palette
  countryColor <- colorFactor(topo.colors(10), countries@data$WORK)

  
  #set options for filtering
  #Here is the problem. I've been trying to get the server to read 
  #the new filter, and change the data column it reads. I've been trying to add
  #in a new if then statement that reads in the map filter string, and change 
  #based on what it sees. However, it breaks  here:
  #Could you help me figure out why it breaks when you get a chance?
  #The comments below in lines 19-26 are my new inputs, and adding in even one
  #If breaks it. 
  
  filteredData <- reactive({
    if (is.null(input$countryFilter)) {data <- countries}
    else {data <- subset(countries, NAME %in% input$countryFilter)}
    #if (input$MapFilter == 1) {
    #    countryColor <- colorFactor(topo.colors(10), countries@data$WORK)
    #    }
    #     } else if (input$MapFilter == "1st Author Map") {
    #    countryColor <- colorFactor(topo.colors(10), countries@data$FIRSTPUB)
    #    } else if (input$MapFilter == "All Authors Map"){ 
    #    countryColor <- colorFactor(topo.colors(10), countries@data$ALLPUB)
    #    } else if (input$MapFilter == "NOT First Authors Map") {
    # {countryColor <- colorFactor(topo.colors(10), countries@data$RESTPUB)}
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(filteredData()) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(0, 0, zoom = 2)
  })
  
  #update map based on changed inputs
  observe({
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(stroke=NULL, smoothFactor=0.5, 
                  color = ~countryColor(WORK), 
                  popup = ~paste("<strong>Areas of Study :</strong>",WORK, "<strong>Country:</strong>",NAME)) %>% 
      addLegend(pal=countryColor, values = ~WORK, position="bottomright")
    
  })
}