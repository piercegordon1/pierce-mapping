  function(input, output, session) {
  
  #set the initial color palette
  countryColor <- colorFactor(topo.colors(10), countries@data$WORK)


  #Sets the necessary filters read from the UI
  
  filteredData <- reactive({
    
    if (is.null(input$crossFilter)) {data <- countries}
    else { 
      print("server.R line 19")
      data <- dataFilter(articles, countries, "", -1, -1, -1, -1, -1, -1, "", "", "", "")}
      #print("server.R line 21")
    
    if (is.null(input$countryFilter)) {data <- data}
    else {data <- subset(countries, NAME %in% input$countryFilter)}
    
    if (is.null(input$HideCountry)) {data <- data}
    else {data <- subset(data, !(NAME %in% input$HideCountry))}
    #subset = gets rid of a column, data is from countries 
  })
  
  #observe ( {
    #print(head(filteredData(), 10)) 
  #})

  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    print("server.R line 33")
    leaflet(filteredData()) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(0, 0, zoom = 3)
    })
  
#HOW DOES ALL OF THIS WORK??   
  #create reactive colorVariable, which updates the color palette based on the type of map chosen.
  colorVariable <- reactive({
    print("server.R line 43")
    # Error: trying to get slot "data" from an object (class "data.frame") that is not an S4 object 
    filteredData()@data[[input$MapFilter]]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    print("server.R line 49")
    colorBin("YlOrRd", colorVariable()) #you can change the color palette here.
  })
  
  #update map based on changed inputs figure out how this is working and see if its similar to the diagram!!! 
  observe({
    print("server.R line 55")
    pal <- colorpal() #set the variable pal equal to the reactive variable colorpal.
    #Where the fourth option breaks!
    colorBy <- input$MapFilter
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(stroke=NULL, smoothFactor=0.5, 
                  color = ~pal(filteredData()@data[,colorBy]),
                  opacity = 0.9,
                  popup = ~paste("<strong>",colorBy,":</strong>",filteredData()@data[,colorBy], "<strong>Country:</strong>",NAME)) %>% 
      addLegend(title=colorBy, pal=colorpal(), values=filteredData()@data[,colorBy], position="bottomright")
    print("server.R line 64")
    print("End");
  })
}
