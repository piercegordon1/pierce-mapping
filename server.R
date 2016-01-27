  function(input, output, session) {
  
  #set the initial color palette
  countryColor <- colorFactor(topo.colors(10), countries@data$WORK)


  #Sets the necessary filters read from the UI
  
  filteredData <- reactive({
    if (is.null(input$countryFilter)) {data <- countries}
    else {data <- subset(countries, NAME %in% input$countryFilter)}
    
    if (is.null(input$HideCountry)) {data <- data}
    else {data <- subset(data, !(NAME %in% input$HideCountry))}
    
    if (is.null(input$crossFilter)) {data <- data}
    else { data <- dataFilter(countries, data, input$crossFilter, "", -1, -1, "", "", "", "", "")}
    
  })
  
  observe ( {
    print(head(filteredData(), 10))
  })

  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(filteredData()) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(0, 0, zoom = 3)
  })
  
  
  #create reactive colorVariable, which updates the color palette based on the type of map chosen.
  colorVariable <- reactive({
    filteredData()@data[[input$MapFilter]]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorBin("YlOrRd", colorVariable()) #you can change the color palette here.
  })
  
  #update map based on changed inputs
  observe({
    pal <- colorpal() #set the variable pal equal to the reactive variable colorpal.
    colorBy <- input$MapFilter
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(stroke=NULL, smoothFactor=0.5, 
                  color = ~pal(filteredData()@data[,colorBy]),
                  opacity = 0.9,
                  popup = ~paste("<strong>",colorBy,":</strong>",filteredData()@data[,colorBy], "<strong>Country:</strong>",NAME)) %>% 
      addLegend(title=colorBy, pal=colorpal(), values=filteredData()@data[,colorBy], position="bottomright")
    
  })
}
