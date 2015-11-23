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
    
    if (is.null(input$HideCountry)) {data <- data}
    else {data <- subset(data, !(NAME %in% input$HideCountry))}
    
    #if (is.null(input$CrossFilter)) {data <- data}
    #else {
    #  data <- participatory2
    #}
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(filteredData()) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(0, 0, zoom = 3)
  })
  
  
  #create reactive colorVariable, which updates the color palette based on a user-defined metric
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