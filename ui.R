shinyUI(
  fluidPage(
    navbarPage("HCD+D Data Chloropleth Tool", id="nav",
               tabPanel("Map", icon = icon("map-marker"),
                        div(class="outer",
                            tags$style(type = "text/css",
                                       ".outer {position: fixed; top: 50px; left: 0; right: 0;
                                       bottom: 0; overflow: hidden; padding: 0}"),
                            leafletOutput("map", width="100%", height="100%"),
                            
                            absolutePanel(top = 30, right = 30, draggable=TRUE, 
                                          
                                          wellPanel(style = "background-color: #ffffff; width: 350px",
                                                    selectizeInput('MapFilter', 'Display a certain chloropleth:',
                                                                   choices= MapTypeList, multiple=FALSE, selected="WORK"),
                                                    selectizeInput('HideCountry', 'Hide these countries:',
                                                                   choices= countryList, multiple=TRUE),
                                                    selectizeInput('countryFilter', 'Show only these countries:',
                                                                   choices = c(countryList), multiple=TRUE),
                                                    selectizeInput('crossFilter', 'Show where this country works:',
                                                                    choices = countryList2, multiple=TRUE),
                                                    sliderInput('citations', 'Filter by Citations', min = 0, max = 100, value = 1, step = NULL, round = FALSE,
                                                               format = NULL, locale = NULL, ticks = TRUE, animate = FALSE,
                                                               width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
                                                               timezone = NULL, dragRange = TRUE),
                                                    sliderInput('Year', 'Filter by Latest Year', min = 1990, max = 2015, value = 1, step = 10, round = FALSE,
                                                                format = NULL, locale = NULL, ticks = TRUE, animate = FALSE,
                                                                width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
                                                                timezone = NULL, dragRange = TRUE),
                                                    sliderInput('GSRank', 'FIlter by GSRank', min = 0, max=100, value=1, step = 10, round = FALSE,
                                                                format = NULL, locale = NULL, ticks = TRUE, animate = FALSE,
                                                                width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
                                                                timezone = NULL, dragRange = TRUE),
                                                    selectizeInput('Authors', 'Filter by Authors:',
                                                                    choices= AuthorList, multiple=TRUE),
                                                    selectizeInput('University', "Filter by 1st Author's University:",
                                                                   choices= UniversityList, multiple=TRUE),
                                                    selectizeInput('Publisher', 'Filter by Publisher:', 
                                                                   choices= UniversityList, multiple=TRUE),
                                                    
                                                    selectizeInput('KeywordList', 'Choose Keywords', 
                                                              choices = c("Poverty", "Developing Countries", "low-resource settings",
                                                                          "low-income", "developing world", "third world", 
                                                                          "resource-limited settings", "resource-limited", 
                                                                          "Global Inequality", "international development"), multiple=TRUE)
                                          )
                            )
                            )
               ),
               tabPanel("About",
                        icon = icon("question"),
                        
                        #content on left hand side of the page
                        h1("About"),
                        br(),
                        p("This map is an interactive tool which depicts geographically the current state of the field of human-centered design for development. This tool shows 
                          the number of first authors from institutions in certain countries completing research,
                          the number of authors altogether from institutions in certain countries completing research,
                          the number of authors that aren't first authors from institutions in certain countries completing research, 
                          the number of papers, per country, where the dataset engages in research.
                          the tool gives researchers the ability to filter by countries where countries decide to work. (e.g. is United States is chosen, one can see where United States-affiliated authors has engaged in research), and conversely, the tool gives the ability to filter by countries that engage in work in a certain country (e.g. if the United States is chosen, one can see authors from separate countries engage in research in the United States).
                          Additionally, the tool can be filtered by critical metrics analyzed from each paper, including GSRank, number of citations, available authors in the field, publications, universities, and search keywords which found the papers.")
                        )
    )
  )
)



# add the other options to the ui - sliderInput, selectizeInput, etc.
