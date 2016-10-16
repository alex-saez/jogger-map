library(shiny)
library(leaflet)
library(data.table)
library(dplyr)
blocks = fread('../data/blocks_Manhattan.csv')
restrooms = fread('../data/restroom_coordinates.csv')
fountains = fread('../data/drink_location.csv')
source('../lib/get_points_from_segment.R')

fountains = fountains %>%
  select(lon, lat) %>%
  filter(!is.na(lon) & !is.na(lat))


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(class = "panel panel-default", 
                top = 10, right = 10,
                h3("Select preferences"),
                sliderInput("tree", "   Trees:", min=1, max=100, value=50),
                sliderInput("slope", label = "   Flatness:", min=1, max=100, value=50),
                hr(),
                checkboxInput("show_restrooms", "Show restrooms", FALSE),
                checkboxInput("show_fountains", "Show fountains", FALSE),
                submitButton("Update")
  )
)



server <- function(input, output, session) {
  
  # initialize map:
  colors = colorRamp(c("red", "black", "green"))
  map = leaflet() %>%
    addTiles() %>%
    setView(lng = -73.975, lat = 40.766, zoom = 16) %>%
    addLegend(position = "bottomright", pal = colorFactor(colors, levels=c('worse', 'neutral','better')), c('better', 'neutral','worse'))

  
  # select subset of segments to display in the area of view:
  update_ind = reactive({
    bounds = input$map_bounds
    with(blocks, which( (start_lon<bounds$east | end_lon<bounds$east) &
                          (start_lon>bounds$west | end_lon>bounds$west) &
                          (start_lat>bounds$south | end_lat>bounds$south) &
                          (start_lat<bounds$north | end_lat<bounds$north) ) )
  })
  
  
  # create icons:
  toilet_icon <- makeIcon(
    iconUrl = "../data/Bathroom-gender-sign.png",
    iconWidth = 30, iconHeight = 30
  )
  fountain_icon <- makeIcon(
    iconUrl = "../data/aiga-drinking-fountain-bg.png",
    iconWidth = 30, iconHeight = 30
  )
  
  # draw icons if wanted:
  observe({
    leafletProxy("map") %>% clearMarkers() 
    if (input$show_restrooms)
      leafletProxy("map") %>% addMarkers(lng = restrooms$LNG, lat = restrooms$LAT, icon = toilet_icon)
    if (input$show_fountains)
      leafletProxy("map") %>% addMarkers(lng = fountains$lon, lat = fountains$lat, icon = fountain_icon)
  })
  
  
  #draw segments:
  colors = colorRamp(c("red", "black", "green"))
  
  observe({
    score = input$tree*(blocks$tree_dens)  +  input$slope*(-blocks$slope)
    score = score[update_ind()]
    score = rank(score)
    
    leafletProxy("map") %>% clearShapes()  
    k = 0
    for(i in update_ind()){
      k = k + 1
      seg_points = get_points_from_segment(blocks$the_geom[i])
      leafletProxy("map") %>% addPolylines(lng = seg_points$lon, 
                                           lat = seg_points$lat,   
                                           col = rgb(colors(score[k]/max(score))/255),
                                           weight = 5, 
                                           opacity = 1)
    }
    
  })
  
  # render map:
  output$map = renderLeaflet({
    map
  })
  
  
}

shinyApp(ui, server)


