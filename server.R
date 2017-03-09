library(shiny)
library(ggplot2)
library(dplyr)
library(rgdal)
library(maps)
library(leaflet)
library(tidyr)
library(htmltools)
library(htmlwidgets)

revenue.data <- read.csv("data/washington_school_district_level_revenue.csv", stringsAsFactors = FALSE)
msp.hspe.data <- read.csv("data/2010_to_2013_MSP_HSPE_Scores_by_District.csv", stringsAsFactors = FALSE)
# reads in shapefile of school district boundaries
school.district.shapefile <- readOGR("./data/shapefiles", "tl_2016_53_unsd", stringsAsFactors = FALSE) %>% 
  rmapshaper::ms_simplify()

server <- function(input, output) {
# Reactives
    map.data <- reactive({
    temp <- school.district.shapefile
    revenue.map.data <- mutate(revenue.data, NAME = district, year = paste0(substr(year, 1, 2), substr(year, 6, 7))) %>% 
      select(NAME, year, total.revenue.per.pupil) %>% 
      filter(year == input$years.slider) %>% 
      spread(year, total.revenue.per.pupil)
    colnames(revenue.map.data) <- c("NAME", "total.revenue.per.pupil")
    temp@data <- left_join(temp@data, revenue.map.data)
    temp
  })
# Renders
  output$map <- renderLeaflet({
    
    
    pal <- colorNumeric(palette = "Blues", domain = school.district.shapefile@data$total.revenue.per.pupil)
    
    map <- leaflet(map.data()) %>% 
      addTiles() %>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 1",
        onClick=JS("function(btn, map){ map.setZoom(1); }"))) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
    
    map <- map %>%
      addPolygons(weight = 1,
                  fillColor = ~pal(total.revenue.per.pupil),
                  fillOpacity = 0.8,
                  label = ~paste0(NAME," (Revenue per Pupil: $", total.revenue.per.pupil, ")")) %>% 
      addLegend("bottomright",
                pal = pal, values = ~total.revenue.per.pupil,
                title = "Total Revenue Per Pupil",
                labFormat = labelFormat(prefix = "$"),
                opacity = 1
      )
    
    
    
  })
  
  
}

shinyServer(server)