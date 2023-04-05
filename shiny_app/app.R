library(shiny)
library(shinyMobile)
library(leaflet)
library(sf)
library(htmltools)
library(dataRetrieval)
library(lubridate)
library(tidyverse)

usgs_gages <- read.csv("usgs_gages.txt")

ui = f7Page(
  title = "Wildfire Indicator",
  options = list(dark = TRUE,pullToRefresh = FALSE),
  f7TabLayout(
    f7Panel(side = "left",theme = "light", effect = "cover",
            h1(p("Quick Links")),
            h2(f7Link(label = "Texas Water Resources Institute", href = "https://twri.tamu.edu")),
            h2(f7Link(label = "Texas Comission on Environmental Quality", href = "https://tceq.texas.gov")),
            h2(f7Link(label = "Clearn Rivers Program", href = "https://www.tceq.texas.gov/waterquality/clean-rivers")),
            h2(f7Link(label = "USGS National Water Information System", href = "https://waterdata.usgs.gov/nwis")),
            br(),
            br(),
            br(),
            h3(p("Made with", a("Shiny",href = "http://shiny.rstudio.com"), ".")),
            img(src = "shinyLogo.png",width = "70px", height = "70px")),
    f7Panel(side = "right", theme = "light", effect = "cover",
            f7Block(
              f7BlockHeader(text = "Description"),
              "",
              f7BlockFooter(text = "Developed By - Shubham Jain")
            )),
    
    navbar = f7Navbar(
      title = "Wildfire Indicator",
      hairline = TRUE,
      shadow = TRUE,
      leftPanel = TRUE,
      rightPanel = TRUE),
    
    f7Tabs(
      animated = TRUE,
      id = "tabs",
      f7Tab(
        tabName = "Map",
        title = "Map",
        icon = f7Icon("map",color = "green"),
        active = TRUE,
        leafletOutput("mymap",width = "100%", height = "100%"),
      ),
      f7Tab(
        tabName = "",
        title = "Wildfire Index",
        h3("USGS Streamflow Data"),
        
        icon = f7Icon("cloud_bolt_rain", color = "blue"),
        active = TRUE
      ),
      f7Tab(
        tabName = "",
        title = "Recent activity",
        h3("Surface Water Quality Monitoring Data"),
        br(),
        br(),
        icon = f7Icon("drop_triangle_fill", color = "red"),
        active = TRUE
      )
    )
  )
)

##########################################################################################
server = function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles()%>%
      addMarkers(data = usgs_gages, ~LNG_GAGE, ~LAT_GAGE, 
                 clusterOptions = markerClusterOptions(), layerId = ~GAGEID)
  })
  
}
shinyApp(ui, server)
