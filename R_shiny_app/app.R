library(shiny)
library(shinyMobile)
library(leaflet)
library(htmltools)
library(dataRetrieval)
library(lubridate)
library(tidyverse)
library(geojsonio)
library(spdplyr)
library(shinyWidgets)
wf_area <- geojson_read("./wf_area.geojson", what = "sp")
wildfires <- read.csv("./data/wildfires.csv")
wildfires <- wildfires[,-1]
ui = f7Page(
  title = "US Large Wildfires Viewer",
  options = list(dark = TRUE,pullToRefresh = FALSE),
  f7TabLayout(
    f7Panel(side = "left",theme = "light", effect = "cover",
            h1(p("Quick Links")),
            h2(f7Link(label = "Texas A&M Forest Service", href = "https://tfsweb.tamu.edu/")),
            h2(f7Link(label = "National Interagency Fire Center", href = "https://www.nifc.gov/")),
            h2(f7Link(label = "InciWeb", href = "https://inciweb.nwcg.gov/")),
            br(),
            h3(p("Made with", a("Shiny",href = "http://shiny.rstudio.com"))),
            img(src ="shinyLogo.png",height = 100, width = 100) ,
            h3("Developed By:"),
            h4("Shubham Jain"),
            img(src ="shubham.jpg",height = 100, width = 100) ,
            f7BlockFooter(text = "PhD student, Water Management and Hydrological Science"),
            h4("Sambandh Dhal"),
            img(src ="Sambandh-Dhal.jpg",height = 100, width = 100) ,
            f7BlockFooter(text = "PhD student, Electrical and Computer Engineering"),
            h4("Krishna Chaitanya Gadepally"),
            img(src ="krishna.jpg",height = 100, width = 100) ,
            f7BlockFooter(text = "Masters Student, Electrical and Computer Engineering"),
            h4("Prathik Vijaykumar"),
            img(src ="pathik.jpg",height = 100, width = 100),
            f7BlockFooter(text = "Masters Student, Electrical and Computer Engineering")),            
            
    
    navbar = f7Navbar(
      title = "US Large Wildfires Viewer",
      hairline = TRUE,
      shadow = TRUE,
      leftPanel = TRUE,
      rightPanel = FALSE),
    
    f7Tabs(
      animated = TRUE,
      id = "tabs",
      f7Tab(
        tabName = "Map",
        title = "Map",
        icon = f7Icon("map",color = "green"),
        br(),
        sliderTextInput("map_year", "Select a year:",grid = TRUE, choices = c("2011","2012","2013","2014","2015","2016","2017","2018","2019","2020"), selected = "2015",width = "100%"),
        br(),
        br(),
        leafletOutput("wfmap",width = "100%", height = "100%"),
        
        active = TRUE),
      
      f7Tab(
        tabName = "data_view",
        title = "Wildfire Data",
        h3("Large Wildfire data"),
        selectInput("wildfire_select","Select Wildfire ID", choices = sort(unique(wildfires$FIRE_ID)),width = "100%"),
        leafletOutput("wf_single",width = "100%", height = "50%"),
        uiOutput("wildfire_table"),
        icon = f7Icon("cloud_sun_bolt", color = "blue"),
        active = TRUE
      ),
      
      f7Tab(
        tabName = "Analysis",
        title = "Explore",
        h3("Data Gallery"),
        selectInput("image_select", "Select a Plot:",
                    choices = c("Average annual large wildfire incidents by states in the US" = "hexbin.png", 
                                "Average monthly large wildfires" = "temperature.png",
                                "Burn area by NLCD land cover in large wildfires between 2011-2020" = "lc_total.png",
                                "Cause of large wildfires (>500 acres) in the contiguous US between 2011-2015" = "cause.png",
                                "Percent of Level IV Ecoregion land burned in large wildfires between 2011-2020" = "burn_Area.png"
                                ),width = "100%"),
        div(style="text-align:center;",
        uiOutput("selected_image")),
        icon = f7Icon("square_grid_2x2", color = "red"),
        active = TRUE
    )
    )
  )
)

server = function(input, output, session) {
  customIcon <- makeIcon(
    iconUrl = "https://img.icons8.com/color/48/null/map-pin.png",
    iconWidth = 30,
    iconHeight = 30
  )
  observeEvent(input$map_year,{
    selected_wf <- wildfires%>%filter(year == as.numeric(input$map_year))
    output$wfmap <- renderLeaflet({
      leaflet() %>%
        addTiles()%>%
        addProviderTiles(providers$Esri.WorldStreetMap) %>%
        addMarkers(data = selected_wf, ~LONGITUDE, ~LATITUDE,  clusterOptions = markerClusterOptions(),popup = ~paste("<b>","FIRE ID - ",FIRE_ID, "</b></br> <button onclick='Shiny.onInputChange(\"button_click\",  Math.random())' id='ID2' type='button' class='btn btn-default action-button'>View Data</button>"),layerId = ~FIRE_ID)
    })
  })
  
  observeEvent(input$wfmap_marker_click,{
    updateSelectInput(session,inputId = "wildfire_select", selected = input$wfmap_marker_click$id)
  })
  
  observeEvent(input$button_click,{
    updateF7Tabs(session, id = "tabs", selected = "data_view")
  })


  
  observeEvent(input$wildfire_select,{
    selected_wildfire_area2 <- wf_area%>%filter(FIRE_ID == as.character(input$wildfire_select))
    wildfire_df <- wildfires%>%filter(FIRE_ID== as.character(input$wildfire_select))
    wildfire_df <- wildfire_df%>%select("FIRE_ID","IG_DATE","LATITUDE","LONGITUDE","ACRES","US_L4CODE","SECTION","ppt","tmean","vpdmax","vpdmin","PDSI","PET","EVI","NDVI","Elevation")
    wildfire_df <- data.frame(t(wildfire_df))
    wildfire_df$Data <- c("FIRE_ID","Ignition Date","Latitude","Longitude","Burn Area (acres)","Level 4 Ecoregion ID","Ecoregion","Monthly Precipitation (mm)","Mean Temperature (Degree Celsius)","Maximum Vapour Pressure Deficit (h Pa)","Minimum Vapour Pressure Deficit (h Pa)","Palmer Drought Severity Index","Potential Evapotranspiration (mm)"
                            ,"Enhanced Vegetation Index","Normalized Difference Vegetation Index","Elevation (m)")
    wildfire_df <- wildfire_df[,c(2,1)]
    names(wildfire_df) <- c("Data","Value")
    rownames(wildfire_df) <- 1:16
    output$wf_single <- renderLeaflet({
      leaflet() %>%
        addTiles()%>%
        addProviderTiles(providers$Esri.WorldStreetMap) %>%
        addPolygons(data = selected_wildfire_area2, color = "#444444", weight = 2, smoothFactor = 1,
                    opacity = 1.0,highlightOptions = highlightOptions(color = "white", weight = 10,
                                                                      bringToFront = TRUE))
    })
    
    output$wildfire_table <- renderUI({f7Table(wildfire_df)})
  })
  
  observeEvent(input$image_select, {
    output$selected_image <- renderUI({
      img(src = input$image_select,width = "50%",height = "50%")
    })
  })
}

shinyApp(ui, server)