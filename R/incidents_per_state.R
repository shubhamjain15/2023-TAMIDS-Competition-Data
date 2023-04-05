library(lubridate)
library(stringr)
library(tidyverse)
library(ggplot2)
library(viridis)
library(geojsonio)
library(RColorBrewer)
library(rgdal)
incidents <- read.csv("./inputs/incidents_state.txt")
states <- read.csv("./inputs/wildfire_states.txt")

incidents$FireDiscov <- mdy(word(incidents$FireDiscov,1))
incidents$year <- year(incidents$FireDiscov)

#5 years 2018 to 2022
incidents <- incidents%>%filter(year >= 2018 & year < 2023 )

#merge state
incidents$POOFips <- floor(incidents$POOFips/1000)
#summarize by year and state
incidents_year_state <- incidents%>%
                          group_by(year,POOFips)%>%
                          summarise(n = n())
incidents_year_state <- incidents_year_state[!is.na(incidents_year_state$POOFips),]                                          
incidents_year_state <- incidents_year_state%>%
                          group_by(POOFips)%>%
                          summarise(avg = mean(n))

incidents_year_state <- incidents_year_state%>%
                            left_join(.,states,by = c("POOFips" = "STATEFP"))

spdf <- geojson_read("inputs/us_states_hexgrid.geojson",  what = "sp")


spdf@data = spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))


plot(spdf)

# I need to 'fortify' the data to be able to show it with ggplot2 (we need a data frame format)
library(broom)
spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "google_name")
spdf_fortified <- spdf_fortified%>%filter(!id %in% c("Alaska","Hawaii"))
# Calculate the centroid of each hexagon to add the label:
library(rgeos)
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

# Now I can plot this shape easily as described before:
ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="skyblue", color="white") +
  geom_text(data=centers, aes(x=x, y=y, label=id)) +
  theme_void() +
  coord_map()

# Merge geospatial and numerical information
spdf_fortified <- spdf_fortified %>%
  left_join(. , states, by=c("id"="NAME")) 

# Make a first chloropleth map
ggplot() +
  geom_polygon(data = spdf_fortified, aes(fill =  Join_Count, x = long, y = lat, group = group)) +
  scale_fill_gradient(trans = "log") +
  theme_void() +
  coord_map()

##hexbin map
# Prepare binning
spdf_fortified$Join_Count[is.na(spdf_fortified$Join_Count)] <- 0
spdf_fortified$bin <- cut(spdf_fortified$Join_Count , breaks=c(0,1,5,10,50,100,200, Inf), labels=c("0-1", "1-5","5-10","10-50","50-100", "100-200","200+" ), include.lowest = TRUE )

# Prepare a color scale coming from the viridis color palette
library(viridis)
my_palette <- rev(magma(8))[c(-1,-9)]

# plot
png("./outputs/hexbin.png", width = 5, height = 5, units="in",res = 1200)
ggplot() +
  geom_polygon(data = spdf_fortified, aes(fill = bin, x = long, y = lat, group = group) , size=0, alpha=0.9) +
  geom_text(data=centers, aes(x=x, y=y, label=id), color="white", size=3, alpha=0.6) +
  theme_void() +
  scale_fill_manual( 
    values=my_palette, 
    name="Total large wildfire incidents, by state (2011-2020)", 
    guide = guide_legend( keyheight = unit(2.5, units = "mm"), keywidth=unit(8, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) 
  ) +
  ggtitle( "" ) +
  theme(
    legend.position = c(0.5, 0.95),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 17, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  )
dev.off()

rm(spdf,spdf_fortified,my_palette,centers)
