
modis <-data.frame()
modis <- modis%>%rbind(read.csv("./inputs/modis/MODIS1.csv"))
modis <- modis%>%rbind(read.csv("./inputs/modis/MODIS2.csv"))
modis <- modis%>%rbind(read.csv("./inputs/modis/MODIS3.csv"))
modis <- modis%>%rbind(read.csv("./inputs/modis/MODIS4.csv"))
modis <- modis%>%rbind(read.csv("./inputs/modis/MODIS5.csv"))

modis <- modis[,c(1,4,8,9)]
names(modis)[3:4] <- c("EVI","NDVI")
modis$Date <- as.Date(modis$Date)
modis$year <- year(modis$Date)
modis$month <- month(modis$Date)
modis <- modis[,-2]

wildfires <- wildfires%>%left_join(modis, by = c("FIRE_ID" = "ID", "month" = "month", "year" = "year"))
