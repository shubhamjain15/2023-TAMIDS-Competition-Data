remotes::install_github("mikejohnson51/AOI")
remotes::install_github("mikejohnson51/climateR")
library(tidyverse)
library(ggplot2)
library(climateR)
library(AOI)
library(tidyterra)
library(ggplot2)
library(terra)
library(tidyr)
library(sf)
#Load wildfire data for 2011 to 2020. Large wildfires in contiguous US
wildfires <- read.csv("inputs/wf_2011_2022.txt")
wildfires <- wildfires[,-1]

#Format IG_DATE
wildfires$IG_DATE <- mdy(word(wildfires$IG_DATE,1))
wildfires$year <- year(wildfires$IG_DATE)
wildfires$month <- month(wildfires$IG_DATE)

#get prism data
#source("PRISM_download.R")
wildfires <- prism_fn(wildfires)

##gridmet PDSI, PET
#source("gridMET.R")

##NLCD
nlcd <- read.csv("./inputs/nlcd_wf.csv")
wildfires <- wildfires%>%left_join(nlcd,by = c("FIRE_ID"))
rm(nlcd)
##DOY
library(lubridate)
wildfires$DOY <- yday(wildfires$IG_DATE)

##MODIS
source("MODIS.R")

##Elevation
elev <- read.csv("./inputs/elevation.txt")
elev <- elev[,-1]
names(elev)[2] <- c("Elevation")  
wildfires <- wildfires%>%left_join(elev, by = c("FIRE_ID" = "FIRE_ID"))
rm(elev)

write.csv(wildfires,"wildfires_data.csv")
##XY
XY <- wildfires%>%dplyr::select("ACRES","LATITUDE","LONGITUDE","DOY","ppt","tmean","vpdmax","vpdmin","PDSI","Open_Water","Developed","Barren",
                         "Deciduous","Evergreen","Mixed","Shrub","grass","Pasture","Crops","Wetlands","NDVI","EVI","Elevation","US_L4CODE")
XY <- na.omit(XY)
XY$ACRES <- log(XY$ACRES)
XY$Forests <- rowSums(XY[,13:15])
XY <- XY[,-c(13:15)]
XY <- XY[-c(10,12,16)]
XY <- XY[,c(1:10,19,11:18)]
write.csv(XY,"./outputs/XY.csv")

XY_summary <- data.frame(Feature = names(XY[,2:18]), Min = as.numeric(apply(XY[,2:18],2,min)), Max = as.numeric(apply(XY[,2:18],2,max)))
write.csv(XY_summary,"./outputs/XY_summary.csv")

#Plots
#a) Plots.R
#b) incidents_per_state.R
#c) SHAP.R

