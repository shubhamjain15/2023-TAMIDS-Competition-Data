library(climateR)
library(AOI)
library(tidyterra)
library(ggplot2)
library(terra)
library(tidyr)
library(sf)

#10 DAY PALMER DROUGHT SEVERITY INDEX GRIDMET

pdsi_df <- data.frame()

sites <- wildfires%>%dplyr::select(c("FIRE_ID","LATITUDE","LONGITUDE","IG_DATE"))
sites <- st_as_sf(wildfires,coords = c("LONGITUDE","LATITUDE"))
st_crs(sites) <- 4326

for(i in as.character(unique(wildfires$IG_DATE))){
  df <- getGridMET(aoi_get(country = "US"),varname = "pdsi",startDate = i)
  sites_wide = extract_sites(r = df, pts = sites, id = "FIRE_ID")
  df2 <- data.frame((sites_wide[[2]][1, 1:nrow(wildfires)]))
  df2 <- df2%>%pivot_longer(cols = !date, names_to = "FIRE_ID",values_to = "PDSI")
  df2$date <- as.Date(i)     # it returns the date for which 10 day data is available so change to our date so its easy to join
  pdsi_df <- rbind(pdsi_df,df2)
}

#write.csv(pdsi_df,"./outputs/pdsi_df.csv")
pdsi_df <- read.csv("./outputs/pdsi_df.csv")
pdsi_df <- pdsi_df[,-1]
pdsi_df$date <- as.Date(pdsi_df$date)
wildfires <- wildfires%>%left_join(pdsi_df, by = c("FIRE_ID" = "FIRE_ID", "IG_DATE" = "date"))
rm(pdsi_df)

##PET
#look at a plot
ggplot() +
  geom_spatraster(data = rast(df))+
  geom_sf(data = sites)

rm(df,df2,pdsi_df,sites,sites_wide,i)


#pet
dates <- as.character(unique(wildfires$IG_DATE))
dates <- dates[!dates %in% as.character(pet_df$date)]
pet_df <- data.frame()
for(i in dates){
  df <- getGridMET(aoi_get(country = "US"),varname = "pet",startDate = i)
  sites_wide = extract_sites(r = df, pts = sites, id = "FIRE_ID")
  df2 <- data.frame((sites_wide[[1]][1, 1:nrow(wildfires)]))
  df2 <- df2%>%pivot_longer(cols = !date, names_to = "FIRE_ID",values_to = "PET")
  df2$date <- as.Date(i)     # it returns the date for which 10 day data is available so change to our date so its easy to join
  pet_df <- rbind(pet_df,df2)
  print(i)
}

pet_df <- pet_df[!duplicated(pet_df),]

#write.csv(pet_df,"./outputs/pet_df.csv")
wildfires <- wildfires%>%left_join(pet_df, by = c("FIRE_ID" = "FIRE_ID", "IG_DATE" = "date"))

rm(df,df2,pet_df,sites,sites_wide,i,dates)

