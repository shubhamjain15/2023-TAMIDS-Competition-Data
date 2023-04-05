library(tidyverse) 
library(prism)     
library(raster)    
library(stringr)
library(magrittr)

#dir.create("./prismtmp")
options(prism.path = './prismtmp')

get_prism_monthlys(type = 'tmean', years = 2011:2020, mo = 1:12, keepZip = F)
get_prism_monthlys(type = 'ppt', years = 2011:2020, mo = 1:12, keepZip = F)
get_prism_monthlys(type = 'vpdmin', years = 2011:2020, mo = 1:12, keepZip = F)
get_prism_monthlys(type = 'vpdmax', years = 2011:2020, mo = 1:12, keepZip = F)

prism_fn <- function(wildfires){
climate_data <- prism_archive_ls() %>%  
  pd_stack(.)  

climate_crs <- climate_data@crs@projargs

sites <- wildfires%>%dplyr::select(c("FIRE_ID","LATITUDE","LONGITUDE","IG_DATE"))

coordinates(sites) <- c('LONGITUDE', 'LATITUDE')
proj4string(sites) <- CRS(climate_crs)

climate_sites <- data.frame(coordinates(sites), 
                           sites$FIRE_ID, 
                           extract(climate_data, sites))


climate_sites <- climate_sites %>% 
  gather(date, value, 4:ncol(climate_sites))

climate_sites$date <- gsub('PRISM_', '', climate_sites$date) %>% 
  gsub('stable_4kmM3_', '', .) %>% 
  gsub('stable_4kmM2_', '', .) %>%
  gsub('_bil', '', .)


climate_sites <- separate(climate_sites, 'date', 
                         into = c('type', 'YearMonth'), 
                         sep = '_')
climate_sites <- separate(climate_sites, 'YearMonth',
                         into = c('year', 'month'),
                         sep = 4)

climate_sites <- unique(climate_sites)
climate_sites <- climate_sites %>% 
  spread(type, value) %>%
  rename(lng = LONGITUDE, lat = LATITUDE, FIRE_ID = sites.FIRE_ID)


climate_sites$year  <- as.numeric(climate_sites$year)

climate_sites <- climate_sites[order(climate_sites$FIRE_ID),]
climate_sites$month <- as.numeric((climate_sites$month))
wildfires <- wildfires%>%left_join(climate_sites[,-c(1:2)], by = c("FIRE_ID" = "FIRE_ID", "year" = "year","month" = "month"))
return(wildfires)
}
