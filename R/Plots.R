library(ggplot2)
library(tidyverse)
## Wildfires total Land cover
wildfires_lc <- read.csv("./inputs/wildfires_total_lc.txt")
wildfires_lc <- wildfires_lc[,4:19]
wildfires_lc <- data.frame(t(wildfires_lc))
row.names(wildfires_lc) <- 1:16
names(wildfires_lc) <- "Area"
wildfires_lc$Land_cover <- c("Open Water","Perennial Ice/Snow","Developed, Open Space", "Developed, Low Intensity", "Developed, Medium Intensity", "Developed, High Intensity",
                             "Barren Land","Deciduous Forest" ,"Evergreen Forest" ,"Mixed Forest","Shrub/Scrub","Grassland/Herbaceous","Pasture/Hay",
                             "Cultivated Crops", "Woody Wetlands", "Emergent Herbaceous Wetlands")
wildfires_lc$Area <- wildfires_lc$Area*(3.861*10^-7)

wildfires_lc$Area[3] <- sum(wildfires_lc$Area[3:6])
wildfires_lc <- wildfires_lc[-c(4:6),]
wildfires_lc <- wildfires_lc[-c(1:2),]
row.names(wildfires_lc) <- 1:11
wildfires_lc$Land_cover[1] <- "Developed" 

png("./outputs/lc_total.png", width = 5, height = 4, units="in",res = 600)
ggplot(data = wildfires_lc)+
  geom_bar(aes(x = reorder(Land_cover,Area),y = Area ), stat = "identity")+
  #scale_y_continuous(trans = "sqrt")+
  coord_flip()+
  theme(axis.text.x = element_text(angle = 45,vjust = 0.7))+
  xlab("Land Cover (NLCD)")+
  ylab("Burn Area, square miles (2011-2020)")+
  theme_bw()+
  theme(text = element_text(size = 10,color = "black"))
dev.off()


##Wildfire causes
wildfire_cause <- read.csv("./inputs/wildfires_cause.txt")
wildfire_cause <- data.frame(table(wildfire_cause$STAT_CAU_1))
wildfire_cause$Freq <- wildfire_cause$Freq*100/sum(wildfire_cause$Freq)

library(treemap)

png("./outputs/cause.png", width = 5, height = 4, units="in",res = 600)
ggplot(wildfire_cause, aes(x=Var1, y=Freq)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=Var1, 
                   xend=Var1, 
                   y=0, 
                   yend=Freq)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle=65, vjust=0.6))+
  ylab("% Large wildfires (2011 - 2015)")+
  xlab("Cause")+
  theme(text = element_text(size = 10,color = "black"))
dev.off()  

##corr plot
library(corrplot)
m <- cor(XY[,2:18])

png("./outputs/corr.png", width = 5, height = 5, units="in",res = 600)
corrplot(m, method = "square",order = "FPC", type = "lower", diag = FALSE,tl.srt = 45)
dev.off() 

#Monthly wildfires
df <- data.frame(table(wildfires$month))
df$Freq <- round(df$Freq/5,0)
df$TAVG <- c(4.6,5.5,8.6,13.5,18.3,21.9,25.8,26.4,23.1,17.9,12.3,6.9)
ylab2 <- "Temperature (°F)"
library(weathermetrics)
df$TAVG <- celsius.to.fahrenheit(df$TAVG)
df$Month <- month.name
df$Month <- factor(df$Month, levels = month.name)

png("./outputs/temperature.png", width = 6, height = 4.2, units="in",res = 1200)
ggplot(df)+
  geom_bar(aes(x = Month,Freq), stat = "identity")+
  geom_line(aes(x = Month,y= TAVG*3,group = 1),col = "red",size = 1)+
  geom_point(aes(x = Month,y= TAVG*3,group = 1),col = "red")+
  scale_y_continuous(sec.axis = sec_axis(~ . /3, name = ylab2))+
  xlab("Month")+
  ylab("Average monthly large wildfire incidents (2011 - 2020)")+
  theme_bw()+
  theme(text = element_text(size = 10))+
  theme( axis.line.y.right = element_line(color = "red"),
         axis.ticks.y.right = element_line(color = "red"),
         axis.title.y.right = element_text(color = "red"))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7))
dev.off()

rm(df,ylab2)
