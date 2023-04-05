shap_data <- read.csv("./inputs/shap_data.csv")
shap_data <- shap_data[,-1]
names(shap_data) <- names(XY)[2:18] 

shap_imp <- data.frame(colMeans(abs(shap_data)))
names(shap_imp) <- "Importance"
shap_imp$Feature <- rownames(shap_imp)
row.names(shap_imp) <- 1:nrow(shap_imp)

png("./outputs/shap_imp.png", width = 5, height = 4, units="in",res = 600)
ggplot(data = shap_imp)+
  geom_bar(aes(x = reorder(Feature,Importance),y = Importance), stat = "identity")+
  #scale_y_continuous(trans = "sqrt")+
  coord_flip()+
  theme(axis.text.x = element_text(angle = 45,vjust = 0.7))+
  xlab("Feature")+
  ylab("Importance (mean(abs(SHAP values)))")+
  theme_bw()+
  theme(text = element_text(size = 10,color = "black"))
dev.off()


plot(x = XY$Forests,y = shap_data$Forests)
plot(x = XY$grass,y = shap_data$grass)
plot(x = XY$LONGITUDE,y = shap_data$LONGITUDE)
plot(x = XY$tmean,y = shap_data$tmean)
plot(x = XY$PDSI,y = shap_data$PDSI)
plot(x = XY$DOY,y = shap_data$DOY)
plot(x = XY$NDVI,y = shap_data$NDVI)
plot(x = XY$Elevation,y = shap_data$Elevation)

plot1 <- ggplot()+
            geom_point(aes(x = XY$Forests, y= shap_data$Forests),size = 0.5)+
            theme_bw()+
            ylab("SHAP value for Forests")+
            xlab("% Forests")

plot1
plot2 <- ggplot()+
  geom_point(aes(x = XY$LONGITUDE, y= shap_data$LONGITUDE),size = 0.5)+
  theme_bw()+
  ylab("SHAP value for Longitude")+
  xlab("Longitude")


plot3 <- ggplot()+
  geom_point(aes(x = XY$NDVI, y= shap_data$NDVI),size = 0.5)+
  theme_bw()+
  ylab("SHAP value for NDVI")+
  xlab("NDVI")

plot4 <- ggplot()+
  geom_point(aes(x = XY$Elevation, y= shap_data$Elevation),size = 0.5)+
  theme_bw()+
  ylab("SHAP value for Elevation")+
  xlab("Elevation (meters)")

library(ggpubr)

png("./outputs/shap_individual.png", width = 8, height = 5, units="in",res = 600)
ggarrange(plot1,plot2,plot3,plot4, nrow = 2, ncol = 2)
dev.off()
