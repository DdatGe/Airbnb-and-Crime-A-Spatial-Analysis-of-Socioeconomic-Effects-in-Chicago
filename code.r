library(spdep)
library(nimble)
library(sf)
library(tidyverse)
library(nimble)
library(mapview)
library(spatialreg)
library(car)
library(tmap)
library(ggplot2)

setwd("C:/Users/kimsuna/Documents/Rdata/spatial/airbnb_Chicago/airbnb_Chicago")
#setwd("C:/Users/SUNA/Documents/R/spatstat/airbnb_Chicago")

#preprocessing
data<-sf::st_read("airbnb_Chicago_2015.shp")
data=data[!is.na(data$price_pp),]
data$rate=data$num_theft/data$num_crimes

#visualize the data
mapview(data, zcol="price_pp")
mapview(data, zcol="num_spots")
mapview(data,zcol="poverty")
mapview(data,zcol="income_pc")


mapview(data, zcol="num_theft")
mapview(data, zcol="num_crimes")
mapview(data, zcol="rate")



#moran test
data_nb <-spdep::poly2nb(data, queen=TRUE)
dataw <-nb2listw(data_nb, style= "W")
(moran <-moran.test(data$num_theft,dataw, alternative = "greater"))
(moranMC <-moran.mc(data$num_theft,dataw, nsim=999))
(moran <-moran.test(data$num_crimes,dataw, alternative = "greater"))
(moranMC <-moran.mc(data$num_crimes,dataw, nsim=999))
(moran <-moran.test(data$rate,dataw, alternative = "greater"))
(moranMC <-moran.mc(data$rate,dataw, nsim=999))

#theft modeling
datab <- spdep::nb2listw(data_nb, style = "B")
car1 <- spautolm(num_theft~num_spots, data=data,
                 family='CAR',listw=datab)
car2 <- spautolm(num_theft~price_pp, data=data,
                 family='CAR',listw=datab)
car3 <- spautolm(num_theft~num_spots*price_pp, data=data,
                 family='CAR',listw=datab)
car4 <- spautolm(num_theft~num_spots*price_pp+income_pc, data=data,
                 family='CAR',listw=datab)
car5 <- spautolm(num_theft~num_spots*price_pp+poverty, data=data,
                 family='CAR',listw=datab)
car6 <- spautolm(num_theft~num_spots*price_pp+poverty*income_pc, data=data,
                 family='CAR',listw=datab)  

sar1 <- spautolm(num_theft~num_spots, data=data,
                 family='SAR',listw=datab)
sar2 <- spautolm(num_theft~price_pp, data=data,
                 family='SAR',listw=datab)
sar3 <- spautolm(num_theft~num_spots*price_pp, data=data,
                 family='SAR',listw=datab)
sar4 <- spautolm(num_theft~num_spots*price_pp+income_pc, data=data,
                 family='SAR',listw=datab)
sar5 <- spautolm(num_theft~num_spots*price_pp+poverty, data=data,
                 family='SAR',listw=datab)
sar6 <- spautolm(num_theft~num_spots*price_pp+poverty*income_pc, data=data,
                 family='SAR',listw=datab) 
cbind(AIC(car1,car2,car3,car4,car5,car6,sar1,sar2,sar3,sar4,sar5,sar6),BIC(car1,car2,car3,car4,car5,car6,sar1,sar2,sar3,sar4,sar5,sar6))

theft_model=sar6
summary(theft_model)

#crime modeling
car1 <- spautolm(num_crimes~num_spots, data=data,
                  family='CAR',listw=datab)
car2 <- spautolm(num_crimes~price_pp, data=data,
                 family='CAR',listw=datab)
car3 <- spautolm(num_crimes~num_spots*price_pp, data=data,
                 family='CAR',listw=datab)
car4 <- spautolm(num_crimes~num_spots*price_pp+income_pc, data=data,
                 family='CAR',listw=datab)
car5 <- spautolm(num_crimes~num_spots*price_pp+poverty, data=data,
                 family='CAR',listw=datab)
car6 <- spautolm(num_crimes~num_spots*price_pp+poverty*income_pc, data=data,
                 family='CAR',listw=datab)  


sar1 <- spautolm(num_crimes~num_spots, data=data,
                 family='SAR',listw=datab)
sar2 <- spautolm(num_crimes~price_pp, data=data,
                 family='SAR',listw=datab)
sar3 <- spautolm(num_crimes~num_spots*price_pp, data=data,
                 family='SAR',listw=datab)

sar4 <- spautolm(num_crimes~num_spots*price_pp+income_pc, data=data,
                 family='SAR',listw=datab)

sar5 <- spautolm(num_crimes~num_spots*price_pp+poverty, data=data,
                 family='SAR',listw=datab) ##얘가 나음음

sar6 <- spautolm(num_crimes~num_spots*price_pp+poverty*income_pc, data=data,
                 family='SAR',listw=datab)
cbind(AIC(car1,car2,car3,car4,car5,car6,sar1,sar2,sar3,sar4,sar5,sar6),BIC(car1,car2,car3,car4,car5,car6,sar1,sar2,sar3,sar4,sar5,sar6))
crime_model=sar5

#rate modeling
car1 <- spautolm(rate~num_spots, data=data,
                 family='CAR',listw=datab)
car2 <- spautolm(rate~price_pp, data=data,
                 family='CAR',listw=datab)
car3 <- spautolm(rate~num_spots*price_pp, data=data,
                 family='CAR',listw=datab)
car4 <- spautolm(rate~num_spots*price_pp+income_pc, data=data,
                 family='CAR',listw=datab)
car5 <- spautolm(rate~num_spots*price_pp+poverty, data=data,
                 family='CAR',listw=datab)
car6 <- spautolm(rate~num_spots*price_pp+poverty*income_pc, data=data,
                 family='CAR',listw=datab)  


sar1 <- spautolm(rate~num_spots, data=data,
                 family='SAR',listw=datab)
sar2 <- spautolm(rate~price_pp, data=data,
                 family='SAR',listw=datab)
sar3 <- spautolm(rate~num_spots*price_pp, data=data,
                 family='SAR',listw=datab)

sar4 <- spautolm(rate~num_spots*price_pp+income_pc, data=data,
                 family='SAR',listw=datab)
sar5 <- spautolm(rate~num_spots*price_pp+poverty, data=data,
                 family='SAR',listw=datab)
sar6 <- spautolm(rate~num_spots*price_pp+poverty*income_pc, data=data,
                 family='SAR',listw=datab) 

cbind(AIC(car1,car2,car3,car4,car5,car6,sar1,sar2,sar3,sar4,sar5,sar6),BIC(car1,car2,car3,car4,car5,car6,sar1,sar2,sar3,sar4,sar5,sar6))
rate_model=sar6


#residual normality
model_list <- list(theft_model = theft_model, 
                   crime_model = crime_model, 
                   rate_model = rate_model)

par(mfrow = c(1, 2))  
par(pty = "s")        

for (model_name in names(model_list)) {
  model <- model_list[[model_name]]
  hist(model$fit$residuals, 
       main = paste("Histogram of", model_name), 
       xlab = "Residuals")
  qqnorm(model$fit$residuals, 
         main = paste("QQ Plot of", model_name))
  qqline(model$fit$residuals)
}



#outlier
par(mfrow=c(1,2)) 

for (model_name in names(model_list)) {
  model <- model_list[[model_name]]
  plot(model$fit$fitted.values, model$fit$residuals,main=paste("Outlier of", model_name),xlab="fitted.values",ylab="residuals") 
  abline(h=0) 
  Boxplot(model$fit$residuals, id.method="y",ylab="residuals")
}

boxplot.stats(theft_model$fit$residuals)$out
boxplot.stats(crime_model$fit$residuals)$out
boxplot.stats(rate_model$fit$residuals)$out



# Moran's I test
moran.test(theft_model$fit$residuals, dataw)
moran.test(crime_model$fit$residuals, dataw)
moran.test(rate_model$fit$residuals, dataw)

#Visualization
#theft model
fitted_values <- fitted(theft_model)
predicted_values <- fitted_values + residuals(theft_model)

data$fitted_values <- fitted_values
data$predicted_values <- predicted_values

tmap_mode("plot")

actual_map <- tm_shape(data) +
  tm_polygons("num_theft", title = "Actual Theft Rates", palette = "Blues") +
  tm_layout(title = "Actual Theft Rates", legend.outside = TRUE)

fitted_map <- tm_shape(data) +
  tm_polygons("fitted_values", title = "Fitted Theft Rates", palette = "Greens") +
  tm_layout(title = "Fitted Theft Rates", legend.outside = TRUE)

predicted_map <- tm_shape(data) +
  tm_polygons("predicted_values", title = "Predicted Theft Rates", palette = "Reds") +
  tm_layout(title = "Predicted Theft Rates", legend.outside = TRUE)

tmap_arrange(actual_map, fitted_map, predicted_map)


# crime model
fitted_values <- fitted(crime_model)
predicted_values <- fitted_values + residuals(crime_model)

data$fitted_values <- fitted_values
data$predicted_values <- predicted_values

tmap_mode("plot")

actual_map <- tm_shape(data) +
  tm_polygons("num_theft", title = "Actual Theft Rates", palette = "Blues") +
  tm_layout(title = "Actual Theft Rates", legend.outside = TRUE)

fitted_map <- tm_shape(data) +
  tm_polygons("fitted_values", title = "Fitted Theft Rates", palette = "Greens") +
  tm_layout(title = "Fitted Theft Rates", legend.outside = TRUE)

predicted_map <- tm_shape(data) +
  tm_polygons("predicted_values", title = "Predicted Theft Rates", palette = "Reds") +
  tm_layout(title = "Predicted Theft Rates", legend.outside = TRUE)

tmap_arrange(actual_map, fitted_map, predicted_map)


# rate model
fitted_values <- fitted(rate_model)
predicted_values <- fitted_values + residuals(rate_model)

data$fitted_values <- fitted_values
data$predicted_values <- predicted_values

tmap_mode("plot")

actual_map <- tm_shape(data) +
  tm_polygons("num_theft", title = "Actual Theft Rates", palette = "Blues") +
  tm_layout(title = "Actual Theft Rates", legend.outside = TRUE)

fitted_map <- tm_shape(data) +
  tm_polygons("fitted_values", title = "Fitted Theft Rates", palette = "Greens") +
  tm_layout(title = "Fitted Theft Rates", legend.outside = TRUE)

predicted_map <- tm_shape(data) +
  tm_polygons("predicted_values", title = "Predicted Theft Rates", palette = "Reds") +
  tm_layout(title = "Predicted Theft Rates", legend.outside = TRUE)

tmap_arrange(actual_map, fitted_map, predicted_map)
