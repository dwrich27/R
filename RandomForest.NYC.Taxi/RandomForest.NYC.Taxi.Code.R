library(tidyverse)
library(ggmap)
library(viridis)
library(tree)
library(lubridate)
library(randomForest)


taxi <- read_csv("taxi.csv")

head(taxi)


taxi <- taxi %>%
    rename(lat = pickup_latitude, long = pickup_longitude)%>%
    filter(fare_amount > 0 | tip_amount > 0)%>%
    mutate(total = log(fare_amount + tip_amount))
    
taxi <- taxi  %>% 
    filter(between(lat, 40.70, 40.83) & between(long, -74.025, -73.93))
    
 manhattan <- readRDS("manhattan.rds")
 
ggmap(manhattan, darken = 0.5) +
   scale_fill_viridis(option = 'plasma') +
   geom_bin2d(data = taxi, aes(x = long, y = lat), bins = 60, alpha = 0.6)+
   labs(x = 'Longitude', y = 'Latitude', fill = 'Journeys')
   
fitted_tree <- tree(total~lat+long, taxi)

plot(fitted_tree)
text(fitted_tree)

taxi <- taxi %>% 
    mutate(hour = hour(pickup_datetime), 
           wday = wday(pickup_datetime, label = TRUE),
           month = month(pickup_datetime, label = TRUE))
           
fitted_tree <- tree(total~lat+long+hour+wday+month, taxi)


plot(fitted_tree)
text(fitted_tree)
summary(fitted_tree)

fitted_forest <- randomForest(total~lat+long+hour+wday+month, taxi, ntree = 80, sampsize = 10000)

fitted_forest

taxi$pred_total <- fitted_forest$predicted

ggmap(manhattan, darken = 0.5) +
   scale_fill_viridis(option = 'plasma') +
   stat_summary_2d(data = taxi, aes(x = long, y = lat, z = pred_total), bins = 60, alpha = 0.6, fun = mean)+
   labs(x = 'Longitude', y = 'Latitude', fill = 'Predicted')
   
mean_if_enough_data <- function(x) { 
    ifelse( length(x) >= 15, mean(x), NA) 
}

ggmap(manhattan, darken = 0.5) +
   scale_fill_viridis(option = 'plasma') +
   stat_summary_2d(data = taxi, aes(x = long, y = lat, z = total), bins = 60, alpha = 0.6, fun = mean_if_enough_data)+
   labs(x = 'Longitude', y = 'Latitude', fill = 'Predicted')
   
