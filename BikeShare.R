library(tidyverse)

library(tidymodels)

library(vroom)

library(patchwork)

library(ggplot2)

bike_data <- vroom("C:\\Users\\mitch\\OneDrive\\Documents\\GitHub\\KaggleBikeShare\\train.csv")

dplyr::glimpse(bike_data) 

skimr::skim(bike_data) 

DataExplorer::plot_intro(bike_data)

DataExplorer::plot_correlation(bike_data)

DataExplorer::plot_bar(bike_data)

DataExplorer::plot_histrograms(bike_data)

DataExplorer::plot_missing(bike_data)

GGally::ggpairs(bike_data)

bike_data$weather <- as.factor(bike_data$weather)
bike_data$season <- as.factor(bike_data$season)

# Are there differences in Bike Shares during different types of weather?
weather_bar <- ggplot(data=bike_data, aes(x=weather)) +
  geom_bar()

# Are there differences in the numbers of non-registered users on days with different temperatures?
casual_temp <- ggplot(data = bike_data, aes(x = temp, y = casual)) +
  geom_point() +
  geom_smooth()
  
# Are there differences in Bike Shares at different wind speeds?
windspeed_hist <- ggplot(data = bike_data, aes(x = windspeed)) +
  geom_histogram()

# Are there differences in Bike Shares during days with different temperatures?
temp_density <- ggplot(data = bike_data, aes(x = temp)) +
  geom_density()


(weather_bar + casual_temp) / (windspeed_hist + temp_density)
