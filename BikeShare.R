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



bike_data <- bike_data %>% select(-casual, -registered)

test_data <- vroom("C:\\Users\\mitch\\OneDrive\\Documents\\GitHub\\KaggleBikeShare\\test.csv")

test_data$weather <- as.factor(test_data$weather)
test_data$season <- as.factor(test_data$season)

my_linear_model <- linear_reg() %>% #Type of mode
  set_engine("lm") %>% # Engine = What R function to use
  set_mode("regression") %>% # Regression just means quantitative response
  fit(log(bike_data$count)~.-datetime, data=bike_data)

## Generate Predictions Using Linear Model
bike_predictions <- predict(my_linear_model,
                            new_data=test_data) # Use fit to predict
bike_predictions ## Look at the output

## Format the Predictions for Submission to Kaggle
kaggle_submission <- bike_predictions %>%
  bind_cols(., test_data) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count = exp(count)) %>% # back-transform
  mutate(count=pmax(0,count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write out the file
vroom_write(x=kaggle_submission, file="./LinearPreds.csv", delim=",")
