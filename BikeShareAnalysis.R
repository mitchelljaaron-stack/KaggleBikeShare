# Bike Share Analysis

library(tidyverse)

library(tidymodels)

library(vroom)

library(patchwork)

library(ggplot2)

### Linear Regression Modeling ###

bike_data <- vroom("C:\\Users\\mitch\\OneDrive\\Documents\\GitHub\\KaggleBikeShare\\train.csv")

bike_data$weather <- as.factor(bike_data$weather)
bike_data$season <- as.factor(bike_data$season)
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
