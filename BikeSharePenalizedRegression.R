# Bike Share Penalized Regression Test

library(tidyverse)

library(tidymodels)

library(vroom)

library(patchwork)

library(ggplot2)

library(glmnet)

# Load In Data

bike_data <- vroom("C:\\Users\\mitch\\OneDrive\\Documents\\GitHub\\KaggleBikeShare\\train.csv") %>% select(-casual, -registered) %>%
  mutate(count_log = log1p(count)) %>%
  select(-count)

test_data <- vroom("C:\\Users\\mitch\\OneDrive\\Documents\\GitHub\\KaggleBikeShare\\test.csv")


# Recipe
my_recipe <- recipe(count_log ~ ., data = bike_data) %>%
  step_mutate(weather = ifelse(weather == 4, 3, weather),
              weekend = ifelse(wday(datetime) %in% c(1,7), 1, 0),
              rush_hour = ifelse(hour(datetime) %in% c(7:9,16:19), 1, 0)) %>%
  step_date(datetime, features = c("month", "year", "dow")) %>%
  step_time(datetime, features = "hour") %>%
  step_rm(datetime) %>%
  step_interact(terms = ~ temp:humidity + temp:windspeed + season:humidity) %>%
  step_poly(temp, humidity, degree = 2) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

# Model
preg_model <- linear_reg(penalty = .25, mixture = .25) %>%
  set_engine("glmnet")

# Workflow
preg_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(preg_model) %>%
  fit(data = bike_data)

# Predict on test set
bike_predictions <- predict(preg_wf, new_data = test_data) %>%
  mutate(.pred = exp(.pred) - 1,
         .pred = pmax(0, .pred))

# Kaggle submission
kaggle_submission <- test_data %>%
  select(datetime) %>%
  bind_cols(bike_predictions) %>%
  rename(count = .pred) %>%
  mutate(datetime = as.character(format(datetime)))

vroom_write(kaggle_submission, "./25_25PenalizedLinearPreds.csv", delim = ",")
