# Bike Share Workflow Practice

library(tidyverse)

library(tidymodels)

library(vroom)

library(patchwork)

library(ggplot2)

# Load In Data

bike_data <- vroom("C:\\Users\\mitch\\OneDrive\\Documents\\GitHub\\KaggleBikeShare\\train.csv")
test_data <- vroom("C:\\Users\\mitch\\OneDrive\\Documents\\GitHub\\KaggleBikeShare\\test.csv")

# Clean Data
bike_data <- bike_data %>% select(-casual,-registered)

# Feature Engineering
my_recipe <- recipe(count ~ ., data = bike_data) %>%
  step_mutate(weather = ifelse(weather == 4, 3, weather))%>%
  step_time(datetime, features = "hour") %>%
  step_date(datetime, features = "dow") %>%
  step_mutate(seasonal_humid = season * humidity) %>%
  step_rm(any_of(c("casual", "registered")))

# Make linear model
my_linear_model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

# Workflow
my_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_linear_model)

# Fit model on training data
lm_fit <- my_workflow %>% fit(data = bike_data)

# Predict on test set
bike_predictions <- predict(lm_fit, new_data = test_data) %>%
  mutate(.pred = pmax(0, .pred))    # enforce non-negative

# Format for Kaggle
kaggle_submission <- test_data %>%
  bind_cols(bike_predictions) %>%
  select(datetime, .pred) %>%
  rename(count = .pred) %>%
  mutate(datetime = as.character(format(datetime))) 

# Write out the file
vroom_write(kaggle_submission, "./MoreLinearPreds.csv", delim = ",")



