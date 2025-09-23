# Bike Share Analysis Using Random Forests
library(ranger)

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


## Random Forest
my_mod <- rand_forest(mtry = tune(),
                      min_n=tune(),
                      trees= 500) %>% #Type of model
  set_engine("ranger") %>% # What R function to use
  set_mode("regression")

## Set Workflow
preg_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_mod)

## Grid of values to tune over
grid_of_tuning_params <- grid_regular(mtry(range = c(1, 15)),
                                      min_n(),
                                      levels = 3)

## Split data for CV
folds <- vfold_cv(bike_data, v = 5, repeats=1)

## Run the CV
CV_results <- preg_wf %>%
  tune_grid(resamples=folds,
            grid=grid_of_tuning_params,
            metrics=metric_set(rmse, mae)) #Or leave metrics NULL

## Plot Results (example)
collect_metrics(CV_results) %>% # Gathers metrics into DF
  filter(.metric=="rmse") %>%
  ggplot(data=., aes(x=penalty, y=mean, color=factor(mixture))) +
  geom_line()

## Find Best Tuning Parameters
bestTune <- CV_results %>%
  select_best(metric="rmse")

# Finalize the Workflow & fit it
final_wf <-
  preg_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data=bike_data)

# Predict on test set
bike_predictions <- predict(final_wf, new_data = test_data) %>%
  mutate(.pred = exp(.pred) - 1,
         .pred = pmax(0, .pred))

# Kaggle submission
kaggle_submission <- test_data %>%
  select(datetime) %>%
  bind_cols(bike_predictions) %>%
  rename(count = .pred) %>%
  mutate(datetime = as.character(format(datetime)))

vroom_write(kaggle_submission, "./Forest_LinearPreds2.csv", delim = ",")

