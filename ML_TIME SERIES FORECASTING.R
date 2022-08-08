#Load Packages
install.packages("modeltime")

library(timetk)   
library(lubridate)
library(tidyverse)



library(tidymodels)
library(modeltime)
library(tidyverse)
library(timetk)

# Used to convert plots from interactive to static
interactive = TRUE


# Read data (BIKESHARING DATASET)
bike_transactions_tbl <- bike_sharing_daily %>%
  select(dteday, cnt) %>%
  set_names(c("date", "value")) 

bike_transactions_tbl


#Visualize
bike_transactions_tbl %>%
  plot_time_series(date, value, .interactive = interactive)


#Train / Test
splits <- bike_transactions_tbl %>%
  time_series_split(assess = "3 months", cumulative = TRUE)

#Visualize the train/test split.
#tk_time_series_cv_plan(): Converts the splits object to a data frame
#plot_time_series_cv_plan(): Plots the time series sampling data using the “date” and “value” columns.

splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, value, .interactive = interactive)

#Modeling

#Recipe Preprocessing Specification
# Add time series signature
recipe_spec_timeseries <- recipe(value ~ ., data = training(splits)) %>%
  step_timeseries_signature(date) 

bake(prep(recipe_spec_timeseries), new_data = training(splits))


#Improve Model Behavior
recipe_spec_final <- recipe_spec_timeseries %>%
  step_fourier(date, period = 365, K = 5) %>%
  step_rm(date) %>%
  step_rm(contains("iso"), contains("minute"), contains("hour"),
          contains("am.pm"), contains("xts")) %>%
  step_normalize(contains("index.num"), date_year) %>%
  step_dummy(contains("lbl"), one_hot = TRUE) 

juice(prep(recipe_spec_final))

#Model Specification
model_spec_lm <- linear_reg(mode = "regression") %>%
  set_engine("lm")

#Workflow
workflow_lm <- workflow() %>%
  add_recipe(recipe_spec_final) %>%
  add_model(model_spec_lm)

workflow_lm

#Training
workflow_fit_lm <- workflow_lm %>% fit(data = training(splits))

#Hyperparameter Tuning
  #Elastic Net
  #XGBoost
  #Random Forest
  #Support Vector Machine (SVM)
  #K-Nearest Neighbors
  #Multivariate Adaptive Regression Spines (MARS)

#Model Time Table
model_table <- modeltime_table(
  workflow_fit_lm
) 

model_table

#Model Calibration
calibration_table <- model_table %>%
  modeltime_calibrate(testing(splits))

calibration_table

#Forecast (Testing Set)
  #With calibrated data, we can visualize the testing predictions (forecast).
  #Use modeltime_forecast() to generate the forecast data for the testing set as a tibble.
  #Use plot_modeltime_forecast() to visualize the results in interactive and static plot formats.

calibration_table %>%
  modeltime_forecast(actual_data = bike_transactions_tbl) %>%
  plot_modeltime_forecast(.interactive = interactive)


#Accuracy (Testing Set)
  #Use modeltime_accuracy() to generate the out-of-sample accuracy metrics as a tibble.
  #Use table_modeltime_accuracy() to generate interactive and static

calibration_table %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = interactive)

#Refit and Forecast Forward
calibration_table %>%
  modeltime_refit(bike_transactions_tbl) %>%
  modeltime_forecast(h = "12 months", actual_data = bike_transactions_tbl) %>%
  plot_modeltime_forecast(.interactive = interactive)





















































