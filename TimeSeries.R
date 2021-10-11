
# LIBRARIES ----
library(modeltime)
library(tidymodels)
library(tidyverse)
library(timetk)
library(lubridate)
pacman::p_load(lubridate)

# DATA ----
data <- read_csv("~/Desktop/RITHM/Research/USDJPY.csv")
data$DATE <- seq.Date(from = as.Date("1974-01-01"), length.out = nrow(data), by = 31)

#Drop data from 1975 to 1985 as there was extream trends that the  differ greatly from current trends
N <- 181
data = data[-(1:N), , drop = FALSE]

#Plot out time series
data %>% plot_time_series(DATE, EXJPUS)


# TRAIN / TEST SPLITS ----

splits <- time_series_split(
  data,
  assess     = "50 months",
  cumulative = TRUE
)

#plot train and testing sets
splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(DATE, EXJPUS)

# FORECAST ----

#Using three diffrent models to compare


# * AUTO ARIMA ----
model_arima <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(EXJPUS ~ DATE, training(splits))
model_arima

# * Prophet ----
model_prophet <- prophet_reg(
  seasonality_yearly = TRUE) %>%
  set_engine("prophet") %>%
  fit(EXJPUS ~ DATE, training(splits))
model_prophet

# * Machine Learning - GLM ----
model_glmnet <- linear_reg(penalty = 0.01) %>%
  set_engine("glmnet") %>%
  fit(
    EXJPUS ~ wday(DATE, label = TRUE)
    + month(DATE, label = TRUE)
    + as.numeric(DATE),
    training(splits)
  )
model_glmnet


# MODELTIME COMPARE ----

#Putting all models in table for easy comparison and forcast
# * Modeltime Table ----
model_tbl <- modeltime_table(
  model_arima,
  model_prophet,
  model_glmnet
)

# * Calibrate ----
calib_tbl <- model_tbl %>%
  modeltime_calibrate(testing(splits))

# * Accuracy ----
calib_tbl %>% modeltime_accuracy()

# * Test Set Visualization ----
calib_tbl %>%
  modeltime_forecast(
    new_data   = testing(splits),
    actual_data = data
  ) %>%
  plot_modeltime_forecast()


#Pradict one time step ahead

# * Forecast Future ----
future_forecast_tbl <- calib_tbl %>%
  modeltime_refit(data) %>%
  modeltime_forecast(
    h = "1 months",
    actual_data = data)
tail(future_forecast_tbl)





