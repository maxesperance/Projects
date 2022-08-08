library(tidyverse)
library(tidyquant) 
library(timetk)

FANG

#Stock Price Time series
FANG %>%
  group_by(symbol) %>%
  plot_time_series(date, adjusted, .facet_ncol = 2, .interactive = FALSE)

#Stock Volume Time Series
FANG %>%
  group_by(symbol) %>%
  plot_time_series(date, volume, .facet_ncol = 2, .interactive = FALSE)

#Summarize by Time / Period Summarization
FANG %>%
  group_by(symbol) %>%
  summarise_by_time(
    date, .by = "quarter",
    volume = SUM(volume)
  ) %>%
  plot_time_series(date, volume, .facet_ncol = 2, .interactive = FALSE, .y_intercept = 0)

#Period Smoothing: Get the first value in each month
FANG %>%
  group_by(symbol) %>%
  summarise_by_time(
    date, .by = "month",
    adjusted = FIRST(adjusted)
  ) %>%
  plot_time_series(date, adjusted, .facet_ncol = 2, .interactive = FALSE)


#Filter By Time / Time Range Filtering: Used to quickly filter a continuous time range.
FANG %>%
  group_by(symbol) %>%
  filter_by_time(date, "2013-09", "2013") %>%
  plot_time_series(date, adjusted, .facet_ncol = 2, .interactive = FALSE)


#Padding Data / Fill in Gaps: Used to fill in (pad) gaps and to go from from low frequency to high frequency. This function uses the awesome padr library for filling and expanding timestamps.

FANG %>%
  group_by(symbol) %>%
  pad_by_time(date, .by = "auto") # Guesses .by = "day"

#Low to High Frequency: Go from Daily to Hourly timestamp intervals for 1 month from the start date. Impute the missing values.

FANG %>%
  group_by(symbol) %>%
  pad_by_time(date, .by = "hour") %>%
  mutate_at(vars(open:adjusted), .funs = ts_impute_vec, period = 1) %>%
  filter_by_time(date, "start", FIRST(date) %+time% "1 month") %>%
  plot_time_series(date, adjusted, .facet_ncol = 2, .interactive = FALSE) 

#  Sliding (Rolling) Calculations:slidify() turns any function into a sliding (rolling) window function

#Rolling Mean
# Make the rolling function
roll_avg_30 <- slidify(.f = AVERAGE, .period = 30, .align = "center", .partial = TRUE)

# Apply the rolling function
FANG %>%
  select(symbol, date, adjusted) %>%
  group_by(symbol) %>%
  # Apply Sliding Function
  mutate(rolling_avg_30 = roll_avg_30(adjusted)) %>%
  pivot_longer(cols = c(adjusted, rolling_avg_30)) %>%
  plot_time_series(date, value, .color_var = name,
                   .facet_ncol = 2, .smooth = FALSE, 
                   .interactive = FALSE)

#For simple rolling calculations (rolling average), we can accomplish this operation faster with slidify_vec() - A vectorized rolling function for simple summary rolls (e.g. mean(), sd(), sum(), etc)
FANG %>%
  select(symbol, date, adjusted) %>%
  group_by(symbol) %>%
  # Apply roll apply Function
  mutate(rolling_avg_30 = slidify_vec(adjusted,  ~ AVERAGE(.), 
                                      .period = 30, .partial = TRUE))

#Rolling Regression
# Rolling regressions are easy to implement using `.unlist = FALSE`
lm_roll <- slidify(~ lm(..1 ~ ..2 + ..3), .period = 90, 
                   .unlist = FALSE, .align = "right")

FANG %>%
  select(symbol, date, adjusted, volume) %>%
  group_by(symbol) %>%
  mutate(numeric_date = as.numeric(date)) %>%
  # Apply rolling regression
  mutate(rolling_lm = lm_roll(adjusted, volume, numeric_date)) %>%
  filter(!is.na(rolling_lm))










