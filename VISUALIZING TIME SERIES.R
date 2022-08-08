#install packages
library(tidyverse)
library(lubridate)
library(timetk)

# Setup for the plotly charts (# FALSE returns ggplots)
interactive <- FALSE


#loadtime series data
taylor_30_min

#The plot_time_series() function generates an interactive chart by default.
taylor_30_min %>% 
  plot_time_series(date, value, 
                   .interactive = interactive,
                   .plotly_slider = TRUE)

#Plotting Groups
m4_daily %>% group_by(id)


#Visualizing
m4_daily %>%
  group_by(id) %>%
  plot_time_series(date, value, 
                   .facet_ncol = 2, .facet_scales = "free",
                   .interactive = interactive)

#Visualizing Transformations & Sub-Groups
m4_hourly %>% group_by(id)


m4_hourly %>%
  group_by(id) %>%
  plot_time_series(date, log(value),             # Apply a Log Transformation
                   .color_var = week(date),      # Color applied to Week transformation
                   # Facet formatting
                   .facet_ncol = 2, 
                   .facet_scales = "free", 
                   .interactive = interactive)

#Static ggplot2 Visualizations & Customizations
taylor_30_min %>%
  plot_time_series(date, value, 
                   .color_var = month(date, label = TRUE),
                   
                   # Returns static ggplot
                   .interactive = FALSE,  
                   
                   # Customization
                   .title = "Taylor's MegaWatt Data",
                   .x_lab = "Date (30-min intervals)",
                   .y_lab = "Energy Demand (MW)",
                   .color_lab = "Month") +
  scale_y_continuous(labels = scales::comma_format())

#Box Plots (Time Series)
m4_monthly %>%
  group_by(id) %>%
  plot_time_series_boxplot(
    date, value,
    .period      = "1 year",
    .facet_ncol  = 2,
    .interactive = FALSE)

#Regression Plots (Time Series)
m4_monthly %>%
  group_by(id) %>%
  plot_time_series_regression(
    .date_var     = date,
    .formula      = log(value) ~ as.numeric(date) + month(date, label = TRUE),
    .facet_ncol   = 2,
    .interactive  = FALSE,
    .show_summary = FALSE
  )









