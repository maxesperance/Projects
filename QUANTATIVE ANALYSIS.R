# Load packages 
library(tidyverse)
library(tidyquant)

#VIEW MUTATE FUNCTIONS
tq_transmute_fun_options() %>% str()

# VIEW ZOO FUNCTIONS
tq_transmute_fun_options()$zoo

# EX: rollapply(data, width, FUN, ..., by = 1, by.column = TRUE, fill = if (na.pad) NA, na.pad = FALSE, partial = FALSE, align = c("center", "left", "right"), coredata = TRUE).rollmax, rollmean, rollmedian, rollsum, etc.#

# VIEW xts FUNCTIONS
tq_transmute_fun_options()$xts

# Period Apply Functions: Apply a function to a time segment(max, min, mean, etc)
  #EX: apply.daily(x, FUN, ...).including apply.daily, weekly, monthly, quarterly,     yearly#

#To-Period Functions:Convert a time series to time series between periods (convert daily to monthly periodicity).
  #EX: to.period(x, period = 'months', k = 1, indexAt, name = NULL, OHLC = TRUE, ...).   including to.minutes, hourly, daily, weekly, monthly, quarterly, yearly.

#Note: The return structure is different for to.period and the to.monthly (to.weekly, to.quarterly, etc). to.period returns a date, while to.months returns a character MON YYYY.Best to use to.period if you want to work with time-series via lubridate.#

# VIEW quantmod FUNCTIONS
tq_transmute_fun_options()$quantmod

# Percentage Change (Delt) and Lag Functions: 
    #Delt: Delt(x1, x2 = NULL, k = 0, type = c("arithmetic", "log"))
    #Variations of Delt: ClCl, HiCl, LoCl, LoHi, OpCl, OpHi, OpLo, OpOp | EX: OpCl(OHLC)
    #Lag: Lag(x, k = 1) / Next: Next(x, k = 1) (Can also use dplyr::lag and dplyr::lead)
#Period Return Functions:Get the arithmetic or logarithmic returns for various periodicity, which include daily, weekly, monthly, quarterly, and yearly.
  #EX: periodReturn(x, period = 'monthly', subset = NULL, type = 'arithmetic', leading =   TRUE, ...)
#Series Functions: Return values that describe the series. IncludING describing the increases/decreases, acceleration/deceleration, and hi/low.
  #EX: seriesHi(x), seriesIncr(x, thresh = 0, diff. = 1L), seriesAccel(x)

# VIEW TTR FUNCTIONS  
tq_transmute_fun_options()$TTR


#popular functions from TTR:

#Welles Wilder’s Directional Movement Index: 
  #ADX(HLC, n = 14, maType, ...)

#Bollinger Bands:
  #BBands(HLC, n = 20, maType, sd = 2, ...): Bollinger Bands

#Rate of Change / Momentum:
  #ROC(x, n = 1, type = c("continuous", "discrete"), na.pad = TRUE): Rate of Change
  #momentum(x, n = 1, na.pad = TRUE): Momentum

#Moving Averages (maType):
  #SMA(x, n = 10, ...): Simple Moving Average
  #EMA(x, n = 10, wilder = FALSE, ratio = NULL, ...): Exponential Moving Average
  #DEMA(x, n = 10, v = 1, wilder = FALSE, ratio = NULL): Double Exponential Moving Average
  #WMA(x, n = 10, wts = 1:n, ...): Weighted Moving Average
  #EVWMA(price, volume, n = 10, ...): Elastic, Volume-Weighted Moving Average
  #ZLEMA(x, n = 10, ratio = NULL, ...): Zero Lag Exponential Moving Average
  #VWAP(price, volume, n = 10, ...): Volume-Weighted Moving Average Price
  #VMA(x, w, ratio = 1, ...): Variable-Length Moving Average
  #HMA(x, n = 20, ...): Hull Moving Average
  #ALMA(x, n = 9, offset = 0.85, sigma = 6, ...): Arnaud Legoux Moving Average


#MACD Oscillator:
  #MACD(x, nFast = 12, nSlow = 26, nSig = 9, maType, percent = TRUE, ...)

#Relative Strength Index:
  #RSI(price, n = 14, maType, ...)

#runFun:
  #runSum(x, n = 10, cumulative = FALSE): returns sums over a n-period moving window.
  #runMin(x, n = 10, cumulative = FALSE): returns minimums over a n-period moving window.
  #runMax(x, n = 10, cumulative = FALSE): returns maximums over a n-period moving window.
  #runMean(x, n = 10, cumulative = FALSE): returns means over a n-period moving window.
  #runMedian(x, n = 10, non.unique = "mean", cumulative = FALSE): returns medians over a n-period moving window.
  #runCov(x, y, n = 10, use = "all.obs", sample = TRUE, cumulative = FALSE): returns covariances over a n-period moving window.
  #runCor(x, y, n = 10, use = "all.obs", sample = TRUE, cumulative = FALSE): returns correlations over a n-period moving window.
  #runVar(x, y = NULL, n = 10, sample = TRUE, cumulative = FALSE): returns variances over a n-period moving window.
  #runSD(x, n = 10, sample = TRUE, cumulative = FALSE): returns standard deviations over a n-period moving window.
  #runMAD(x, n = 10, center = NULL, stat = "median", constant = 1.4826, non.unique = "mean", cumulative = FALSE): returns median/mean absolute deviations over a n-period moving window.
  #wilderSum(x, n = 10): retuns a Welles Wilder style weighted sum over a n-period moving window.
#Stochastic Oscillator / Stochastic Momentum Index:
  #stoch(HLC, nFastK = 14, nFastD = 3, nSlowD = 3, maType, bounded = TRUE, smooth = 1, ...)
  #SMI(HLC, n = 13, nFast = 2, nSlow = 25, nSig = 9, maType, bounded = TRUE, ...)


# VIEW PERFORMANCE ANALYTICS FUNTIONS
tq_transmute_fun_options()$PerformanceAnalytics

#PerformanceAnalytics mutation functions all deal with returns:
  #Return.annualized and Return.annualized.excess: Takes period returns and consolidates into annualized returns
  #Return.clean: Removes outliers from returns
  #Return.excess: Removes the risk-free rate from the returns to yield returns in excess of the risk-free rate
  #zerofill: Used to replace NA values with zeros.


#LOAD PACKAGES 
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyquant) 
library(timetk) 
library(quantmod)

#LOAD DATA FROM FILE
BIG5 <- read.csv("BIG5.csv", header= TRUE)

#Historical View of Closing Price
BIG5%>%
  ggplot(aes(x = Date, y = Close, fill = Ticker)) +
  geom_col()

#SAVE CHART
HV <- BIG5%>%
  ggplot(aes(x = Date, y = Close, fill = Ticker)) +
  geom_col()

#USING ADJUSTED CLOSING STOCK PRICES FOR ANNUAL RETURNS
BIG5_annual_returns <- BIG5 %>%
  group_by(Ticker) %>%
  tq_transmute(select     = Adjusted, 
               mutate_fun = periodReturn, 
               period     = "yearly", 
               type       = "arithmetic")
BIG5_annual_returns

#CHARTING ANNUAL RETURNS WITH ggplot2
BIG5_annual_returns %>%
  ggplot(aes(x = Date, y = yearly.returns, fill = Ticker)) +
  geom_col() +
  geom_hline(yintercept = 0, color = palette_light()[[1]]) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "BIG5: Annual Returns",
       subtitle = "annual returns quickly",
       y = "Annual Returns", x = "") + 
  facet_wrap(~ Ticker, ncol = 2, scales = "free_y") +
  theme_tq() + 
  scale_y_continuous()

#GETTING DAILY RETURN LOGS
BIG5_daily_log_returns <- BIG5 %>%
  group_by(Ticker) %>%
  tq_transmute(select     = Adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily", 
               type       = "log",
               col_rename = "monthly.returns")

#CHARTING DAILY RETURN LOGS ggplot2
BIG5_daily_log_returns %>%
  ggplot(aes(x = monthly.returns, fill = Ticker)) +
  geom_density(alpha = 0.5) +
  labs(title = "BIG5: Charting the Daily Log Returns",
       x = "Monthly Returns", y = "Density") +
  theme_tq() +
  scale_fill_tq() + 
  facet_wrap(~ Ticker, ncol = 2)


#USING xts to.period TO CHANGE BETWEEN PERIODS
BIG5M <- BIG5%>% group_by(Ticker) %>%tq_transmute(select = Open:Volume,mutate_fun=to.period,period="months")


#DAILY PLOTS
BIG5_daily <- BIG5 %>% group_by(Ticker)

BIG5_daily %>%
  ggplot(aes(x = Date, y = Adjusted, color = Symbol)) +
  geom_line(size = 1) +
  labs(title = "Daily Stock Prices",
       x = "", y = "Adjusted Prices", color = "") +
  facet_wrap(~ Ticker, ncol = 2, scales = "free_y") +
  scale_y_continuous(labels = scales::dollar) +
  theme_tq() + 
  scale_color_tq()


#MONTHLY PLOTS
BIG5_monthly <- BIG5 %>% group_by(Ticker) %>% tq_transmute(select = Adjusted, mutate_fun = to.period,period = "months")

BIG5_monthly %>%
  ggplot(aes(x = Date, y = Adjusted, color = Ticker)) +
  geom_line(size = 1) +
  labs(title = "Monthly Stock Prices",
       x = "", y = "Adjusted Prices", color = "") +
  facet_wrap(~ Ticker, ncol = 2, scales = "free_y") +
  scale_y_continuous(labels = scales::dollar) +
  theme_tq() + 
  scale_color_tq()

#USING TTR runCor TO VISUALIZE ROLLING CORRELATIONS OF RETURNS
# Asset Returns
BIG5_returns_monthly <- BIG5 %>%group_by(Ticker) %>% tq_transmute(select = Adjusted, 
mutate_fun = periodReturn,period = "monthly")

# Baseline Returns
baseline_returns_monthly <- "XLK" %>% tq_get(get  = "stock.prices",from = "2013-01-01",to= "2016-12-31") %>%tq_transmute(select= Adjusted, mutate_fun = periodReturn,period = "monthly")

#Join the asset returns with the baseline returns by date.
returns_joined <- left_join(BIG5_returns_monthly,baseline_returns_monthly,by = "date")
returns_joined

#TTR::runCor function TO EVALUATE xy ROLLING CORRELATIONS 
BIG5_rolling_corr <- returns_joined %>% 
  tq_transmute_xy(
    x= monthly.returns.x,
    y=monthly.returns.y,
    mutate_fun=runCor,
    n = 6,
    col_rename = "rolling.corr.6")

BIG5_rolling_corr %>%
  ggplot(aes(x = Date, y = rolling.corr.6, color = Ticker)) +
  geom_hline(yintercept = 0, color = palette_light()[[1]]) +
  geom_line(size = 1) +
  labs(title = "BIG5: Six Month Rolling Correlation",
       x = "", y = "Correlation", color = "") +
  facet_wrap(~ Ticker, ncol = 2) +
  theme_tq() + 
  scale_color_tq()

#USING TTR MACD FOR MOVING AVERAGE CONVERGENCE DIVERGENCE
BIG5_macd <- BIG5 %>%
  group_by(Ticker) %>%
  tq_mutate(select     = Close, 
            mutate_fun = MACD, 
            nFast      = 12, 
            nSlow      = 26, 
            nSig       = 9, 
            maType     = SMA) %>%
  mutate(diff = macd - signal) %>%
  select(-(Open:Volume))
BIG5_macd


#USING TTR MACD TO VISULAIZE MOVING AVERAGE CONVERGENCE DIVERGENCE
BIG5_macd %>%
  filter(Date >= as_date("2016-10-01")) %>%
  ggplot(aes(x = Date)) + 
  geom_hline(yintercept = 0, color = palette_light()[[1]]) +
  geom_line(aes(y = macd, col = Ticker)) +
  geom_line(aes(y = signal), color = "blue", linetype = 2) +
  geom_bar(aes(y = diff), stat = "identity", color = palette_light()[[1]]) +
  facet_wrap(~ Ticker, ncol = 2, scale = "free_y") +
  labs(title = "BIG5: Moving Average Convergence Divergence",
       y = "MACD", x = "", color = "") +
  theme_tq() +
  scale_color_tq()


#USING xts apply.quarterly TO  GET THE MAX AND MIN PRICE FOR EACH QUARTER
BIG5_max_by_qtr <- BIG5 %>%
  group_by(Ticker) %>%
  tq_transmute(select     = Adjusted, 
               mutate_fun = apply.quarterly, 
               FUN        = max, 
               col_rename = "max.close") %>%
  mutate(year.qtr = paste0(year(Date), "-Q", quarter(Date))) %>%
  select(-Date)
BIG5_max_by_qtr

#data frames can be joined using left_join to get the max and min by quarter.
BIG5_min_by_qtr <- BIG5 %>%
  group_by(Ticker) %>%
  tq_transmute(select     = Adjusted, 
               mutate_fun = apply.quarterly, 
               FUN        = min, 
               col_rename = "min.close") %>%
  mutate(year.qtr = paste0(year(Date), "-Q", quarter(Date))) %>%
  select(-Date)

BIG5_by_qtr1 <- left_join(BIG5_max_by_qtr, BIG5_min_by_qtr,
                         by = c("Ticker"   = "Ticker",
                                "year.qtr" = "year.qtr"))
BIG5_by_qtr

#VISUALIZE THE DATA
BIG5_by_qtr %>%
  ggplot(aes(x = year.qtr, color = Ticker)) +
  geom_segment(aes(xend = year.qtr, y = min.close, yend = max.close),
               size = 1) +
  geom_point(aes(y = max.close), size = 2) +
  geom_point(aes(y = min.close), size = 2) +
  facet_wrap(~ symbol, ncol = 2, scale = "free_y") +
  labs(title = "BIG5: Min/Max Price By Quarter",
       y = "Stock Price", color = "") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x = element_blank())

#USING zoo rollapply FOR ROLLING REGRESSION
# Get stock pairs
stock_prices <- c("AMZN", "AAPL") %>%
  tq_get(get  = "stock.prices",
         from = "2020-01-01",
         to   = "2021-12-31") %>%
  group_by(Ticker) 


stock_pairs <- stock_prices %>%
  tq_transmute(select     = Adjusted,
               mutate_fun = periodReturn,
               period     = "daily",
               type       = "log",
               col_rename = "returns") %>%
  spread(key = Ticker, value = returns)

#VISUALIZE RELATIONSHIP
stock_pairs %>%
  ggplot(aes(x = AMZN, y = AAPL)) +
  geom_point(color = palette_light()[[1]], alpha = 0.5) +
  geom_smooth(method = "lm") +
  labs(title = "Visualizing Returns Relationship of APPLE/AMAZON") +
  theme_tq()


#STATISTICS ON RELATION SUING lm FUNCTION
lm(AMZN ~ AAPL, data = stock_pairs) %>%
  summary()

# rollapply function from zoo package to plot a rolling regression, showing how the model coefficient varies over time on a rolling basis 

# 1.create a custom regression function
regr_fun <- function(data) {
  coef(lm(AMZN ~ AAPL, data = timetk::tk_tbl(data, silent = TRUE)))
}

#2 Apply the function with tq_mutate(mutate_fun = rollapply)
stock_pairs <- stock_pairs %>%
  tq_mutate(mutate_fun = rollapply,
            width      = 90,
            FUN        = regr_fun,
            by.column  = FALSE,
            col_rename = c("coef.0", "coef.1"))
stock_pairs

#VISUALIZE COEFFICIENTS
stock_pairs %>%
  ggplot(aes(x = Date, y = coef.1)) +
  geom_line(size = 1, color = palette_light()[[1]]) +
  geom_hline(yintercept = 0.8134, size = 1, color = palette_light()[[2]]) +
  labs(title = "AMZN ~ AAPL: Visualizing Rolling Regression Coefficient", x = "") +
  theme_tq()

#STOCK RETURNS DURING THIS TIME PERIOD
stock_prices %>%
  tq_transmute(Adjusted, 
               periodReturn, 
               period = "daily", 
               type = "log", 
               col_rename = "returns") %>%
  mutate(wealth.index = 100 * cumprod(1 + returns)) %>%
  ggplot(aes(x = Date, y = wealth.index, color = Ticker)) +
  geom_line(size = 1) +
  labs(title = "AMZN and AAPL: Stock Prices") +
  theme_tq() + 
  scale_color_tq()

# USING Return.clean AND  Return.excess to clean and calculate excess returns
BIG5 %>%
  group_by(Ticker) %>%
  tq_transmute(adjusted, periodReturn, period = "daily") %>%
  tq_transmute(daily.returns, Return.clean, alpha = 0.05) %>%
  tq_transmute(daily.returns, Return.excess, Rf = 0.03 / 252)


