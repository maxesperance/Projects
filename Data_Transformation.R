## In this template, we will learn how to transform your data using dplyr package, which is part of tidyverse package. 

#Often we want to see a subset of data, summarize data by groups, create new variables, delete existing variable, or remove data points.


# Various packages within tidyverse were created to make data cleaning easier. We will be using the functions within these packages. You can use R's functions to do these operations but tidyverse is inreasingly popular due to its capability and ease of writing the code. 

## Load tidyverse package
library(tidyverse)
library(lubridate)
## Transportation department collects and makes transportation data avaliable. In this class we will be working on a dataset that has information related to On-time performance  for all flights that departed NYC (i.e. JFK, LGA or EWR) in 2013.

# load library where the dataset is stored. Several datasets are available within this library -> flights, airlines, airports, planes, weather.
library(nycflights13)

#We will work with flights dataset. 
#?flights will give you information about this data set.
?flights

# import the dataset to our global environment as a tibble
flights <- flights

#Display data
flights
head(flights)
glimpse(flights)
#Data is already stored as a tibble. "int" implies it is an integer, "dbl" stands for real number, "chr" stands for characters,  "dttm" stands for date-times, "lgl" represents logical (true or false), "date" stands for dates, "fctr" stands for factors to represent categorical variables.

## "dplyr" package -  A Grammar of Data Manipulation with a set of verbs that can help us solve the most common data manipulation challenges:
# 1. select() picks variables based on their names. [extract existing variables]
# 2.  filter() picks cases based on their values. [extract existing observations]
# 3. arrange() changes the ordering of the rows.
# 4.  mutate() adds new variables that are functions of existing variables [derive new variable]
# 5. summarise() reduces multiple values down to a single summary.
# 6. group_by() perform above operations by groups
## This package is maintained by Hadley Wickham. 
# Reference: https://dplyr.tidyverse.org/

#syntax of the above verbs: verb(dataFrame, arguments)



#****************************************************************************************
## Select verb for pulling out variables
#****************************************************************************************
select(flights,carrier)
select(flights, origin, dest)

# remember the base R way to do this -> flights[,c("origin","dest")]. Why use select in dplyr package?
flights[,c("origin","dest")]


##Or you can combine some syntax from R in tidyverse functions
select(flights, c("origin", "dest"))


select(flights, -year)

select(flights, starts_with("a")) 

select(flights, everything()) 
select(flights, contains("dep"))
select(flights, last_col())
select(flights, -year)
flights_partial <- select(flights,carrier, origin, dest)
#****************************************************************************************
## Filter verb for pulling out variables
#****************************************************************************************
# Allows you to extract a subset of existing observations 
filter(flights,carrier=="DL")
# Filter data by month
filter(flights, month == 1)

# Filter data by month, date, and year
filter(flights, month == 1, year == 2013, day ==1)
filter(flights, month == 11)
flights_12 <- filter(flights, day == 12)

# When you run any verb, you it executes the operation and displays the result. If you want to save the result that you need to create a new dataset (or replace the current dataset) that consists of filtered data by month, date, and year

new_data <- filter(flights, month == 1, year == 2013, day ==1)

## comma seperator works as and operator. 
test <- filter(flights, month == 1 & year == 2013 & day ==1 & carrier=="B6")

## OR operator or |
test2 <- filter(flights, month == 1 & year == 2013 & day ==1 & (carrier=="B6" | carrier=="DL"))
delta_UA <- filter(flights, carrier=="DL" | carrier=="UA")
## What is the difference between the previous and the next line of code?
test3 <- filter(flights, month == 1 & year == 2013 & day ==1 & carrier=="B6"|carrier=="DL" )

## using pipes with verbs
filter(flights, dep_delay>1000)

flights %>% filter(carrier=="B6")
flights %>% filter(dep_delay>200) %>% filter(carrier == "DL") %>% select(dest)



#****************************************************************************************
## Arrange verb for pulling out variables
#****************************************************************************************
# Allows you to arrange your dataset based on arguments
arrange(flights,dep_time,day,month)
arrange(flights, year, month, day)

arrange(flights, arr_delay)

arrange(flights, desc(arr_delay))


## using pipes with verbs
#pipes allows us to use multiple verbs at once.
flights %>% arrange(dep_time)
flights %>% arrange(desc(dep_time))
flights %>% arrange (dep_time, day, month)
flights_carrier <- flights %>% arrange(carrier)

flights %>% filter(carrier=="DL") %>% filter(dep_delay>20) %>% arrange(desc(dep_delay))


#****************************************************************************************
## Mutate verb for pulling out variables
mutate(flights, dep_delay_hrs=dep_delay/60)
dep_delay_hrs
new_flights <- mutate(flights, dep_delay_hrs=dep_delay/60)
#****************************************************************************************
# Allows you to create new variables

flights_2 <- mutate(flights, airtime_hr = air_time/60)

flights_2 <- flights %>% mutate(airtime_hr = air_time/60)



#****************************************************
 # Exercise
#****************************************************
a_flights <- select(flights, carrier,dest,arr_time)
head(a_flights)
glimpse(a_flights)

# Look at arr_time variable. It is an integer. We may want to convert it to time format.

a_flights <- mutate(a_flights,arr_hr=arr_time%/%100) 
head(a_flights)
#%/% integer division

a_flights <- mutate(a_flights,arr_min=arr_time%%100) 
head(a_flights)
a_flights <- mutate(a_flights,arr_hr=arr_time%/%100, arr_min=arr_time%%100 )
head(a_flights)
a_flights <- mutate(a_flights, arrival_time= as.character(paste(a_flights$arr_hr, a_flights$arr_min, sep=":")))
a_flights$arrival_time <- hm(a_flights$arrival_time)
head(a_flights)
## modulus operator, %% (gives you the remainder)


flights_3 <- mutate(flights_2, arrival_time = as.character(paste(flights_2$arr_hr, flights_2$arr_min, sep = ":")))

flights_3$arrival_time <- hm(flights_3$arrival_time)



#**********************************************************
#Creating Functions
#*********************************************************
int2time <- function(x) {
  hm(as.character(paste(x%/%100, x%%100, sep = ":")))
}


flights<- mutate(flights, departure_time = int2time(dep_time))

head(flights)

#**********************************************************
#Function for converting time in hrs to minutes
#*********************************************************
time2mins <- function(x) {
  (x %/% 100 * 60 + x %% 100) %% 1440
}
#***********************************************************

#****************************************************************************************
## Rename verb for renaming variable names
#****************************************************************************************

flights_3 <- rename(flights, tail_num=tailnum)




#****************************************************************************************
## Summarize verb  
#***********************************************************************************
flights <- arrange(flights,dep_time)
summarize(flights, delay=mean(dep_delay))
summarize(flights, delay = mean(dep_delay, na.rm=TRUE))

#na.rm -> a logical value indicating whether NA values should be stripped before the computation proceeds.

#summarize is always used in tandem with group_by. This changes unit of analysis from the whole data to individual groups.
flights %>% group_by(carrier) %>% summarize(delay= mean(dep_delay, na.rm=TRUE))

by_carrier <- group_by(flights, carrier)
summarize(by_carrier,  delay = mean(dep_delay, na.rm=TRUE))

by_day <- group_by(flights, year, month, day)
summarize(by_day,  delay = mean(dep_delay, na.rm=TRUE))


##Using pipes

flights %>% group_by(carrier) %>% summarize(delay = mean(dep_delay, na.rm=TRUE))
 

flights %>% group_by(origin)  %>% summarize(delay = mean(dep_delay, na.rm=TRUE))

#**************************************************
  #Summarize operators
#**************************************************
#standard deviation
summarize(flights, delay = sd(dep_delay, na.rm=TRUE))

#median
summarize(flights, delay = median(dep_delay, na.rm=TRUE))

#count
flights %>% group_by(dest, carrier)  %>% summarize( number_flights= n())

#count
flights %>% group_by(dest)  %>% summarize(count = n(), dist = mean(distance, na.rm=TRUE), arr_delay = mean(arr_delay, na.rm=TRUE))

#**************************************************
#Missing Data
#**************************************************
# How to remove observations with missing data

flights %>% filter (is.na(dep_delay))

flights %>% filter (!is.na(dep_delay))

not_missing_data<- flights %>% filter (!is.na(dep_delay),!is.na(arr_delay))


#**************************************************
#Merge Data
#see https://stat545.com/join-cheatsheet.html
#understand: leftjoin, rightjoin, fulljoin
#**************************************************
flights_trim <- flights %>% select(year, month, day, carrier, origin, dest, dep_time)
airlines <- airlines


merged_data <- left_join(flights_trim,airlines, by="carrier")
merged_data <- full_join(flights_trim,airlines, by="carrier")


#add a dummy airline to Airline data
dummy_airline <- data.frame(carrier="NO", name="DUMMY AIRLINE")
new_airline<- rbind(airlines, dummy_airline)

##use left join and inner join and see what happens
merged_data3 <- left_join(flights_trim,new_airline, by="carrier")
merged_data4 <- full_join(flights_trim,new_airline, by="carrier")
