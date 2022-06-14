library(tidyverse)

cash_flow <- tibble(company = c("A", "A","B","B","B","C","C","C"), cash_flow = c(100, 200, 300, 400, 500, 800, 900,1000), year = c(2011, 2013, 2011,2012,2013,2011,2012,2013))

company_location <- tibble(company = c("A","B", "D", "E", "F"), location = c("New York", "New Orleans",  "Houston", "Dallas", "Chicago"))

company_location2 <- tibble(firm = c("A","B", "D", "E", "F"), location = c("New York", "New Orleans",  "Houston", "Dallas", "Chicago"))

sales <- tibble(firm = c("A", "A","A", "C", "C", "C"), sales = c(1015, 2100, 1300, 5400, 1500, 5800), year = c(2013, 2014, 2015, 2013, 2014, 2015))

 Merge1 <- full_join(cash_flow,company_location, by="company")
 Merge2 <- left_join(cash_flow,company_location,by="company")
 Merge3 <- right_join(cash_flow,company_location,by="company")
Merge4 <- inner_join(cash_flow,company_location,by="company") 

example <- right_join(company_location,cash_flow,by="company") 
####Same as left join above, order is different###

Merge1a <- full_join(cash_flow,company_location2,by=c("company"="firm"))
################################################################################
Mergea <- full_join(cash_flow,sales,by=c("company"="firm","year"="year"))
###Merge tables by two varibles###
Mergeb <- left_join(cash_flow,sales,by=c("company"="firm","year"="year"))
Mergec <- right_join(cash_flow,sales,by=c("company"="firm","year"="year"))
Merged <- inner_join(cash_flow,sales,by=c("company"="firm","year"="year"))


 FullData<- full_join(Merge1,sales,by=c("company"="firm","year"="year"))
 
FullData1 <- FullData %>% filter(!is.na(location))
### Removes data for observations where location is NA###
