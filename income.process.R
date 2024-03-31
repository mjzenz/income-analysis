### Analysis of Income and Taxes

library(tidyverse)
library(quantmod)
library(zoo)

income.tax.data <- read_csv("Tax_Summary.csv", 
                        col_types = cols(Year = col_date(format = "%Y")))

getSymbols("CPIAUCSL", src='FRED') 



inflation.index <- data.frame(date=zoo::index(CPIAUCSL), zoo::coredata(CPIAUCSL))
                  

income.tax <- income.tax.data |>
              left_join(inflation.index, by = c("Year"= "date")) |>
              mutate(`2008 Index` = inflation.index[which(inflation.index$date=="2008-01-01"),]$CPIAUCSL / CPIAUCSL,
                     `Income (2008 Dollars)` = Income * `2008 Index`,
                     `Fed Tax (2008 Dollars)` = `Fed Tax` * `2008 Index`)


ggplot(income.tax,  aes(x = Year)) + 
  geom_line(aes(y = `Income (2008 Dollars)`), color = "blue") +
  geom_line(aes(y = `Income`), linetype = 2)
