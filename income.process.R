### Analysis of Income and Taxes

library(tidyverse)
library(quantmod)
library(zoo)

income.tax.data <- read.csv("Tax Summary.csv")

income.tax.data <- income.tax.data |>
                mutate(Year = date(paste(Year, "-12-01", 
                            sep = ""))) |>
              rename(`Fed Tax` = Fed.Tax)

getSymbols("CPIAUCSL", src='FRED') 
avg.cpi <- apply.yearly(CPIAUCSL, mean)


inflation.index <- data.frame(Year=zoo::index(avg.cpi), zoo::coredata(avg.cpi))

income.tax <- income.tax.data |>
              left_join(inflation.index, by = c("Year")) |>
              mutate(`2008 Index` = inflation.index[which(inflation.index$Year=="2008-12-01"),]$CPIAUCSL / CPIAUCSL,
              `Income (2008 Dollars)` = Income * `2008 Index`,
              `Fed Tax (2008 Dollars)` = `Fed Tax` * `2008 Index`, 
              `Fed Tax Rate` = `Fed Tax`/ Income) 


ggplot(income.tax,  aes(x = Year)) + 
  geom_line(aes(y = `Income (2008 Dollars)`), color = "blue") +
  geom_line(aes(y = `Income`), linetype = 2) +
  scale_y_continuous(labels = scales::dollar, limits = c(0,200000)) +
  labs(title = "Combined Income",
       y = "Income",
       x = "Year") +
  annotate("text", 
           x = as.Date("2021-12-01"), 
           y = 90000, 
           label = "2008 Dollars", color = "blue") +
  annotate("text", 
           x = as.Date("2018-12-01"), 
           y = 130000, label = "Nominal Dollars") +
  theme_minimal() 
  



ggplot(income.tax,  aes(x = Year)) + 
  geom_line(aes(y = `Fed Tax (2008 Dollars)`), color = "red") +
  geom_line(aes(y = `Fed Tax`), linetype = 2) + 
  ##Add a title to the graph and the two axes
  labs(title = "Federal Taxes",
       y = "Federal Taxes",
       x = "Year") +
  scale_y_continuous(labels = scales::dollar)  +
        annotate("text", 
       x = as.Date("2021-12-01"), 
          y = 9500, 
        label = "2008 Dollars", color = "red") +
  annotate("text", 
           x = as.Date("2018-12-01"), 
           y = 15000, label = "Nominal Dollars") +
  theme_minimal() 
  
  

ggplot(income.tax, aes(x = Year, y = `Fed Tax Rate`)) + geom_line()





