#Analysis of change in real value of income.

library(tidyverse)
library(quantmod)
library(zoo)

income.changes.data <- read.csv("Income_History.csv")
income.changes.data$Date = as.Date(income.changes.data$Date)

getSymbols("CPIAUCSL", src='FRED') 


all.months <- data.frame(Date = seq.Date(from = min(income.changes.data$Date), 
         to = Sys.Date(), by = "month")) |>
  mutate(yr_month = zoo::as.yearmon(Date))



## convert CPIAUCSL to data frame
CPIAUCSL <- data.frame(Date=index(CPIAUCSL), coredata(CPIAUCSL))

pay.data <- income.changes.data |>
  right_join(all.months, by = c("Date")) |>
  arrange(yr_month) |>
  fill(Pay, .direction = "down") |>
  inner_join(CPIAUCSL, by = c("Date")) |>
  rename(Inflation_Index = CPIAUCSL) |>
  mutate(`2019 Index` = CPIAUCSL[which(CPIAUCSL$Date =="2019-08-01"),]$CPIAUCSL / Inflation_Index,
         `Pay (2019 Dollars)` = Pay * `2019 Index`) 

vlines <- pay.data[which(pay.data$Type == "P"),"Pay (2019 Dollars)"]


ggplot(pay.data,  aes(x = Date)) + 
  geom_line(aes(y = `Pay (2019 Dollars)`), color = "blue") +
  geom_line(aes(y = `Pay`), linetype = 2) +
  scale_y_continuous(labels = scales::dollar, limits = c(50000,120000)) +
  labs(title = "Nominal vs. Real Pay",
       #subtitle = "Red lines are real pay at performance raises",
       y = "Pay",
       x = "Date") +
  annotate("text", 
           x = as.Date("2022-12-01"), 
           y = 75000, 
           label = "2019 Dollars", color = "blue") +
  annotate("text", 
           x = as.Date("2022-12-01"), 
           y = 108000, label = "Nominal Dollars") +
 # geom_hline(yintercept = vlines[1], linetype = 1, color = "red", 
  #           alpha = .2) +
  #geom_hline(yintercept = vlines[2], linetype = 1, color = "red", 
  #           alpha = .2) +
  theme_minimal() 





