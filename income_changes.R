#Analysis of change in real value of income.

library(tidyverse)
library(quantmod)
library(zoo)

income.changes.data <- read.csv("Income_History.csv")
income.changes.data$Date <- as.Date(income.changes.data$Date)

getSymbols("CPIAUCSL", src='FRED') 


all.months <- data.frame(Date = seq.Date(from = min(income.changes.data$Date), 
         to = as.Date("2025-03-01"), by = "month")) |>
  mutate(yr_month = zoo::as.yearmon(Date))




## convert CPIAUCSL to data frame
CPIAUCSL <- data.frame(Date=index(CPIAUCSL), coredata(CPIAUCSL))

twopercentyoy_month <- CPIAUCSL[nrow(CPIAUCSL),]$CPIAUCSL + .02 * CPIAUCSL[nrow(CPIAUCSL),]$CPIAUCSL

#create a sequence of months from March 2024 to March 2025
ADD_MONTHS <- data.frame(Date = seq.Date(from = as.Date("2024-03-01"), 
                                         to = as.Date("2025-03-01"), by = "month"), 
                         CPIAUCSL = seq(from = CPIAUCSL[nrow(CPIAUCSL),]$CPIAUCSL ,
                                        to = twopercentyoy_month, 
                                        length.out = 13))

ADD_MONTHS <- ADD_MONTHS[-1,]



CPIAUCSL <- rbind(CPIAUCSL, ADD_MONTHS) |>
  mutate(yoy = (CPIAUCSL - lag(CPIAUCSL, 12))/lag(CPIAUCSL, 12)) 
  
  




# 
# AVG_CHANGE_RECENT <- mean(c(CPIAUCSL[nrow(CPIAUCSL),
#                                    "CPIAUCSL"]- CPIAUCSL[nrow(CPIAUCSL)-1,
#                                                          "CPIAUCSL"],
#                           CPIAUCSL[nrow(CPIAUCSL)-1,
#                                     "CPIAUCSL"]- CPIAUCSL[nrow(CPIAUCSL)-2,
#                                                           "CPIAUCSL"],
#                           CPIAUCSL[nrow(CPIAUCSL-2),
#                                     "CPIAUCSL"]- CPIAUCSL[nrow(CPIAUCSL)-3,
#                                                           "CPIAUCSL"]))  
# 
# ## Add rows for months with no data
# ADD_MONTHS <- data.frame(Date = as.Date(c("2024-04-01", "2024-05-01")),
#                 CPIAUCSL = c(CPIAUCSL[nrow(CPIAUCSL),
#                           "CPIAUCSL"] + AVG_CHANGE_RECENT,
#                           CPIAUCSL[nrow(CPIAUCSL),
#                                    "CPIAUCSL"] + 2*AVG_CHANGE_RECENT))






pay.data <- income.changes.data |>
  right_join(all.months, by = c("Date")) |>
  arrange(yr_month) |>
  fill(Pay, .direction = "down") |>
  inner_join(CPIAUCSL, by = c("Date")) |>
  rename(Inflation_Index = CPIAUCSL) |>
  mutate(`2019 Index` = CPIAUCSL[which(CPIAUCSL$Date =="2019-08-01"),]$CPIAUCSL / Inflation_Index,
         `Pay (2019 Dollars)` = Pay * `2019 Index`, 
         Proj = Date > Sys.Date()) 

vlines <- pay.data[which(pay.data$Type == "P"),"Pay (2019 Dollars)"]


ggplot(pay.data,  aes(x = Date)) + 
  geom_line(aes(y = `Pay (2019 Dollars)`, linetype = Proj), color = "blue") +
  geom_line(aes(y = `Pay`, linetype = Proj)) +
  scale_y_continuous(labels = scales::dollar, limits = c(50000,120000)) +
  labs(title = "Nominal vs. Real Pay",
       subtitle = "Red lines are real pay at performance raises, dotted proj.",
       y = "Pay",
       x = "Date") +
  annotate("text", 
           x = as.Date("2022-12-01"), 
           y = 75000, 
           label = "2019 Dollars", color = "blue") +
  annotate("text", 
           x = as.Date("2022-12-01"), 
           y = 108000, label = "Nominal Dollars") +
  geom_hline(yintercept = vlines[1], linetype = 1, color = "red", 
             alpha = .2) +
  geom_hline(yintercept = vlines[2], linetype = 1, color = "red", 
             alpha = .2) +
  geom_hline(yintercept = vlines[3], linetype = 1, color = "red", 
             alpha = .2) +
  #eliminate legend
  theme_minimal() +
  theme(legend.position = "none")


#Calculate real salary growth by year
pay.data |>
  filter(month(Date) == 8) |>
  select(Date, `Pay (2019 Dollars)`) |>
  mutate(yoy = (`Pay (2019 Dollars)` - lag(`Pay (2019 Dollars)`))/lag(`Pay (2019 Dollars)`)) |>
  ggplot(aes(x = Date, y = yoy)) +
  geom_line() 
## inflation exploration

## Calculate yoy inflation growth from CPIAUCSL
 CPIAUCSL |>
  mutate(yoy = (CPIAUCSL - lag(CPIAUCSL, 12))/lag(CPIAUCSL, 12)) |>
   ggplot(aes(x = Date, y = yoy)) +
  geom_line() +
   #Make horizontal line at .02
  geom_hline(yintercept = .02, linetype = 2, color = "red")



