### Simulations of Annual portfolio returns
library(tidyverse)
library(ggplot2)


NUM_YEARS <- 20
INITIAL_PORTFOLIO_VALUE <- 700
YEARLY_INVESTMENT <- 0
NUM_SIMULATIONS <- 10

####
#Annual return

for (SIM in 1:NUM_SIMULATIONS){
  yearly.return <-rnorm(NUM_YEARS,  mean = .07, sd = .09)
  
if(SIM == 1){
  
portfolio_values <- data.frame("Year" = 1:NUM_YEARS, "Value" = numeric(NUM_YEARS),
                               "SIM" = rep(1, NUM_YEARS),
                               "YRRate" = yearly.return,
                               "Invested" = numeric(NUM_YEARS))
}else{
  portfolio_values <- rbind(portfolio_values,
                          data.frame("Year" = 1:NUM_YEARS, "Value" = numeric(NUM_YEARS),
                                  "SIM" = rep(SIM, NUM_YEARS),
                                  "YRRate" = yearly.return,
                                  "Invested" = numeric(NUM_YEARS)))        
}


portfolio_value <- INITIAL_PORTFOLIO_VALUE

for (year in 1:NUM_YEARS){
  portfolio_value <- portfolio_value + YEARLY_INVESTMENT
  portfolio_value <- portfolio_value * (1 + portfolio_values[which(portfolio_values$Year == year &
                                                              portfolio_values$SIM == SIM) , "YRRate"])

  
  portfolio_values[which(portfolio_values$Year == year &
                          portfolio_values$SIM == SIM), "Value"] <- portfolio_value

  
  
  if(year == 1){
    portfolio_values[which(portfolio_values$Year == year &
                             portfolio_values$SIM == SIM), "Invested"] <- INITIAL_PORTFOLIO_VALUE + YEARLY_INVESTMENT
  }else{
    portfolio_values[which(portfolio_values$Year == year &
                             portfolio_values$SIM == SIM), "Invested"] <- YEARLY_INVESTMENT + portfolio_values[which(portfolio_values$Year == (year - 1) &
                                                                                      portfolio_values$SIM == SIM),"Invested"]
  }
  
  
}

}




ggplot(yearly.return, aes(x = year, y = return)) +
  geom_line()

