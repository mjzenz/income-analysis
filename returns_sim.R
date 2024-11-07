### Simulations of Annual portfolio returns
library(tidyverse)
library(ggplot2)


NUM_YEARS <- 20
INITIAL_PORTFOLIO_VALUE <- 700
YEARLY_INVESTMENT <- 80
NUM_SIMULATIONS <- 2000

####
#Annual return

for (SIM in 1:NUM_SIMULATIONS){
  yearly.return <-rnorm(NUM_YEARS,  mean = .04, sd = .05)
  
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

print(SIM)

}


### Summary Statistics

distribution.stats <- portfolio_values %>%
  group_by(Year) %>%
  summarize(max95 = quantile(Value, prob = .95), 
            q3 = quantile(Value, prob = .75),
            median = median(Value), 
            q1 = quantile(Value, prob = .25),
            min95 = quantile(Value, prob = .05))

ggplot(distribution.stats, aes(x = Year, y = median, group = SIM)) +
  geom_ribbon(aes(ymin = q1, ymax = q3))+
  geom_line(show.legend = FALSE)

