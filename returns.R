library(ggplot2)
library(tidyverse)

# Function to compute the yearly investment portfolio values
compute_yearly_values <- function(initial_investment, yearly_investment, 
                                  annual_return_min, annual_return_max, 
                                  years) {
  portfolio_values <- data.frame("Year" = 1:years, "Min_Value" = numeric(years),
                                "Max_Value" = numeric(years),  "Invested" = numeric(years))
  portfolio_value_min <- initial_investment
  portfolio_value_max <- initial_investment
  
  
  for (year in 1:years) {
    portfolio_value_min <- portfolio_value_min + yearly_investment
    portfolio_value_max <- portfolio_value_max + yearly_investment
    portfolio_value_min <- portfolio_value_min * (1 + annual_return_min)
    portfolio_value_max <- portfolio_value_max * (1 + annual_return_max)
    

    portfolio_values[year, "Min_Value"] <- portfolio_value_min
    portfolio_values[year, "Max_Value"] <- portfolio_value_max
    
    if(year == 1){
      portfolio_values[year, "Invested"] <- initial_investment + yearly_investment
    }else{
      portfolio_values[year, "Invested"] <- yearly_investment + portfolio_values[year-1,"Invested"]
      }
    
  }
  
  return(portfolio_values)
}

# Example usage
initial_investment <- 570000 # Initial investment in dollars
yearly_investment <- 90000    # Yearly investment in dollars
annual_return_min <- .04 # Annual return percentage
annual_return_max <- .06
years <- 20                  # Number of years
HowMuchToLive <- 70000
NeededAssets <- 70000/.04

# Calculate the yearly portfolio values
yearly_values <- compute_yearly_values(initial_investment, yearly_investment, annual_return_min, 
                                       annual_return_max, years)

yearly_values <- pivot_longer(yearly_values, 
                              cols = c(Min_Value, Max_Value), 
                              names_to = "Type")


# Plot the yearly portfolio values
ggplot(yearly_values, aes(x = Year, y = value, 
                          color = Type, group = Type )) +
  geom_line(size = 1) +
  geom_line(aes(y = Invested), linetype = 2, color = "black")+
  geom_hline(yintercept = NeededAssets) +
  scale_color_manual(values = c("green", "red")) +
  labs(title = "Yearly Portfolio Value Over Time",
       x = "Year",
       y = "Portfolio Value (in dollars)") +
  theme_minimal()

# Print the data frame
print(yearly_values)
