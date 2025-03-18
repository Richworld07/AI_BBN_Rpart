# Load required libraries
library(zoo)
library(dplyr)

# Create the input data
unemployment_data <- data.frame(
  Year = c(2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 
           2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023),
  Unemployment_Rate = c(26.7, 27.21, 27.69, 28.29, 27.85, 27.4, 26.75, 25.99, 
                        25.12, 24.43, 23.82, 23.28, 22.72, 24.83, 26.98, 29.05, 
                        34.23, 35.71, 37.85, 37.64)
)
#Unemployment Data Disaggregation Script

# Function to interpolate quarterly data
interpolate_quarterly <- function(data) {
  # Create an empty dataframe to store quarterly results
  quarterly_data <- data.frame()
  
  # Loop through each pair of consecutive years
  for (i in 1:(nrow(data) - 1)) {
    current_year <- data$Year[i]
    next_year <- data$Year[i+1]
    current_rate <- data$Unemployment_Rate[i]
    next_rate <- data$Unemployment_Rate[i+1]
    
    # Linear interpolation for quarterly values
    q1_rate <- current_rate + (next_rate - current_rate) * 0.25
    q2_rate <- current_rate + (next_rate - current_rate) * 0.5
    q3_rate <- current_rate + (next_rate - current_rate) * 0.75
    
    # Create quarterly dataframe
    year_quarterly <- data.frame(
      Date = as.Date(paste0(c(current_year, current_year, current_year, next_year), 
                            c("-03-31", "-06-30", "-09-30", "-03-31"))),
      Unemployment_Rate = c(current_rate, q1_rate, q2_rate, q3_rate)
    )
    
    quarterly_data <- rbind(quarterly_data, year_quarterly)
  }
  
  # Add the last data point for the final year
  last_year <- data$Year[nrow(data)]
  last_rate <- data$Unemployment_Rate[nrow(data)]
  final_quarterly <- data.frame(
    Date = as.Date(paste0(last_year, c("-03-31", "-06-30", "-09-30"))),
    Unemployment_Rate = rep(last_rate, 3)
  )
  
  quarterly_data <- rbind(quarterly_data, final_quarterly)
  
  return(quarterly_data)
}

# Generate quarterly unemployment data
quarterly_unemployment <- interpolate_quarterly(unemployment_data)

# Optional: Extend to June 2024 by carrying forward the last known rate
# This assumes the rate remains constant for the first two quarters of 2024
extension_data <- data.frame(
  Date = as.Date(c("2024-03-31", "2024-06-30")),
  Unemployment_Rate = rep(37.64, 2)
)

quarterly_unemployment <- rbind(quarterly_unemployment, extension_data)

# View the results
print(quarterly_unemployment)

# Optional: Write to CSV
write.csv(quarterly_unemployment, "quarterly_unemployment.csv", row.names = FALSE)