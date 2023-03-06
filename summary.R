#load libraries
library("dplyr")
#load data into variable called A3_series_data
A3_series_data <- read.csv("Percy_Jackson.csv")
#Total number of checkouts for the series per year
checkouts_by_year <- A3_series_data %>% 
  group_by(CheckoutYear) %>% 
  summarize(Total_Checkouts = sum(Checkouts, na.rm = TRUE))
#Year with the highest number of checkouts for all books within the series
most_checkouts_year <- checkouts_by_year %>% 
  filter(Total_Checkouts == max(Total_Checkouts)) %>% 
  pull(CheckoutYear)
#Year with the lowest number of checkouts for all books within the series
least_checkouts_year <- checkouts_by_year %>% 
  filter(Total_Checkouts == min(Total_Checkouts)) %>% 
  pull(CheckoutYear)
#Average number of checkouts for each book within the series
book_avg_checkouts <- A3_series_data %>%
  group_by(Title) %>%
  summarize(avg_checkouts = mean(Checkouts, na.rm = TRUE))
#Total number of checkouts for the series per month
monthly_checkouts <- A3_series_data %>%
  group_by(CheckoutMonth) %>%
  summarize(Monthly_Checkouts = sum(Checkouts, na.rm = TRUE))
#Month with the highest number of checkouts for all books within the series
most_checkouts_month <- monthly_checkouts %>%
  filter(Monthly_Checkouts == max(Monthly_Checkouts))%>%
  pull(CheckoutMonth)
#Month with the lowest number of checkouts for all books within the series
least_checkouts_month <- monthly_checkouts %>%
  filter(Monthly_Checkouts == min(Monthly_Checkouts))%>%
  pull(CheckoutMonth)
