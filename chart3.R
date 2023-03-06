#load libraries
library("dplyr")
library("ggplot2")

#load data into variable called A3_series_data
A3_series_data <- read.csv("Percy_Jackson_Riordan.csv")
A3_choice_data <- read.csv("Kane_Riordan.csv")
#filter only the books with material type Ebook 
A3_series_data <- A3_series_data %>%
  filter(MaterialType == "EBOOK")

A3_choice_data <- A3_choice_data %>%
  filter(MaterialType == "EBOOK")
#Find total number of checkouts for all books of the series per year
PercyJ_checkouts <- A3_series_data %>%
  group_by(CheckoutYear) %>%
  summarize(Olympians_checkouts = sum(Checkouts))

#Find the average number of checkouts for each year
Kane_checkouts <- A3_choice_data %>%
  group_by(CheckoutYear) %>%
  summarize(Chronicles_checkouts = sum(Checkouts))
#Combine the two data frames
combined_data <- left_join(PercyJ_checkouts, Kane_checkouts, by = "CheckoutYear")
#create line chart
chart3 <- ggplot(combined_data, aes(x = CheckoutYear, color = "Legend")) +
  geom_line(aes(y = Olympians_checkouts, color = "Percy Jackson & the Olympians series")) +
  geom_line(aes(y = Chronicles_checkouts, color = "Kane Chronicles series")) +
  scale_x_continuous(breaks = seq(2013, 2023, 1)) +
  labs(title = "Ebook checkouts of Percy Jackson & the Olympian series & Kane chronicle series from 2013-2023",
       x = "Year",
       y = "Number of Checkouts",
       color = "Series Title") 