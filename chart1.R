#Question: Which material type had more checkouts for the Percy Jackson and the Olympians series from 2013-2023?
#load libraries
library("dplyr")
library("ggplot2")
#load data into variable called A3_series_data
A3_series_data <- read.csv("Percy_Jackson.csv")
#Filter only the checkouts with material type Ebook, and find the total for ebook checkouts per year 
Ebook_checkouts <- A3_series_data %>%
  filter(MaterialType == "EBOOK") %>%
  group_by(CheckoutYear) %>%
  summarize(Ebook_total = sum(Checkouts))
#Filter only the checkouts with material type Audiobook, and find the total for audiobook checkouts per year 
Audiobook_checkouts <- A3_series_data %>%
  filter(MaterialType == "AUDIOBOOK") %>%
  group_by(CheckoutYear) %>%
  summarize(Audiobook_total = sum(Checkouts))
#Combine the two data frames
combined_data <- left_join(Ebook_checkouts, Audiobook_checkouts, by = "CheckoutYear")
#create line chart
chart1 <- ggplot(combined_data, aes(x = CheckoutYear, color = MaterialType)) +
  geom_line(aes(y = Ebook_total, color = "EBOOK")) +
  geom_line(aes(y = Audiobook_total, color = "AUDIOBOOK")) +
  scale_x_continuous(breaks = seq(2012, 2022, 1)) +
    labs(title = "Checkouts of Percy Jackson & Olympian series EBooks & Audiobooks from 2012-2022",
         x = "Year",
         y = "Number of Checkouts",
         color = "MaterialType") 




