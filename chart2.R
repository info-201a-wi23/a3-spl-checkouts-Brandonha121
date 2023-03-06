#Question: Which book in the Percy Jackson and olympian series had the most ebook checkouts?
#load libraries
library("dplyr")
library("ggplot2")

#load data into variable called A3_series_data
A3_series_data <- read.csv("Percy_Jackson_Riordan.csv")

#Filter only ebooks
A3_series_data <- A3_series_data %>% 
  filter(MaterialType == "EBOOK")
#Filter only the checkouts for each book, and find the total checkouts per year
Title1_checkouts <- A3_series_data %>%
  filter(Title == "The Lightning Thief: Percy Jackson and the Olympians Series, Book 1") %>%
  group_by(CheckoutYear) %>%
  summarize(Title1_total = sum(Checkouts))

Title2_checkouts <- A3_series_data %>%
  filter(Title == "The Sea of Monsters: Percy Jackson and the Olympians Series, Book 2") %>%
  group_by(CheckoutYear) %>%
  summarize(Title2_total = sum(Checkouts))

Title3_checkouts <- A3_series_data %>%
  filter(Title == "The Titan's Curse: Percy Jackson and the Olympians Series, Book 3") %>%
  group_by(CheckoutYear) %>%
  summarize(Title3_total = sum(Checkouts))

Title4_checkouts <- A3_series_data %>%
  filter(Title == "The Battle of the Labyrinth: Percy Jackson and the Olympians Series, Book 4") %>%
  group_by(CheckoutYear) %>%
  summarize(Title4_total = sum(Checkouts))

Title5_checkouts <- A3_series_data %>%
  filter(Title == "The Last Olympian: Percy Jackson and the Olympians Series, Book 5") %>%
  group_by(CheckoutYear) %>%
  summarize(Title5_total = sum(Checkouts))

#Combine the data frames into a single data frame
combined_data <- left_join(Title1_checkouts, Title2_checkouts, by = "CheckoutYear") %>%
  left_join(Title3_checkouts, by = "CheckoutYear") %>%
  left_join(Title4_checkouts, by = "CheckoutYear") %>%
  left_join(Title5_checkouts, by = "CheckoutYear")
#Create line chart
chart2 <- ggplot(combined_data, aes(x = CheckoutYear, color = Title)) +
  geom_line(aes(y = Title1_total, color = "The Lightning Thief, Book 1")) +
  geom_line(aes(y = Title2_total, color = "The Sea of Monsters, Book 2")) +
  geom_line(aes(y = Title3_total, color = "The Titan's Curse, Book 3")) +
  geom_line(aes(y = Title4_total, color = "The Battle of the Labyrinth, Book 4")) +
  geom_line(aes(y = Title5_total, color = "The Last Olympian, Book 5")) +
  scale_x_continuous(breaks = seq(2013, 2023, 1)) +
  labs(title = "Checkouts of Percy Jackson and the Olympians Series (2013-2023)",
       x = "Year",
       y = "Number of Checkouts",
       color = "Book Titles")