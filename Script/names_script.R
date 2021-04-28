# Load libraries
library(tidyverse)
library(data.table)
library(scales)

# Load data
data <- fread("https://raw.githubusercontent.com/byuidatascience/data4names/master/data-raw/names_year/names_year.csv")

# Explore data
glimpse(data)
summary(data)

## Answer Questions

# 1. How does your name at your birth year compare to its use historically?
data %>% 
  filter(name == "John") %>% 
  mutate(Color = case_when(year == 1997 ~ "red",
                           TRUE ~ "grey")) %>% 
  ggplot(aes(x = year, y = Total, fill = Color)) +
  geom_bar(stat = "identity", width = 1) +
  labs(title = "The Name John Was Used a lot More Before 1997",
       x = "Year",
       y = "Total Times John Used") +
  theme_bw() +
  theme(legend.position="none",
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  scale_fill_manual(values = c("grey", "red")) +
  scale_y_continuous(label = comma) +
  scale_x_continuous(breaks = c(seq(from = 1910, to = 2015, by = 10)))
                                
# 2. If you talked to someone named Brittany on the phone, what is your guess of their age? What ages would you not guess?
data %>% 
  filter(name == "Brittany") %>% 
  mutate(Color = case_when(year == 1990 ~ "red",
                           TRUE ~ "grey")) %>% 
  ggplot(aes(x = year, y = Total, fill = Color)) +
  geom_bar(stat = "identity", width = 1) +
  labs(title = "The name Brittany Reached its Peak in the 1990s",
       subtitle = "A Good Age Guess Would be in the 30 Range, Above 40 or Below 20 Would be a Bad Guess",
       x = "Year",
       y = "Total Times Brittany Used") +
  theme_bw() +
  theme(legend.position="none",
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  scale_fill_manual(values = c("grey", "red")) +
  scale_y_continuous(label = comma) +
  scale_x_continuous(breaks = c(seq(from = 1910, to = 2015, by = 10)))

# 3. Mary, Martha, Peter, and Paul are all Christian names. From 1920 - 2000, compare the name usage of each of the four names.
data %>% 
  filter(name %in% c('Mary', 'Martha', 'Peter', 'Paul')) %>% 
  ggplot(aes(x = year, y = Total, fill = name)) +
  geom_area() +
  labs(title = "Martha and Mary are Far More Popular Names Than Paul and Peter",
       x = "Year",
       y = "Total Times Name Used") +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  scale_fill_brewer(palette="Spectral") +
  scale_y_continuous(label = comma) +
  scale_x_continuous(breaks = c(seq(from = 1920, to = 2000, by = 10)),
                     limits = c(1920, 2000))

# 4. Think of a unique name from a famous movie. Plot that name and see how increases line up with the movie release.
data %>% 
  filter(name == "Don") %>% 
  mutate(Color = case_when(year == 1952 ~ "red",
                           year > 1952 & year <= 1959 ~ "green",
                           TRUE ~ "grey")) %>% 
  ggplot(aes(x = year, y = Total, fill = Color)) +
  geom_bar(stat = "identity", width = 1) +
  labs(title = "The Name Don Shows a Huge Increase after Singing in the Rain Release (1952)",
       x = "Year",
       y = "Total Times Don Used") +
  theme_bw() +
  theme(legend.position="none",
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  scale_fill_manual(values = c("green", "grey", "red")) +
  scale_y_continuous(label = comma) +
  scale_x_continuous(breaks = c(seq(from = 1910, to = 2015, by = 10)))

