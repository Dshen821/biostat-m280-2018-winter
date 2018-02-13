library("tidyverse")
library("viridis")
library("forcats")
library("nycflights13")

# categorical
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

diamonds %>% 
  count(cut)

# continuous 
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

diamonds %>% 
  count(cut_width(carat, 0.5))

smaller <- diamonds %>% 
  filter(carat < 3)

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.1)

# overlapping
ggplot(data = smaller, mapping = aes(x = carat, colour = cut)) +
  geom_freqpoly(binwidth = 0.1)

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

  # sets y range from 0 to 50.
ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

unusual <- diamonds %>% 
  filter(y < 3 | y > 20) %>% 
  select(price, x, y, z) %>%
  arrange(y)
unusual


  # Question 7.3.4:
  # Explore the distribution of each of the x, y, and z variables in diamonds. What do you learn? Think about 
  # a diamond and how you might decide which dimension is the length, width, and depth.

library(ggplot2)
ggplot(data = diamonds, mapping = aes(x = x)) +
  geom_freqpoly(binwidth = 0.01) +
  coord_cartesian(xlim = c(0, 20)) -> p1

ggplot(data = diamonds, mapping = aes(x = y)) +
  geom_freqpoly(binwidth = 0.01) +
  coord_cartesian(xlim = c(0, 20)) -> p2

ggplot(data = diamonds, mapping = aes(x = z)) +
  geom_freqpoly(binwidth = 0.01) +
  coord_cartesian(xlim = c(0, 20)) -> p3

source("http://peterhaschke.com/Code/multiplot.R")
multiplot(p1, p2, p3, cols=2)

# x is length, y is width, z is depth


## Explore the distribution of price. Do you discover anything unusual or surprising? 
## (Hint: Carefully think about the binwidth and make sure you try a wide range of values.)
library(dplyr)

  # entire plot shows an unusual bump around ~8000.
ggplot(data = diamonds, mapping = aes(x = price)) +
  geom_freqpoly(binwidth = 1) -> p1

lower <- diamonds %>% 
  filter(price < 2000)
# zoom in on lower section and discover no price at ~1500.
ggplot(data = lower, mapping = aes(x = price)) +
  geom_histogram(binwidth = 10) -> p2
# zoom in on mid-section.
bigger <- diamonds %>%
  filter(price <10000 & price > 6000)
ggplot(data = bigger, mapping = aes(x = price)) +
  geom_histogram(binwidth = 50) -> p3

upper <- diamonds %>% 
  filter(price >5000)
# zoom in on upper section.
ggplot(data = upper, mapping = aes(x = price)) +
  geom_histogram(binwidth = 10) -> p4

multiplot(p1, p2, p3, p4, cols=2)

## diamond discrepancy 0.99/1.00
one <- diamonds %>% 
  filter(carat == 1 | carat == 0.99) %>% 
  count(carat)
one


## coord_cart vs xlim()/ylim()

ggplot(diamonds) +
  geom_histogram(mapping = aes(x = price)) +
  xlim(1000, 6000) +
  ylim(0, 3500) -> p1
# coord_cartesian()
ggplot(diamonds) +
  geom_histogram(mapping = aes(x = price)) +
  coord_cartesian(xlim = c(1000, 6000), ylim = c(0, 3500)) -> p2

multiplot(p2, p1, cols = 1)

ggplot(diamonds) +
  geom_histogram(mapping = aes(x = price)) +
  coord_cartesian(xlim = c(5999, 6000), ylim = c(0, 3500)) -> p2

ggplot(diamonds) +
  geom_histogram(mapping = aes(x = price)) +
  xlim(5999, 6000) +
  ylim(0, 3500) -> p1

multiplot(p2, p1, cols = 1)

  # 7.4.1
  # Creating a subset from diamonds, removing lower and upper length.
diamondMidLength <- diamonds %>%
  mutate(x = ifelse(x < 3 | x > 20, NA, x)) %>%
ggplot(diamondMidLength) +
  geom_histogram(mapping = aes(x = x))

  # Creating a subset from
n<-nrow(diamonds)
diamondMidClarity <- diamonds %>%
  mutate( clarity = if_else(rnorm(n) < 0.0001, NA_character_, as.character(cut))) 
ggplot(diamondMidClarity) +
  geom_bar(mapping = aes(x = clarity))


  # What does na.rm = TRUE do in mean() and sum()?

  # In this case, we are asking which elements of (ranVals %in% sample) are TRUE:
ranVals <- rnorm(n = 100, mean = 10, sd = 10)
  # Randomly chooses 20 indices to be replaced with NA
miss <- which(ranVals %in% sample(ranVals,20))
ranVals[miss] <- NA

mean(ranVals, na.rm = FALSE)
sum(ranVals, na.rm = FALSE)

mean(ranVals, na.rm = TRUE)
sum(ranVals, na.rm = TRUE)