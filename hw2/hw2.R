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
  mutate(x = ifelse(x < 3 | x > 20, NA, x))
ggplot(diamondMidLength) +
  geom_histogram(mapping = aes(x = x))

  # Creating a subset from
n<-nrow(diamonds)
diamondMidClarity <- diamonds %>%
  mutate( clarity = if_else(rnorm(n) < 1, NA_character_, as.character(cut))) 
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

# Use what you’ve learned to improve the visualisation of the departure times of
# cancelled vs. non-cancelled flights.
library(nycflights13)



  # If the departure time is na = canceled flight;

flightSub <- nycflights13::flights %>%
  mutate( canceledFlight = is.na(dep_time),
          min = (sched_dep_time %% 100) / 60,
          hour = sched_dep_time %/% 100,
          departureTime = hour + min
        ) %>%
  ggplot() + geom_boxplot(mapping = aes(y = departureTime, x = canceledFlight))


# 2) What variable in the diamonds dataset is most important for predicting the price of a diamond?
#How is that variable correlated with cut? Why does the combination of those
#two relationships lead to lower quality diamonds being more expensive?

  #lvplot shows that carat is significant for predicting price of diamond.
# install.packages("lvplot")
library(lvplot)
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_lv()


# appears to be a slight negative correlation between cut and carat. As cut improves, carat decreases.
# fair diamonds have greater carat
ggplot(data = diamonds) +
  geom_boxplot(aes(cut, carat))

ggplot(diamonds, aes(carat, colour = cut)) +
  geom_density(position = "dodge")

diamonds %>%
  group_by(cut) %>%
  summarise(cor(carat, price))

# Carat and price are highly correlated between and within diamond quality.
# Carat is most important for predicting the price of diamond and Carat is negatively correlated with cut
# Consequently as quality of cut increases, price of diamond decreases. (Thus, lower quality cuts, on average, are higher in price)

# Install the ggstance package, and create a horizontal boxplot. How does this compare to using coord_flip()?
library(ggstance)

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
  coord_flip() -> p1

ggplot(data = mpg) +
  geom_boxploth(mapping = aes(x = hwy,
                              y = reorder(class, hwy, FUN = median))
                              ) -> p2
multiplot(p1, p2, numcol = 1)

# One problem with boxplots is that they were developed in an era of much smaller datasets and tend to 
# display a prohibitively large number of “outlying values”. One approach to remedy this problem is the 
# letter value plot. Install the lvplot package, and try using geom_lv() to display the distribution of price vs cut. What do you learn? How do you interpret the plots?

  # install.packages("lvplot")
library(lvplot)
ggplot(diamonds, aes(x = cut, y = price)) +
  geom_lv()

# Compare and contrast geom_violin() with a facetted geom_histogram(), or a coloured geom_freqpoly(). What are the pros and cons of each method?

ggplot(data = diamonds, mapping = aes(x = price, colour = cut)) +
  geom_freqpoly(binwidth = 500)

ggplot(data = diamonds, mapping = aes(x = price, colour = cut)) +
  geom_histogram(binwidth = 50) +
  facet_wrap(~cut, ncol = 1, scales = "free_y")

ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_violin(aes(fill = cut)) +
  coord_flip()

# If you have a small dataset, it’s sometimes useful to use geom_jitter() to see the relationship between a continuous and categorical variable. The ggbeeswarm 
# package provides a number of methods similar to geom_jitter(). List them and briefly describe what each one does.

  # install.packages("ggbeeswarm")
library("ggbeeswarm")


ggplot(data = mpg) +
  geom_jitter(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) -> p1

ggplot(data = mpg) +
  geom_quasirandom(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) -> p2

ggplot(data = mpg) +
  geom_beeswarm(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) -> p3

multiplot(p1, p2, p3, col = 1)


## 7.5.2.1
#How could you rescale the count dataset above to more clearly show the distribution of cut within colour, or colour within cut?

  # Cut within Colour.
diamonds %>% 
  count(color, cut) %>%
  group_by(color) %>%
  mutate(percentage = n / sum(n)) %>%
  ggplot(mapping = aes(x = color, y = cut, fill = percentage)) +
  geom_tile() -> p1
  # Colour within Cut.
diamonds %>% 
  count(color, cut) %>%
  group_by(cut) %>%
  mutate(percentage = n / sum(n)) %>%
  ggplot(mapping = aes(x = color, y = cut, fill = percentage)) +
  geom_tile() -> p2

multiplot(p1, p2, ncol = 2)

# Use geom_tile() together with dplyr to explore how average flight delays vary by destination and month of year.
# What makes the plot difficult to read? How could you improve it?

library(viridis)

flights %>%
  group_by(dest, month) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(month), y = dest, fill = dep_delay)) +
  geom_tile() +
  labs( x = "Month of Departure", y = "Destination", fill = "Delay Time")

  # Here we are ensuring plotted points have a delay in departure AND flights in all 12 months. 
flights %>%
  filter(dep_delay > 0) %>%
  group_by(dest, month) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  filter (n() == 12) %>%
  ungroup() %>%
  ggplot(aes(x = factor(month), y = dest, fill = dep_delay)) +
  geom_tile()

  # 3) Why is it slightly better to use aes(x = color, y = cut) rather than aes(x = cut, y = color) in the example above?

diamonds %>% 
  count(color, cut) %>%  
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n)) -> p1

diamonds %>% 
  count(color, cut) %>%  
  ggplot(mapping = aes(x = cut, y = color)) +
  geom_tile(mapping = aes(fill = n)) -> p2

multiplot (p1, p2, ncol = 2)

# 1) Instead of summarising the conditional distribution with a boxplot, you could use a frequency polygon. What do you need
#to consider when using cut_width() vs cut_number()? How does that impact a visualisation of the 2d distribution of carat and price?

ggplot(data = diamonds,
       mapping = aes(x = price,
                     colour = cut_width(carat, 0.5))) +
  geom_freqpoly() -> p1

ggplot(data = diamonds,
       mapping = aes(x = price,
                     y = ..density..,
                     colour = cut_width(carat, 0.5))) +
  geom_freqpoly() -> p2

ggplot(data = diamonds,
       mapping = aes(x = price,
                     colour = cut_number(carat, 10))) +
  geom_freqpoly() -> p3

ggplot(data = diamonds,
       mapping = aes(x = price,
                     y = ..density..,
                     colour = cut_number(carat, 10))) +
  geom_freqpoly() -> p4

multiplot(p1, p2, p3, p4, ncol = 2)
