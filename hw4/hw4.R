
  # Disconnect spark.
spark_disconnect_all()

library(sparklyr)
library(dplyr)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(ggrepel)
library(lubridate)
  Sys.setenv(SPARK_HOME="/usr/lib/spark")
  config <- spark_config()
  sc <- spark_connect(master = "local", config = config)

flights_tbl <- tbl(sc,'flights')
airports_tbl <- tbl(sc, 'airports')
airlines_tbl <- tbl(sc, 'airlines')

head(flights_tbl)
head(airports_tbl)
head(airlines_tbl)

#Map the top 10 busiest airports. Size of dots should reflect the number of 
#flights through that destination.


  # Determine the n count of flight origins
originCount <- flights_tbl %>%
  filter(year == 1991) %>%
  group_by(origin) %>%
  tally() %>%
  arrange(desc(n)) %>%
  collect() %>%
  head(15)
  # Determine the n count of flight destinations
departureCount <- flights_tbl %>%
  filter(year == 1991) %>%
  group_by(dest) %>%
  tally() %>%
  arrange(desc(n)) %>%
  collect() %>%
  head(15)

colnames(originCount) <- c("origin", "outgoing")
colnames(departureCount) <- c("destination", "incoming")

  # Determine the "busiest" airports by summing incoming and outcoming flights.
flightCount <-originCount %>%
  inner_join(departureCount, by = c("origin" = "destination")) %>%
  transmute(airport = origin, totalFlights = incoming + outgoing) %>%
  arrange(desc(totalFlights)) %>%
head(10)

  # Location Retrieval.
topTenLocations <- flightCount %>%
  inner_join(airports_tbl, by = c("airport" = "faa"), copy = TRUE) %>%
  select(airport, totalFlights, lon, lat) %>%
  mutate(lat = as.double(lat),
         lon = as.double(lon),
         proportion = totalFlights/sum(totalFlights)) %>%
  collect()

  # Create compressed RDS  file.
library(tidyverse)
write_rds(topTenLocations, "hw4/busiestAirports.rds")

  # Create a plot of the US MAP.

usmap <- map_data("usa")
states <- map_data("state")

map <- get_map(location = "united states", zoom = 4)

busyUSports <- ggmap(map) +
  geom_point(data = topTenLocations,
             aes(x = lon, y = lat, size = proportion),
                 color = "black") +
  geom_label_repel(data = topTenLocations,
                   aes(x= lon, y = lat, label =  airport, size = proportion
                      ), min.segment.length = 0, size = 3.5) +
  labs(title = " US Airports (Highest # of flights in/out) in 1991")
busyUSports

#Map the top 10 busiest direct routes.
#Size of lines should reflect the number of flights through that route.

topRoutes <-flights_tbl %>%
  filter(year == 2002) %>%
  group_by(origin, dest) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  collect() %>%
  head(10)

# Retrieve latitude and longitide of origin and dest

topTenOrigin<- topRoutes %>%
  inner_join(airports_tbl, by = c("origin" = "faa"), copy = TRUE) %>%
  select(origin, lat, lon, name,n) %>%
  mutate(latO = as.double(lat),
         lonO = as.double(lon)) %>%
  collect()

topTenDest <- topRoutes %>%
  inner_join(airports_tbl, by = c("dest" = "faa"), copy = TRUE) %>%
  select(origin, lat, lon, name,n) %>%
  mutate(latD = as.double(lat),
         lonD = as.double(lon)) %>%
  collect()

q2 <- bind_cols(topTenOrigin, topTenDest)

q2 <- q2 %>%
  mutate(scale = (n / max(n)) ^ 10)

 #  For the labels, we want to remove duplicate origins so that its not cluttered: 3 different ways to do so:
q2Sliced <- q2 %>%
  group_by(origin) %>%
  filter(row_number() == 1)

q2Sliced2 <- q2 %>%
  .[c(1:6, 8),]

q2Sliced3 <- q2 %>%
  slice(c(1:6,8))

### google map
map <- get_map(location = "united states", zoom = 4)
ggmap(map) +
  geom_point(data = q2,
             aes(x = lonO, y = latO),
             colour = "red", alpha = 0.2) +
  geom_point(data = q2,
             aes(x = lonD, y = latD),
             colour = "red", alpha = 0.2)+
  geom_curve(data = q2,
             aes(x = lonO, y = latO, xend = lonD, yend = latD),    
             arrow = arrow(angle = 15, ends = "first", length = unit(0.2, "cm"), type = "closed"),
             alpha = 0.7, curvature = 0.15, inherit.aes = TRUE) +
  scale_size_continuous(range=c(1,30)) +
  coord_cartesian()


#### simple map

usmap <- map_data("usa")
states <- map_data("state")
unitedStates <- ggplot(data = states) +
geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
coord_fixed(1.3) +
  geom_point(data = q2,
             aes(x = lonO, y = latO),
             colour = "red", alpha = 0.2) +
  geom_point(data = q2,
             aes(x = lonD, y = latD),
             colour = "red", alpha = 0.2)+
  geom_curve(data = q2,
             aes(x = lonO, y = latO, xend = lonD, yend = latD),    
             arrow = arrow(angle = 15, ends = "first", length = unit(0.5, "cm"), type = "closed"),
             alpha = 0.7, curvature = 0.15, size = q2$scale, inherit.aes = TRUE) +
  scale_size_continuous(range=c(1,30)) +
  coord_cartesian() +
  guides(fill=FALSE) +
  geom_label_repel(data = q2Sliced,
                   aes(x= lonO, y = latO, label = q2Sliced$origin, size = 2)
                  , min.segment.length = 0, size = 3.5) +
  ggtitle("Busiest US Routes in 2002")
unitedStates





#(a). Reproduce above plot. Visualize and explain some prominent features you observe. For example, what happened at points 1-5?
laxFlights <-flights_tbl %>%
  filter(origin == "LAX" | dest == "LAX") %>%
  select(origin, dayofmonth, month, year) %>%
  collect()

#Subset Between 2008 and 1998 Years of LAX flights.
laxFlightDate <- laxFlights %>%
  mutate(date = make_date(year, month, dayofmonth)) %>%
  filter(date >= ymd(19980101) & date <= ymd(20081231)) %>%
  collect()

# Create compressed RDS file.
write_rds(laxFlightDate, "hw4/laxflightdates.rds")

# Plot

laxFlightDate %>%
  group_by(date) %>%
  count() %>%
  ggplot(aes(x = date, y = n)) +
  geom_line() +
  geom_label(aes(x = ymd(20011230), y = 1110, label = "1")) +
  geom_label(aes(x = ymd(20041201), y = 1000, label = "2")) +
  geom_label(aes(x = ymd(20040630), y = 950, label = "3")) +
  geom_label(aes(x = ymd(20080101), y = 1300, label = "4")) +
  geom_label(aes(x = ymd(20010130), y = 1150, label = "5")) +
  ggtitle("LAX air traffic") 

 # Visualize prominent features.

# Point 1
laxFlightDate  %>%
  group_by(date) %>%
  filter(date >= ymd(20010901) & date <= ymd(20011201)) %>%
  group_by(date) %>%
  count() %>%
  arrange(n) %>%
  collect() %>%
  ggplot(aes(x = date, y = n)) +
  geom_line() +
  ggtitle("Point 1")
  
  

# Point 2
laxFlightDate  %>%
  group_by(date) %>%
  filter(date >= ymd(20040731) & date <= ymd(20050101)) %>%
  group_by(date) %>%
  count() %>%
  arrange(n) %>%
  collect() %>%
  ggplot(aes(x = date, y = n)) +
  geom_line() +
  ggtitle("Point 2")


# Point 3
laxFlightDate  %>%
  group_by(date) %>%
  filter(date >= ymd(20040301) & date <= ymd(20041101)) %>%
  group_by(date) %>%
  count() %>%
  arrange(n) %>%
  collect() %>%
  ggplot(aes(x = date, y = n)) +
  geom_line() +
  ggtitle("Point 3")

# Point 4
laxFlightDate  %>%
  group_by(date) %>%
  filter(date >= ymd(20070928) & date <= ymd(20080130)) %>%
  group_by(date) %>%
  count() %>%
  arrange(n) %>%
  collect() %>%
  ggplot(aes(x = date, y = n)) +
  geom_line() +
  ggtitle("Point 4")

# Point 5
laxFlightDate  %>%
  group_by(date) %>%
  filter(date >= ymd(20070928) & date <= ymd(20080130)) %>%
  group_by(date) %>%
  count() %>%
  arrange(n) %>%
  collect() %>%
  ggplot(aes(x = date, y = n)) +
  geom_line() +
  ggtitle("Point 5")
# (b). Visualize and explain seasonal effects.

flightsBySeason <-flights_tbl %>%
  select(origin, dest, month, year) %>%
  filter(year >= 1998 && year <= 2008) %>%
  filter(dest == "LAX" | origin == "LAX") %>%
  mutate(season = if_else(month %in% c(3, 4, 5), "spring",
                          if_else(month %in% c(6, 7, 8), "summer",
                                  if_else(month %in% c(9,10, 11), "fall",
                                          if_else(month %in% c(12, 1, 2), "winter", "")
                                          )
                                  )
                          )
        ) %>%
  #group_by(year, season) %>%
  #tally() %>%
  count(year, season) %>%
  arrange(year) %>%
  collect()

ggplot(flightsBySeason) +
  geom_col(aes(x = as.factor(year), y = n, fill = season),
           color = "black") +
  labs(x = "Year", y = "Number of Flights", title = "LAX Air Traffic by Season") +
  scale_fill_brewer(palette = "Pastel2")
#(c). Visualize and explain weekly effects.
flightsByWeekday <- flights_tbl %>%
  select(origin, dest, year, dayofweek) %>%
  filter(year >= 1998 && year <= 2008) %>%
  filter(dest == "LAX" | origin == "LAX") %>%
  count(year, dayofweek) %>%
  collect()

flightsByWeekday$dayofweek<-as.factor(flightsByWeekday$dayofweek)
ggplot(flightsByWeekday) +
  geom_col(aes(x = as.factor(year), y = n, fill = dayofweek),
           color = "black") +
  labs(x = "Year", y = "Number of Flights", title = "LAX Air Traffic by Day of Week") +
  scale_fill_brewer(palette = "Pastel2")

#(d). Map top 10 destinations from LAX. Size of dots should reflect the number of flights from LAX to that destination.

# Filter the routes by destination: LAX.
topRoutes <-flights_tbl %>%
  select(origin, dest) %>%
  filter(origin == "LAX") %>%
  collect() %>% 
  count(dest) %>%
  arrange(desc(n)) %>%
  head(10)

# Location Retrieval.
destLAX <- topRoutes %>%
  left_join(airports_tbl, by = c("dest" = "faa"), copy = TRUE) %>%
  select(dest, lon, lat, n) %>%
  mutate(lat = as.double(lat),
         lon = as.double(lon)
         ) %>%
  mutate(airport = dest, proportion = n / sum(n))

ggmap(map) +
  geom_point(data = destLAX,
             aes(x = lon, y = lat, size = proportion),
             color = "black") +
  geom_label_repel(data = destLAX,
                   aes(x= lon, y = lat, label =  dest, size = proportion
                   ), min.segment.length = 0, size = 3.5) +
  labs(title = "Top 10 Locations from LAX")
