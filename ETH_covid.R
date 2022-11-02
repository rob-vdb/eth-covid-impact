#### 1) Installing Packages ####

library(ggplot2)
library(tidyverse)
library(dplyr)
library(lubridate)
library(scales)
library(cowplot)
library(patchwork)
library(httr)
library(jsonlite)
library(RCurl)
library(jsonlite)

#### 2) Loading Data ####

# Each sub section here is the same.
# Set the parameters needed in the url to retrieve daily data for ETH Spot and Futures prices via the FTX API.
# The results are stored as an object called res (a JSON file).
# The 'content' component of each JSON file will have the data. We use 2 functions (the rawToChat and fromJSON functions) to extract the data.

##### 2.a) ETH-Spot #####

SYMBOL <- "ETH/USD"
resolution = 60*60*24 
start <- "2019-12-01 00:00:00 UTC"
START <- as.numeric(as.POSIXct(start))
end <- "2020-06-28 00:00:00 UTC"
END <- as.numeric(as.POSIXct(end))

url <- paste0("https://ftx.com/api/markets/", SYMBOL,"/candles?resolution=", resolution,"&start_time=", START,"&end_time=",END)

res <- GET(url)

data = fromJSON(rawToChar(res$content))
data
names(data)

ETH_Spot <- data$result
ETH_Spot

# Here we can see there is an extra time column, which can be discarded.
ETH_Spot <- ETH_Spot %>% subset(select = -c(time))
ETH_Spot <- ETH_Spot %>% rename(time = startTime)
glimpse(ETH_Spot)

# Here we can see that the date and time variable is of class character.
# We can use ymd_hmd() from lubridate to fix that.

class(ETH_Spot$time)
ETH_Spot$time <- ymd_hms(ETH_Spot$time)
class(ETH_Spot$time)

# Converting the data set to tidy format for use in ggplot. 
ETH_Spot <- ETH_Spot %>% pivot_longer(cols = open:volume, names_to = "OHLCV", values_to = "Prices") 

ETH_Spot <- ETH_Spot %>% mutate(Asset = "Spot")

ETH_Spot

##### 2.b) ETH-March 2020 Future #####

SYMBOL <- "ETH-20200327"
resolution = 60*60*24
start <- "2019-12-01 00:00:00 UTC"
START <- as.numeric(as.POSIXct(start))
end <- "2020-06-28 00:00:00 UTC"
END <- as.numeric(as.POSIXct(end))

url <- paste0("https://ftx.com/api/markets/", SYMBOL,"/candles?resolution=", resolution,"&start_time=", START,"&end_time=",END)

res <- GET(url)

data = fromJSON(rawToChar(res$content))
data
names(data)

ETH_20200327 <- data$result
ETH_20200327

# Here we can see there is an extra time column, which can be discarded.
ETH_20200327 <- ETH_20200327 %>% subset(select = -c(time))
ETH_20200327 <- ETH_20200327 %>% rename(time = startTime)
glimpse(ETH_20200327)

# Here we can see that the date and time variable is of class character.
# We can use ymd_hmd() from lubridate to fix that.

class(ETH_20200327$time)
ETH_20200327$time <- ymd_hms(ETH_20200327$time)
class(ETH_20200327$time)

# Converting the data set to tidy format for use in ggplot. 
ETH_20200327 <- ETH_20200327 %>% pivot_longer(cols = open:volume, names_to = "OHLCV", values_to = "Prices") 

ETH_20200327 <- ETH_20200327 %>% mutate(Asset = "Mar-27")

ETH_20200327

##### 2.c) ETH-June 2020 Future #####

SYMBOL <- "ETH-20200626"
resolution = 60*60*24
start <- "2019-12-01 00:00:00 UTC"
START <- as.numeric(as.POSIXct(start))
end <- "2020-06-28 00:00:00 UTC"
END <- as.numeric(as.POSIXct(end))

url <- paste0("https://ftx.com/api/markets/", SYMBOL,"/candles?resolution=", resolution,"&start_time=", START,"&end_time=",END)

res <- GET(url)

data = fromJSON(rawToChar(res$content))
data
names(data)

ETH_20200626 <- data$result
glimpse(ETH_20200626)

# Here we can see there is an extra time column, which can be discarded.
ETH_20200626 <- ETH_20200626 %>% subset(select = -c(time))
ETH_20200626 <- ETH_20200626 %>% rename(time = startTime)
glimpse(ETH_20200626)

# Here we can see that the date and time variable is of class character.
# We can use ymd_hmd() from lubridate to fix that.

class(ETH_20200626$time)
ETH_20200626$time <- ymd_hms(ETH_20200626$time)
class(ETH_20200626$time)

# Converting the data set to tidy format for use in ggplot. 
ETH_20200626 <- ETH_20200626 %>% pivot_longer(cols = open:volume, names_to = "OHLCV", values_to = "Prices") 

ETH_20200626 <- ETH_20200626 %>% mutate(Asset = "Jun-26")

ETH_20200626

#### 3) Joining Datasets ####

ETH <- full_join(ETH_Spot, ETH_20200327, by = c("time", "Asset", "Prices", "OHLCV"))

ETH <- full_join(ETH, ETH_20200626, by = c("time", "Asset", "Prices", "OHLCV"))

glimpse(ETH)

# ETH is very big. Since we are mostly concerned with closing prices, we will create a separate dataset with closing prices to use when the other prices are not relevant.

ETH_close <- ETH %>% filter(OHLCV == "close")

ETH_close <- ETH_close %>% select(-OHLCV)

glimpse(ETH_close)

#### 4) First Graphs ####

# We create the first graphic, which is just a plot of all the prices over time:
all_plot <- ggplot(data = ETH_close) +
  geom_line(aes(x = time, y = Prices, color = Asset)) +
  theme(legend.position = "right") +
  scale_color_manual(name = "Ethereum",
                     breaks = c("Spot", "Mar-27", "Jun-26"),
                     labels = c("Spot", "Mar-27", "Jun-26"), 
                     values = c("red", "blue", "green") ) +
  scale_y_continuous(name = "Price (USD)") +
  scale_x_datetime(name = "Time", breaks = date_breaks("1 month"), labels = date_format("%m-%Y")) +
  theme_light() +
  theme(legend.position = "top", axis.text.x = element_text(angle = 45, hjust = 1))

all_plot

# While not useless, this is not very informative. It is  unclear what the relationship is between the prices of the different assets. We will instead look at just the March-25 future and the spot over a smaller time interval.

Mar_plot <-ggplot(data = 
                    ETH_close %>% 
                    filter(time >= min(ETH_20200327$time) & 
                             time <= max(ETH_20200327$time) &
                             (Asset == "Spot" | Asset == "Mar-27") ) ) +
  geom_line(aes(x = time, y = Prices, color = Asset)) +
  scale_color_manual(name = "Ethereum",
                     breaks = c("Spot", "Mar-27"),
                     labels = c("Spot", "Mar-27"), 
                     values = c("red", "blue") ) +
  scale_y_continuous(name = "Price (USD)") +
  scale_x_datetime(name = "Time", breaks = date_breaks("1 month"), labels = date_format("%m-%Y")) +
  theme_light() +
  theme(legend.position = "top", axis.text.x = element_text(angle = 45, hjust = 1))

Mar_plot

# This adds the 2 plots so that they are displayed side by side (only possible after installing the patchwork package):
all_plot + Mar_plot

# While better, this is still not very clear. The prices seem to be too close together the closer we get to the expiration date (this is actually a well documented phenomenon). We need to be able to get a better idea of what the difference (i.e. spread) between them was. In order to do this we need to pivot the data set wider so that we can calculate the difference between the spot price and the futures price at every hour interval.

# While better, this is still not very clear. The prices seem to be too close together the closer we get to the expiration date (this is actually a well documented phenomenon). We need to be able to get a better idea of what the difference (i.e. spread) between them was. In order to do this we need to pivot the data set wider so that we can calculate the difference between the spot price and the futures price at every hour interval.

#### 5) Spreads Graphs ####

ETH_close <- ETH_close %>% pivot_wider(id_cols = time, names_from = Asset, values_from = Prices)

ETH_close

# We add Spread columns to the dataset defined by "Future price - Spot price" for all the futures:

ETH_close <- ETH_close %>% mutate(Mar_spread = `Mar-27` - Spot, Jun_spread = `Jun-26` - Spot)

# Plot of March Spreads:
Mar_spread_plot <- 
  ggplot(data = ETH_close %>% 
           filter(time >= min(ETH_20200327$time) 
                  & time < max(ETH_20200327$time))) +
  aes(x = time) +
  geom_line(aes( y = Mar_spread, color = "blue")) +
  scale_color_manual(name = "Ethereum Mar Futures Spread 2020",
                     labels = c("Mar-27 - Spot"), 
                     values = c("blue") ) +
  scale_y_continuous(name = "Spread (USD)") +
  scale_x_datetime(name = "Time", breaks = date_breaks("1 week"), labels = date_format("%d-%m-%Y")) +
  theme_light() +
  theme(legend.position = "top", 
        axis.text.x = element_text(angle = 45, hjust = 1))

Mar_spread_plot

# Plot of June Spreads:
Jun_spread_plot <- 
  ggplot(data = ETH_close %>% 
           filter(time >= min(ETH_20200626$time) 
                  & time < max(ETH_20200626$time))) +
  aes(x = time) +
  geom_line(aes( y = Jun_spread, color = "black")) +
  scale_color_manual(name = "Ethereum Jun Futures Spread 2020",
                     labels = c("Jun-26 - Spot"), 
                     values = c("black") ) +
  scale_y_continuous(name = "Spread (USD)") +
  scale_x_datetime(name = "Time", breaks = date_breaks("1 week"), labels = date_format("%d-%m-%Y")) +
  theme_light() +
  theme(legend.position = "top", 
        axis.text.x = element_text(angle = 45, hjust = 1))

Jun_spread_plot

#### 6) Extra: Relative Spreads ####

# It is easier to get an idea of the changes in the spread by looking at the relative spread - which is just the spread divided by the spot price. 

ETH_close <- ETH_close %>% mutate(Mar_rel_spread = (`Mar-27` - Spot)/Spot, Jun_rel_spread = (`Jun-26` - Spot)/Spot )

# Plot of March Relative Spread:
Mar_rel_spread_plot <- 
  ggplot(data = ETH_close %>% 
           filter(time >= min(ETH_20200327$time) 
                  & time < max(ETH_20200327$time))) +
  aes(x = time) +
  geom_line(aes( y = Mar_rel_spread, color = "blue")) +
  scale_color_manual(name = "ETH Mar Futures Relative Spread 2020",
                     labels = c("Mar-27 - Spot"), 
                     values = c("blue") ) +
  scale_y_continuous(name = "Relative Spread") +
  scale_x_datetime(name = "Time", breaks = date_breaks("1 week"), labels = date_format("%d-%m-%Y")) +
  theme_light() +
  theme(legend.position = "top", 
        axis.text.x = element_text(angle = 45, hjust = 1))

Mar_rel_spread_plot

# Plot of June Relative Spread:
Jun_rel_spread_plot <- 
  ggplot(data = ETH_close %>% 
           filter(time >= min(ETH_20200626$time) 
                  & time < max(ETH_20200626$time))) +
  aes(x = time) +
  geom_line(aes( y = Jun_rel_spread, color = "black")) +
  scale_color_manual(name = "ETH Jun Futures Relative Spread 2020",
                     labels = c("Jun-26 - Spot"), 
                     values = c("black") ) +
  scale_y_continuous(name = "Relative Spread") +
  scale_x_datetime(name = "Time", breaks = date_breaks("1 week"), labels = date_format("%d-%m-%Y")) +
  theme_light() +
  theme(legend.position = "top", 
        axis.text.x = element_text(angle = 45, hjust = 1))

Jun_rel_spread_plot

# This adds the 2 plots so that they are displayed side by side (only possible after installing the patchwork package, better in full screen):

Mar_rel_spread_plot + Mar_spread_plot

Jun_rel_spread_plot + Jun_spread_plot

