library(httr)
library(jsonlite)
library(ggplot2)
library(DescTools)
library(tidyverse)
library(magrittr)
library(rlang)
library(lubridate)
library(anytime)
library(readr)
library(yaml)

#### 1: Beginning of script

# Load function for posting GQL-queries and retrieving data: 
source("functions/GQL_function.r")

# The URL we will use is stored below: 

configs <- 
  read_yaml("vegvesen_configs.yml")


gql_metadata_qry <- read_file("gql-queries/station_metadata.gql")

# Let's try submitting the query: 

stations_metadata <-
  GQL(
    query=gql_metadata_qry,
    .url = configs$vegvesen_url
    ) 


#### 2: Transforming metadata

source("functions/data_transformations.r")

stations_metadata_df <- 
  stations_metadata %>% 
  transform_metadata_to_df(.)


#### 3: Testing metadata
source("functions/data_tests.r")
test_stations_metadata(stations_metadata_df) # All tests are a pass :)


### 5: Final volume query: 

source("gql-queries/vol_qry.r")

stations_metadata_df %>% 
  filter(latestData > Sys.Date() - days(7)) %>% 
  sample_n(1) %$% # sample a random station
  vol_qry(
    id = id,
    from = to_iso8601(latestData, -4),
    to = to_iso8601(latestData, 0)
  ) %>% 
  GQL(., .url = configs$vegvesen_url) %>%
  transform_volumes() %>% 
  ggplot(aes(x=from, y=volume, group=1)) + 
  geom_line() + 
  theme_classic() 

### 6: Adjust codes so that we include the name of the selected traffic station

# Sample a random station from the last 7 days
station_sample <- stations_metadata_df %>% 
  filter(latestData > Sys.Date() - days(7)) %>% 
  sample_n(1)

# Query and retrieve traffic data for the selected station
traffic_data <- vol_qry(
  id = station_sample$id,
  from = to_iso8601(station_sample$latestData, -4),
  to = to_iso8601(station_sample$latestData, 0)
) %>% 
  GQL(.url = configs$vegvesen_url) %>%
  transform_volumes()

# Create and display the plot with the station name as the title
ggplot(traffic_data, aes(x = from, y = volume, group = 1)) + 
  geom_line() + 
  labs(title = paste("Traffic Volume for Station:", station_sample$name),
       x = "Date and Time",
       y = "Traffic Volume") +
  theme_classic() 

