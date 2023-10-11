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
test_stations_metadata(stations_metadata_df)


### 5: Final volume query: 

source("gql-queries/vol_qry.r")

sampled_data <- stations_metadata_df %>%
  filter(latestData > Sys.Date() - days(7)) %>%
  sample_n(1) %$%
  {  
    id_sample <- .
    vol_data <- vol_qry(
      id = id_sample$id, 
      from = to_iso8601(id_sample$latestData, -4),
      to = to_iso8601(id_sample$latestData, 0)
    ) %>%
      GQL(., .url = configs$vegvesen_url) %>%
      transform_volumes()
    return(list(id_sample, vol_data))
  }

# Extract the id and volume data
id_sample <- sampled_data[[1]]
vol_data <- sampled_data[[2]]

# Create the plot with the id in the legend
ggplot(vol_data, aes(x = from, y = volume, color = as.factor(id_sample$name))) +
  geom_line() +
  theme_classic() +
  xlab("Date and time of observation") +
  ylab("Number of vehicles an hour") +
  scale_x_datetime(
    date_breaks = "1 day", 
    date_labels = "%Y-%m-%d %H:00"
  ) +
  scale_color_manual(
    name = "Name of station",
    values = c("black"),
    labels = id_sample$name 
  )
