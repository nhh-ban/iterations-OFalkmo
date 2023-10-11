#Function to transform the df 
transform_metadata_to_df = function(x) {
  result_df <- stations_metadata[[1]] %>% 
    map(as_tibble) %>% 
    list_rbind() %>% 
    mutate(latestData = as.character(latestData)) %>% 
    mutate(latestData = as_datetime(latestData, tz = "UTC")) %>% 
    unnest_wider(location) %>% 
    unnest_wider(latLon)
  
  return(result_df)
}

#function to transform the time to desired format and offset
to_iso8601 = function(time, offset) {
  
  # Add the offset in days to the date time
  adjusted_date_time <- time + lubridate::days(offset)
  
  # Format the adjusted date and time in ISO8601 format with "Z"
  iso8601_date_time <- format(adjusted_date_time, format = "%Y-%m-%dT%H:%M:%SZ")
  
  return(iso8601_date_time)
}

#Function to flatten the list from the query from Vegvesenet 
flatten_nested_list <- function(data_list) {
  if (is.list(data_list)) {
    # If the input is a list, recursively flatten its elements
    result_df <- data.frame()
    for (item in data_list) {
      item_df <- flatten_nested_list(item)
      result_df <- rbind(result_df, item_df)
    }
    return(result_df)
  } else {
    # If the input is not a list, create a data frame with the value
    result_df <- data.frame(value = data_list)
    return(result_df)
  }
}

#Function to splitt the rows from the flatten function into three columns.
#Split every row into the three column based on the pattern of the df where 
#each variable is repeted every third row.
split_rows_into_columns <- function(input_df) {
  n <- nrow(input_df)
  
  # Check if the number of rows is a multiple of 3
  if (n %% 3 != 0) {
    stop("Number of rows should be a multiple of 3.")
  }
  
  # Initialize the result data frame
  result_df <- data.frame(
    from = rep(NA, n / 3),
    to = rep(NA, n / 3),
    volume = rep(NA, n / 3)
  )
  #Loop trough the df and put it into wanted column
  for (i in 1:(n / 3)) {
    result_df$from[i] <- input_df$value[3 * (i - 1) + 1]
    result_df$to[i] <- input_df$value[3 * (i - 1) + 2]
    result_df$volume[i] <- as.numeric(input_df$value[3 * (i - 1) + 3])
  }
  
  return(result_df)
}

#Transform the time variables into correct format. Also split into sperate date 
#time variables, in case this would be usefull at a later stage
transform_datetime <- function(input_df) {
  df <- input_df %>%
    mutate(
      from = ymd_hms(from),
      to = ymd_hms(to),
      from_date = as.Date(from),
      from_time = format(from, format = "%H:%M"),
      to_date = as.Date(to),
      to_time = format(to, format = "%H:%M"),
      from = as.POSIXct(paste(from_date, from_time), format = "%Y-%m-%d %H:%M"),
      to = as.POSIXct(paste(to_date, to_time), format = "%Y-%m-%d %H:%M")
    ) %>%
    select(from_date, from_time, from, to_date, to_time, to, volume)
  
  return(df)
}

#Combine the functions that goes into "transform_volumes" into the requered 
#sequence to get the desired output
transform_volumes <- function(input_data) {

  flatten_data <- flatten_nested_list(input_data)
  
  split_data <- split_rows_into_columns(flatten_data)
  
  Time_adjust <- transform_datetime(split_data)
  
  return(Time_adjust)
}





