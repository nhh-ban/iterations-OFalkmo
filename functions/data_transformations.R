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


to_iso8601 = function(time, offset) {
  
  # Add the offset in days to the date time
  adjusted_date_time <- time + lubridate::days(offset)
  
  # Format the adjusted date and time in ISO8601 format with "Z"
  iso8601_date_time <- format(adjusted_date_time, format = "%Y-%m-%dT%H:%M:%SZ")
  
  return(iso8601_date_time)
}

