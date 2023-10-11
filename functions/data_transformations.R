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
