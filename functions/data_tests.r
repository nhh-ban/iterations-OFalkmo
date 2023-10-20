# This file contains tests to be applied to 
# the Vegvesen stations-data *after* being transformed
# to a data frame. 
# 
# All tests are packed in a function test_stations_metadata that apples
# all the aforementioned tests

test_stations_metadata_colnames <-
  function(df) {
    #Defines the names we except for the column
    expected_colnames <- c("id", "name", "latestData", "lat", "lon")
    #Test whether the name of the df we created is equal what we expect 
    if (all(colnames(df) == expected_colnames) == TRUE) {
      print("PASS: Data has the correct columns")
    } else{
      print("FAIL: Columns do not match the correct specification")
    }
  }

test_stations_metadata_nrows <-
  function(df) {
    #define the max and min length of the df (in rows)
    min_expected_rows <- 5000
    max_expected_rows <- 10000
    #Return a comment of the number of rows
    if (nrow(df) > min_expected_rows & nrow(df) < max_expected_rows) {
      print("PASS: Data has a reasonable number of rows")
    } else if (nrow(df) <= min_expected_rows) {
      print("FAIL: Data has suspiciously few rows")
    } else {
      print("FAIL: Data has suspiciously many rows")
    }
  }

test_stations_metadata_coltypes <-
  function(df) {
    #Defines the properties we except for the column
    expected_coltypes <-
      c("character", "character", "double", "double", "double")
    #Test whether the properties of the df we created is equal what we expect 
    if (all(df %>%
            map_chr( ~ typeof(.)) == expected_coltypes) == TRUE) {
      print("PASS: All cols have the correct specifications")
    } else{
      print("FAIL: Columns do not have the correct specification")
    }
  }
  
test_stations_metadata_nmissing <-
  function(df) {
    #Define a max number of missing values that is allowable 
    max_miss_vals <- 200
    #Test whether there are more or less (equal aswell) na values than allowable
    if (df %>% map_int( ~ sum(is.na((.)))) %>% sum(.) < max_miss_vals) {
      print("PASS: Amount of missing values is reasonable")
    } else {
      print("FAIL: Too many missing values in data set")
    }
  }

test_stations_metadata_latestdata_timezone <-
  function(df) {
    #Test wheter the timezone is set to UTC
    if (attr(df$latestData,"tzone")=="UTC") {
      print("PASS: latestData has UTC-time zone")
    } else {
      print("FAIL: latestData does not have expected UTC-time zone")
    }
  }

#Combines all function above into one coherent function with input of the df
test_stations_metadata <- 
  function(df){
    test_stations_metadata_colnames(df)
    test_stations_metadata_coltypes(df)
    test_stations_metadata_nmissing(df)
    test_stations_metadata_nrows(df)
    test_stations_metadata_latestdata_timezone(df)
  }





