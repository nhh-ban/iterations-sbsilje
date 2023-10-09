# This file contains tests to be applied to 
# the Vegvesen stations-data *after* being transformed
# to a data frame. 
# 
# All tests are packed in a function test_stations_metadata that apples
# all the aforementioned tests

# Function that tests whether the transformed data frame has correct column names
test_stations_metadata_colnames <-
  function(df) {
    
    # Create a vector with all the expected column names in the data frame 
    expected_colnames <- c("id", "name", "latestData", "lat", "lon")
    
    # If the column names in the data frame contains the column names in the 
    # expected_colnames vector, then it is a pass. Otherwise, it is a fail. 
    if (all(colnames(df) == expected_colnames) == TRUE) {
      print("PASS: Data has the correct columns")
    } else{
      print("FAIL: Columns do not match the correct specification")
    }
  }

# Function that tests whether the data frame has a reasonable number of rows
test_stations_metadata_nrows <-
  function(df) {
    
    # Define the minimum and maximum number of expected rows 
    min_expected_rows <- 5000
    max_expected_rows <- 10000
    
    # If the number of rows in the data frame is within our defined min- and max-
    # range, then it is a pass. Otherwise, it is a fail (because it is 
    # less or greater than our range).
    if (nrow(df) > min_expected_rows & nrow(df) < max_expected_rows) {
      print("PASS: Data has a reasonable number of rows")
    } else if (nrow(df) <= min_expected_rows) { 
      print("FAIL: Data has suspiciously few rows")
    } else {
      print("FAIL: Data has suspiciously many rows")
    }
  }

# Function that tests whether the variables in the data frame have correct 
# specifications, i.e., if they are in a character or double format
test_stations_metadata_coltypes <-
  function(df) {
    
    # A vector with all the expected formats for each column 
    expected_coltypes <-
      c("character", "character", "double", "double", "double")
    
    # If all the columns have the correct specifications, it is a pass.
    if (all(df %>%
            map_chr( ~ typeof(.)) == expected_coltypes) == TRUE) {
      print("PASS: All cols have the correct specifications")
    } else{
      print("FAIL: Columns do not have the correct specification")
    }
  }

# Function that tests whether the amount of NA-values in the data frame
# is reasonable. 
test_stations_metadata_nmissing <-
  function(df) {
    
    # Define the maximum number of missing values that is considered reasonable
    max_miss_vals <- 200
    
    # If the total number of missing values in the data frame is less than
    # our defined threshold, then it is a pass. 
    if (df %>% map_int( ~ sum(is.na((.)))) %>% sum(.) < max_miss_vals) {
      print("PASS: Amount of missing values is reasonable")
    } else {
      print("FAIL: Too many missing values in data set")
    }
  }

# Function that tests whether the latestData column is in a UTC format
test_stations_metadata_latestdata_timezone <-
  function(df) {
    
    # If the latestData column is in a UTC format, then it is a pass. 
    if (attr(df$latestData,"tzone")=="UTC") {
      print("PASS: latestData has UTC-time zone")
    } else {
      print("FAIL: latestData does not have expected UTC-time zone")
    }
  }

# Function that compiles all the test function, 
# to apply all the aforementioned tests simultaneously.
test_stations_metadata <- 
  function(df){
    test_stations_metadata_colnames(df)
    test_stations_metadata_coltypes(df)
    test_stations_metadata_nmissing(df)
    test_stations_metadata_nrows(df)
    test_stations_metadata_latestdata_timezone(df)
  }





