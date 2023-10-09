
# This script contains functions from task 2, 4 and 5

# (2) Function that transforms metadata to a data frame
transform_metadata_to_df <- function(df){
  df[[1]] |> 
    # Transform list into a data frame
    map(as_tibble) |> 
    list_rbind() |> 
    # Mutate the contents of the latestData-columns such that it is in a character format
    mutate(latestData = map_chr(latestData, 1, .default = NA_character_)) |> 
    # Reformat the date format of the latestData-column
    mutate(latestData = as_datetime(latestData, tz = "UTC"))  |> 
    # Split the location variable into two variables: lat and lon 
    unnest_wider(location) |> 
    unnest_wider(latLon) 
}

# (4a) Function that returns the date time variable in ISO8601 format, 
# with the offset (measured in days) added.

to_iso8601 <- function(date, offset){
  
  # Adjust the date-time with the offset measured in days 
  adjusted_date <- date + days(offset)
  
  # Format the adjusted date-time in ISO8601 format, with "Z" to indicate UTC
  iso8601_date <- format(adjusted_date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  
  return(iso8601_date)
}

# (5) Function that transforms the json-return from the API to a data frame
# that can be used for plotting

transform_volumes <- function(json_data){
  
  # Extract the relevant information from the JSON data
  edges <- json_data$trafficData$volume$byHour$edges
  
  # Use map() to extract data from each edge
  data <- map_df(edges, function(edge) {
    from <- edge$node$from
    to <- edge$node$to
    volume <- edge$node$total$volumeNumbers$volume
    
    # Transform the information to a data frame
    return(data.frame(from = from, to = to, volume = volume)) 
  })
  
  return(data)
}

json <- transform_volumes(json_test)
