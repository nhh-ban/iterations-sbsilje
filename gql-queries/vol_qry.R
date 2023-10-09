# This script contains a function that returns a query of hourly traffic volumes
# for a given station and time period we want the data

vol_qry <- function(id, from, to) {
  
  # Query string (given in the task), where our arguments are in curly brackets
  query <- ' 
  {{
    trafficData(trafficRegistrationPointId: "{id}") {{
      volume {{
        byHour(from: "{from}", to: "{to}") {{
          edges {{
            node {{
              from
              to
              total {{
                volumeNumbers {{
                  volume
                }}
              }}
            }}
          }}
        }}
      }}
    }}
  }}'
  
  # Replace the placeholders in the query using glue()
  query <- glue::glue(
    query,
    .open = "{",
    .close = "}",
    id = id,
    from = from,
    to = to
  )
  
  # Return the query 
  return(query)
}