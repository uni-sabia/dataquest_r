# Trefle API provides plant information.
## https://docs.trefle.io/docs/guides/getting-started
## Our goal
## we want to authenticate to the API and
## extract the common_name of the oldest (top 20) discovered plants worldwide.

library(httr)
library(dplyr)

# Write a function that retrieves API queries
endpoint <- "api/v1/plants"

get_trefle <- function(endpoint, queries) { 

  url <- modify_url("https://trefle.io", path=endpoint)
  response <- GET(url, query=queries)
  if (http_error(response)) {
    stop("Something went wrong", call. = FALSE)
  }
  
  if (http_type(response) != "application/json") {
    stop("API did not return json", call.=FALSE)
  }
  
  json_text <- content(response, "text")
  dataframe <- jsonlite::fromJSON(json_text)
  dataframe[[1]]
  
}

dataframe <- get_trefle(endpoint, list(token="1knQ-ftYATzI4XCF0PBlMI-o6qdXaJLmKm8usRrar68"))

oldest <- dataframe %>% arrange(year) %>% select(common_name) %>% top_n(20)
