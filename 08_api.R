# Trefle API provides plant information.
## https://docs.trefle.io/docs/guides/getting-started
## Our goal
## we want to authenticate to the API and
## extract the common_name of the oldest (top 20) discovered plants worldwide.

library(httr)
library(dplyr)
library(ggplot2)
library(ggthemes)

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

# Use the function to retrieve data
dataframe <- get_trefle(endpoint, list(token="enter token"))

# Get the oldest plants discovered
oldest <- dataframe %>% arrange(year) %>% select(common_name) %>% top_n(20)


# Challenge 2.
## Using Climate Data API, compare two general circulation models (GCM) for future temperatures in Nigeria.
## https://datahelpdesk.worldbank.org/knowledgebase/articles/902061

### 1. Extract data for both models 

queries <- "list(page=100)"
get_climate <- function(endpoint, queries=list()) {
    url <- modify_url("http://climatedataapi.worldbank.org",
                  path=endpoint)
    response <- GET(url, query=queries)
    
    if (http_error(response)) {
      stop("something went wrong", .call=FALSE)
    }
    if (http_type(response) != "application/json") {
      stop("not json!", .call=FALSE)
    }
    
    text <- content(response, as="text")
    df <- jsonlite::fromJSON(text)
    anom <- unlist(df[[4]]) # Extract the 4th list, which gives you temperature anomaly
    
    # Add month names to the dataframe
    month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec")
    result <- as.data.frame(cbind(month, anom))
    colnames(result) <- c("month", "value")
    result$month <- factor(result$month, levels=month)
    
    result
    
}


#### 1) Model 1. bccr_bcm2_0

endpoint_bccr <- "climateweb/rest/v1/country/manom/bccr_bcm2_0/a2/tas/2020/2039/NGA"
df_bccr <- get_climate(endpoint_bccr, queries) %>% 
  mutate(gcm="bccr_bcm2_0")
str(df_bccr)

#### 2) Model 2. cccma_cgcm3_1

endpoint_cccma <- "climateweb/rest/v1/country/manom/cccma_cgcm3_1/a2/tas/2020/2039/NGA"
df_cccma <- get_climate(endpoint_cccma, queries) %>%
  mutate(gcm="cccma_cgcm3_1")
str(df_cccma)

#### 3) Combine two datasets 
df_both <- rbind(df_bccr, df_cccma)

## 2. Plot the data

ggplot(df_both, aes(x=month, y=value, group=gcm, color=gcm)) +
  geom_line() + 
  ylab("Average monthly change of Temperature (anomaly)") + 
  xlab("Month") +
  theme_few() +
  theme(axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        legend.position="bottom")

