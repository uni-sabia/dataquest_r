library(dplyr)
library(rvest) #for websraping
library(janitor) #for clean datasets
library(ggplot2) # for data visualization
<<<<<<< HEAD
library(stringr) # for extracting text
=======
>>>>>>> 896c097978b7e656dd890fcf7f6cc98af8793977

# Mission. Write a function that allows us to scrape websites.

## "output" parameter can be either "text", "table", "attrs" or names of an attribute.   
## if all_nodes is set to TRUE, the function extracts all values. If false, extract only the first node. 

scraper <- function(url, selector, 
                    output = "text", # Set "text" as default value. 
                    all_nodes = TRUE) { # Set "TRUE" as default. 
  content <- read_html(url) # Get content

  if (all_nodes==TRUE) {
    extract <- content %>% html_nodes(selector) 
  }
  else {
    extract <- content %>% html_node(selector) 
  }
  
  if (output=="text") {
    result <- extract %>% html_text()
  }
  else if (output=="table") {
    result <- extract %>% html_table()
  }
  else if (output=="attrs") {
    result <- extract %>% html_attrs()
  }
  else {
    result <- extract %>% html_attr(output)
  }
  
  result
}

# Test drive 1. Scrape weather data from AccuWeather (https://www.accuweather.com/).
## Extract the high and low temperatures recorded on 02.01.2020 in Brussels, Belgium

url <- "http://dataquestio.github.io/web-scraping-pages/Brussels_Belgium_Weather_AccuWeather.html"
selector <- ".half-day-card-header , .temperature"
weather_brussels <- scraper(url, selector)
readr::parse_number(weather_brussels) # Get numbers only

# Test drive 2. Get data from Wikipedia on Earth https://en.wikipedia.org/wiki/Earth
## Get Earth's mean radius to compute distances using the longitude and latitude coordinates

url <- "http://dataquestio.github.io/web-scraping-pages/Earth-Wiki.html"
selector <- "tr:nth-child(20) td"
earth <- scraper(url, selector)
readr::parse_number(earth)

# Test drive 3. Extract the the accepted answer and the author of that answer from Stack Exchange.

url <- "http://dataquestio.github.io/web-scraping-pages/WebSraping-ethics-SE.html"
selector <- ".accepted-answer .s-prose"
answer <- scraper(url, selector, all_nodes=FALSE)

## Extract the author's name as text

selector <- ".accepted-answer .user-details a"
author <- scraper(url, selector, all_nodes=FALSE)

# Test drive 4. Extract yearly change percentage of the world's population from 1950 to 2019 and visualize it. 
# Extract data from Worldometer, which freely provides world statistics about many fields of interest over time

url <- "http://dataquestio.github.io/web-scraping-pages/Worldometer-Population.html"
selector <- "td , th"
worldpop_text <- scraper(url, selector) 

## Turn the result (a vector) into a dataframe
worldpop_df <- matrix(worldpop_text, ncol=7, byrow = TRUE) %>%
  as.data.frame() %>% row_to_names(row_number=1) %>% clean_names()

## Convert the YearlyChange column into numeric data type
worldpop_df <- worldpop_df %>% mutate(yearly_change = gsub("[\\%]", "", yearly_change))  
worldpop_df$yearly_change <- as.numeric(worldpop_df$yearly_change)
worldpop_df$year <- as.numeric(worldpop_df$year)

worldpop_subset <- worldpop_df %>% filter(year > 1950 & year < 2019)

## Visualize the dataset
ggplot(worldpop_subset, aes(x=year, y=yearly_change)) + 
         geom_line() + geom_point() + theme_classic()
<<<<<<< HEAD

# Test drive 5. Build an image dataset from Billboard Hot 100 
## https://www.billboard.com/charts/hot-100

## From the image tag, extract "style" attribute values from the webpage
url <- "http://dataquestio.github.io/web-scraping-pages/The%20Hot%20100%20Chart%20_%20Billboard.html"
selector <- ".chart-element__image"
output <- "style"
image <- scraper(url, selector, output)[1:5]

### This is the regex that can be used to extract URLs of images
url_pattern <- "(?i)http[s]?://(?:[a-z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-f][0-9a-f]))+\\.jpg"
image_list <- str_match(image, url_pattern)[,1]
=======
       
>>>>>>> 896c097978b7e656dd890fcf7f6cc98af8793977
