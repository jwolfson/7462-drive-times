library(dplyr)
library(ggplot2)
library(googleway)
library(lubridate)
library(purrr)

## Helper function to get the drive time minutes from the object returned by `google_directions`
## Do not edit!
extract_minutes <- function(directions) {
  if(directions$status != "OK") return(NA)
  return(directions$routes$legs[[1]]$duration_in_traffic$value/60)
}

## Function to get optimistic, best guess, and pessimistic travel times for a given
## origin, destination, and departure time
get_travel_times <- function(orig, dest, depart = Sys.time()) {
  
  get_time <- function(tm) {
    google_directions(
      origin = orig,
      destination = dest,
      departure_time = depart,
      mode = "driving",
      traffic_model = tm,
      key = api_key
    ) %>% extract_minutes()
  }
  
  df <- map_dfc(c(optimistic="optimistic", 
                  best_guess="best guess", 
                  pessimistic="pessimistic"), 
                get_time) %>%
    mutate(departure_time = depart)
    
  return(df)
}

get_route_polyline <- function(orig, dest) {
  dir <- google_directions(
    origin = orig,
    destination = dest,
    mode = "driving",
    key = api_key
  )
  
  if(dir$status != "OK") return(NULL)
  
  return(dir$routes$overview_polyline)
}
