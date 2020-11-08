library(tidyverse)
library(sf)
library(lubridate)
library(USAboundaries) # for state geography

# US state boundaries (keep just states)
states <- USAboundaries::us_states() %>%
  filter(jurisdiction_type == "state")

# Create lookup of state names and codes
state_names <- states %>%
  st_set_geometry(NULL) %>% # drop the geometry
  select(state_name = name,
         state_code = stusps) %>%
  mutate(state_name = str_to_lower(state_name),
         state_code = str_c(" ", state_code))

# Read in daily files of all tweets
csv_list <- list.files("data/daily files/")

# Create function to read in csv
func_read_tweets <- function(csv_name,
                             state_lookup = state_names) {
  
  # Read in the tweets for the day
  day_summary <- read_csv(str_c("data/daily files/", csv_name), 
                     col_types = cols_only(coordinates = col_character(),
                                           geo = col_character(),
                                           hate_score_consensus = col_integer(),
                                           created_at = col_datetime(),
                                           user_location = col_character())) %>%
    # Capture the date and day of the week
    mutate(created_date = date(created_at),
           created_day = weekdays(created_date)) %>%
    
    # Is there a state name or state code in the user_location
    mutate(state_from_name = str_extract(str_to_lower(user_location), 
                                         paste(state_lookup$state_name, collapse="|")),
           state_code = str_extract(user_location, 
                                         paste(state_lookup$state_code, collapse="|"))) %>%
    left_join(state_lookup, by = c("state_code" = "state_code")) %>%
    
    # Prioritise state name over state code
    mutate(state = ifelse(!is.na(state_from_name), state_from_name, state_name)) %>%
    select(created_date, 
           created_day, 
           state,
           coordinates,
           hate_score_consensus) %>%
    
    # Create totals for date by state
    group_by(created_date, created_day, state) %>%
    summarise(no_tweets = n(),
              coord_tweet = sum(!is.na(coordinates)),
              no_hate = sum(hate_score_consensus)) 
}

# Bring in all tweets and remove any without date
all_days <- purrr::map_df(csv_list, func_read_tweets) %>%
  filter(!is.na(created_date))

all_days <- all_days %>%
  mutate(state = str_to_title(state))

# Tidy up
rm(csv_list)

# Create tweets per day summary
hate_per_day <- all_days %>%
  group_by(created_date, created_day) %>%
  summarise(no_tweets = sum(no_tweets),
            no_hate = sum(no_hate),
            no_coords = sum(coord_tweet)) %>%
  mutate(perc_hate = round(no_hate/no_tweets * 100, 2)) %>%
  filter(no_hate > 0)

# Write to csv
write_csv(hate_per_day, "outputs/hate_per_day.csv")
  
# State day summary for choropleth
states <- states %>%
  left_join(all_days, by = c("name" = "state")) %>%
  filter(created_date <= max(hate_per_day$created_date),
         created_date >= min(hate_per_day$created_date)) %>%
  mutate(perc_hate = round(no_hate/no_tweets * 100, 2),
         perc_coord = round(coord_tweet/no_tweets * 100, 2))

# Tidy up
rm(all_days)
