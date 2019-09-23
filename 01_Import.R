# IMPORT DATA -----------------------------------------------------------------

# The purpose of this script is to import listing data from redfin.

# SETUP -----------------------------------------------------------------------

# Clear workspace
rm(list = ls())

# Load libraries
library(tidyverse)
library(httr)

# DOWNLOAD DATA ---------------------------------------------------------------

# Define user agent
ua_text <- 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/35.0.1916.47 Safari/537.36'
ua <- user_agent(ua_text)
set_config(ua)

# Define URL
url <- "https://www.redfin.com/stingray/api/gis-csv?al=1&market=seattle&max_price=500000&min_price=100000&min_stories=1&num_homes=100000&ord=redfin-recommended-asc&page_number=1&poly=-122.89329%2047.35408%2C-121.76829%2047.35408%2C-121.76829%2047.86551%2C-122.89329%2047.86551%2C-122.89329%2047.35408&sf=1,2,3,5,6,7&status=1&uipt=1,2,3,4,5,6&v=8"

# Make GET request
response <- GET(url, ua, timeout(60))
content <- content(response, type = "text")
write_lines(content, "listings.csv")

# Save data
df_listings_raw <- read_csv("listings.csv")
write_rds(df_listings_raw, "listings_raw.rds")


# CLEAN DATA ------------------------------------------------------------------
df_listings <- df_listings_raw %>%
  rename_all(str_to_lower) %>%
  rename_all(str_replace_all, pattern = "[:space:]", replacement = "_") %>%
  rename(
    state = state_or_province,
    zip = zip_or_postal_code,
    cost_per_square_foot = `$/square_feet`,
    payment_hoa = `hoa/month`,
    url = `url_(see_http://www.redfin.com/buy-a-home/comparative-market-analysis_for_info_on_pricing)`,
    mls = `mls#`
  ) %>%
  select(
    -sold_date,
    -favorite,
    -interested
  ) %>%
  drop_na(
    -next_open_house_end_time,
    -next_open_house_start_time,
    -hoa_cost,
    -lot_size
  ) %>%
  replace_na(list(hoa_cost = 0, lot_size = 0)) %>%
  mutate(mls = as.character(mls))


# GOOGLE MAP DISTANCES --------------------------------------------------------

# Define helper function for getting distance/time
get_dist <- function(origins, destination, departure_time) {
  
  # Call API
  dist_raw <- googleway::google_distance(
    origins = origins,
    destinations = c(destination),
    departure_time = departure_time,
    key = keyring::key_get("google_maps_api")
  ) 
  
  # Extract time value
  dist_values <- dist_raw %>%
    pluck("rows") %>%
    pluck("elements") %>%
    map(~.x %>% pluck("duration") %>% pluck("value")) %>%
    unlist()
  
  # Convert to minutes
  dist_values <- dist_values / 60
  
  return(dist_values)
  
}

# Create chunk groups for api
df_listings <- df_listings %>%
  mutate(
    index = 1:nrow(.),
    group = cut_width(index, 100),
    origin = map2(latitude, longitude, ~c(.x, .y)),
  )

# Call Google distance api to points of interest
df_listings <- df_listings %>%
  group_by(group) %>%
  mutate(
    time_commute_joe = get_dist(
      origins = origin,
      destination = "480 Houser Way N, Renton, WA 98057",
      departure_time = as.POSIXct("2019-09-23 08:00:00 PDT")
    ),
    time_commute_kira = get_dist(
      origins = origin,
      destination = "1300 SW 27th St, Renton, WA 98057",
      departure_time = as.POSIXct("2019-09-23 08:00:00 PDT")
    ),
    time_downtown = get_dist(
      origins = origin,
      destination = "911 Pine St, Seattle, WA 98101",
      departure_time = as.POSIXct("2019-09-28 19:00:00 PDT")
    ),
    time_hiking = get_dist(
      origins = origin,
      destination = "11400 Issaquah-Hobart Road Southeast, Issaquah, WA 98027",
      departure_time = as.POSIXct("2019-09-29 09:00:00 PDT")
    ),
    time_airport = get_dist(
      origins = origin,
      destination = "17801 International Blvd, SeaTac, WA 98188",
      departure_time = as.POSIXct("2019-09-23 08:00:00 PDT")
    )
  ) %>%
  ungroup() %>%
  select(-index, -group, -origin)

# Save data
write_rds(df_listings, "listings_prepared.rds")
