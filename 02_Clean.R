# CLEAN AND SCORE LISTINGS ----------------------------------------------------

# Apply minimum criteria and score listings based on criteria.

# SETUP -----------------------------------------------------------------------

# Clear workspace
rm(list = ls())

# Load libraries
library(tidyverse)

# Load Data
df_listings <- read_rds("listings_prepared.rds")


# PARAMETERS ------------------------------------------------------------------

# Status parameters
downpayment <- 50000
rate_interest <- 0.0375
loan_term <- 30  # years
rate_tax <- 0.0125
rate_hoi <- 0.003
rate_pmi <- 0.005
rate_maint <- 0.01


# Define filtering/scoring criteria
criteria <- list(
  sale_types = c("For-Sale-by-Owner Listing", "MLS Listing"),
  property_types = c("Condo/Co-op", "Multi-Family (2-4 Unit)", "Single Family Residential", "Townhouse", "Other"),
  baths = list(lim_bad = 1, lim_good = 2, weight = 1),
  beds = list(lim_bad = 1, lim_good = 3, weight = 2),
  payment = list(lim_bad = 3000, lim_good = 1250, weight = 4, min = 500),
  square_feet = list(lim_bad = 600, lim_good = 1000, weight = 2),
  time_commute = list(lim_bad = 45, lim_good = 10, weight = 4),
  time_downtown = list(lim_bad = 45, lim_good = 15, weight = 1),
  time_hiking = list(lim_bad = 60, lim_good = 20, weight = 0.5),
  time_airport = list(lim_bad = 60, lim_good = 20, weight = 0.5)
)

# Review list
tibble(name = names(criteria), criteria = criteria) %>%
  unnest_wider(criteria) %>%
  select(name, lim_bad, lim_good, weight) %>%
  drop_na()


# ADD FEATURES ----------------------------------------------------------------

loan_payment <- function(price, downpayment, interest = 0.0375, term = 30) {
  p <- price - downpayment  # determine principal
  r <- interest / 12  # monthly interest
  n <- term * 12  # monthly terms
  
  payment <- p*((r*(1+r)^n) / (((1+r)^n)-1))
  
}

pmi_payment <- function(price, downpayment, rate) {
  
  # Determine PMI
  pmi <- (price - downpayment) * rate / 12
  
  # Set to zero if greater than 20%
  pmi <- ifelse((downpayment / price) > 0.2, 0, pmi)
  
  return(pmi)
}

df_listings <- df_listings %>%
  mutate(
    time_commute = pmax(time_commute_joe, time_commute_kira),
    payment_loan = loan_payment(price, downpayment, rate_interest, loan_term),
    payment_pmi = pmi_payment(price, downpayment, rate_pmi),
    payment_taxes = price * rate_tax / 12,
    payment_maint = price * rate_maint / 12,
    payment_hoi = price * rate_hoi / 12
  ) %>%
  mutate(payment_total = rowSums(select(., starts_with("payment_"))))


# MINIMUM REQUIREMENTS - LIMITS -----------------------------------------------

# Filter listings based on minimum criteria
df_listings <- df_listings %>%
  filter(
    sale_type %in% criteria$sale_types,
    property_type %in% criteria$property_types,
    baths >= criteria$baths$lim_bad,
    beds >= criteria$beds$lim_bad,
    payment_total <= criteria$payment$lim_bad,
    payment_total >= criteria$payment$min,
    square_feet >= criteria$square_feet$lim_bad,
    time_commute_joe <= criteria$time_commute$lim_bad,
    time_commute_kira <= criteria$time_commute$lim_bad,
    time_downtown <= criteria$time_downtown$lim_bad,
    time_hiking <= criteria$time_hiking$lim_bad,
    time_airport <= criteria$time_airport$lim_bad
  )



# SCORING LISTINGS ------------------------------------------------------------

score <- function(x, limit_bad, limit_good, weight) {
  
  # Center and scale relative to limits
  if (limit_good > limit_bad) {
    x <- x - limit_bad
    x <- x / (limit_good - limit_bad)
  } else {
    x <- -x + limit_bad
    x <- x / (limit_bad - limit_good)
  }
  
  # Create step function in case outside of limits
  x <- ifelse(x <= 0, 0, x)
  x <- ifelse(x >= 1, 1, x)
  
  # Multiply by PI for sine function
  x <- x * pi
  
  # Create sine function with output between 0 and 1 for x from 0 to pi
  y <- 0.5*sin(x - 0.5*pi) + 0.5
  
  # Multiply by weight
  score <- y * weight
  
  # Round
  score <- round(score, 3)
  
  return(score)
}

# Create individual scores
df_listings <- df_listings %>%
  mutate(
    beds_score = score(beds, criteria$beds$lim_bad, criteria$beds$lim_good, criteria$beds$weight),
    baths_score = score(baths, criteria$baths$lim_bad, criteria$baths$lim_good, criteria$baths$weight),
    payment_score = score(payment_total, criteria$payment$lim_bad, criteria$payment$lim_good, criteria$payment$weight),
    square_feet_score = score(square_feet, criteria$square_feet$lim_bad, criteria$square_feet$lim_good, criteria$square_feet$weight),
    time_commute_score = score(time_commute, criteria$time_commute$lim_bad, criteria$time_commute$lim_good, criteria$time_commute$weight),
    time_downtown_score = score(time_downtown, criteria$time_downtown$lim_bad, criteria$time_downtown$lim_good, criteria$time_downtown$weight),
    time_hiking_score = score(time_hiking, criteria$time_hiking$lim_bad, criteria$time_hiking$lim_good, criteria$time_hiking$weight),
    time_airport_score = score(time_airport, criteria$time_airport$lim_bad, criteria$time_airport$lim_good, criteria$time_airport$weight)
  )

# Sum Scores
df_listings <- df_listings %>%
  mutate(total_score = rowSums(select(., ends_with("score"))))


# Save data
write_rds(df_listings, "listings_scored.rds")
write_csv(df_listings, "listings_scored.csv")
