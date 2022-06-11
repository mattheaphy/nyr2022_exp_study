# This script contains assumed factors used in the derivation of simulated
# surrender rates. All values below are theoretical / illustrative and 
# do not represent real experience of any particular product.

require(tidyverse)

# behavioral factors ---------------------------------------------
# surrender charge years remaining and income guarantee
base_rates <- read_csv("rates/base_rates.csv", col_types = "idd") |> 
  pivot_longer(-pol_yr, names_to = "inc_guar", values_to = "base_rate") |> 
  mutate(inc_guar = if_else(inc_guar == "base_rate", FALSE, TRUE) |> 
           factor())


# qualified money
qual_mult <- function(qual, age) {
  
  is_qual <- qual == "TRUE"
  
  case_when(is_qual & age >= 70 ~ 0.8,
            is_qual & age >= 60 ~ 0.9,
            is_qual ~ 1,
            !is_qual ~ 1.1)
}

# attained age
age_mult <- function(age, glwb) {
  
  glwb <- glwb == "TRUE"
  
  if_else(!glwb, 
          case_when(
            age < 50 ~ 1.3,
            age < 60 ~ 1,
            age < 65 ~ 0.9,
            age < 70 ~ 0.8,
            age < 80 ~ 1,
            age < 90 ~ 1.2,
            TRUE ~ 1.5),
          case_when(
            age < 50 ~ 1.2,
            age < 60 ~ 1,
            age < 65 ~ 0.9,
            age < 70 ~ 0.8,
            age < 80 ~ 0.7, # very different response at old ages - surrender rates plummet
            age < 90 ~ 0.4,
            TRUE ~ 0.25)
  )
}


# product
prod_mult <- c(a = 1.25, b = 0.9, c = 1)

# gender
gender_mult <- c(M = 1.1, F = 1)

# withdrawal timing
wd_time_mult <- function(exercised, age, inc_guar) {
  
  exercised = exercised == "TRUE"
  
  case_when(
    inc_guar == "FALSE" ~ 1,
    exercised ~ 1,
    age > 80 ~ 3,
    age > 70 ~ 2.25,
    age < 60 ~ 1.5,
    TRUE ~ 2
  )
  
}


# did not include - crediting rate performance, interest rates, inflation, equity market performance
