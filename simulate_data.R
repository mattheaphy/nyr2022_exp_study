library(tidyverse)

source("assumptions.R")
source("expact.R")

base_rates |> 
  ggplot(aes(pol_yr, base_rate, color = inc_guar)) + 
  geom_line() + geom_point() + 
  labs(title = "Base Surrender Rates")

base_rates |> 
  group_by(inc_guar) |> 
  mutate(surv = cumprod(1 - base_rate)) |> 
  ggplot(aes(pol_yr, surv, color = inc_guar)) + 
  geom_line() + geom_point() + 
  labs(title = "Survivorship")


crossing(is_qual = c(TRUE, FALSE) |> factor(), age = 50:90) |> 
  mutate(qual_mult = qual_mult(is_qual, age)) |> 
  ggplot(aes(age, qual_mult, color = is_qual)) + 
  geom_line() + geom_point() + 
  labs(title = "Qualified Money Multiples")


crossing(is_qual = c(TRUE, FALSE) |> factor(), 
         glwb = c(TRUE, FALSE) |> factor(), age = 50:90) |> 
  mutate(qual_mult = qual_mult(is_qual, age) * age_mult(age, glwb)) |> 
  ggplot(aes(age, qual_mult, color = is_qual)) + 
  geom_line() + geom_point() + facet_wrap(~glwb) + 
  labs(title = "Qualified Money and Age Multiples")




# simulation function -----------------------------------------------------

sim_data <- function(n_pol = 1) {
  
  # surrender charge years remaining
  pol_yr_dist <- tibble(
    value = 1:15,
    prob = 1 / 15)
  
  # income guarantee
  inc_guar_dist <- tibble(
    value = c(FALSE, TRUE) |> factor(),
    prob = c(0.4, 0.6))
  
  # qualified money
  qual_dist <- tibble(
    value = c(FALSE, TRUE) |> factor(),
    prob = c(0.45, 0.55))
  
  # issue age
  iss_age_dist <- tribble(~age, ~prob,
                          40,0.01,
                          45,0.02,
                          50,0.05,
                          55,0.15,
                          60,0.17,
                          65,0.225,
                          70,0.175,
                          75,0.1,
                          80,0.1)
  
  iss_age_dist <- tibble(value = 40:80) |> 
    mutate(prob = approx(iss_age_dist$age, iss_age_dist$prob, xout = value)$y,
           prob = prob / sum(prob))
  
  # product
  product_dist <- tibble(
    value = letters[1:3],
    prob = c(0.25, 0.25, 0.5))
  
  # gender
  gender_dist <- tibble(value = c("F", "M"), prob = 0.5)
  
  # withdrawal timing
  wd_time_dist <- tribble(~value, ~cprob,
                          40, 0,
                          50, 0,
                          55, 0.05,
                          60, 0.15,
                          65, 0.40,
                          70, 0.65,
                          75, 0.80,
                          80, 0.95,
                          85, 0.95)
  
  wd_time_dist <- tibble(value = 40:85, 
                         cprob = approx(wd_time_dist$value, wd_time_dist$cprob, 40:85)$y) |> 
    mutate(prob = diff(c(0, cprob)))
    
  # check that probabilities sum to 1
  stopifnot(near(sum(pol_yr_dist$prob), 1))
  stopifnot(near(sum(inc_guar_dist$prob), 1))
  stopifnot(near(sum(qual_dist$prob), 1))
  stopifnot(near(sum(iss_age_dist$prob), 1))
  stopifnot(near(sum(product_dist$prob), 1))
  stopifnot(sum(wd_time_dist$prob) <= 1)
  
  draw <- function(x) sample(x$value, n_pol, replace = TRUE, prob = x$prob)
  
  tibble(
    pol_num = 1:n_pol,
    pol_yr = draw(pol_yr_dist),
    inc_guar = draw(inc_guar_dist),
    qual = draw(qual_dist),
    age = draw(iss_age_dist),
    product = draw(product_dist),
    gender = draw(gender_dist),
    wd_age = draw(wd_time_dist) |> pmax(age)
  )
  
}


expand_sim <- function(dat) {
  
  dat <- dat |> 
    expose() |> 
    mutate(t = pol_yr) |> 
    add_q_expected() |> 
    select(-t)
    
  persist <- function(x) {
    # default to status 2 = already terminated
    res <- rep_len(2L, length(x))
    for (i in seq_along(x)) {
      # if new termination, set to status 1 = just terminated and exit
      if (runif(1) < x[[i]]) {
        res[[i]] <- 1L
        break
      }
      # otherwise set to status 0 = active
      res[[i]] <- 0L
    }
    
    res
  }
  
  # add claims
  set.seed(4375)
  dat |> 
    group_by(pol_num) |>
    mutate(term = persist(q_exp)) |> 
    ungroup() |>
    filter(term <= 1)
    
}

set.seed(123)
census_dat <- sim_data(2E4)
expo_dat <- expand_sim(census_dat)

expo_dat |> group_by(pol_yr, inc_guar, qual) |> 
  summarize(theoretical = mean(q_exp), observed = mean(term), 
            n = n(),
            .groups = "drop") |> 
  pivot_longer(c(theoretical:observed), values_to = "prob",
               names_to = "basis") |> 
  ggplot(aes(pol_yr, prob, color = qual, lty = basis)) +
  geom_line() + 
  facet_wrap(~inc_guar, scales = "free_y")

expo_dat |> group_by(pol_yr, inc_guar, product) |> 
  summarize(theoretical = mean(q_exp), observed = mean(term), 
            n = n(),
            .groups = "drop") |> 
  pivot_longer(c(theoretical:observed), values_to = "prob",
               names_to = "basis") |> 
  ggplot(aes(pol_yr, prob, color = product, lty = basis)) +
  geom_line() + 
  facet_wrap(~inc_guar, scales = "free_y")

expo_dat |> group_by(pol_yr, gender) |> 
  summarize(theoretical = mean(q_exp), observed = mean(term), 
            n = n(),
            .groups = "drop") |> 
  pivot_longer(c(theoretical:observed), values_to = "prob",
               names_to = "basis") |> 
  ggplot(aes(pol_yr, prob, color = gender, lty = basis)) +
  geom_line()


# Save data ---------------------------------------------------------------

final_status <- expo_dat |>
  group_by(pol_num) |> 
  filter(pol_yr == max(pol_yr)) |> 
  ungroup() |> 
  select(pol_num, term, pol_yr)

# add final policy statuses and termination times back to the exposure data
census_dat <- census_dat |> 
  select(-pol_yr) |> 
  inner_join(final_status, by = "pol_num") |> 
  select(pol_num, term, pol_yr, everything())

saveRDS(census_dat, "data/census_data.rds")

expo_dat |> 
  select(-ends_with("_mult"), -base_rate) |> 
  saveRDS("data/sim_data.rds")


# holdout block -----------------------------------------------------------


set.seed(1234)
test_dat <- sim_data(5000) |> expand_sim() |> 
  select(-ends_with("_mult"), -base_rate)
saveRDS(test_dat, "data/sim_data_test.rds")
