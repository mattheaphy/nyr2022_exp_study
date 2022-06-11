# this script predicts future surrender rates and actuarial present values
# for the training and test data sets.

library(tidyverse)
library(tidymodels)

tidymodels_prefer()

source("expact.R")
source("assumptions.R")

dat <- dat <- readRDS("data/sim_data.rds")


mortality <- paste0("rates/",
                    c("M" = "iamb2012_male.csv", "F" = "iamb2012_female.csv")) |> 
  map_dfr(read_csv, col_types = "id", .id = "gender") |> 
  rename(q_death = qx)

omega_age <- max(mortality$age)

# un-decremented account value projection 
av_0 <- 2000
inc_amt <- 100
wd_pct <- 0.05
cred_rate <- 0.03

# no income rider
av_no_income <- av_0 * (1 + cred_rate) ^ (1:121) * (1 - wd_pct) ^ (1:121)
wd_no_income <- av_0 * (1 + cred_rate) ^ (1:121) * (1 - wd_pct) ^ (0:120) * wd_pct


av_income <- matrix(0, nrow = 121, ncol = 41)
av_income[1, ] <- av_0
for (j in 1:41) {
  for (t in 2:121) {
    av_income[t, j] <- av_income[t-1, j] * (1 + cred_rate) - 
      inc_amt * (t > j)
    if (av_income[t, j] < 0) {
      av_income[t, j] <- 0
      break
    }
  }
}



# inforce policies
active <- dat |> 
  select(-q_exp) |> 
  group_by(pol_num) |> 
  filter(pol_yr == max(pol_yr), term == 0L) |> 
  ungroup() |> 
  mutate(rex_yr = pmax(1, wd_age - age + pol_yr))

stopifnot(nrow(active) == n_distinct(active$pol_num))

# read predictive models
rf_model <- readRDS("models/rf_wf_final.rds")
xgb_model <- readRDS("models/xgb_wf_final.rds")
log_model <- readRDS("models/log_model1_fit.rds")
trad_model <- readRDS("models/trad_assump3.rds")
cox_pred <- readRDS("models/cox_pred.rds")

future_q_surr <- active |> 
  mutate(t = map(age, ~1:(omega_age - .x + 1))) |> 
  unnest_longer(t) |> 
  mutate(pol_yr = pol_yr + t - 1) |> 
  add_q_expected() |> 
  # temporary variables to add in joins
  mutate(iage = age - pol_yr + 1,
         pol_yr2 = pmin(pol_yr, 15))

rf_prob <- predict(rf_model, future_q_surr, type = "prob") |> 
  select(q_rf = .pred_1)
xgb_prob <- predict(xgb_model, future_q_surr, type = "prob") |> 
  select(q_xgb = .pred_1)
log_prob <- predict(log_model, future_q_surr, type = "prob") |> 
  select(q_log = .pred_1)
trad_prob <- future_q_surr |> 
  left_join(trad_model, by = c("inc_guar", "pol_yr")) |> 
  select("q_trad")
cox_prob <- future_q_surr |> 
  left_join(cox_pred, 
            by = c("pol_yr2" = "pol_yr", "inc_guar", "qual", 
                   "iage" = "age", "product",
                   "gender", "wd_age")) |> 
  select(q_coxph)

# plot of average surrender rates
future_q_surr |> 
  bind_cols(rf_prob, xgb_prob, log_prob, cox_prob, trad_prob) |> 
  filter(pol_yr <= 15, inc_guar == "TRUE") |> 
  group_by(inc_guar, exercised, pol_yr) |> 
  summarize(across(q_exp:q_trad, mean), .groups = "drop") |> 
  pivot_longer(starts_with("q_")) |> 
  mutate(state = case_when(inc_guar == "FALSE" ~ "No GLWB",
                           exercised == "FALSE" ~ "Pre Exercised",
                           TRUE ~ "Exercised")) |> 
  ggplot(aes(pol_yr, value, color = state)) + 
  geom_line() + geom_point() + 
  facet_wrap(~name)

future_q_surr <- future_q_surr |> 
  select(pol_num, q_exp) |> 
  bind_cols(rf_prob, xgb_prob, log_prob, cox_prob, trad_prob) |> 
  nest(q_surr = q_exp, q_rf = q_rf, q_xgb = q_xgb, 
       q_log = q_log, q_trad = q_trad,
       q_coxph = q_coxph) |> 
  mutate(across(starts_with("q_"), ~ map(.x, simplify) |> map(unname)))

active2 <- active |> left_join(future_q_surr, by = "pol_num") |> 
  mutate(
    across(starts_with("q_"), ~ list(age, gender, pol_yr, inc_guar, 
                                     rex_yr, .x) |> 
             pmap_dbl(apv, disc = 0.03), .names = "apv_{str_remove(.col, '^q_')}"),
    
    across(starts_with("q_"), ~ list(age, gender, .x) |> 
             pmap_dbl(ax, disc = 0.03), .names = "ax_{str_remove(.col, '^q_')}")
  ) |> 
  select(-starts_with("q_"))

active2 |> 
  pivot_longer(starts_with("apv_"), names_to = "method", values_to = "apv") |> 
  ggplot(aes(apv, fill = method)) + 
  geom_histogram(position = "dodge") + 
  facet_grid(method ~ inc_guar) + 
  labs("Distribution of APV by Income Guarantee")

ae_check_apv(active2)
ae_check_apv(active2, inc_guar)
ae_check_apv(active2, inc_guar, exercised)
ae_check_apv(active2, product)

ae_check_ax(active2)
ae_check_ax(active2, inc_guar)
ae_check_ax(active2, inc_guar, exercised)
ae_check_ax(active2, product)

rmse_check_apv(active2)
rmse_check_apv(active2, product)
rmse_check_apv(active2, inc_guar)
rmse_check_apv(active2, qual)
rmse_check_apv(active2, gender)
rmse_check_apv(active2, age) |> 
  ggplot(aes(age, .estimate, color = name)) + 
  geom_point() + geom_smooth()

rmse_check_ax(active2)
rmse_check_ax(active2, product)
rmse_check_ax(active2, inc_guar)
rmse_check_ax(active2, qual)
rmse_check_ax(active2, gender)
rmse_check_ax(active2, age) |> 
  ggplot(aes(age, .estimate, color = name)) + 
  geom_point() + geom_smooth()



# holdout data ------------------------------------------------------------


test_dat <- readRDS("data/sim_data_test.rds")

# inforce policies
test_active <- test_dat |> 
  select(-q_exp) |> 
  group_by(pol_num) |> 
  filter(pol_yr == max(pol_yr), term == 0L) |> 
  ungroup() |> 
  mutate(rex_yr = pmax(1, wd_age - age + 1))


test_future_q_surr <- test_active |> 
  mutate(t = map(age, ~1:(omega_age - .x + 1))) |> 
  unnest_longer(t) |> 
  mutate(pol_yr = pol_yr + t - 1) |> 
  add_q_expected() |> 
  # temporary variables to add in joins
  mutate(pol_yr2 = pol_yr,
         iage = age - pol_yr2 + 1,
         pol_yr2 = pmin(pol_yr2, 15))

test_rf_prob <- predict(rf_model, test_future_q_surr, type = "prob") |> 
  select(q_rf = .pred_1)
test_xgb_prob <- predict(xgb_model, test_future_q_surr, type = "prob") |> 
  select(q_xgb = .pred_1)
test_log_prob <- predict(log_model, test_future_q_surr, type = "prob") |> 
  select(q_log = .pred_1)
test_trad_prob <- test_future_q_surr |> 
  left_join(trad_model, by = c("inc_guar", "pol_yr")) |> 
  select("q_trad")
test_cox_prob <- test_future_q_surr |> 
  left_join(cox_pred, 
            by = c("pol_yr2" = "pol_yr", "inc_guar", "qual", 
                   "iage" = "age", "product",
                   "gender", "wd_age")) |> 
  select(q_coxph)

# NOTE: for the NYR presentation, the XGBoost and Cox PH models were not 
# mentioned for brevity. As such, these models' predicted surrender rates are 
# not included in the call to bind_cols below. 

test_future_q_surr <- test_future_q_surr |> 
  select(pol_num, q_exp) |> 
  bind_cols(test_rf_prob, test_log_prob, 
            test_trad_prob) |> 
  nest(q_surr = q_exp, q_rf = q_rf, 
       q_log = q_log, q_trad = q_trad) |> 
  mutate(across(starts_with("q_"), ~ map(.x, simplify) |> map(unname)))

test_active2 <- test_active |> 
  left_join(test_future_q_surr, by = "pol_num") |> 
  mutate(
    across(starts_with("q_"), ~ list(age, gender, pol_yr, inc_guar, 
                                     rex_yr, .x) |> 
             pmap_dbl(apv, disc = 0.03), .names = "apv_{str_remove(.col, '^q_')}"),
    
    across(starts_with("q_"), ~ list(age, gender, .x) |> 
             pmap_dbl(ax, disc = 0.03), .names = "ax_{str_remove(.col, '^q_')}")
  ) |> 
  select(-starts_with("q_"))

ae_check_apv(test_active2)
ae_check_apv(test_active2, inc_guar)

rmse_check_apv(test_active2)
rmse_check_apv(test_active2, inc_guar)

ae_check_ax(test_active2)
ae_check_ax(test_active2, inc_guar)

rmse_check_ax(test_active2)
rmse_check_ax(test_active2, inc_guar)

test_active2 |> 
  select(inc_guar, starts_with("apv_")) |> 
  mutate(across(-c(apv_surr, inc_guar), ~.x - apv_surr)) |> 
  pivot_longer(-c(apv_surr, inc_guar), names_to = "assumption") |> 
  ggplot(aes(value, fill = inc_guar)) + 
  geom_histogram(bins = 50, position = "identity", alpha = 0.5) + 
  facet_grid(assumption ~ inc_guar) + xlim(c(-10, 10))

# save files for future use
test_active2 |> saveRDS("data/test_results.rds")

# re-do test, but floor policy year at 15 -----------------------
test_future_q_surr <- test_active |> 
  mutate(t = map(age, ~1:(omega_age - .x + 1))) |> 
  unnest_longer(t) |> 
  mutate(pol_yr = pol_yr + t - 1) |> 
  add_q_expected() |> 
  # temporary variables to add in joins
  mutate(pol_yr2 = pol_yr,
         iage = age - pol_yr2 + 1,
         pol_yr = pmin(pol_yr2, 15))

test_rf_prob <- predict(rf_model, test_future_q_surr, type = "prob") |> 
  select(q_rf = .pred_1)
test_xgb_prob <- predict(xgb_model, test_future_q_surr, type = "prob") |> 
  select(q_xgb = .pred_1)
test_log_prob <- predict(log_model, test_future_q_surr, type = "prob") |> 
  select(q_log = .pred_1)
test_trad_prob <- test_future_q_surr |> 
  left_join(trad_model, by = c("inc_guar", "pol_yr")) |> 
  select("q_trad")
test_cox_prob <- test_future_q_surr |> 
  left_join(cox_pred, 
            by = c("pol_yr", "inc_guar", "qual", 
                   "iage" = "age", "product",
                   "gender", "wd_age")) |> 
  select(q_coxph)

test_future_q_surr <- test_future_q_surr |> 
  select(pol_num, q_exp) |> 
  bind_cols(test_rf_prob, test_log_prob, 
            test_trad_prob) |> 
  nest(q_surr = q_exp, q_rf = q_rf, 
       q_log = q_log, q_trad = q_trad) |> 
  mutate(across(starts_with("q_"), ~ map(.x, simplify) |> map(unname)))

test_active3 <- test_active |> 
  left_join(test_future_q_surr, by = "pol_num") |> 
  mutate(
    across(starts_with("q_"), ~ list(age, gender, pol_yr, inc_guar, 
                                     rex_yr, .x) |> 
             pmap_dbl(apv, disc = 0.03), .names = "apv_{str_remove(.col, '^q_')}"),
    
    across(starts_with("q_"), ~ list(age, gender, .x) |> 
             pmap_dbl(ax, disc = 0.03), .names = "ax_{str_remove(.col, '^q_')}")
  ) |> 
  select(-starts_with("q_"))

ae_check_apv(test_active3)
rmse_check_apv(test_active3)

test_active3 |> saveRDS("data/test_results_v2.rds")
