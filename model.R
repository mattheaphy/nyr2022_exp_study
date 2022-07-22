# this script creates all assumptions for the case study, which includes
# a traditional tabular assumption and machine learning models

library(tidymodels)
library(censored)

tidymodels_prefer()

source("expact.R")

dat <- readRDS("data/sim_data.rds") |> 
  # dropping the wd_age variable since it is 100% correlated with 
  #    issue age + exercised + policy year should be used
  select(-wd_age)

# Experience cross tabs ---------------------------------------------------

dat |> exp_stats()
dat |> group_by(pol_yr) |> exp_stats()
dat |> mutate(pol_yr_band = cut(pol_yr, c(0, 5, 8, 10, 11, Inf))) |> 
  group_by(pol_yr_band) |> exp_stats()
dat |> group_by(inc_guar) |> exp_stats()
dat |> group_by(qual) |> exp_stats()
dat |> mutate(age_band = cut_width(age, 5, 2.5)) |> 
  group_by(age_band) |> exp_stats()
dat |> group_by(product) |> exp_stats()



# Traditional Experience Study --------------------------------------------

# traditional assumption #1 = use observed rates varying by policy number with rounding
trad_assump1 <- dat |> group_by(pol_yr) |> exp_stats()

trad_assump1 |> 
  pivot_longer(-(pol_yr:exposures), names_to = "basis", values_to = "value") |> 
  ggplot(aes(pol_yr, value, color = basis)) + 
  geom_point() + geom_line() + 
  labs(title = "Traditional Assumption #1",
        subtitle = "Assumption = Past History, No Differentiation by Rider")

trad_assump1 |> select(pol_yr, q_trad = q_obs) |> 
  complete(pol_yr = 1:120) |> 
  fill(q_trad) |> 
  saveRDS("models/trad_assump1.rds")

# traditional assumption #2 - apply multiples based on rider status

inc_mult <- dat |> group_by(inc_guar) |> exp_stats()
inc_mult <- inc_mult$q_obs[[2]] / inc_mult$q_obs[[1]]

trad_assump2 <- dat |> group_by(pol_yr, inc_guar) |> exp_stats() |> 
  left_join(trad_assump1 |> select(pol_yr, q_trad = q_obs), by = "pol_yr") |> 
  mutate(q_trad = q_trad * if_else(inc_guar == "TRUE", inc_mult, 1))

trad_assump2 |> 
  pivot_longer(-(pol_yr:exposures), names_to = "basis", values_to = "value") |> 
  ggplot(aes(pol_yr, value, color = basis)) + 
  geom_point() + geom_line() + facet_wrap(~inc_guar, scales = "free_y") + 
  labs(title = "Traditional Assumption #2",
       subtitle = "Add Flat Multiples for Rider Presence")

trad_assump2 |> select(pol_yr, inc_guar, q_trad) |> 
  complete(nesting(inc_guar), pol_yr = 1:120) |> 
  fill(q_trad) |> 
  saveRDS("models/trad_assump2.rds")

# a problem with traditional assumption #3 is that it assumes the impact of the income
# guarantee is identical in all time periods
# traditional assumption #3 - develop rider and non-rider rates separately

trad_assump3 <- dat |> group_by(pol_yr, inc_guar) |> exp_stats()

trad_assump3 |> 
  pivot_longer(-(pol_yr:exposures), names_to = "basis", values_to = "value") |> 
  ggplot(aes(pol_yr, value, color = inc_guar)) + 
  geom_point() + geom_line() + 
  labs(title = "Traditional Assumption #3",
       subtitle = "Vary Experience by Rider Presence and Apply Smoothing")

trad_assump3 |> select(pol_yr, inc_guar, q_trad = q_round2) |> 
  complete(nesting(inc_guar), pol_yr = 1:120) |> 
  fill(q_trad) |> 
  saveRDS("models/trad_assump3.rds")

# NOTE: only traditional model 3 was used in the final slides. traditional
# models 1 and 2 wouldn't have been interesting examples as they're simply
# bad models.

# Logistic Regression - No Penalties --------------------------------------

# data updates for modeling
model_dat <- dat |> 
  mutate(term = factor(term))

log_rec <- recipe(term ~ ., data = model_dat) |> 
  step_rm(pol_num, q_exp) |> 
  step_mutate(sc_group = case_when(
    pol_yr <= 10 ~ "SC Period",
    pol_yr == 11 ~ "Shock",
    TRUE ~ "PostShock"
  ) |> factor()) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_ns(pol_yr, age, deg_free = 7) |> 
  step_interact(terms = ~starts_with("sc_group"):starts_with("inc_guar")) |> 
  step_interact(terms = ~starts_with("pol_yr"):starts_with("inc_guar")) |> 
  step_interact(terms = ~starts_with("age"):starts_with("inc_guar"))
  


log_model1 <- workflow(log_rec,
                       logistic_reg() |> set_engine("glm"))

log_model1_fit <- fit(log_model1, model_dat)
saveRDS(log_model1_fit, "models/log_model1_fit.rds")

model_dat |> 
  bind_cols(predict(log_model1_fit, model_dat, type = "prob") |> 
              select(q_mod = .pred_1)) |> 
  group_by(pol_yr, inc_guar) |> 
  exp_stats(c("q_exp", "q_mod")) |>
  select(-starts_with("ae_")) |> 
  pivot_longer(-(pol_yr:exposures), names_to = "basis", values_to = "value") |> 
  ggplot(aes(pol_yr, value, color = basis)) + 
  geom_point() + geom_line() + facet_wrap(~inc_guar, scales = "free_y") + 
  labs(title = "Unpenalized Logistic Regression")


# Random Forest -----------------------------------------------------------


rf_spec <- rand_forest() |> 
  set_mode("classification") |> 
  set_engine("ranger")

rf_rec <- recipe(term ~ ., data = model_dat) |> 
  step_rm(pol_num, q_exp) |> 
  step_mutate(sc_group = case_when(
    pol_yr <= 10 ~ "SC Period",
    pol_yr == 11 ~ "Shock",
    TRUE ~ "PostShock"
  ) |> factor()) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_interact(terms = ~starts_with("sc_group"):starts_with("inc_guar")) |> 
  step_interact(terms = ~starts_with("pol_yr"):starts_with("inc_guar"))

rf_wf <- workflow(rf_rec, rf_spec)

set.seed(3254)
rf_fit <- fit(rf_wf, model_dat)

saveRDS(rf_fit, "models/rf_wf_final.rds")

model_dat |> 
  bind_cols(predict(rf_fit, model_dat, type = "prob") |> 
              select(q_mod = .pred_1)) |> 
  group_by(pol_yr, inc_guar) |> 
  exp_stats(c("q_exp", "q_mod")) |>
  select(-starts_with("ae_")) |> 
  pivot_longer(-(pol_yr:exposures), names_to = "basis", values_to = "value") |> 
  ggplot(aes(pol_yr, value, color = basis)) + 
  geom_point() + geom_line() + facet_wrap(~inc_guar, scales = "free_y") + 
  labs(title = "Random Forest")


# xgboost -----------------------------------------------------------------

xgb_spec <- boost_tree() |> 
  set_mode("classification") |> 
  set_engine("xgboost")

# using the same recipe as the rf model
xgb_wf <- workflow(rf_rec, xgb_spec)

set.seed(3254)
xgb_fit <- fit(xgb_wf, model_dat)

saveRDS(xgb_fit, "models/xgb_wf_final.rds")

model_dat |> 
  bind_cols(predict(xgb_fit, model_dat, type = "prob") |> 
              select(q_mod = .pred_1)) |> 
  group_by(pol_yr, inc_guar) |> 
  exp_stats(c("q_exp", "q_mod")) |>
  select(-starts_with("ae_")) |> 
  pivot_longer(-(pol_yr:exposures), names_to = "basis", values_to = "value") |> 
  ggplot(aes(pol_yr, value, color = basis)) + 
  geom_point() + geom_line() + facet_wrap(~inc_guar, scales = "free_y") + 
  labs(title = "XG Boost")


# Survival - Cox Proportional Hazards -------------------------------------


census <- readRDS("data/census_data.RDS")


cox_spec <- proportional_hazards() |> 
  set_engine("survival") |> 
  set_mode("censored regression")

cox_fit <- cox_spec |> 
  fit(Surv(pol_yr, term) ~ ., select(census, -pol_num))

saveRDS(cox_fit, "models/cox_fit.rds")

census_distinct <- crossing(inc_guar = factor(c("FALSE", "TRUE")),
                            qual = factor(c("FALSE", "TRUE")),
                            age = 40:80, 
                            product = letters[1:3], 
                            gender = c("F", "M"), 
                            wd_age = 40:85) |> 
  filter(wd_age >= age)

cox_pred <- predict(cox_fit, census_distinct, 
        type = "survival", time = 1:15) |> 
  bind_cols(census_distinct) |> 
  unnest(.pred)



summarized_preds <- list(coxph = cox_pred) |>
  map_dfr(~ .x |>
        group_by(inc_guar, .time) |>
        summarize(.pred_survival = mean(.pred_survival),
                  .groups = "drop"),
        .id = "model")


# expected S(x)
S_exp <- dat |> 
  group_by(inc_guar, pol_yr) |> 
  summarize(.pred_survival = sum(term == 0) / n(), .groups = "drop_last") |>
  mutate(.pred_survival = cumprod(.pred_survival)) |> 
  ungroup() |> 
  rename(.time = pol_yr) |> 
  mutate(model = "expected")

surv_dat <- bind_rows(S_exp, summarized_preds) |> 
  group_by(model, inc_guar) |> 
  mutate(qL = 1 - .pred_survival / dplyr::lag(.pred_survival, default = 1)) |> 
  ungroup()
  
surv_dat |> 
  ggplot(aes(.time, .pred_survival, color = model)) + 
  geom_line() + geom_point() + 
  facet_wrap(~inc_guar) + 
  labs(title = "Survival Model Comparison")

surv_dat |> 
  ggplot(aes(.time, qL, color = model)) + 
  geom_line() + geom_point() + 
  facet_wrap(~inc_guar, scales = "free_y") + 
  labs(title = "Survival Model Comparison - Surrender Probability")

# save survival model predictions
tpx_to_qx <- function(x) {
  1 - x / dplyr::lag(x, default = 1)
}

cox_pred <- cox_pred |> 
  rename(pol_yr = .time, tpx = .pred_survival) |> 
  group_by(inc_guar, qual, age, product, gender, wd_age) |> 
  mutate(q_coxph = tpx_to_qx(tpx)) |> 
  ungroup()


cox_pred_summary <- cox_pred |> 
  group_by(pol_yr, inc_guar) |> 
  summarize(q_coxph = mean(q_coxph), .groups = "drop")

model_dat |> 
  group_by(pol_yr, inc_guar) |> 
  exp_stats("q_exp") |> 
  left_join(cox_pred_summary, by = c("pol_yr", "inc_guar")) |> 
  select(-starts_with("ae_")) |> 
  pivot_longer(-(pol_yr:exposures), names_to = "basis", values_to = "value") |> 
  ggplot(aes(pol_yr, value, color = basis)) + 
  geom_point() + geom_line() + facet_wrap(~inc_guar, scales = "free_y") + 
  labs(title = "Cox Proportional Hazards")


saveRDS(cox_pred, "models/cox_pred.rds")
