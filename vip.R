library(tidymodels)
library(vip)

tidymodels_prefer()

dat <- readRDS("data/sim_data.rds")

model_dat <- study_data2 |> 
  mutate(term = as.factor(term))

rf_rec <- recipe(term ~ ., data = model_dat) |> 
  update_role(pol_num, q_exp, new_role = "ignore")

rf_spec <- rand_forest() |> 
  set_mode("classification") |> 
  set_engine("ranger", importance = "impurity")

rf_vip <- workflow(rf_rec, rf_spec) |> fit(model_dat)

saveRDS(rf_vip, "rf_vip.rds")

rf_vip |> extract_fit_parsnip() |> vip()
