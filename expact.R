# This script contains supporting functions used by various scripts in this
# project.

library(rlang)

#'@description Basic experience study summary function

exp_stats <- function(dat, expected) {
  
  .groups <- groups(dat)
  
  if (!missing(expected)) {
    ex_mean <- glue::glue("mean({expected})") %>% 
      set_names(expected) %>% 
      rlang::parse_exprs()
    
    ex_ae <- glue::glue("{expected} / q_obs") %>% 
      set_names(glue::glue("ae_{expected}")) %>% 
      rlang::parse_exprs()
  } else {
    ex_ae <- ex_mean <- NULL
  }
  
  if(is.factor(dat$term)) dat$term = if_else(dat$term == "1", 1, 0)
  
  res <- dat %>% 
    summarize(claims = sum(term),
              exposures = n(),
              q_obs = mean(term),
              !!!ex_mean,
              !!!ex_ae,
              .groups = "drop")
  
  structure(res, class = c("exp_df", class(res)),
            groups = .groups)
  
}

cleanup <- function(x, ...) {
  UseMethod("cleanup")
}

cleanup.exp_df <- function(x, fontsize = 100) {
  
  x |> 
    gt() |> 
    fmt_number(c(claims, exposures), decimals = 0) |> 
    fmt_percent(starts_with("q_"), decimals = 1) |> 
    fmt_percent(starts_with("ae_"), decimals = 1) |> 
    tab_options(table.font.size = pct(fontsize))
  
}


autoplot.exp_df <- function(dat) {
  
  .groups <- attr(dat, "groups")
  
  # set up aesthetics
  x <- .groups[[1]]
  defaultNULL <- function(x) if (length(.groups) < x) NULL else .groups[[x]]
  color <- defaultNULL(2)
  fill <- defaultNULL(2)
  facet <- defaultNULL(3)
  
  p <- ggplot(dat, aes(!!x, q_obs, color = !!color)) + 
    geom_point() + 
    geom_line() + 
    scale_y_continuous(labels = scales::label_percent(accuracy = 0.1))
  
  if (is.null(facet)) return(p)
  p + facet_wrap(vars(!!facet))
  
}

#' @description Convert a census data frame to an exposure data frame
expose <- function(dat) {
  dat |> 
    slice(rep(row_number(), pol_yr)) |> 
    group_by(pol_num) |> 
    mutate(pol_yr = row_number()) |> 
    ungroup()
}

#' @description Attaches expected surrender rates
add_q_expected <- function(dat) {
  
  dat |> 
    mutate(age = age + t - 1,
           exercised = factor(age >= wd_age & inc_guar == "TRUE", 
                              levels = c("FALSE", "TRUE")),
           pol_yr2 = pmin(max(base_rates$pol_yr), pol_yr)) |> 
    left_join(base_rates, by = c("pol_yr2" = "pol_yr", "inc_guar")) |> 
    mutate(
      qual_mult = qual_mult(qual, age),
      age_mult = age_mult(age, inc_guar),
      prod_mult = prod_mult[product],
      gender_mult = gender_mult[gender],
      wd_time_mult = wd_time_mult(exercised, age, inc_guar),
      q_exp = pmin(qual_mult * age_mult * prod_mult * base_rate * wd_time_mult, 
                   0.99),
    ) |> 
    select(-pol_yr2)
  
}

# actuarial present value
apv <- function(age, gender, pol_yr, disc, inc_guar, rex_yr, q_surr) {
  q_death <- mortality$q_death[(age:omega_age) + 1 + 121 * (gender == "F")]
  t_omega <- length(q_death)
  # index for subsetting by policy year
  ix <- pol_yr - 1 + (1:t_omega)
  q_surr <- q_surr[pmin(length(q_surr), 1:t_omega)]
  # ensure no surrenders after AV reaches zero
  if (inc_guar == "TRUE") {
    no_av <- av_income[ix, rex_yr] == 0
    q_surr[no_av] <- 0
  }
  
  tpx <- cumprod((1 - q_death) * (1 - q_surr))
  v <- (1 + disc) ^ -(1:t_omega)
  terms <- -diff(c(1, tpx))
  
  # PV income payments
  if (inc_guar == "TRUE") {
    sum(v * (inc_amt * tpx * (ix >= rex_yr) + av_income[ix, rex_yr] * terms))
  } else {
    sum(v * (wd_no_income[ix] * tpx + av_no_income[ix] * terms))
  }
  
}

# actuarial annuity factor 
ax <- function(age, gender, disc, q_surr) {
  
  q_death <- mortality$q_death[(age:omega_age) + 1 + 121 * (gender == "F")]
  t_omega <- length(q_death)
  
  q_surr <- q_surr[pmin(length(q_surr), 1:t_omega)]
  
  tpx <- cumprod((1 - q_death) * (1 - q_surr))
  v <- (1 + disc) ^ -(1:t_omega)
  
  sum(tpx * v)
  
}

ae_check_apv <- function(x, ...) {
  
  .grps <- enquos(...)
  
  x |> 
    group_by(!!!.grps) |> 
    summarize(across(contains("apv"), sum), .groups = "drop") |> 
    mutate(ae_trad = apv_trad / apv_surr,
           ae_rf = apv_rf / apv_surr,
           ae_log = apv_log / apv_surr)
}

ae_check_ax <- function(x, ...) {
  
  .grps <- enquos(...)
  
  x |> 
    group_by(!!!.grps) |> 
    summarize(across(contains("ax"), sum), .groups = "drop") |> 
    mutate(ae_trad = ax_trad / ax_surr,
           ae_rf = ax_rf / ax_surr,
           ae_log = ax_log / ax_surr)
}


rmse_check_apv <- function(x, ...) {
  
  .grps <- enquos(...)
  
  x |> 
    select(starts_with("apv"), !!!.grps) |> 
    pivot_longer(-c(apv_surr, !!!.grps)) |> 
    group_by(name, !!!.grps) |> 
    rmse(truth = apv_surr, estimate = value) |> 
    mutate(.estimate = round(.estimate, 2))
}

rmse_check_ax <- function(x, ...) {
  
  .grps <- enquos(...)
  
  x |> 
    select(starts_with("ax"), !!!.grps) |> 
    pivot_longer(-c(ax_surr, !!!.grps)) |> 
    group_by(name, !!!.grps) |> 
    rmse(truth = ax_surr, estimate = value) |> 
    mutate(.estimate = round(.estimate, 2))
}

