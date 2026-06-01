#############
### SETUP ###
#############

#install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

##############
### PART 1 ###
##############

# load data
mlb_team_seasons = read_csv("../data/01_mlb-team-seasons.csv")

# transformed regression data for estimating alpha
pythag_data = mlb_team_seasons %>%
  mutate(
    logit_wp = log(WP / (1 - WP)),
    log_rs_ra = log(RS / RA)
  )

# no-intercept model: log(WP / (1 - WP)) = alpha * log(RS / RA)
alpha_model = lm(logit_wp ~ 0 + log_rs_ra, data = pythag_data)
summary(alpha_model)

# alpha estimate + uncertainty
alpha_hat = coef(alpha_model)[["log_rs_ra"]]
alpha_se = summary(alpha_model)$coefficients["log_rs_ra", "Std. Error"]
alpha_ci_95 = alpha_hat + c(-1, 1) * qt(0.975, df = df.residual(alpha_model)) * alpha_se

# helper for converting exponent alpha back to predicted WP
pythag_wp = function(rs, ra, alpha) {
  rs^alpha / (rs^alpha + ra^alpha)
}

# compare fitted-alpha model vs Bill James alpha = 2
pythag_data = pythag_data %>%
  mutate(
    wp_hat_alpha = pythag_wp(RS, RA, alpha_hat),
    wp_hat_bj = pythag_wp(RS, RA, 2),
    resid_alpha = WP - wp_hat_alpha,
    resid_bj = WP - wp_hat_bj
  )

rmse = function(y, yhat) sqrt(mean((y - yhat)^2, na.rm = TRUE))
rmse_alpha = rmse(pythag_data$WP, pythag_data$wp_hat_alpha)
rmse_bj = rmse(pythag_data$WP, pythag_data$wp_hat_bj)

##############
### PART 2 ###
##############

# load data
mlb_payrolls = read_csv("../data/01_mlb-payrolls.csv")

# remove 2020 covid-shortened season
payroll_data = mlb_payrolls %>%
  filter(year_id != 2020)

# model A: WP ~ payroll / median
model_a = lm(wp ~ payroll_median_ratio, data = payroll_data)

# model B: WP ~ log(payroll / median)
model_b = lm(wp ~ log_payroll_median_ratio, data = payroll_data)

# fitted values + residuals for both models
payroll_data = payroll_data %>%
  mutate(
    fitted_a = fitted(model_a),
    resid_a = resid(model_a),
    fitted_b = fitted(model_b),
    resid_b = resid(model_b)
  )

# example: CI and PI for one selected team-season
# replace row_idx with your selected row index
row_idx = 1
selected_row = payroll_data[row_idx, ]

ci_model_b = predict(model_b, newdata = selected_row, interval = "confidence", level = 0.95)
pi_model_b = predict(model_b, newdata = selected_row, interval = "prediction", level = 0.95)
