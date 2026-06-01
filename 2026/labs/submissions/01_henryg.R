#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

##############
### PART 1 ###
##############

# load team-season data
mlb_team_seasons = read_csv("../data/01_mlb-team-seasons.csv")

# transform variables used in the Pythagorean exponent model
pythag_data = mlb_team_seasons %>%
  mutate(
    logit_wp = log(WP / (1 - WP)),
    log_rs_ra = log(RS / RA)
  )

# TODO:
# 1) Fit no-intercept model: logit_wp ~ 0 + log_rs_ra
# 2) Extract alpha estimate and a 95% confidence interval
# 3) Build helper to convert RS/RA + alpha -> predicted WP
# 4) Compare fitted-alpha predictions vs Bill James alpha = 2

##############
### PART 2 ###
##############

# load payroll data
mlb_payrolls = read_csv("../data/01_mlb-payrolls.csv")

# remove 2020 covid-shortened season
payroll_data = mlb_payrolls %>%
  filter(year_id != 2020)

# TODO:
# 1) Fit model A: wp ~ payroll_median_ratio
# 2) Fit model B: wp ~ log_payroll_median_ratio
# 3) Compare fits (residual behavior, RMSE, and interpretation)
# 4) Compute confidence and prediction intervals for a selected team-season
