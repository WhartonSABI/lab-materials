#############
### SETUP ###
#############

# install.packages(c("ggplot2", "nnet", "splines", "tidyverse"))
library(ggplot2)
library(nnet)
library(splines)
library(tidyverse)

# set seed
set.seed(6)

##############
### PART 1 ###
##############

# run from 2026/labs/submissions/ or 2026/labs/starter-code/
nfl_data = read_csv("../data/05_expected-points.csv")

# Task 1:
# - Fit a multinomial logistic regression for pts_next_score using yardline_100 only
# - Start with expected points modeled as a purely linear function of yardline_100
# - Convert fitted class probabilities into expected points
# - Plot estimated expected points against yardline_100
# - State briefly what implausible pattern this linear-yardline model produces

# Task 2:
# - Replace the linear yardline term with a spline using splines::bs(...)
# - Refit the multinomial model and recompute expected points
# - Plot the revised expected-points curve against yardline_100
# - Briefly explain why the spline improves on the linear-yardline model

# Task 3:
# - Extend the model to include yardline_100 and down
# - Decide whether down should be encoded as numeric or categorical
# - Plot expected points against yardline_100 and color by down
# - Briefly explain how down should be encoded and why

# Task 4:
# - Extend the model to include yardline_100, down, and ydstogo
# - Consider whether ydstogo should enter linearly, on a log scale, or through a spline
# - Plot expected points against yardline_100
# - Color by ydstogo and facet by down
# - Briefly describe how yards to go changes the expected-points surface across downs

# Task 5:
# - Add half_seconds_remaining to your model
# - Try both:
#   * a linear term in half_seconds_remaining
#   * a spline term in half_seconds_remaining
# - Restrict attention to 1st-and-10 when building the comparison plot
# - Plot expected points against yardline_100 and color by time remaining
# - Make one plot for the linear-time model and one for the spline-time model
# - Briefly compare the two time specifications

##############
### PART 2 ###
##############

# Task 1:
# - Let M be your preferred expected-points model from Part 1
# - Fit an adjusted model M_prime that also includes posteam_spread
# - A linear spread term is a reasonable starting point

# Task 2:
# - Compare expected points from M_prime at posteam_spread = 0 to expected points from M
# - Overlay the two curves as a function of yardline_100
# - Plot the difference M_prime(spread = 0) - M as a function of yardline_100
# - Briefly explain why conditioning on spread changes the target estimand

# Discussion:
# - Are these the same or different, and why?
#   * the percentage of all 3-point attempts made in the NBA this year
#   * the true 3-point make percentage of an average NBA player
# - If they differ, state which you expect to be higher
# - Briefly explain how you could adjust for this selection-bias problem
