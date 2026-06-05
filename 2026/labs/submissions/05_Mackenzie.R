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
nfl_data = read_csv("/Users/mackenziebuckner/Desktop/lab-materials/2026/labs/data/05_expected-points.csv")

get_expected_points <- function(model, new_data) {
  probs <- predict(model, newdata = new_data, type = "probs")
  # Convert to data frame in case predict returns a matrix
  probs <- as.data.frame(probs)
  # Make sure column names are numeric point values
  point_values <- as.numeric(colnames(probs))
  ep <- as.matrix(probs) %*% point_values
  return(as.numeric(ep))
}

# Task 1:
# - Fit a multinomial logistic regression for pts_next_score using yardline_100 only
# - Start with expected points modeled as a purely linear function of yardline_100
model_1_1 <- multinom(
  pts_next_score ~ yardline_100,
  data = nfl_data,
  trace = FALSE
)

# Create yard line grid for plotting
yardline_grid <- tibble(
  yardline_100 = seq(1, 99, by = 1)
)

# - Convert fitted class probabilities into expected points
yardline_grid <- yardline_grid %>% mutate(ep_linear = get_expected_points(model_1_1, yardline_grid))

# - Plot estimated expected points against yardline_100
ggplot(yardline_grid, aes(x = yardline_100, y = ep_linear)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Task 1.1: Expected Points vs Yard Line",
    subtitle = "Multinomial logistic regression with linear yard line",
    x = "Yard Line: Distance to Opponent's End Zone",
    y = "Expected Points"
  ) +
  theme_minimal(base_size = 13)

# - State briefly what implausible pattern this linear-yardline model produces
# The linear-yardline model is too rigid because it forces expected points
# to change in a straight line as field position changes, creating an
# implausible EP curve that cannot capture the real nonlinear value of field position.

# Task 2:
# - Replace the linear yardline term with a spline using splines::bs(...)
model_1_2 <- multinom(
  pts_next_score ~ bs(yardline_100, df = 6),
  data = nfl_data,
  trace = FALSE
)

# - Refit the multinomial model and recompute expected points
yardline_grid <- yardline_grid %>%
  mutate(
    ep_spline = get_expected_points(model_1_2, yardline_grid)
  )

# - Plot the revised expected-points curve against yardline_100
ggplot(yardline_grid, aes(x = yardline_100, y = ep_spline)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Task 1.2: Expected Points vs Yard Line",
    subtitle = "Multinomial logistic regression with spline on yard line",
    x = "Yard Line: Distance to Opponent's End Zone",
    y = "Expected Points"
  ) +
  theme_minimal(base_size = 13)
# - Briefly explain why the spline improves on the linear-yardline model
# The spline improves the model because it allows expected points to change
# nonlinearly with field position rather than forcing one constant linear trend.

# Task 3:
# - Extend the model to include yardline_100 and down
nfl_data <- nfl_data %>%
  mutate(
    down = factor(down, levels = c(1, 2, 3, 4))
  )

# - Decide whether down should be encoded as numeric or categorical
# Down should be encoded as a categorical variable because it represents distinct game situations 
# (1st, 2nd, 3rd, 4th down) rather than a continuous numeric relationship. Each down has a different strategic 
# implication for the offense, and treating it as categorical allows the model to capture these differences without
# assuming a linear relationship between down and expected points.

# Fit model with spline yard line + categorical down
model_1_3 <- multinom(
  pts_next_score ~ bs(yardline_100, df = 6) + down,
  data = nfl_data,
  trace = FALSE
)

# Create prediction grid:
yardline_down_grid <- expand_grid(
  yardline_100 = seq(1, 99, by = 1),
  down = factor(c(1, 2, 3, 4), levels = c(1, 2, 3, 4))
)

# Predict expected points
yardline_down_grid <- yardline_down_grid %>%
  mutate(
    ep_down = get_expected_points(model_1_3, yardline_down_grid)
  )

# - Plot expected points against yardline_100 and color by down
ggplot(yardline_down_grid, aes(x = yardline_100, y = ep_down, color = down)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Task 1.3: Expected Points by Yard Line and Down",
    subtitle = "Multinomial logistic regression with spline yard line and categorical down",
    x = "Yard Line: Distance to Opponent's End Zone",
    y = "Expected Points",
    color = "Down"
  ) +
  theme_minimal(base_size = 13)

# - Briefly explain how down should be encoded and why
# Down should be encoded as a categorical variable because it represents distinct game situations
# (1st, 2nd, 3rd, 4th down) rather than a continuous numeric relationship. Each down has a different strategic
# implication for the offense, and treating it as categorical allows the model to capture these differences without
# assuming a linear relationship between down and expected points.

# Task 4:
# Make sure down is categorical
nfl_data <- nfl_data %>%
  mutate(
    down = factor(down, levels = c(1, 2, 3, 4))
  ) %>%
  filter(
    !is.na(pts_next_score),
    !is.na(yardline_100),
    !is.na(down),
    !is.na(ydstogo)
  )

# - Extend the model to include yardline_100, down, and ydstogo
model_1_4 <- multinom(
  pts_next_score ~ bs(yardline_100, df = 6) + down + bs(ydstogo, df = 4),
  data = nfl_data,
  trace = FALSE
)

# - Consider whether ydstogo should enter linearly, on a log scale, or through a spline
yardline_down_ydstogo_grid <- expand_grid(
  yardline_100 = seq(1, 99, by = 1),
  down = factor(c(1, 2, 3, 4), levels = c(1, 2, 3, 4)),
  ydstogo = c(1, 5, 10, 15, 20)
)

# Predict expected points for the Task 1.4 model
yardline_down_ydstogo_grid <- yardline_down_ydstogo_grid %>%
  mutate(
    ep_1_4 = get_expected_points(model_1_4, yardline_down_ydstogo_grid),
    ydstogo_label = factor(ydstogo)
  )

# - Plot expected points against yardline_100
# - Color by ydstogo and facet by down
ggplot(
  yardline_down_ydstogo_grid,
  aes(
    x = yardline_100,
    y = ep_1_4,
    color = ydstogo_label,
    group = ydstogo_label
  )
) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ down) +
  labs(
    title = "Task 1.4: Expected Points by Yard Line, Down, and Yards to Go",
    subtitle = "Multinomial logistic regression with spline yard line, categorical down, and spline yards to go",
    x = "Yard Line: Distance to Opponent's End Zone",
    y = "Expected Points",
    color = "Yards to Go"
  ) +
  theme_minimal(base_size = 13)


model_1_4 <- multinom(
  pts_next_score ~ bs(yardline_100, df = 6) + down + ydstogo,
  data = nfl_data,
  trace = FALSE
)

# - Briefly describe how yards to go changes the expected-points surface across downs
# Yards to go changes the expected-points surface across downs by reflecting the increased 
# difficulty of converting a first down as yards to go increases. For example, on 1st down, 
# having 10 yards to go may not significantly reduce expected points compared to 1 yard to go, 
# but on 3rd down, having 10 yards to go can drastically reduce expected points because 
# it is much harder to convert. The model captures this interaction by allowing the effect of 
# yards to go on expected points to vary across different downs.

# Task 5:
# - Add half_seconds_remaining to your model
# - Try both:
#   * a linear term in half_seconds_remaining
model_1_5_linear_time <- multinom(
  pts_next_score ~ 
    bs(yardline_100, df = 6) +
    down +
    bs(ydstogo, df = 4) +
    half_seconds_remaining,
  data = nfl_data,
  trace = FALSE
)

#   * a spline term in half_seconds_remaining
model_1_5_spline_time <- multinom(
  pts_next_score ~ 
    bs(yardline_100, df = 6) +
    down +
    bs(ydstogo, df = 4) +
    bs(half_seconds_remaining, df = 5),
  data = nfl_data,
  trace = FALSE
)

# - Restrict attention to 1st-and-10 when building the comparison plot
time_values <- c(120, 300, 600, 1200, 1800)

time_grid <- expand_grid(
  yardline_100 = seq(1, 99, by = 1),
  down = factor(1, levels = c(1, 2, 3, 4)),
  ydstogo = 10,
  half_seconds_remaining = time_values
) %>%
  mutate(
    time_label = factor(
      half_seconds_remaining,
      levels = time_values,
      labels = c("2 min", "5 min", "10 min", "20 min", "30 min")
    )
  )


time_grid <- time_grid %>%
  mutate(
    ep_linear_time = get_expected_points(model_1_5_linear_time, time_grid),
    ep_spline_time = get_expected_points(model_1_5_spline_time, time_grid)
  )

# - Plot expected points against yardline_100 and color by time remaining
# - Make one plot for the linear-time model and one for the spline-time model

ggplot(
  time_grid,
  aes(
    x = yardline_100,
    y = ep_linear_time,
    color = time_label,
    group = time_label
  )
) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Task 1.5: Expected Points with Linear Time Term",
    subtitle = "1st down and 10 yards to go",
    x = "Yard Line: Distance to Opponent's End Zone",
    y = "Expected Points",
    color = "Time Remaining"
  ) +
  theme_minimal(base_size = 13)


ggplot(
  time_grid,
  aes(
    x = yardline_100,
    y = ep_spline_time,
    color = time_label,
    group = time_label
  )
) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Task 1.5: Expected Points with Spline Time Term",
    subtitle = "1st down and 10 yards to go",
    x = "Yard Line: Distance to Opponent's End Zone",
    y = "Expected Points",
    color = "Time Remaining"
  ) +
  theme_minimal(base_size = 13)

# - Briefly compare the two time specifications
# The linear-time model assumes that each additional second remaining in the half
# has the same effect on expected points. This is probably too restrictive because
# time matters much more near the end of the half than it does earlier in the half.
# The spline-time model is more flexible because it allows the effect of time
# remaining to change nonlinearly, especially in late-half situations.


##############
### PART 2 ###
##############
nfl_data_task2 <- nfl_data %>%
  mutate(
    pts_next_score = factor(
      pts_next_score,
      levels = c(0, -7, -3, -2, 2, 3, 7)
    ),
    down = factor(down, levels = c(1, 2, 3, 4))
  ) %>%
  filter(
    !is.na(pts_next_score),
    !is.na(yardline_100),
    !is.na(down),
    !is.na(ydstogo),
    !is.na(half_seconds_remaining),
    !is.na(posteam_spread)
  )

# Helper function to convert predicted probabilities into expected points
get_expected_points <- function(model, new_data) {
  
  probs <- predict(model, newdata = new_data, type = "probs")
  probs <- as.data.frame(probs)
  
  point_values <- as.numeric(colnames(probs))
  
  ep <- as.matrix(probs) %*% point_values
  
  return(as.numeric(ep))
}


# Task 1:
# - Let M be your preferred expected-points model from Part 1
M <- multinom(
  pts_next_score ~
    bs(yardline_100, df = 6) +
    down +
    bs(ydstogo, df = 4) +
    bs(half_seconds_remaining, df = 5),
  data = nfl_data_task2,
  trace = FALSE
)

# - Fit an adjusted model M_prime that also includes posteam_spread
M_prime <- multinom(
  pts_next_score ~
    bs(yardline_100, df = 6) +
    down +
    bs(ydstogo, df = 4) +
    bs(half_seconds_remaining, df = 5) +
    posteam_spread,
  data = nfl_data_task2,
  trace = FALSE
)

# - A linear spread term is a reasonable starting point
# M estimates expected points using field position, down, yards to go, and time remaining.
# M-prime estimates expected points after also controlling for pre-game team strength
# through posteam_spread.



# Task 2
# - Compare expected points from M_prime at posteam_spread = 0 to expected points from M
compare_grid <- tibble(
  yardline_100 = seq(1, 99, by = 1),
  down = factor(1, levels = c(1, 2, 3, 4)),
  ydstogo = 10,
  half_seconds_remaining = 900,
  posteam_spread = 0
)

# Predict EP from both models
compare_grid <- compare_grid %>%
  mutate(
    ep_M = get_expected_points(M, compare_grid),
    ep_M_prime_spread_0 = get_expected_points(M_prime, compare_grid),
    difference = ep_M_prime_spread_0 - ep_M
  )

# - Overlay the two curves as a function of yardline_100
compare_grid_long <- compare_grid %>%
  select(yardline_100, ep_M, ep_M_prime_spread_0) %>%
  pivot_longer(
    cols = c(ep_M, ep_M_prime_spread_0),
    names_to = "model",
    values_to = "expected_points"
  ) %>%
  mutate(
    model = recode(
      model,
      ep_M = "M: No spread adjustment",
      ep_M_prime_spread_0 = "M-prime: Spread-adjusted, spread = 0"
    )
  )

ggplot(
  compare_grid_long,
  aes(
    x = yardline_100,
    y = expected_points,
    linetype = model
  )
) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Task 2: Comparing M and M-prime",
    subtitle = "1st down, 10 yards to go, 15 minutes left, spread = 0",
    x = "Yard Line: Distance to Opponent's End Zone",
    y = "Expected Points",
    linetype = "Model",
  ) +
  theme_minimal(base_size = 13)

# - Plot the difference M_prime(spread = 0) - M as a function of yardline_100
compare_grid <- compare_grid %>%
  mutate(
    ep_M = get_expected_points(M, compare_grid),
    ep_M_prime_spread_0 = get_expected_points(M_prime, compare_grid),
    difference = ep_M_prime_spread_0 - ep_M
  )
ggplot(compare_grid, aes(x = yardline_100, y = difference)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Difference Between Spread-Adjusted and Unadjusted EP",
    subtitle = "Difference = EP from M-prime at spread = 0 minus EP from M",
    x = "Yard Line: Distance to Opponent's End Zone",
    y = "Difference in Expected Points"
  ) +
  theme_minimal(base_size = 13)
compare_grid <- tibble(
  yardline_100 = seq(1, 99, by = 1),
  down = factor(1, levels = c(1, 2, 3, 4)),
  ydstogo = 10,
  half_seconds_remaining = 900,
  posteam_spread = 0
)

# - Briefly explain why conditioning on spread changes the target estimated
# M and M-prime are not necessarily the same, even when posteam_spread is set to 0.
# M estimates expected points under the empirical distribution of observed plays,
# meaning it reflects the actual mix of teams and situations in the data.
# M-prime conditions on team quality, so setting posteam_spread = 0 estimates
# expected points for a neutral team-strength matchup.
# Because stronger teams may generate more plays and better scoring opportunities,
# adjusting for spread changes the target estimand from observed-average EP
# to neutral-team-quality EP.


# Discussion:
# - Are these the same or different, and why?
# These two quantities are different.
#   * the percentage of all 3-point attempts made in the NBA this year
#   * the true 3-point make percentage of an average NBA player

# The percentage of all 3-point attempts made in the NBA this year is an
# attempt-weighted average. Players who shoot more 3s have more influence on
# this number.
# The true 3-point make percentage of an average NBA player is a player-weighted
# quantity. Each player would count more equally, regardless of how many 3s they
# actually attempted.

# - If they differ, state which you expect to be higher
# I would expect the percentage of all 3-point attempts made to be higher than
# the true 3-point percentage of an average NBA player because better shooters
# are usually allowed to take more 3-point attempts. Poor shooters take fewer
# attempts, so they have less influence on the league-wide attempt percentage.
# - Briefly explain how you could adjust for this selection-bias problem
# This is a selection-bias problem because the observed shot attempts are not a
# random sample from all NBA players. They are selected based on coach decisions,
# player skill, offensive role, and game context.
# One way to adjust for this would be to estimate each player's true 3-point
# ability first, then average those player-level estimates equally. Another
# approach would be to fit a model that controls for player identity, shot
# difficulty, and shot context, then estimate the make percentage for an average
# player under a common set of conditions.