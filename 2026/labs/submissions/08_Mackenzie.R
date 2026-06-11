#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

# set seed
set.seed(8)

#########################
### PART 1: NBA FREE THROWS ###
#########################

# load data
nba_players = read_delim(
  "/Users/mackenziebuckner/Desktop/lab-materials/2026/labs/data/08_nba-free-throws.csv",
  delim = ";",
  show_col_types = FALSE
)

# Task 1:
# - Modify the dataset to include:
#   * Player
#   * FT_total = total free throws made across the season
#   * FTA_total = total free throws attempted across the season
#   * FT_percent = FT_total / FTA_total
# - Remember that FT and FTA are per-game values, so convert them to totals using G

nba_players_cleaned = nba_players %>%
  group_by(Player) %>%
  filter(
    # If a player has a TOT row, keep only the TOT row.
    # If a player does not have a TOT row, keep their regular row.
    if (any(Tm == "TOT")) Tm == "TOT" else TRUE
  ) %>%
  ungroup() %>%
  mutate(
    FT_total = round(FT * G),
    FTA_total = round(FTA * G),
    FT_percent = FT_total / FTA_total
  ) %>%
  
  # Task 2:
  # - Filter the dataset to players with at least 25 total free-throw attempts
  # - Decide how you want to handle players with multiple team rows
  # - Make sure the final player-level dataset has one row per player-season
  filter(FTA_total >= 25) %>%
  select(
    Player,
    Tm,
    G,
    FT,
    FTA,
    FT_total,
    FTA_total,
    FT_percent
  )

# Check cleaned data
print(nba_players_cleaned)

# Task 3:
# - Construct 95% Wald confidence intervals for each player's free-throw probability
# - Construct 95% Agresti-Coull confidence intervals for each player's free-throw probability
# - Make a plot with:
#   * x-axis = FT_percent
#   * y-axis = player name
#   * both interval types overlaid
# - Comment on which intervals look most different and why

z = 1.96

nba_players_ci = nba_players_cleaned %>%
  mutate(
    n = FTA_total,
    x = FT_total,
    p_hat = FT_percent,
    
    # Wald confidence interval
    wald_se = sqrt(p_hat * (1 - p_hat) / n),
    wald_lower = p_hat - z * wald_se,
    wald_upper = p_hat + z * wald_se,
    
    # Agresti-Coull confidence interval
    n_tilde = n + z^2,
    p_tilde = (x + z^2 / 2) / n_tilde,
    ac_se = sqrt(p_tilde * (1 - p_tilde) / n_tilde),
    ac_lower = p_tilde - z * ac_se,
    ac_upper = p_tilde + z * ac_se
  )

# To make the plot readable, we will plot the 40 players with the fewest
# free-throw attempts. These are where the two interval types differ most.
# If you want every player, remove the slice_min() line.

plot_data = nba_players_ci %>%
  slice_min(order_by = FTA_total, n = 40) %>%
  arrange(FT_percent) %>%
  mutate(
    Player = factor(Player, levels = Player),
    y = as.numeric(Player)
  )

ci_long = bind_rows(
  plot_data %>%
    transmute(
      Player,
      y = y - 0.12,
      method = "Wald",
      lower = wald_lower,
      upper = wald_upper
    ),
  plot_data %>%
    transmute(
      Player,
      y = y + 0.12,
      method = "Agresti-Coull",
      lower = ac_lower,
      upper = ac_upper
    )
)

ggplot() +
  geom_segment(
    data = ci_long,
    aes(x = lower, xend = upper, y = y, yend = y, color = method),
    linewidth = 0.9,
    alpha = 0.85
  ) +
  geom_point(
    data = plot_data,
    aes(x = FT_percent, y = y),
    size = 1.6
  ) +
  scale_y_continuous(
    breaks = plot_data$y,
    labels = plot_data$Player
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  coord_cartesian(xlim = c(0, 1)) +
  labs(
    title = "NBA Free-Throw Percentage with 95% Confidence Intervals",
    subtitle = "Wald vs. Agresti-Coull intervals for players with the fewest attempts",
    x = "Free-throw percentage",
    y = "Player",
    color = "Interval type"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 7),
    plot.title = element_text(face = "bold")
  )
# Written answer for Task 3:
# The Wald and Agresti-Coull intervals look most different for players with
# small numbers of free-throw attempts and for players whose FT_percent is
# close to 0 or 1. The Wald interval uses p_hat directly, so it can behave
# poorly near the boundaries. The Agresti-Coull interval adjusts the estimate
# slightly toward the middle, which usually gives more sensible intervals
# when the sample size is small or the observed percentage is extreme.

###########################
### PART 2: LLN WARM-UP ###
###########################

lln_p = 0.75
lln_n = 1000
lln_paths = 100

# Task 1:
# - Simulate one Bernoulli sequence with true probability p = 0.75 and length 1000
# - Compute the running estimate
#     phat_n = (1 / n) * sum_{i=1}^n X_i
#   for n = 1, ..., 1000

lln_one_path = tibble(
  n = 1:lln_n,
  x = rbinom(n = lln_n, size = 1, prob = lln_p)
) %>%
  mutate(
    phat_n = cumsum(x) / n
  )

print(head(lln_one_path))

# Task 2:
# - Plot the running estimate against n
# - Add a horizontal line at p = 0.75

ggplot(lln_one_path, aes(x = n, y = phat_n)) + 
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = lln_p, linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Law of Large Numbers: One Simulated Free-Throw Sequence",
    subtitle = "Running estimate of free-throw percentage with true p = 0.75",
    x = "Number of free throws attempted",
    y = "Running estimate of p"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  )

# - Describe what happens as n grows
# As n grows, the running estimate phat_n gets closer to the true probability
# p = 0.75. Early on, the estimate jumps around a lot because it is based on
# only a few free throws. As more attempts are included, the estimate becomes
# more stable.

# Task 3:
# - Repeat the simulation 100 times
# - Overlay the 100 running-estimate paths on one figure
# - Describe how the variability changes with n

lln_many_paths = crossing(
  path = 1:lln_paths,
  n = 1:lln_n
) %>%
  group_by(path) %>%
  mutate(
    x = rbinom(n = n(), size = 1, prob = lln_p),
    phat_n = cumsum(x) / n
  ) %>%
  ungroup()

ggplot(lln_many_paths, aes(x = n, y = phat_n, group = path)) +
  geom_line(alpha = 0.25) +
  geom_hline(yintercept = lln_p, linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Law of Large Numbers: 100 Simulated Free-Throw Paths",
    subtitle = "Each path shows the running estimate of p over 1000 attempts",
    x = "Number of free throws attempted",
    y = "Running estimate of p"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  )

# Written description for Task 3:
# Across the 100 simulated paths, the estimates vary a lot when n is small.
# As n gets larger, the paths become less spread out and concentrate around
# the true value p = 0.75. This shows the Law of Large Numbers: sample averages
# become more stable as the sample size increases.

################################
### PART 3: SIMULATION STUDY ###
################################

z_975 = qnorm(0.975)
p_grid = seq(0, 1, length.out = 1000)
sample_sizes = c(10, 50, 100, 250, 500, 1000)
M = 100

# A convenient helper structure is:
# - loop over p
# - loop over n
# - simulate Binomial(n, p) data
# - compute both intervals
# - record whether each interval contains the true p

# Task 1:
# - Use the grid p_grid as your set of candidate true probabilities
# - For each p and each n in sample_sizes, generate binomial data

simulation_results = crossing(
  p = p_grid,
  n = sample_sizes,
  sim = 1:M
)%>%
  mutate(
    # Simulate total made free throws:
    # S_n ~ Binomial(n, p)
    S_n = rbinom(n = n(), size = n, prob = p),
    
    # Sample proportion
    p_hat = S_n / n,
    
# Task 2:
# - Compute the 95% Wald confidence interval
# - Compute the 95% Agresti-Coull confidence interval using
#     n_tilde = n + z_975^2
#     p_tilde = (S_n + z_975^2 / 2) / n_tilde
# - Interpret this as approximately adding 2 successes and 2 failures

    wald_se = sqrt(p_hat * (1 - p_hat) / n),
    
    wald_lower = p_hat - z_975 * wald_se,
    wald_upper = p_hat + z_975 * wald_se,
    
    # Does the Wald interval contain the true p?
    wald_covers = wald_lower <= p & p <= wald_upper,
    
    n_tilde = n + z_975^2,
    p_tilde = (S_n + z_975^2 / 2) / n_tilde,
    
    ac_se = sqrt(p_tilde * (1 - p_tilde) / n_tilde),
    
    ac_lower = p_tilde - z_975 * ac_se,
    ac_upper = p_tilde + z_975 * ac_se,
    
    # Does the Agresti-Coull interval contain the true p?
    ac_covers = ac_lower <= p & p <= ac_upper
  )

# Task 3:
# - Repeat the simulation M = 100 times for each (n, p) pair
# - For each method, estimate coverage as the fraction of intervals that contain p

# Estimate coverage probability for each method, sample size, and p
coverage_results = simulation_results %>%
  group_by(n, p) %>%
  summarise(
    Wald = mean(wald_covers),
    `Agresti-Coull` = mean(ac_covers),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(Wald, `Agresti-Coull`),
    names_to = "method",
    values_to = "coverage"
  )
    
# Task 4:
# - Plot coverage probability vs p
# - Facet by sample size
# - Include both methods
# - Add horizontal line at 0.95

ggplot(coverage_results, aes(x = p, y = coverage, color = method)) +
  geom_line(alpha = 0.8, linewidth = 0.6) +
  geom_hline(yintercept = 0.95, linetype = "dashed", linewidth = 0.8) +
  facet_wrap(~ n, labeller = label_both) +
  labs(
    title = "Coverage Probability of 95% Confidence Intervals",
    subtitle = "Wald vs. Agresti-Coull intervals across true probabilities and sample sizes",
    x = "True probability p",
    y = "Estimated coverage probability",
    color = "Interval method"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  )

# Written comment for Task 4:
# The Wald interval tends to undercover when p is close to 0 or 1,
# especially for small sample sizes like n = 10 or n = 50. This happens
# because the Wald interval depends directly on p_hat, and when p_hat is
# close to 0 or 1, the estimated standard error can become too small.
# The Agresti-Coull interval usually performs better near the boundaries
# because it adjusts the estimate by approximately adding 2 successes and
# 2 failures. As n increases, both methods generally get closer to the
# target 95% coverage level.


############################
### PART 4: SKITTLES DEMO ###
############################

# Replace these with your observed counts from the Skittles activity
skittles_n = 89
skittles_r = 16

z_975 = qnorm(0.975)

# Task 1:
# - Compute the observed red proportion r / n
# - Construct a 95% Wald confidence interval for the true red probability

skittles_p_hat = skittles_r / skittles_n

skittles_wald_se = sqrt(
  skittles_p_hat * (1 - skittles_p_hat) / skittles_n
)

skittles_wald_lower = skittles_p_hat - z_975 * skittles_wald_se
skittles_wald_upper = skittles_p_hat + z_975 * skittles_wald_se


# Task 2:
# - Construct a 95% Agresti-Coull confidence interval for the same probability

skittles_n_tilde = skittles_n + z_975^2

skittles_p_tilde = (
  skittles_r + z_975^2 / 2
) / skittles_n_tilde

skittles_ac_se = sqrt(
  skittles_p_tilde * (1 - skittles_p_tilde) / skittles_n_tilde
)

skittles_ac_lower = skittles_p_tilde - z_975 * skittles_ac_se
skittles_ac_upper = skittles_p_tilde + z_975 * skittles_ac_se


# Print results in a clean table

skittles_results = tibble(
  method = c("Wald", "Agresti-Coull"),
  estimate = c(skittles_p_hat, skittles_p_tilde),
  lower = c(skittles_wald_lower, skittles_ac_lower),
  upper = c(skittles_wald_upper, skittles_ac_upper)
)

print(skittles_results)


# Task 3:
# - Compare the two intervals
# - State which one seems more sensible when the observed red proportion is near 0 or 1

# The observed red proportion is 16 / 89 = about 0.180, or 18.0%.
# The Wald and Agresti-Coull intervals are fairly similar here because the
# sample size is not tiny and the observed proportion is not extremely close
# to 0 or 1.
# However, if the observed red proportion were very close to 0 or 1, the
# Agresti-Coull interval would usually be more sensible. The Wald interval can
# behave poorly near the boundaries because it uses p_hat directly in the
# standard error. This can make the interval too narrow or even produce
# impossible probability values below 0 or above 1. The Agresti-Coull interval
# adjusts the estimate toward the middle by approximately adding 2 successes
# and 2 failures, making it more stable.