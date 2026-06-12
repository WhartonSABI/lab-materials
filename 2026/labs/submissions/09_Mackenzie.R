#############
### SETUP ###
#############

# install.packages(c("ggplot2", "nnet", "readr", "splines", "tidyverse"))
library(tidyverse)
library(nnet)      # multinom()
library(splines)   # bs()

set.seed(9)


##############################
### PART 1: EXPECTED POINTS ###
##############################

# run from 2026/labs/submissions/ or 2026/labs/starter-code/
ep_data <- read_csv("/Users/mackenziebuckner/Desktop/lab-materials/2026/labs/data/09_expected-points.csv", show_col_types = FALSE) %>%
  drop_na(
    game_id,
    pts_next_score,
    yardline_100,
    down,
    ydstogo,
    half_seconds_remaining,
    posteam_spread
  ) %>%
  mutate(
    # Treat down as categorical
    down = factor(down),
    
    # Multinomial model needs the response as a factor
    pts_next_score_factor = factor(pts_next_score)
  )


# Task 1:
  # - Refit your preferred expected-points model from Lab 5
  # - A concrete default is:
  #   pts_next_score ~ bs(yardline_100, df = 6) +
  #                    factor(down) +
  #                    log(ydstogo) +
  #                    bs(half_seconds_remaining, df = 5)
  # - Convert fitted class probabilities into expected points

ep_formula <- pts_next_score_factor ~ 
  bs(yardline_100, df = 5) +
  down +
  log1p(ydstogo) +
  bs(half_seconds_remaining, df = 5) +
  posteam_spread

ep_model <- multinom(
  ep_formula,
  data = ep_data,
  trace = FALSE,
  MaxNWts = 20000,
  maxit = 300
)

# Target state for the bootstrap study:

target_state <- tibble(
  yardline_100 = 35,
  down = factor(1, levels = levels(ep_data$down)),
  ydstogo = 10,
  half_seconds_remaining = 1800,
  posteam_spread = 0
)


# Optional helper:
# - Write a function that takes a matrix/data frame of predicted class probabilities
#   and converts it to expected points using score_values

# Store possible scoring outcomes as numbers
score_levels <- levels(ep_data$pts_next_score_factor)
score_values <- as.numeric(score_levels)

get_target_ep <- function(model) {
  
  # Predict probabilities of each possible next scoring outcome
  probs <- predict(model, newdata = target_state, type = "probs")
  
  # If predict gives a matrix, keep the first row
  if (is.matrix(probs)) {
    probs <- probs[1, ]
  }
  
  # Make sure probabilities are in the same order as the score values
  probs <- probs[score_levels]
  
  # Expected points = sum(probability * scoring value)
  ep <- sum(probs * score_values)
  
  return(ep)
}

target_ep_hat <- get_target_ep(ep_model)

cat("\nFitted expected points at target state:", round(target_ep_hat, 3), "\n")



# Task 2:
# - Decide which bootstrap variation is most appropriate here:
#   * observation bootstrap
#   * cluster bootstrap
#   * parametric bootstrap
#   * residual bootstrap
# - State your choice in comments
# - Explain why it matches the dependence structure of this dataset

# I use a cluster bootstrap by game_id.
# Why this is appropriate:
# NFL play-by-play rows are not independent. Plays from the same game share
# teams, weather, strategy, game script, and other game-level factors.
# A row-by-row bootstrap would incorrectly treat every play as independent.
# The cluster bootstrap resamples whole games with replacement, which keeps
# the within-game dependence structure intact.


# Task 3:
# - Implement your chosen bootstrap with at least B = 200 resamples
# - For each resample:
#   * create a bootstrap dataset
#   * refit the EP model
#   * recompute expected points at target_state

B_ep <- 200

# Split data into one dataset per game
ep_by_game <- split(ep_data, ep_data$game_id)
game_ids <- names(ep_by_game)

boot_ep <- numeric(B_ep)

for (b in 1:B_ep) {
  
  # Print progress every 10 bootstrap samples
  if (b %% 10 == 0) {
    cat("Expected-points bootstrap sample:", b, "of", B_ep, "\n")
  }
  
  # Resample games with replacement
  sampled_games <- sample(game_ids, size = length(game_ids), replace = TRUE)
  
  # Combine all plays from the sampled games
  boot_data <- bind_rows(ep_by_game[sampled_games])
  
  # Refit model on bootstrap sample
  boot_fit <- tryCatch(
    multinom(
      ep_formula,
      data = boot_data,
      trace = FALSE,
      MaxNWts = 20000,
      maxit = 300
    ),
    error = function(e) NULL
  )
  
  # If model fails to fit, store NA
  if (is.null(boot_fit)) {
    boot_ep[b] <- NA
  } else {
    boot_ep[b] <- get_target_ep(boot_fit)
  }
}

# Remove failed bootstrap samples, if any
boot_ep <- boot_ep[!is.na(boot_ep)]



# Expected-points bootstrap results

# Task 5:
# - Compute:
#   * the original fitted expected-points estimate
#   * the bootstrap standard error
#   * the 95% percentile interval

ep_boot_se <- sd(boot_ep)

ep_boot_ci <- quantile(
  boot_ep,
  probs = c(0.025, 0.975),
  names = FALSE
)

ep_results <- tibble(
  target_ep_estimate = target_ep_hat,
  bootstrap_standard_error = ep_boot_se,
  percentile_ci_lower = ep_boot_ci[1],
  percentile_ci_upper = ep_boot_ci[2],
  bootstrap_samples_used = length(boot_ep)
)

print(ep_results)


# Task 4:
# - Store the bootstrap estimates in a vector
# - Make a plot of the bootstrap distribution

ep_boot_plot <- tibble(boot_ep = boot_ep) %>%
  ggplot(aes(x = boot_ep)) +
  geom_histogram(bins = 30, color = "white") +
  geom_vline(xintercept = target_ep_hat, linewidth = 1) +
  geom_vline(xintercept = ep_boot_ci, linetype = "dashed", linewidth = 1) +
  labs(
    title = "Bootstrap Distribution of Target Expected Points",
    subtitle = "Cluster bootstrap by game_id",
    x = "Bootstrap expected-points estimate",
    y = "Count"
  ) +
  theme_minimal()

ep_boot_plot

# Task 6:
# - In comments, explain why a naive row-by-row observation bootstrap
#   is less appropriate for this dataset

# I used a cluster bootstrap by game_id for the expected-points model.
# This is better than a naive row-by-row bootstrap because plays within the
# same NFL game are not independent. They share game context, teams, score,
# weather, strategy, and other within-game factors.
#
# A row-by-row bootstrap would break this dependence structure by treating
# each play as independent. The cluster bootstrap keeps games together by
# resampling whole games with replacement.
#
# The fitted expected-points estimate is target_ep_hat.
# The bootstrap standard error is ep_boot_se.
# The 95% percentile interval is ep_boot_ci.


################################
### PART 2: NBA FREE THROWS ####
################################

nba_ft_raw <- read_delim(
  "/Users/mackenziebuckner/Desktop/lab-materials/2026/labs/data/09_nba-free-throws.csv",
  delim = ";",
  locale = locale(encoding = "Latin1"),
  show_col_types = FALSE
)


# Task 1:
# - Recreate the player-level free-throw dataset from Lab 8
# - Include:
#   * Player
#   * FT_total = approximate total free throws made across the season
#   * FTA_total = approximate total free throws attempted across the season
#   * FT_percent
# - Basketball Reference reports FT and FTA as per-game values in this table,
#   so convert them to approximate totals using G before treating them as counts
# - Filter to players with at least 25 approximate total free-throw attempts

traded_players <- nba_ft_raw %>%
  filter(Tm == "TOT") %>%
  pull(Player)

ft_data <- nba_ft_raw %>%
  filter(Tm == "TOT" | !(Player %in% traded_players)) %>%
  mutate(
    # The dataset gives FT and FTA per game, so approximate season totals:
    ft_att = round(G * FTA),
    ft_made = round(`FT%` * ft_att),
    ft_pct = ft_made / ft_att
  ) %>%
  filter(ft_att > 0)


# Task 2:
# - For each player, construct a 95% bootstrap confidence interval
#   for free-throw percentage
# - Overlay the bootstrap intervals on your Lab 8 player plot
# - Compare bootstrap, Wald, and Agresti-Coull intervals

# You may want helper functions for:
# - one bootstrap resample of a player's free throws
# - a percentile interval from bootstrap draws

z_975 <- qnorm(0.975)

ft_intervals <- ft_data %>%
  mutate(
    # Wald interval
    wald_se = sqrt(ft_pct * (1 - ft_pct) / ft_att),
    wald_lower = pmax(0, ft_pct - z_975 * wald_se),
    wald_upper = pmin(1, ft_pct + z_975 * wald_se),
    
    # Agresti-Coull interval
    ac_n = ft_att + z_975^2,
    ac_p = (ft_made + z_975^2 / 2) / ac_n,
    ac_se = sqrt(ac_p * (1 - ac_p) / ac_n),
    ac_lower = pmax(0, ac_p - z_975 * ac_se),
    ac_upper = pmin(1, ac_p + z_975 * ac_se)
  )


bootstrap_player_ft <- function(made, att, B = 1000) {
  
  # Recreate the player's free throws:
  # 1 = made free throw
  # 0 = missed free throw
  shots <- c(rep(1, made), rep(0, att - made))
  
  # Bootstrap by resampling that player's shots
  boot_pcts <- replicate(B, {
    mean(sample(shots, size = att, replace = TRUE))
  })
  
  # Percentile interval
  ci <- quantile(boot_pcts, probs = c(0.025, 0.975), names = FALSE)
  
  tibble(
    boot_lower = ci[1],
    boot_upper = ci[2],
    boot_se = sd(boot_pcts)
  )
}

B_player <- 1000

ft_boot <- map2_dfr(
  ft_intervals$ft_made,
  ft_intervals$ft_att,
  function(made, att) {
    bootstrap_player_ft(made, att, B = B_player)
  }
)

ft_intervals <- bind_cols(ft_intervals, ft_boot)



# Compare interval widths


interval_widths <- ft_intervals %>%
  summarise(
    avg_wald_width = mean(wald_upper - wald_lower),
    avg_agresti_coull_width = mean(ac_upper - ac_lower),
    avg_bootstrap_width = mean(boot_upper - boot_lower)
  )

print(interval_widths)



# Player plot with all three intervals


players_to_plot <- ft_intervals %>%
  slice_max(ft_att, n = 40) %>%
  arrange(ft_pct) %>%
  mutate(Player = factor(Player, levels = Player))

plot_data <- players_to_plot %>%
  select(
    Player,
    ft_pct,
    wald_lower,
    wald_upper,
    ac_lower,
    ac_upper,
    boot_lower,
    boot_upper
  ) %>%
  pivot_longer(
    cols = c(
      wald_lower,
      wald_upper,
      ac_lower,
      ac_upper,
      boot_lower,
      boot_upper
    ),
    names_to = c("method", ".value"),
    names_pattern = "(wald|ac|boot)_(lower|upper)"
  ) %>%
  mutate(
    method = recode(
      method,
      wald = "Wald",
      ac = "Agresti-Coull",
      boot = "Bootstrap"
    )
  )

ft_player_plot <- ggplot(
  plot_data,
  aes(x = Player, y = ft_pct, ymin = lower, ymax = upper, color = method)
) +
  geom_pointrange(
    position = position_dodge(width = 0.6)
  ) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "NBA Free-Throw Percentage Intervals",
    subtitle = "Wald vs Agresti-Coull vs Bootstrap intervals",
    x = "Player",
    y = "Estimated true free-throw percentage",
    color = "Interval method"
  ) +
  theme_minimal()

ft_player_plot



# comparison for player intervals

# The Wald interval is easy to compute, but it can be unreliable when a player
# has few attempts or when the observed percentage is close to 0 or 1.
#
# The Agresti-Coull interval is usually more stable because it adds pseudo-made
# and pseudo-missed free throws.
#
# The bootstrap interval uses the player's observed made/missed free throws to
# approximate the sampling distribution of their free-throw percentage.
#
# In general, the bootstrap and Agresti-Coull intervals are more sensible than
# the naive Wald interval, especially for players with fewer attempts or extreme
# shooting percentages.


# Task 3:
# - Revisit the Lab 8 simulation study
# - Add bootstrap confidence intervals to the same coverage framework
# - Plot bootstrap coverage probability against p
# - Compare to Wald and Agresti-Coull

p_grid <- seq(0.01, 0.99, length.out = 41)
sample_sizes <- c(10, 50, 100, 250, 500, 1000)

M <- 100
B_sim_boot <- 200


wald_ci <- function(x, n) {
  
  p_hat <- x / n
  se <- sqrt(p_hat * (1 - p_hat) / n)
  
  lower <- pmax(0, p_hat - z_975 * se)
  upper <- pmin(1, p_hat + z_975 * se)
  
  c(lower, upper)
}

agresti_coull_ci <- function(x, n) {
  
  n_tilde <- n + z_975^2
  p_tilde <- (x + z_975^2 / 2) / n_tilde
  se <- sqrt(p_tilde * (1 - p_tilde) / n_tilde)
  
  lower <- pmax(0, p_tilde - z_975 * se)
  upper <- pmin(1, p_tilde + z_975 * se)
  
  c(lower, upper)
}

bootstrap_ci <- function(shots, B = 200) {
  
  n <- length(shots)
  
  boot_means <- replicate(B, {
    mean(sample(shots, size = n, replace = TRUE))
  })
  
  quantile(boot_means, probs = c(0.025, 0.975), names = FALSE)
}



# Run simulation

coverage_rows <- list()
row_id <- 1

for (n_value in sample_sizes) {
  
  cat("Running simulation for n =", n_value, "\n")
  
  for (p_value in p_grid) {
    
    wald_covers <- logical(M)
    ac_covers <- logical(M)
    boot_covers <- logical(M)
    
    for (m in 1:M) {
      
      # Simulate n free throws with true make probability p_value
      shots <- rbinom(n = n_value, size = 1, prob = p_value)
      x <- sum(shots)
      
      # Compute the three intervals
      wald <- wald_ci(x, n_value)
      ac <- agresti_coull_ci(x, n_value)
      boot <- bootstrap_ci(shots, B = B_sim_boot)
      
      # Check whether each interval contains the true p
      wald_covers[m] <- wald[1] <= p_value & p_value <= wald[2]
      ac_covers[m] <- ac[1] <= p_value & p_value <= ac[2]
      boot_covers[m] <- boot[1] <= p_value & p_value <= boot[2]
    }
    
    coverage_rows[[row_id]] <- tibble(
      p = p_value,
      n = n_value,
      wald_coverage = mean(wald_covers),
      agresti_coull_coverage = mean(ac_covers),
      bootstrap_coverage = mean(boot_covers)
    )
    
    row_id <- row_id + 1
  }
}

coverage_summary <- bind_rows(coverage_rows)

print(coverage_summary)



# Plot coverage probability


coverage_plot_data <- coverage_summary %>%
  pivot_longer(
    cols = c(
      wald_coverage,
      agresti_coull_coverage,
      bootstrap_coverage
    ),
    names_to = "method",
    values_to = "coverage"
  ) %>%
  mutate(
    method = recode(
      method,
      wald_coverage = "Wald",
      agresti_coull_coverage = "Agresti-Coull",
      bootstrap_coverage = "Bootstrap"
    )
  )

coverage_plot <- ggplot(
  coverage_plot_data,
  aes(x = p, y = coverage, color = method)
) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0.95, linetype = "dashed") +
  facet_wrap(~ n) +
  labs(
    title = "Coverage Probability for 95% Confidence Intervals",
    subtitle = "Dashed line shows ideal 95% coverage",
    x = "True free-throw probability",
    y = "Coverage probability",
    color = "Interval method"
  ) +
  theme_minimal()

coverage_plot



# simulation comparison


# The Wald interval usually performs worst near p = 0 or p = 1,
# especially when sample size is small.
#
# The Agresti-Coull interval is more stable because it avoids being too
# confident when the observed sample has all makes or all misses.
#
# The bootstrap interval is more data-driven, but it can still struggle when
# n is small or p is close to 0 or 1. In those cases, the bootstrap samples
# are based only on the limited observed data, so the bootstrap distribution
# can be too narrow.
#
# Overall, the coverage plot should show that Agresti-Coull and bootstrap are
# generally more reliable than Wald. As n gets larger, all three methods should
# get closer to the target 95% coverage.


