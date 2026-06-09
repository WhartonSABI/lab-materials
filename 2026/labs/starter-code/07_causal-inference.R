#############
### SETUP ###
#############

# install.packages(c("tidyverse", "hoopR", "sensemakr"))
library(tidyverse)
library(hoopR)

# optional, for the sensitivity-analysis section at the end
# library(sensemakr)

set.seed(7)

# hoopR uses the ending year for NBA seasons:
# 2022 = 2021-2022, ..., 2026 = 2025-2026
seasons <- 2022:2026
swing_threshold <- 7
horizon_seconds <- 180

########################################
### HELPER FUNCTIONS FOR THE LAB DATA ###
########################################

timeout_now_lookup <- function(game_df, play_number, seconds_remaining, focal_team_id) {
  window <- game_df %>%
    filter(
      game_play_number > play_number,
      end_game_seconds_remaining == seconds_remaining
    ) %>%
    mutate(
      timeout_flag = str_detect(str_to_lower(coalesce(type_text, "")), "timeout") |
        str_detect(str_to_lower(coalesce(text, "")), "timeout")
    )

  any(window$timeout_flag & window$team_id == focal_team_id, na.rm = TRUE)
}

future_margin_lookup <- function(game_df, play_number, target_seconds_remaining, focal_side) {
  future_rows <- game_df %>%
    filter(
      game_play_number > play_number,
      end_game_seconds_remaining <= target_seconds_remaining
    )

  if (nrow(future_rows) == 0) {
    future_row <- game_df %>% slice_tail(n = 1)
  } else {
    future_row <- future_rows %>% slice(1)
  }

  if (focal_side == "home") {
    future_row$home_score - future_row$away_score
  } else {
    future_row$away_score - future_row$home_score
  }
}

prior_margin_lookup <- function(game_df, play_number, target_seconds_remaining, focal_side) {
  prior_rows <- game_df %>%
    filter(
      game_play_number < play_number,
      end_game_seconds_remaining >= target_seconds_remaining
    )

  if (nrow(prior_rows) == 0) {
    return(NA_real_)
  }

  prior_row <- prior_rows %>% slice_tail(n = 1)

  if (focal_side == "home") {
    prior_row$home_score - prior_row$away_score
  } else {
    prior_row$away_score - prior_row$home_score
  }
}

timeout_in_window_lookup <- function(game_df, play_number, current_seconds_remaining, target_seconds_remaining) {
  any(
    game_df$timeout_flag &
      game_df$game_play_number < play_number &
      game_df$end_game_seconds_remaining >= current_seconds_remaining &
      game_df$end_game_seconds_remaining <= target_seconds_remaining,
    na.rm = TRUE
  )
}

build_timeout_opportunities <- function(pbp,
                                        swing_threshold = 7,
                                        horizon_seconds = 180) {
  pbp_clean <- pbp %>%
    filter(season_type == 2) %>%
    arrange(game_id, game_play_number) %>%
    mutate(
      timeout_flag = str_detect(str_to_lower(coalesce(type_text, "")), "timeout") |
        str_detect(str_to_lower(coalesce(text, "")), "timeout")
    ) %>%
    group_by(game_id) %>%
    mutate(timeout_segment = cumsum(timeout_flag)) %>%
    ungroup()

  scoring_events <- pbp_clean %>%
    filter(scoring_play, score_value > 0, !is.na(team_id)) %>%
    mutate(
      scoring_side = case_when(
        team_id == home_team_id ~ "home",
        team_id == away_team_id ~ "away",
        TRUE ~ NA_character_
      ),
      focal_side = if_else(scoring_side == "home", "away", "home"),
      focal_team_id = if_else(focal_side == "home", home_team_id, away_team_id),
      home_focal = focal_side == "home",
      current_margin = if_else(
        focal_side == "home",
        home_score - away_score,
        away_score - home_score
      ),
      raw_spread_focal = if_else(home_focal, home_team_spread, -home_team_spread),
      spread_missing = as.integer(is.na(raw_spread_focal)),
      pregame_spread_focal = replace_na(raw_spread_focal, 0),
      start_window_seconds_remaining = end_game_seconds_remaining + horizon_seconds,
      target_end_game_seconds_remaining = pmax(0, end_game_seconds_remaining - horizon_seconds)
    )

  game_lookup <- split(pbp_clean, pbp_clean$game_id)

  scoring_events %>%
    mutate(
      prior_margin_180 = pmap_dbl(
        list(game_id, game_play_number, start_window_seconds_remaining, focal_side),
        function(game_id, play_number, start_window_seconds_remaining, focal_side) {
          prior_margin_lookup(
            game_df = game_lookup[[as.character(game_id)]],
            play_number = play_number,
            target_seconds_remaining = start_window_seconds_remaining,
            focal_side = focal_side
          )
        }
      ),
      swing_last_180 = current_margin - prior_margin_180,
      timeout_in_prior_180 = pmap_lgl(
        list(game_id, game_play_number, end_game_seconds_remaining, start_window_seconds_remaining),
        function(game_id, play_number, current_seconds_remaining, start_window_seconds_remaining) {
          timeout_in_window_lookup(
            game_df = game_lookup[[as.character(game_id)]],
            play_number = play_number,
            current_seconds_remaining = current_seconds_remaining,
            target_seconds_remaining = start_window_seconds_remaining
          )
        }
      ),
      timeout_now = pmap_lgl(
        list(game_id, game_play_number, end_game_seconds_remaining, focal_team_id),
        function(game_id, play_number, seconds_remaining, focal_team_id) {
          timeout_now_lookup(
            game_df = game_lookup[[as.character(game_id)]],
            play_number = play_number,
            seconds_remaining = seconds_remaining,
            focal_team_id = focal_team_id
          )
        }
      ),
      future_margin = pmap_dbl(
        list(game_id, game_play_number, target_end_game_seconds_remaining, focal_side),
        function(game_id, play_number, target_seconds_remaining, focal_side) {
          future_margin_lookup(
            game_df = game_lookup[[as.character(game_id)]],
            play_number = play_number,
            target_seconds_remaining = target_seconds_remaining,
            focal_side = focal_side
          )
        }
      ),
      margin_change_next_180 = future_margin - current_margin,
      timeout_now = as.integer(timeout_now)
    ) %>%
    filter(
      !is.na(swing_last_180),
      !timeout_in_prior_180,
      swing_last_180 <= -swing_threshold,
      end_game_seconds_remaining >= horizon_seconds
    ) %>%
    group_by(game_id, focal_side, timeout_segment) %>%
    arrange(game_play_number, .by_group = TRUE) %>%
    slice(1) %>%
    ungroup() %>%
    select(
      season,
      game_id,
      game_play_number,
      timeout_segment,
      swing_last_180,
      prior_margin_180,
      current_margin,
      period_number,
      end_game_seconds_remaining,
      target_end_game_seconds_remaining,
      home_focal,
      focal_side,
      focal_team_id,
      pregame_spread_focal,
      spread_missing,
      timeout_now,
      margin_change_next_180
    )
}

smd_one <- function(x, z, w = NULL) {
  if (is.null(w)) {
    mu1 <- mean(x[z == 1], na.rm = TRUE)
    mu0 <- mean(x[z == 0], na.rm = TRUE)
    v1 <- var(x[z == 1], na.rm = TRUE)
    v0 <- var(x[z == 0], na.rm = TRUE)
  } else {
    mu1 <- weighted.mean(x[z == 1], w[z == 1], na.rm = TRUE)
    mu0 <- weighted.mean(x[z == 0], w[z == 0], na.rm = TRUE)
    v1 <- weighted.mean((x[z == 1] - mu1)^2, w[z == 1], na.rm = TRUE)
    v0 <- weighted.mean((x[z == 0] - mu0)^2, w[z == 0], na.rm = TRUE)
  }

  (mu1 - mu0) / sqrt((v1 + v0) / 2)
}

balance_table <- function(data, covariates, treatment, weights = NULL) {
  z <- data[[treatment]]

  tibble(
    covariate = covariates,
    smd = map_dbl(covariates, function(v) smd_one(data[[v]], z, w = weights))
  )
}

nearest_ps_match <- function(data, treat = "timeout_now", score = "ps_hat") {
  treated <- data %>%
    filter(.data[[treat]] == 1) %>%
    arrange(.data[[score]])

  controls <- data %>%
    filter(.data[[treat]] == 0) %>%
    arrange(.data[[score]])

  used <- rep(FALSE, nrow(controls))
  matches <- vector("list", nrow(treated))
  next_id <- 1L

  for (i in seq_len(nrow(treated))) {
    distances <- abs(controls[[score]] - treated[[score]][i])
    distances[used] <- Inf
    j <- which.min(distances)

    if (length(j) == 0 || is.infinite(distances[j])) {
      next
    }

    used[j] <- TRUE

    matches[[next_id]] <- bind_rows(
      treated[i, ] %>% mutate(match_role = "treated", match_id = next_id),
      controls[j, ] %>% mutate(match_role = "control", match_id = next_id)
    )

    next_id <- next_id + 1L
  }

  bind_rows(matches)
}

############################
### DOWNLOAD / CONSTRUCT ###
############################

# load regular-season play-by-play
pbp <- hoopR::load_nba_pbp(seasons = seasons)

# build one row per threshold-crossing timeout decision opportunity
analysis_data <- build_timeout_opportunities(
  pbp = pbp,
  swing_threshold = swing_threshold,
  horizon_seconds = horizon_seconds
)

# inspect the analysis dataset
glimpse(analysis_data)

###########################
### TASK 1: DESCRIPTIVES ###
###########################

# dataset size
nrow(analysis_data)

# timeout rate
mean(analysis_data$timeout_now)

# naive difference in means
naive_estimate <- with(
  analysis_data,
  mean(margin_change_next_180[timeout_now == 1], na.rm = TRUE) -
    mean(margin_change_next_180[timeout_now == 0], na.rm = TRUE)
)

naive_estimate

# useful descriptive plot
ggplot(analysis_data, aes(x = swing_last_180, fill = factor(timeout_now))) +
  geom_bar(position = "fill") +
  scale_fill_discrete(name = "Timeout now") +
  labs(
    x = "Score-margin swing over the last 180 seconds",
    y = "Fraction of opportunities",
    title = "How often do coaches call timeout by recent score swing?"
  )

##################################
### TASK 2: PROPENSITY MODELING ###
##################################

# Starter formula for the propensity-score model.
# This is the part you should inspect and modify carefully.
# At minimum, keep this as a function of pre-treatment covariates only.
ps_formula <- timeout_now ~
  swing_last_180 +
  current_margin +
  factor(period_number) +
  end_game_seconds_remaining +
  home_focal +
  pregame_spread_focal +
  spread_missing

ps_model <- glm(
  formula = ps_formula,
  data = analysis_data,
  family = binomial()
)

analysis_data <- analysis_data %>%
  mutate(
    ps_hat = predict(ps_model, type = "response"),
    ps_hat = pmin(pmax(ps_hat, 0.01), 0.99)
  )

# overlap plot
ggplot(analysis_data, aes(x = ps_hat, fill = factor(timeout_now))) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  scale_fill_discrete(name = "Timeout now") +
  labs(
    x = "Estimated propensity score",
    y = "Count",
    title = "Propensity-score overlap"
  )

# pre-adjustment balance
covariates <- c(
  "swing_last_180",
  "current_margin",
  "period_number",
  "end_game_seconds_remaining",
  "home_focal",
  "pregame_spread_focal"
)

balance_table(
  data = analysis_data,
  covariates = covariates,
  treatment = "timeout_now"
)

#####################################
### TASK 3: CAUSAL ESTIMATORS ###
#####################################

# regression adjustment
outcome_formula <- margin_change_next_180 ~
  timeout_now +
  swing_last_180 +
  current_margin +
  factor(period_number) +
  end_game_seconds_remaining +
  home_focal +
  pregame_spread_focal +
  spread_missing

reg_model <- lm(
  formula = outcome_formula,
  data = analysis_data
)

summary(reg_model)

# simple nearest-neighbor matching on the estimated propensity score
matched_data <- nearest_ps_match(
  data = analysis_data,
  treat = "timeout_now",
  score = "ps_hat"
)

matched_effects <- matched_data %>%
  group_by(match_id) %>%
  summarize(
    pair_effect = margin_change_next_180[timeout_now == 1] -
      margin_change_next_180[timeout_now == 0],
    .groups = "drop"
  )

matched_att <- mean(matched_effects$pair_effect, na.rm = TRUE)
matched_att

balance_table(
  data = matched_data,
  covariates = covariates,
  treatment = "timeout_now"
)

# inverse-probability weighting
analysis_data <- analysis_data %>%
  mutate(
    ipw_ate = if_else(timeout_now == 1, 1 / ps_hat, 1 / (1 - ps_hat))
  )

ipw_model <- lm(
  margin_change_next_180 ~ timeout_now,
  data = analysis_data,
  weights = ipw_ate
)

summary(ipw_model)

balance_table(
  data = analysis_data,
  covariates = covariates,
  treatment = "timeout_now",
  weights = analysis_data$ipw_ate
)

# comparison table
estimate_table <- tibble(
  method = c(
    "Naive difference",
    "Regression adjustment",
    "Matching ATT",
    "IPW"
  ),
  estimate = c(
    naive_estimate,
    coef(reg_model)["timeout_now"],
    matched_att,
    coef(ipw_model)["timeout_now"]
  )
)

estimate_table

##########################################
### TASK 4: SENSITIVITY / INTERPRETATION ###
##########################################

# The regression-adjusted model is the simplest place to run a sensitivity analysis.
# If you want to use this section, install and load sensemakr.

if (requireNamespace("sensemakr", quietly = TRUE)) {
  sensitivity_fit <- sensemakr::sensemakr(
    model = reg_model,
    treatment = "timeout_now",
    benchmark_covariates = c("swing_last_180", "current_margin"),
    kd = 1
  )

  print(summary(sensitivity_fit))
  plot(sensitivity_fit)
} else {
  message("Install the sensemakr package if you want to run the sensitivity-analysis section.")
}
