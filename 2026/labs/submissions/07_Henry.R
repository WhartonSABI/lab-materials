#############
### SETUP ###
#############

# install.packages(c("tidyverse", "hoopR", "sensemakr"))
library(tidyverse)
library(hoopR)

# optional, for the sensitivity-analysis section at the end
# library(sensemakr)

set.seed(7)

# all plots are written here
plots_dir <- "plots"
if (!dir.exists(plots_dir)) dir.create(plots_dir)

# hoopR uses the ending year for NBA seasons:
# 2022 = 2021-2022, ..., 2026 = 2025-2026
seasons <- 2022:2026
swing_threshold <- 7
horizon_seconds <- 180

########################################
### HELPER FUNCTIONS FOR THE LAB DATA ###
########################################

timeout_now_lookup <- function(game_df, play_number, seconds_remaining, focal_team_id) {
  idx <- which(
    game_df$game_play_number > play_number &
      game_df$end_game_seconds_remaining == seconds_remaining
  )
  if (length(idx) == 0) return(FALSE)

  any(game_df$timeout_flag[idx] & game_df$team_id[idx] == focal_team_id, na.rm = TRUE)
}

future_margin_lookup <- function(game_df, play_number, target_seconds_remaining, focal_side) {
  idx <- which(
    game_df$game_play_number > play_number &
      game_df$end_game_seconds_remaining <= target_seconds_remaining
  )

  row <- if (length(idx) == 0) nrow(game_df) else idx[1]

  if (focal_side == "home") {
    game_df$home_score[row] - game_df$away_score[row]
  } else {
    game_df$away_score[row] - game_df$home_score[row]
  }
}

prior_margin_lookup <- function(game_df, play_number, target_seconds_remaining, focal_side) {
  idx <- which(
    game_df$game_play_number < play_number &
      game_df$end_game_seconds_remaining >= target_seconds_remaining
  )

  if (length(idx) == 0) {
    return(NA_real_)
  }

  row <- idx[length(idx)]

  if (focal_side == "home") {
    game_df$home_score[row] - game_df$away_score[row]
  } else {
    game_df$away_score[row] - game_df$home_score[row]
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

  # plain data.frames keyed by game id give the per-row lookups fast column access
  game_lookup <- lapply(split(pbp_clean, pbp_clean$game_id), as.data.frame)

  # prior margin (and hence the 180s swing) is needed for every scoring event, so
  # compute it first and immediately drop everything that cannot be an opportunity.
  candidates <- scoring_events %>%
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
      swing_last_180 = current_margin - prior_margin_180
    ) %>%
    filter(
      !is.na(swing_last_180),
      swing_last_180 <= -swing_threshold,
      end_game_seconds_remaining >= horizon_seconds
    )

  # the treatment, timeout-history, and outcome lookups only matter for survivors
  candidates %>%
    mutate(
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
      !timeout_in_prior_180
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

# 1. number of decision opportunities
cat("Number of decision opportunities:", nrow(analysis_data), "\n")

# 2. fraction where the coach called timeout immediately
cat("Timeout rate:", mean(analysis_data$timeout_now), "\n")

# 3. naive difference in mean outcomes between timeout and no-timeout moments
naive_estimate <- with(
  analysis_data,
  mean(margin_change_next_180[timeout_now == 1], na.rm = TRUE) -
    mean(margin_change_next_180[timeout_now == 0], na.rm = TRUE)
)

cat("Naive difference in means:", naive_estimate, "\n")

# useful descriptive plot
p_task1 <- ggplot(analysis_data, aes(x = swing_last_180, fill = factor(timeout_now))) +
  geom_bar(position = "fill") +
  scale_fill_discrete(name = "Timeout now") +
  labs(
    x = "Score-margin swing over the last 180 seconds",
    y = "Fraction of opportunities",
    title = "How often do coaches call timeout by recent score swing?"
  )
ggsave(file.path(plots_dir, "p1_task1_timeout_rate_by_swing.png"),
       p_task1, width = 8, height = 5, dpi = 120)

# 4. Unit / treatment / control / outcome / estimand.
#    Unit: a single decision opportunity -- the first opponent scoring play in a
#      timeout-free stretch that pushes the focal team's 180s margin swing to <= -7.
#    Treatment: the focal team's coach calls a timeout immediately, before the next
#      clock change (timeout_now == 1).
#    Control: no immediate timeout is called at that opportunity (timeout_now == 0).
#    Outcome: margin_change_next_180, the change in the focal team's score margin
#      over the next 180 seconds of game time.
#    Target estimand: the average treatment effect of calling an immediate timeout
#      on the next-180s margin change, among these 7-point-swing opportunities
#      (an ATE over the opportunity population; matching targets the ATT specifically).

# 5. Pre-treatment covariates to adjust for (all fixed at time zero, before the
#    timeout decision), and a bad control:
#    - swing_last_180     (how steep the recent run against the focal team was)
#    - current_margin     (the score margin the focal team currently faces)
#    - period_number      (game phase; late-game urgency differs)
#    - end_game_seconds_remaining (time left -- shapes both timeout use and scoring)
#    - home_focal         (home/away status of the team at risk)
#    - pregame_spread_focal + spread_missing (team-strength proxy)
#    Bad control: margin_change_next_180 itself, or anything measured AFTER the
#    decision (e.g. whether the focal team scored on the next possession). These are
#    post-treatment / outcome variables; conditioning on them blocks part of the
#    causal effect and induces collider bias.

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

# 1. overlap plot: estimated propensity-score distributions by treatment
p_task2 <- ggplot(analysis_data, aes(x = ps_hat, fill = factor(timeout_now))) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  scale_fill_discrete(name = "Timeout now") +
  labs(
    x = "Estimated propensity score",
    y = "Count",
    title = "Propensity-score overlap"
  )
ggsave(file.path(plots_dir, "p2_task2_ps_overlap.png"),
       p_task2, width = 8, height = 5, dpi = 120)

# 2. Overlap assessment (see printed range + plot).
#    Overlap is strong. Because timeouts are relatively rare (~13% of
#    opportunities), every estimated propensity score is low: treated moments fall
#    in roughly [0.04, 0.26] and controls in [0.01, 0.27], so the control
#    distribution fully covers the region where treated units sit. There is no
#    treated mass without comparable controls, so common support holds well and
#    no units need to be trimmed for the adjustment methods.
cat("PS range treated:",
    range(analysis_data$ps_hat[analysis_data$timeout_now == 1]), "\n")
cat("PS range control:",
    range(analysis_data$ps_hat[analysis_data$timeout_now == 0]), "\n")

# 3. pre-adjustment balance
covariates <- c(
  "swing_last_180",
  "current_margin",
  "period_number",
  "end_game_seconds_remaining",
  "home_focal",
  "pregame_spread_focal"
)

cat("\nPre-adjustment balance (standardized mean differences):\n")
print(balance_table(
  data = analysis_data,
  covariates = covariates,
  treatment = "timeout_now"
))

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

print(summary(reg_model))

# Note: spread_missing is dropped ("1 not defined because of singularities") -- in
# these five seasons every opportunity has a pregame spread, so spread_missing is
# constant 0 and collinear with the intercept. This is expected, not an error.

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
cat("Matching ATT:", matched_att, "\n")

# 2. post-adjustment balance for the matched sample
cat("\nPost-matching balance (standardized mean differences):\n")
print(balance_table(
  data = matched_data,
  covariates = covariates,
  treatment = "timeout_now"
))

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

print(summary(ipw_model))

# 2. post-adjustment balance for the IPW (weighted) analysis
cat("\nPost-weighting balance (standardized mean differences):\n")
print(balance_table(
  data = analysis_data,
  covariates = covariates,
  treatment = "timeout_now",
  weights = analysis_data$ipw_ate
))

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

cat("\nComparison of causal estimates:\n")
print(estimate_table)

# 3. Do the methods agree? (see estimate_table)
#    Yes -- they agree almost completely, and on essentially NO effect. All four
#    estimates are small and POSITIVE: naive +0.083, regression +0.082, matching
#    ATT +0.050, IPW +0.058 points of next-180s margin. None is statistically
#    significant (regression p = 0.41, IPW p = 0.38). The point estimates sit well
#    inside noise, so the takeaway is that an immediate timeout shows no detectable
#    effect on the next three minutes of score margin in this operationalization.
#
# 4. Why might the estimates differ (slightly)?
#    They weight the data differently and lean on different assumptions. The naive
#    estimate ignores confounding entirely; regression adjustment assumes a correct
#    linear outcome model; matching targets the ATT and discards unmatched controls;
#    IPW reweights by the propensity model. Here the pre-treatment imbalance was
#    already mild (largest SMDs ~0.13-0.17 for period and time remaining) and
#    overlap is good, so adjustment barely moves the estimate -- the small spread
#    is mostly the ATT (matching) vs ATE (others) target and finite-sample noise.

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

  png(file.path(plots_dir, "p3_task4_sensitivity_contour.png"),
      width = 8, height = 6, units = "in", res = 120)
  plot(sensitivity_fit)
  dev.off()
} else {
  message("Install the sensemakr package if you want to run the sensitivity-analysis section.")
}

# 1. Plausible unmeasured confounding.
#    Coaches act on information absent from public play-by-play: visible player
#    fatigue, foul trouble, momentum/body language, injuries, matchup problems,
#    shot quality, and bench readiness. A coach is more likely to call timeout
#    exactly when things look worst on dimensions we cannot measure, so the
#    no-timeout comparison moments are systematically "less dire."
#
# 2. Sensitivity to hidden bias.
#    The estimate is tiny to begin with (partial R2 of treatment with outcome ~0,
#    robustness value q=1 ~0.0063). The RV literally says a confounder explaining
#    >0.63% of the residual variance of both treatment and outcome would push the
#    point estimate to zero. But this "fragility" is not interesting here: the
#    effect is already indistinguishable from zero and not significant, so there is
#    essentially no positive finding for hidden bias to explain away. The honest
#    reading is a precise null rather than a robust effect.
#
# 3. Strongest defensible causal claim.
#    Only a weak, conditional one: after adjusting for the observed pre-treatment
#    covariates, we find NO evidence that calling an immediate timeout after a
#    7-point swing changes the next-180s score margin (estimates ~+0.05 to +0.08
#    points, not significant). We cannot claim timeouts help or hurt; at most we can
#    say any short-run margin effect, if it exists, is small in this data.
#
# 4. What would make the design more convincing.
#    Richer pre-treatment controls (lineup quality, rest, fouls, shot quality,
#    win probability), a sharper comparison such as regression discontinuity around
#    the -7 threshold or matching within the same game/segment, an instrument for
#    timeout timing, and pre-registration of the estimand to limit researcher
#    degrees of freedom.
