#############
### SETUP ###
#############

install.packages(c("tidyverse", "hoopR", "sensemakr"))

library(tidyverse)
library(hoopR)

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
      timeout_flag = str_detect(str_to_lower(coalesce(as.character(type_text), "")), "timeout") |
        str_detect(str_to_lower(coalesce(as.character(text), "")), "timeout")
    )
  
  any(
    window$timeout_flag &
      as.character(window$team_id) == as.character(focal_team_id),
    na.rm = TRUE
  )
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
  
  # Add optional columns safely if hoopR does not include them.
  if (!"type_text" %in% names(pbp)) {
    pbp$type_text <- NA_character_
  }
  
  if (!"text" %in% names(pbp)) {
    pbp$text <- NA_character_
  }
  
  if (!"home_team_spread" %in% names(pbp)) {
    pbp$home_team_spread <- NA_real_
  }
  
  required_vars <- c(
    "game_id",
    "season",
    "season_type",
    "game_play_number",
    "team_id",
    "scoring_play",
    "score_value",
    "home_score",
    "away_score",
    "home_team_id",
    "away_team_id",
    "period_number",
    "end_game_seconds_remaining"
  )
  
  missing_vars <- setdiff(required_vars, names(pbp))
  
  if (length(missing_vars) > 0) {
    stop(
      "The play-by-play data is missing these required variables: ",
      paste(missing_vars, collapse = ", ")
    )
  }
  
  pbp_clean <- pbp %>%
    mutate(
      season_type_chr = str_to_lower(as.character(season_type)),
      game_play_number = as.numeric(game_play_number),
      end_game_seconds_remaining = as.numeric(end_game_seconds_remaining),
      home_score = as.numeric(home_score),
      away_score = as.numeric(away_score),
      score_value = as.numeric(score_value),
      period_number = as.numeric(period_number),
      home_team_spread = suppressWarnings(as.numeric(home_team_spread))
    ) %>%
    filter(
      as.character(season_type) == "2" |
        str_detect(season_type_chr, "regular")
    ) %>%
    arrange(game_id, game_play_number) %>%
    mutate(
      timeout_flag = str_detect(str_to_lower(coalesce(as.character(type_text), "")), "timeout") |
        str_detect(str_to_lower(coalesce(as.character(text), "")), "timeout"),
      scoring_play_flag = scoring_play %in% c(TRUE, 1, "1", "TRUE", "true", "T", "t")
    ) %>%
    group_by(game_id) %>%
    mutate(timeout_segment = cumsum(timeout_flag)) %>%
    ungroup()
  
  scoring_events <- pbp_clean %>%
    filter(scoring_play_flag, score_value > 0, !is.na(team_id)) %>%
    mutate(
      scoring_side = case_when(
        as.character(team_id) == as.character(home_team_id) ~ "home",
        as.character(team_id) == as.character(away_team_id) ~ "away",
        TRUE ~ NA_character_
      ),
      focal_side = case_when(
        scoring_side == "home" ~ "away",
        scoring_side == "away" ~ "home",
        TRUE ~ NA_character_
      ),
      focal_team_id = if_else(
        focal_side == "home",
        as.character(home_team_id),
        as.character(away_team_id)
      ),
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
    ) %>%
    filter(!is.na(focal_side))
  
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

#################################
### HELPER FUNCTIONS: BALANCE ###
#################################

smd_one <- function(x, z, w = NULL) {
  if (is.logical(x)) {
    x <- as.numeric(x)
  }
  
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
  
  denom <- sqrt((v1 + v0) / 2)
  
  if (is.na(denom) || denom == 0) {
    return(NA_real_)
  }
  
  (mu1 - mu0) / denom
}

balance_table <- function(data, covariates, treatment, weights = NULL) {
  z <- data[[treatment]]
  
  tibble(
    covariate = covariates,
    smd = map_dbl(covariates, function(v) smd_one(data[[v]], z, w = weights))
  ) %>%
    mutate(
      abs_smd = abs(smd),
      balance_flag = case_when(
        is.na(abs_smd) ~ "Check manually",
        abs_smd < 0.10 ~ "Good balance",
        abs_smd < 0.20 ~ "Some imbalance",
        TRUE ~ "Large imbalance"
      )
    )
}

nearest_ps_match <- function(data, treat = "timeout_now", score = "ps_hat") {
  treated <- data %>%
    filter(.data[[treat]] == 1) %>%
    arrange(.data[[score]])
  
  controls <- data %>%
    filter(.data[[treat]] == 0) %>%
    arrange(.data[[score]])
  
  if (nrow(treated) == 0 || nrow(controls) == 0) {
    stop("Matching failed because one treatment group has zero observations.")
  }
  
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

pbp <- hoopR::load_nba_pbp(seasons = seasons)

analysis_data <- build_timeout_opportunities(
  pbp = pbp,
  swing_threshold = swing_threshold,
  horizon_seconds = horizon_seconds
)

glimpse(analysis_data)

###########################
### TASK 1: DESCRIPTIVES ###
###########################

n_opportunities <- nrow(analysis_data)
timeout_rate <- mean(analysis_data$timeout_now, na.rm = TRUE)

treated_mean <- mean(
  analysis_data$margin_change_next_180[analysis_data$timeout_now == 1],
  na.rm = TRUE
)

control_mean <- mean(
  analysis_data$margin_change_next_180[analysis_data$timeout_now == 0],
  na.rm = TRUE
)

naive_estimate <- treated_mean - control_mean

task1_summary <- tibble(
  item = c(
    "Decision opportunities",
    "Timeout rate",
    "Mean outcome after timeout",
    "Mean outcome after no timeout",
    "Naive difference: timeout minus no timeout"
  ),
  value = c(
    n_opportunities,
    timeout_rate,
    treated_mean,
    control_mean,
    naive_estimate
  )
)

print(task1_summary)

timeout_by_swing_plot <- ggplot(
  analysis_data,
  aes(x = swing_last_180, fill = factor(timeout_now, labels = c("No timeout", "Timeout")))
) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Score-margin swing over the last 180 seconds",
    y = "Fraction of opportunities",
    fill = "Decision",
    title = "Timeout decisions by recent score swing"
  ) +
  theme_minimal()

print(timeout_by_swing_plot)

##################################
### TASK 2: PROPENSITY MODELING ###
##################################

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
  family = binomial(link = "logit")
)

summary(ps_model)

analysis_data <- analysis_data %>%
  mutate(
    ps_hat_raw = predict(ps_model, type = "response"),
    ps_hat = pmin(pmax(ps_hat_raw, 0.01), 0.99)
  )

propensity_overlap_plot <- ggplot(
  analysis_data,
  aes(x = ps_hat, fill = factor(timeout_now, labels = c("No timeout", "Timeout")))
) +
  geom_density(alpha = 0.45) +
  labs(
    x = "Estimated propensity score",
    y = "Density",
    fill = "Decision",
    title = "Propensity-score overlap"
  ) +
  theme_minimal()

print(propensity_overlap_plot)

ps_overlap_summary <- analysis_data %>%
  mutate(timeout_label = if_else(timeout_now == 1, "Timeout", "No timeout")) %>%
  group_by(timeout_label) %>%
  summarize(
    n = n(),
    min_ps = min(ps_hat, na.rm = TRUE),
    q25_ps = quantile(ps_hat, 0.25, na.rm = TRUE),
    median_ps = median(ps_hat, na.rm = TRUE),
    q75_ps = quantile(ps_hat, 0.75, na.rm = TRUE),
    max_ps = max(ps_hat, na.rm = TRUE),
    .groups = "drop"
  )

print(ps_overlap_summary)

treated_ps_range <- range(analysis_data$ps_hat[analysis_data$timeout_now == 1], na.rm = TRUE)
control_ps_range <- range(analysis_data$ps_hat[analysis_data$timeout_now == 0], na.rm = TRUE)

common_lower <- max(treated_ps_range[1], control_ps_range[1])
common_upper <- min(treated_ps_range[2], control_ps_range[2])

prop_in_common_support <- mean(
  analysis_data$ps_hat >= common_lower &
    analysis_data$ps_hat <= common_upper,
  na.rm = TRUE
)

overlap_strength <- case_when(
  prop_in_common_support >= 0.90 ~ "strong",
  prop_in_common_support >= 0.75 ~ "moderate",
  TRUE ~ "weak"
)

overlap_interpretation <- tibble(
  item = "Overlap interpretation",
  answer = paste0(
    "The common support interval is approximately [",
    round(common_lower, 3), ", ",
    round(common_upper, 3), "]. About ",
    round(100 * prop_in_common_support, 1),
    "% of observations fall inside this shared range, so overlap looks ",
    overlap_strength,
    ". Use the plot to confirm whether the two distributions visually overlap."
  )
)

print(overlap_interpretation)

covariates <- c(
  "swing_last_180",
  "current_margin",
  "period_number",
  "end_game_seconds_remaining",
  "home_focal",
  "pregame_spread_focal"
)

balance_before <- balance_table(
  data = analysis_data,
  covariates = covariates,
  treatment = "timeout_now"
) %>%
  mutate(adjustment = "Before adjustment")

print(balance_before)

#################################
### TASK 3: CAUSAL ESTIMATORS ###
#################################

### 1. Naive difference in means

n_treated <- sum(analysis_data$timeout_now == 1, na.rm = TRUE)
n_control <- sum(analysis_data$timeout_now == 0, na.rm = TRUE)

naive_se <- sqrt(
  var(analysis_data$margin_change_next_180[analysis_data$timeout_now == 1], na.rm = TRUE) / n_treated +
    var(analysis_data$margin_change_next_180[analysis_data$timeout_now == 0], na.rm = TRUE) / n_control
)

### 2. Regression adjustment

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

reg_estimate <- unname(coef(reg_model)["timeout_now"])
reg_se <- unname(coef(summary(reg_model))["timeout_now", "Std. Error"])

### 3. Nearest-neighbor matching on propensity score

matched_data <- nearest_ps_match(
  data = analysis_data,
  treat = "timeout_now",
  score = "ps_hat"
)

matched_effects <- matched_data %>%
  group_by(match_id) %>%
  summarize(
    treated_outcome = margin_change_next_180[timeout_now == 1][1],
    control_outcome = margin_change_next_180[timeout_now == 0][1],
    pair_effect = treated_outcome - control_outcome,
    .groups = "drop"
  )

matched_att <- mean(matched_effects$pair_effect, na.rm = TRUE)
matched_se <- sd(matched_effects$pair_effect, na.rm = TRUE) / sqrt(nrow(matched_effects))

balance_after_matching <- balance_table(
  data = matched_data,
  covariates = covariates,
  treatment = "timeout_now"
) %>%
  mutate(adjustment = "After matching")

print(balance_after_matching)

### 4. Inverse-probability weighting

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

ipw_estimate <- unname(coef(ipw_model)["timeout_now"])
ipw_se <- unname(coef(summary(ipw_model))["timeout_now", "Std. Error"])

balance_after_ipw <- balance_table(
  data = analysis_data,
  covariates = covariates,
  treatment = "timeout_now",
  weights = analysis_data$ipw_ate
) %>%
  mutate(adjustment = "After IPW")

print(balance_after_ipw)

### Balance summary before and after adjustment

balance_summary <- bind_rows(
  balance_before,
  balance_after_matching,
  balance_after_ipw
) %>%
  select(adjustment, covariate, smd, abs_smd, balance_flag) %>%
  arrange(covariate, adjustment)

print(balance_summary, n = Inf)

### Estimate comparison table

estimate_table <- tibble(
  method = c(
    "Naive difference in means",
    "Regression adjustment",
    "Nearest-neighbor matching on propensity score",
    "Inverse-probability weighting"
  ),
  estimand = c(
    "Difference in observed means",
    "Adjusted ATE-style coefficient",
    "ATT among matched timeout opportunities",
    "IPW ATE-style estimate"
  ),
  estimate = c(
    naive_estimate,
    reg_estimate,
    matched_att,
    ipw_estimate
  ),
  std_error = c(
    naive_se,
    reg_se,
    matched_se,
    ipw_se
  )
) %>%
  mutate(
    lower_95 = estimate - 1.96 * std_error,
    upper_95 = estimate + 1.96 * std_error
  )

print(estimate_table)

methods_same_sign <- all(estimate_table$estimate >= 0) ||
  all(estimate_table$estimate <= 0)

estimate_range <- max(estimate_table$estimate, na.rm = TRUE) -
  min(estimate_table$estimate, na.rm = TRUE)

method_agreement_sentence <- if (methods_same_sign) {
  paste0(
    "The methods broadly agree in sign, although they differ in size. ",
    "The range across estimates is about ",
    round(estimate_range, 3),
    " margin points."
  )
} else {
  paste0(
    "The methods do not fully agree in sign, which suggests the estimate is sensitive ",
    "to modeling and adjustment choices. The range across estimates is about ",
    round(estimate_range, 3),
    " margin points."
  )
}

print(method_agreement_sentence)

##########################################
### TASK 4: SENSITIVITY / INTERPRETATION ###
##########################################

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

########################################
### WRITTEN ANSWERS / REPORT TEXT ###
########################################

task1_writeup <- tibble(
  section = "Task 1",
  question = c(
    "Unit",
    "Treatment",
    "Control",
    "Outcome",
    "Target estimand",
    "Pre-treatment covariates",
    "Bad control example"
  ),
  answer = c(
    "The unit is a team-level timeout decision opportunity: the first opponent scoring play in a timeout-free stretch that pushes the team's 180-second score-margin swing to -7 or worse.",
    "The treatment is the coach calling a timeout immediately, before the next clock change.",
    "The control condition is not calling a timeout immediately at that decision opportunity.",
    "The outcome is the team's change in score margin over the next 180 seconds of game time.",
    "The target estimand is the average causal effect, for these decision opportunities, of calling an immediate timeout versus not calling one on the next-180-second score-margin change.",
    "I adjust for swing_last_180, current_margin, period_number, end_game_seconds_remaining, home_focal, and pregame_spread_focal. These are all measured before the timeout decision.",
    "A bad control would be margin_change_next_180, future_margin, points scored after the timeout decision, or any variable affected by whether the timeout was called."
  )
)

task2_writeup <- tibble(
  section = "Task 2",
  question = c(
    "Propensity model",
    "Overlap"
  ),
  answer = c(
    "I estimate the probability of an immediate timeout using logistic regression with only pre-treatment covariates: recent score swing, current margin, period, game time remaining, home/away status, and pregame spread proxy.",
    overlap_interpretation$answer
  )
)

task3_writeup <- tibble(
  section = "Task 3",
  question = c(
    "Do the methods agree?",
    "Why might estimates differ?"
  ),
  answer = c(
    method_agreement_sentence,
    "The estimates might differ because each method uses a different adjustment strategy. Matching estimates an ATT for matched timeout opportunities, while regression and IPW are closer to ATE-style estimates. Differences can also come from weak overlap, model misspecification, or remaining covariate imbalance."
  )
)

task4_writeup <- tibble(
  section = "Task 4",
  question = c(
    "Plausible unmeasured confounding",
    "Sensitivity to hidden bias",
    "Strongest causal claim",
    "What would make the design stronger?"
  ),
  answer = c(
    "Plausible unmeasured confounders include player fatigue, lineup quality on the court, injuries, coach strategy, defensive matchups, game importance, crowd momentum, and whether the opponent's run looked fluky or sustainable.",
    "Use the sensemakr output and plot to describe sensitivity. If a confounder as strong as swing_last_180 or current_margin could explain away the timeout coefficient, the conclusion is sensitive. If it would require a much stronger confounder, the conclusion is more robust.",
    "The strongest cautious claim is that, conditional on the measured pre-treatment covariates and assuming no important unmeasured confounding, immediate timeouts are associated with the estimated change in score margin over the next 180 seconds. Because this is observational, I would avoid claiming definitive proof that timeouts cause the change.",
    "The design would be stronger with richer tracking data, lineup/player fatigue information, coach-specific tendencies, better pregame team strength measures, and ideally a natural experiment or randomized timeout rule that creates more as-if-random variation."
  )
)

all_writeup_answers <- bind_rows(
  task1_writeup,
  task2_writeup,
  task3_writeup,
  task4_writeup
)

print(all_writeup_answers, n = Inf)

##########################################
### OPTIONAL: SAVE TABLES / FIGURES ###
##########################################

# These files will save in your current R working directory.
write_csv(analysis_data, "lab7_analysis_data.csv")
write_csv(task1_summary, "lab7_task1_summary.csv")
write_csv(balance_summary, "lab7_balance_summary.csv")
write_csv(estimate_table, "lab7_estimate_table.csv")
write_csv(all_writeup_answers, "lab7_writeup_answers.csv")

ggsave(
  filename = "lab7_propensity_overlap.png",
  plot = propensity_overlap_plot,
  width = 7,
  height = 5,
  dpi = 300
)