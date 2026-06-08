#############
### SETUP ###
#############

# install.packages(c("dplyr", "ggplot2", "readr"))
library(dplyr)
library(ggplot2)
library(readr)

set.seed(7)

##############
### PART 1 ###
##############

diving_data <- read_csv("/Users/mackenziebuckner/Desktop/lab-materials/2026/labs/data/06_diving.csv",show_col_types = FALSE)

# Standardize column names so the rest of the code is consistent
diving_data <- diving_data %>%
  rename_with(toupper)

# Check that the needed columns exist
needed_cols <- c(
  "EVENT", "DIVER", "COUNTRY", "RANK", "DIVENO",
  "DIFFICULTY", "JUDGE", "JCOUNTRY", "JSCORE"
)

missing_cols <- setdiff(needed_cols, names(diving_data))

if (length(missing_cols) > 0) {
  stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
}

# Create same-country indicator and discrepancy 
# same_country = 1 if judge and diver are from the same country
# discrepancy = judge score minus the average score from the OTHER judges on that same dive

diving_data <- diving_data %>%
  mutate(
    same_country = as.integer(COUNTRY == JCOUNTRY)
  ) %>%
  group_by(EVENT, DIVER, COUNTRY, RANK, DIVENO, DIFFICULTY) %>%
  mutate(
    n_judge_scores = sum(!is.na(JSCORE)),
    other_judge_mean = if_else(
      n_judge_scores > 1,
      (sum(JSCORE, na.rm = TRUE) - JSCORE) / (n_judge_scores - 1),
      NA_real_
    ),
    discrepancy = JSCORE - other_judge_mean
  ) %>%
  ungroup() %>%
  filter(
    !is.na(discrepancy),
    !is.na(same_country)
  )

# Test statistic:
# mean discrepancy for same-country dives minus
# mean discrepancy for other-country dives

calc_stat <- function(discrepancy, same_country) {
  n_same <- sum(same_country == 1)
  n_other <- sum(same_country == 0)
  
  if (n_same == 0 | n_other == 0) {
    return(NA_real_)
  }
  
  mean(discrepancy[same_country == 1]) -
    mean(discrepancy[same_country == 0])
}


### Task 1 and 2: Permutation tests
B <- 5000  # number of permutations

judge_list <- split(diving_data, diving_data$JUDGE)

judge_stats <- lapply(names(judge_list), function(judge_name) {
  
  judge_data <- judge_list[[judge_name]]
  
  observed_stat <- calc_stat(
    discrepancy = judge_data$discrepancy,
    same_country = judge_data$same_country
  )
  
  if (is.na(observed_stat)) {
    
    null_distribution <- rep(NA_real_, B)
    p_value <- NA_real_
    
  } else {
    
    null_distribution <- replicate(B, {
      shuffled_match <- sample(judge_data$same_country)
      
      calc_stat(
        discrepancy = judge_data$discrepancy,
        same_country = shuffled_match
      )
    })
    
    # Two-sided permutation p-value
    # Adding 1 to numerator and denominator avoids a p-value of exactly 0
    p_value <- (sum(abs(null_distribution) >= abs(observed_stat)) + 1) / (B + 1)
  }
  
  tibble(
    JUDGE = judge_name,
    n_total = nrow(judge_data),
    n_same_country = sum(judge_data$same_country == 1),
    n_other_country = sum(judge_data$same_country == 0),
    observed_stat = observed_stat,
    p_value_unadjusted = p_value,
    null_distribution = list(null_distribution)
  )
  
}) %>%
  bind_rows()


#Task 3: Multiple-testing adjustment 


judge_stats <- judge_stats %>%
  mutate(
    p_value_adjusted = p.adjust(p_value_unadjusted, method = "BH")
  ) %>%
  arrange(p_value_unadjusted)


#Task 4: Identify significant judges

alpha <- 0.05

judge_stats <- judge_stats %>%
  mutate(
    evidence_before_adjustment = p_value_unadjusted < alpha,
    evidence_after_adjustment = p_value_adjusted < alpha
  )

# Full results table
judge_results_table <- judge_stats %>%
  select(
    JUDGE,
    n_total,
    n_same_country,
    n_other_country,
    observed_stat,
    p_value_unadjusted,
    p_value_adjusted,
    evidence_before_adjustment,
    evidence_after_adjustment
  )

print(judge_results_table)

# Judges with evidence before adjustment
judges_before_adjustment <- judge_results_table %>%
  filter(evidence_before_adjustment == TRUE)

print(judges_before_adjustment)

# Judges with evidence after adjustment
judges_after_adjustment <- judge_results_table %>%
  filter(evidence_after_adjustment == TRUE)

print(judges_after_adjustment)


# Task 4:
# - Identify which judges show evidence of nationality bias before adjustment
# - Identify which judges still show evidence after adjustment
# - Make at least one plot that helps explain the strongest case(s)

# Plot 1: P-values before and after adjustment 

pval_plot_data <- bind_rows(
  judge_results_table %>%
    transmute(
      JUDGE,
      p_value = p_value_unadjusted,
      type = "Unadjusted p-value"
    ),
  judge_results_table %>%
    transmute(
      JUDGE,
      p_value = p_value_adjusted,
      type = "BH-adjusted p-value"
    )
)

ggplot(
  pval_plot_data,
  aes(x = reorder(JUDGE, p_value), y = p_value, fill = type)
) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = alpha, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(
    title = "Judge-Level Permutation Test P-Values",
    subtitle = "Dashed red line shows alpha = 0.05",
    x = "Judge",
    y = "P-value",
    fill = ""
  ) +
  theme_minimal()


# Plot 2: Null distribution for strongest judge case

strongest_judge <- judge_stats %>%
  filter(!is.na(p_value_unadjusted)) %>%
  slice_min(p_value_unadjusted, n = 1) %>%
  pull(JUDGE)

strongest_row <- judge_stats %>%
  filter(JUDGE == strongest_judge)

strongest_null <- unlist(strongest_row$null_distribution)
strongest_observed <- strongest_row$observed_stat

strongest_plot_data <- tibble(
  null_stat = strongest_null
)

ggplot(strongest_plot_data, aes(x = null_stat)) +
  geom_histogram(bins = 40, color = "white") +
  geom_vline(
    xintercept = strongest_observed,
    color = "red",
    linewidth = 1.2
  ) +
  labs(
    title = paste("Permutation Null Distribution:", strongest_judge),
    subtitle = paste(
      "Observed statistic =",
      round(strongest_observed, 4),
      "| Unadjusted p-value =",
      round(strongest_row$p_value_unadjusted, 4)
    ),
    x = "Permuted test statistic",
    y = "Count"
  ) +
  theme_minimal()



##############
### PART 2 ###
##############

# load data
tto_data <- read_csv(
  "/Users/mackenziebuckner/Desktop/lab-materials/2026/labs/data/06_tto.csv",
  show_col_types = FALSE
)

# Clean column names just in case
names(tto_data) <- names(tto_data) %>%
  toupper() %>%
  gsub("[^A-Z0-9]+", "_", .) %>%
  gsub("^_|_$", "", .)

# Check ORDER_CT values so we know whether it is coded as 1, 2, 3
# or as batter sequence numbers like 1, 2, ..., 27
print(range(tto_data$ORDER_CT, na.rm = TRUE))
print(table(tto_data$ORDER_CT, useNA = "ifany"))

# Your output suggests ORDER_CT is coded as 1, 2, 3, not 1 through 27.
# So the correct cutoffs are:
# 2TTO: ORDER_CT >= 2
# 3TTO: ORDER_CT >= 3

tto_data <- tto_data %>%
  mutate(
    tto_2 = as.integer(ORDER_CT >= 2),
    tto_3 = as.integer(ORDER_CT >= 3)
  )


# Task 1


# Model 1:
# y_i = beta_1
#     + beta_2 * 1{t_i >= 2TTO}
#     + beta_3 * 1{t_i >= 3TTO}
#     + beta_BQ * BQ_i
#     + beta_PQ * PQ_i
#     + beta_hand * hand_i
#     + beta_home * home_i
#     + error_i

model_1 <- lm(
  EVENT_WOBA_19 ~ tto_2 + tto_3 +
    WOBA_FINAL_BAT_19 + WOBA_FINAL_PIT_19 +
    HAND_MATCH + BAT_HOME_IND,
  data = tto_data
)


# Task 2

model_2 <- lm(
  EVENT_WOBA_19 ~ ORDER_CT +
    WOBA_FINAL_BAT_19 + WOBA_FINAL_PIT_19 +
    HAND_MATCH + BAT_HOME_IND,
  data = tto_data
)


# Task 3

summary(model_1)
summary(model_2)


# Extract coefficient estimates, SEs, t-stats, p-values 

coef_table_1 <- as.data.frame(coef(summary(model_1)))
coef_table_1$term <- rownames(coef_table_1)
coef_table_1$model <- "Model 1"

coef_table_2 <- as.data.frame(coef(summary(model_2)))
coef_table_2$term <- rownames(coef_table_2)
coef_table_2$model <- "Model 2"

coef_table <- bind_rows(coef_table_1, coef_table_2) %>%
  select(
    model,
    term,
    estimate = Estimate,
    std_error = `Std. Error`,
    test_statistic = `t value`,
    p_value = `Pr(>|t|)`
  )

print(coef_table)


# Focus on pitcher decline / TTO-related terms

pitcher_decline_terms <- coef_table %>%
  filter(term %in% c("tto_2", "tto_3", "ORDER_CT")) %>%
  mutate(
    significant_at_5_percent = p_value < 0.05
  )

print(pitcher_decline_terms)


# Joint tests for whether TTO terms matter


model_controls_only <- lm(
  EVENT_WOBA_19 ~ WOBA_FINAL_BAT_19 + WOBA_FINAL_PIT_19 +
    HAND_MATCH + BAT_HOME_IND,
  data = tto_data
)

# Does adding 2TTO and 3TTO indicators improve the model?
anova(model_controls_only, model_1)

# Does adding linear ORDER_CT improve the model?
anova(model_controls_only, model_2)


# Plot predicted wOBA by time through the order


prediction_data <- tibble(
  ORDER_CT = sort(unique(tto_data$ORDER_CT)),
  WOBA_FINAL_BAT_19 = mean(tto_data$WOBA_FINAL_BAT_19, na.rm = TRUE),
  WOBA_FINAL_PIT_19 = mean(tto_data$WOBA_FINAL_PIT_19, na.rm = TRUE),
  HAND_MATCH = 1,
  BAT_HOME_IND = 1
) %>%
  mutate(
    tto_2 = as.integer(ORDER_CT >= 2),
    tto_3 = as.integer(ORDER_CT >= 3)
  )

# Do predictions after creating all needed variables
prediction_data$pred_model_1 <- predict(model_1, newdata = prediction_data)
prediction_data$pred_model_2 <- predict(model_2, newdata = prediction_data)

prediction_long <- bind_rows(
  prediction_data %>%
    transmute(
      ORDER_CT,
      model = "Model 1: TTO Indicators",
      predicted_woba = pred_model_1
    ),
  prediction_data %>%
    transmute(
      ORDER_CT,
      model = "Model 2: Linear TTO Trend",
      predicted_woba = pred_model_2
    )
)

ggplot(prediction_long, aes(x = ORDER_CT, y = predicted_woba, color = model)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = sort(unique(tto_data$ORDER_CT))) +
  labs(
    title = "Predicted wOBA by Time Through the Order",
    x = "Time through the order",
    y = "Predicted wOBA",
    color = "Model"
  ) +
  theme_minimal()


# In Model 1, tto_2 estimates the change in expected wOBA when a pitcher reaches
# the second time through the order, holding batter quality, pitcher quality,
# handedness match, and home indicator fixed. tto_3 estimates the additional
# change when the pitcher reaches the third time through the order.

# In Model 2, ORDER_CT captures a smooth linear increase or decrease in expected
# wOBA as the batter sequence number rises. The tto_2 and tto_3 coefficients then
# capture extra jumps at the start of the second and third times through the order,
# after accounting for that linear trend.

# To decide whether pitcher decline is statistically significant, look at the
# p-values for ORDER_CT, tto_2, and tto_3. If the p-value is below 0.05, then that
# coefficient is statistically significant at the 5% level. If it is above 0.05,
# then we do not have strong evidence that the coefficient differs from zero.