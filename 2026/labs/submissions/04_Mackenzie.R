# install.packages(c("dplyr", "ggplot2", "mgcv", "readr"))

library(dplyr)
library(ggplot2)
library(readr)

###############################
### PART 1: BATTING AVERAGE ###
###############################

# Load data
ba_data = read_csv("/Users/mackenziebuckner/Desktop/lab-materials/2026/labs/data/04_ba-2020-2021.csv", show_col_types = FALSE)

# Task 1:
# Fit the day-1 linear regression model:
linearRegression_model = lm(BA_2021 ~ BA_2020, data = ba_data)

# Fit the binomial GLM:
binomialGLM_model = glm(cbind(H_2021, AB_2021 - H_2021) ~ BA_2020, data = ba_data, family = binomial(link = "logit"))

# Print summaries
summary(linearRegression_model)
summary(binomialGLM_model)

# Extract coefficients
lm_coef = coef(linearRegression_model)
glm_coef = coef(binomialGLM_model)

# Write down fitted linear regression model
cat("\nFitted Linear Regression Model:\n")
cat("BA_2021_hat =",
    round(lm_coef[1], 4), "+",
    round(lm_coef[2], 4), "* BA_2020\n")

# Write down fitted binomial GLM model
cat("\nFitted Binomial GLM Model:\n")
cat("logit(p_hat) =",
    round(glm_coef[1], 4), "+",
    round(glm_coef[2], 4), "* BA_2020\n")

cat("\nEquivalently:\n")
cat("p_hat = exp(logit) / (1 + exp(logit))\n")


# Task 2
# - Plot BA 2021 against BA 2020.
# Create a smooth grid of BA_2020 values for cleaner fitted curves
ba_grid = data.frame(
  BA_2020 = seq(
    from = min(ba_data$BA_2020, na.rm = TRUE),
    to = max(ba_data$BA_2020, na.rm = TRUE),
    length.out = 300
  )
)

# - Overlay the fitted mean from the linear regression model.
# - Overlay the fitted mean from the binomial GLM.
ba_grid = ba_grid %>%
  mutate(
    linear_fit = predict(linearRegression_model, newdata = ba_grid),
    glm_fit = predict(binomialGLM_model, newdata = ba_grid, type = "response")
  )

# legend
fit_lines = rbind(
  data.frame(
    BA_2020 = ba_grid$BA_2020,
    fitted_mean = ba_grid$linear_fit,
    Model = "Linear Regression"
  ),
  data.frame(
    BA_2020 = ba_grid$BA_2020,
    fitted_mean = ba_grid$glm_fit,
    Model = "Binomial GLM"
  )
)

# Plot observed data and fitted mean curves
ggplot() +
  
  # Scatterplot: point size reflects number of at-bats in 2021
  geom_point(
    data = ba_data,
    aes(x = BA_2020, y = BA_2021, size = AB_2021),
    alpha = 0.45,
    color = "gray35"
  ) +
  
  # Overlay fitted mean curves
  geom_line(
    data = fit_lines,
    aes(x = BA_2020, y = fitted_mean, color = Model),
    linewidth = 1.4
  ) +
  
  # - Make it visually clear which players had more at-bats in 2021
  scale_size_area(
    max_size = 7,
    name = "2021 At-Bats"
  ) +
  
  scale_color_manual(
    values = c(
      "Linear Regression" = "#1f77b4",
      "Binomial GLM" = "#d62728"
    ),
    name = "Fitted Model"
  ) +
  
  labs(
    title = "2021 Batting Average vs. 2020 Batting Average",
    subtitle = "Fitted means from linear regression and binomial GLM; point size reflects 2021 at-bats",
    x = "2020 Batting Average",
    y = "2021 Batting Average"
  ) +

  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray30"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )


#Briefly explain how the two fits differ and why the binomial GLM is a more natural model for hits and outs.
# The linear regression model directly predicts BA_2021 as a continuous outcome.
# The binomial GLM predicts the probability of a hit in 2021 using hits and outs:
# cbind(H_2021, AB_2021 - H_2021).
#
# The two fitted curves look similar, but the binomial GLM is more natural
# because batting average is not just a regular continuous variable. It comes from
# a number of hits out of a number of at-bats. A player with 200 hits out of 600
# at-bats gives more information than a player with 20 hits out of 60 at-bats,
# even if their batting averages are the same. The GLM accounts for that
# denominator, while the linear regression treats each batting average equally.


# Task 3:
# - Report the coefficient estimates, standard errors, and a 95% confidence interval for the GLM coefficients.
glm_summary = summary(binomialGLM_model)
glm_coef_estimates = glm_summary$coefficients[, "Estimate"]
glm_coef_se = glm_summary$coefficients[, "Std. Error"]
glm_coef_ci_lower = glm_coef_estimates - 1.96 * glm_coef_se
glm_coef_ci_upper = glm_coef_estimates + 1.96 * glm_coef_se
glm_results = data.frame(
  Coefficient = names(glm_coef_estimates),
  Estimate = glm_coef_estimates,
  Std_Error = glm_coef_se,
  CI_Lower = glm_coef_ci_lower,
  CI_Upper = glm_coef_ci_upper
)
print(glm_results)

# - Interpret the GLM coefficient on the log-odds scale

glm_slope = glm_coef_estimates["BA_2020"]

cat("\nInterpretation of GLM slope:\n")
cat("The estimated slope on BA_2020 is", round(glm_slope, 4), "\n")
cat("This means that for a 1.000 increase in BA_2020, the log-odds of getting a hit in 2021 increase by",
    round(glm_slope, 4), ".\n")
cat("Since a 1.000 increase in batting average is unrealistically large, it is more useful to interpret a 0.010 increase.\n")

# - Translate the slope into the effect of a 0.010 increase in 2020 batting average.

log_odds_increase = glm_slope * 0.010
odds_multiplier = exp(log_odds_increase)

cat("\nEffect of a 0.010 increase in BA_2020 on the odds of a hit in 2021:\n")
cat("Log-odds increase:", round(log_odds_increase, 4), "\n")
cat("Odds multiplier:", round(odds_multiplier, 4), "\n")

# Written interpretation:
# A 0.010 increase in BA_2020 increases the predicted log-odds of a hit in 2021
# by glm_slope * 0.010. Equivalently, the odds of a hit are multiplied by
# exp(glm_slope * 0.010).


# Task 4:
# - Pick one hypothetical player with BA_2020 = 0.260
hypothetical_player = data.frame(BA_2020 = 0.260)

# - Using your fitted GLM, estimate that player's 2021 hit probability p

p = predict(
  binomialGLM_model,
  newdata = hypothetical_player,
  type = "response"
)

cat("\nEstimated hit probability for hypothetical player with BA_2020 = 0.260:",
    round(p, 4), "\n")

# - Then compare two possible workloads:
#     (a) AB_2021 = 60
#     (b) AB_2021 = 600
# - For each workload, report:
#     * expected hits = AB_2021 * p
#     * an approximate 95% interval for batting average using p +/- 1.96 * sqrt(p(1-p)/AB_2021)

workloads = c(60, 600)

for (AB in workloads) {
  expected_hits = AB * p
  se = sqrt(p * (1 - p) / AB)
  ci_lower = p - 1.96 * se
  ci_upper = p + 1.96 * se
  
  cat("\nWorkload: AB_2021 =", AB, "\n")
  cat("Expected hits:", round(expected_hits, 2), "\n")
  cat("Approximate 95% interval for observed batting average: [",
      round(ci_lower, 4), ",", round(ci_upper, 4), "]\n")
}

# Explain why the low-at-bat player has a much wider interval even though the underlying hit probability is the same.
# The low-at-bat player has a much wider interval because batting average is
# based on hits divided by at-bats. With only 60 at-bats, there is much more
# random variation in the observed batting average. With 600 at-bats, the
# observed batting average is based on a much larger sample, so it is more
# stable.
# The standard error is sqrt(p(1-p)/AB). As AB increases,
# the denominator gets larger, so the standard error gets smaller. This is why
# the 600-at-bat player has a much narrower interval than the 60-at-bat player,
# even though both players have the same underlying hit probability p.



#######################################
### PART 2: FIELD-GOAL SUCCESS GAM ###
#######################################

fg_data = read_csv("/Users/mackenziebuckner/Desktop/lab-materials/2026/labs/data/04_field-goals.csv", show_col_types = FALSE)


# Task 1: Fit competing probability models.

# Model 1: Simple logistic GLM
# logit(p_i) = beta_0 + beta_1 * ydl_i
fg_glm_simple = glm(fg_made ~ ydl, data = fg_data, family = binomial(link = "logit"))

# Model 2: Richer logistic GLM
fg_glm_rich = glm(fg_made ~ ydl + I(ydl^2) + kq, data = fg_data, family = binomial(link = "logit"))

# Model 3: Logistic GAM
# logit(p_i) = beta_0 + f(ydl_i) + gamma * kq_i
fg_gam = gam(fg_made ~ s(ydl, k = 12) + kq, data = fg_data, family = binomial(link = "logit"), method = "REML")

summary(fg_glm_simple)
summary(fg_glm_rich)
summary(fg_gam)

# Task 2 : Compare the models out of sample

set.seed(2026)

n = nrow(fg_data)
train_indices = sample(1:n, size = floor(0.8 * n), replace = FALSE)

train_data = fg_data[train_indices, ]
test_data = fg_data[-train_indices, ]

# Fit models on training data
fg_glm_simple_train = glm(
  fg_made ~ ydl,
  data = train_data,
  family = binomial(link = "logit")
)

fg_glm_rich_train = glm(
  fg_made ~ ydl + I(ydl^2) + kq,
  data = train_data,
  family = binomial(link = "logit")
)

fg_gam_train = gam(
  fg_made ~ s(ydl, k = 12) + kq,
  data = train_data,
  family = binomial(link = "logit"),
  method = "REML"
)

# Predict probabilities on test data
test_data = test_data %>%
  mutate(
    pred_simple = predict(fg_glm_simple_train, newdata = ., type = "response"),
    pred_rich = predict(fg_glm_rich_train, newdata = ., type = "response"),
    pred_gam = predict(fg_gam_train, newdata = ., type = "response")
  )

# Log loss function
log_loss = function(actual, predicted) {
  eps = 1e-15
  predicted = pmin(pmax(predicted, eps), 1 - eps)
  -mean(actual * log(predicted) + (1 - actual) * log(1 - predicted))
}

log_loss_simple = log_loss(test_data$fg_made, test_data$pred_simple)
log_loss_rich = log_loss(test_data$fg_made, test_data$pred_rich)
log_loss_gam = log_loss(test_data$fg_made, test_data$pred_gam)

log_loss_results = data.frame(
  Model = c("Simple GLM", "Richer GLM", "GAM"),
  Test_Log_Loss = c(log_loss_simple, log_loss_rich, log_loss_gam)
)

print(log_loss_results)

best_model_name = log_loss_results$Model[which.min(log_loss_results$Test_Log_Loss)]

cat("\nPreferred model based on test-set log loss:", best_model_name, "\n")

# - State which model you prefer and why.
# Lower log loss means better out-of-sample probability predictions.
# I prefer the model with the lowest test-set log loss because it performs best
# on data that was not used to fit the model.
# If the GAM has the lowest log loss, I prefer it because it can learn a flexible
# nonlinear relationship between ydl and make probability.
# If one of the GLMs has the lowest log loss, I prefer it because it gives better
# predictive performance while remaining simpler and easier to interpret.

# Choose preferred GLM for later visualization
if (log_loss_rich <= log_loss_simple) {
  preferred_glm = fg_glm_rich
  preferred_glm_name = "Richer GLM"
} else {
  preferred_glm = fg_glm_simple
  preferred_glm_name = "Simple GLM"
}

# Task 3

gam_summary = summary(fg_gam)

# Coefficient, standard error, and 95% CI for kq
kq_coef = gam_summary$p.coeff["kq"]
kq_se = gam_summary$se["kq"]

kq_ci_lower = kq_coef - 1.96 * kq_se
kq_ci_upper = kq_coef + 1.96 * kq_se

# Effective degrees of freedom for smooth term
edf_ydl = gam_summary$s.table["s(ydl)", "edf"]

cat("\nGAM coefficient for kq:\n")
cat("Estimate:", round(kq_coef, 4), "\n")
cat("Standard Error:", round(kq_se, 4), "\n")
cat("95% Confidence Interval: [",
    round(kq_ci_lower, 4), ",",
    round(kq_ci_upper, 4), "]\n")

cat("\nEffective Degrees of Freedom for s(ydl):",
    round(edf_ydl, 4), "\n")

# - Explain what it means if the edf is noticeably larger than 1.

# The coefficient on kq measures how kicker quality affects the log-odds of
# making a field goal, holding ydl fixed.
# The edf measures how flexible the fitted smooth curve is.
# If the edf is close to 1, the relationship between ydl and log-odds of making
# the field goal is close to linear.
# If the edf is noticeably larger than 1, the GAM is fitting a more curved,
# nonlinear relationship between ydl and make probability.


# Task 4
median_kq = median(fg_data$kq, na.rm = TRUE)

# Grid for predicted curves
ydl_grid = data.frame(
  ydl = seq(
    from = min(fg_data$ydl, na.rm = TRUE),
    to = max(fg_data$ydl, na.rm = TRUE),
    length.out = 300
  ),
  kq = median_kq
)

ydl_grid = ydl_grid %>%
  mutate(
    pred_glm = predict(preferred_glm, newdata = ., type = "response"),
    pred_gam = predict(fg_gam, newdata = ., type = "response")
  )

# Binned observed make rates
fg_binned = fg_data %>%
  mutate(
    ydl_bin = cut(
      ydl,
      breaks = seq(
        from = floor(min(ydl, na.rm = TRUE)),
        to = ceiling(max(ydl, na.rm = TRUE)),
        length.out = 13
      ),
      include.lowest = TRUE
    )
  ) %>%
  group_by(ydl_bin) %>%
  summarize(
    ydl_mid = mean(ydl, na.rm = TRUE),
    observed_make_rate = mean(fg_made, na.rm = TRUE),
    attempts = n(),
    .groups = "drop"
  )

# Plot predicted curves and binned observed make rates
ggplot() +
  geom_point(
    data = fg_binned,
    aes(x = ydl_mid, y = observed_make_rate, size = attempts),
    alpha = 0.65,
    color = "gray35"
  ) +
  geom_line(
    data = ydl_grid,
    aes(x = ydl, y = pred_glm, color = preferred_glm_name),
    linewidth = 1.3
  ) +
  geom_line(
    data = ydl_grid,
    aes(x = ydl, y = pred_gam, color = "GAM"),
    linewidth = 1.3
  ) +
  scale_color_manual(
    values = c(
      "Simple GLM" = "#1f77b4",
      "Richer GLM" = "#d62728",
      "GAM" = "#2ca02c"
    ),
    name = "Model"
  ) +
  scale_size_area(
    max_size = 7,
    name = "Number of Attempts"
  ) +
  labs(
    title = "Predicted Field-Goal Make Probability by Yard Line",
    subtitle = "Lines show model predictions; points show binned observed make rates",
    x = "Yard Line Measured from Opponent's End Zone",
    y = "Make Probability"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray30"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# - Comment on where the GAM improves on the simpler GLM and where the two fits are similar.
# The GAM improves on the GLM in places where the relationship between ydl and
# make probability is curved rather than purely linear or quadratic.
# This is especially useful if make probability drops more sharply over some
# ranges of ydl than others.
# The GLM and GAM are similar in regions where the observed make rates follow a
# smooth simple trend. If the two fitted curves are close together, that suggests
# the simpler GLM is capturing the main relationship in that range.

# Task 5

new_data = data.frame(
  ydl = c(20, 35, 50),
  kq = median_kq
)

# Use GAM for predictions
gam_predictions = predict(
  fg_gam,
  newdata = new_data,
  type = "link",
  se.fit = TRUE
)

new_data = new_data %>%
  mutate(
    pred_logit = gam_predictions$fit,
    se_logit = gam_predictions$se.fit,
    ci_lower_logit = pred_logit - 1.96 * se_logit,
    ci_upper_logit = pred_logit + 1.96 * se_logit,
    pred_prob = plogis(pred_logit),
    ci_lower_prob = plogis(ci_lower_logit),
    ci_upper_prob = plogis(ci_upper_logit)
  )

prediction_results = new_data %>%
  select(
    ydl,
    kq,
    pred_prob,
    ci_lower_prob,
    ci_upper_prob
  )

print(prediction_results)


# Task 6 - reflection

# In a GLM, we choose the functional form by hand. For example, we decide whether
# to include ydl, ydl^2, or other polynomial terms. This means the shape of the
# fitted curve depends heavily on the terms we manually choose.
# In a GAM, we allow the model to learn a smooth function of ydl from the data.
# Instead of forcing the relationship to be linear or quadratic, the GAM estimates
# a flexible curve.
# One reason a GAM can help:
# A GAM can capture nonlinear patterns that a simple GLM might miss. This can
# improve predicted field-goal probabilities if make probability changes in a
# curved or uneven way as ydl changes.
# One reason a GAM can hurt:
# A GAM can overfit if it becomes too flexible, especially in areas with limited
# data. It may fit random noise instead of the true relationship, which can hurt
# out-of-sample predictions and make the model harder to interpret.

