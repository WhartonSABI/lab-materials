#############
### SETUP ###
#############

# Load libraries
library(tidyverse)
library(glmnet)
library(broom)

# Set seed
set.seed(15)

# Load data
nba_lineups <- readRDS(file.choose())  
head(nba_lineups)

X <- model.matrix(pts_poss ~ . - 1, data = nba_lineups)
y <- nba_lineups$pts_poss


ols_model <- lm(pts_poss ~ . - 1, data = nba_lineups)
summary(ols_model)

lambdas <- 10^seq(-3, 3, 0.2)
ridge_model <- cv.glmnet(X, y, alpha = 0, lambda = lambdas, nfolds = 5, standardize = FALSE)
best_lambda <- ridge_model$lambda.min
print(best_lambda)
plot(ridge_model)


coef_ols <- coef(ols_model)[-1]
coef_ridge <- as.numeric(coef(ridge_model, s = best_lambda))[-1]

comparison <- tibble(
  term = colnames(X),
  OLS = coef_ols,
  Ridge = coef_ridge
)

ggplot(comparison, aes(x = OLS, y = Ridge)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "OLS vs Ridge", x = "OLS", y = "Ridge") +
  theme_minimal()
