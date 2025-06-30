#############
### SETUP ###
#############

library(ggplot2)
library(glmnet)
library(tidyverse)
install.packages("truncnorm")
library(truncnorm)

set.seed(16)

# Load data
park_effects <- read_csv(file.choose())

#ids
park_effects <- park_effects %>%
  mutate(park_id = as.integer(factor(PARK)),
         off_id  = as.integer(factor(OT_YR)),
         def_id  = as.integer(factor(DT_YR)))

n_parks <- length(unique(park_effects$PARK))
n_teams <- length(unique(park_effects$OT_YR))

#b
beta_0    <- 0.4
beta_park <- rnorm(n_parks, mean = 0.04, sd = sqrt(0.065))
beta_off  <- rnorm(n_teams, mean = 0.02, sd = sqrt(0.045))
beta_def  <- rnorm(n_teams, mean = 0.03, sd = sqrt(0.07))

 
X <- model.matrix(~ factor(park_id) + factor(off_id) + factor(def_id), data = park_effects)

 
n_sim <- 100
y_list <- vector("list", n_sim)
for (m in 1:n_sim) {
  mu <- beta_0 +
    beta_park[park_effects$park_id] +
    beta_off[park_effects$off_id] +
    beta_def[park_effects$def_id]
  y_list[[m]] <- round(rtruncnorm(nrow(park_effects), a = 0, mean = mu, sd = 1))
}


ols_mat   <- matrix(NA, n_sim, n_parks - 1)
ridge_mat <- matrix(NA, n_sim, n_parks - 1)
lambdas   <- 10^seq(-3, 3, 0.2)

for (m in 1:n_sim) {
  y <- y_list[[m]]

  ols_fit <- lm(y ~ factor(park_id) + factor(off_id) + factor(def_id), data = park_effects)
  park_terms <- grep("^factor\\(park_id\\)", names(coef(ols_fit)), value = TRUE)
  ols_mat[m, ] <- coef(ols_fit)[park_terms]
  

  ridge_fit <- cv.glmnet(X, y, alpha = 0, lambda = lambdas, nfolds = 5, standardize = FALSE)
  ridge_coefs <- as.numeric(coef(ridge_fit, s = "lambda.min"))[
    grep("^factor\\(park_id\\)", rownames(coef(ridge_fit)))
  ]
  ridge_mat[m, ] <- ridge_coefs
}


true_park <- beta_park - mean(beta_park)  
ols_mean    <- colMeans(ols_mat)
ridge_mean  <- colMeans(ridge_mat)

bias_ols    <- sqrt(sum((ols_mean - true_park[-1])^2))
bias_ridge  <- sqrt(sum((ridge_mean - true_park[-1])^2))

var_ols     <- mean(apply(ols_mat, 2, var))
var_ridge   <- mean(apply(ridge_mat, 2, var))


summary(ols_mat)
summary(ridge_mat)

tibble(
  Method = c("OLS", "Ridge"),
  Bias   = c(bias_ols, bias_ridge),
  Variance = c(var_ols, var_ridge)
)