# Global test values
t0 <- 1.0
tau <- 1.5
bw <- 0.5
set.seed(12345)

# Definition of weighting function in old code
Wi.FUN <- function(tt, data, t0, tau, weight.given = NULL)	{
  Xi <- data[,1]; Di <- data[,2]; wihat <- rep(0, length(Xi))
  tmpind1 <- (Xi > t0)&(Xi <= (t0+tau)); tt0 <- c(Xi[tmpind1], t0 + tau); Ghat.tt0 <- Ghat.FUN(tt0,data, weight.given=weight.given)
  wihat[Xi > (t0+tau)] <- 1/Ghat.tt0[length(tt0)]
  wihat[tmpind1] <- Di[tmpind1]/Ghat.tt0[-length(tt0)]
  wihat
}

# Function to generate synthetic landmark prediction data
generate_test_data <- function(n = 500, p = 5) {
  # Generate covariates
  Z <- matrix(rnorm(n * p), nrow = n, ncol = p)
  colnames(Z) <- paste0("Z", 1:p)

  # True coefficients for survival models
  beta_long <- rnorm(p, mean = 0, sd = 0.3)
  beta_short <- rnorm(p, mean = 0, sd = 0.3)

  # Set some specific values for reproducibility
  if(p >= 1) beta_long[1] <- -0.5
  if(p >= 2) beta_long[2] <- 0.3
  if(p >= 3) beta_long[3] <- -0.2
  if(p >= 1) beta_short[1] <- 0.2
  if(p >= 2) beta_short[2] <- -0.3
  if(p >= 3) beta_short[3] <- 0.25

  # Generate event times based on exponential model
  linear_pred_long <- Z %*% beta_long
  X_L_raw <- rexp(n, rate = exp(linear_pred_long))

  linear_pred_short <- Z %*% beta_short
  X_S_raw <- rexp(n, rate = exp(linear_pred_short))

  # Generate censoring times
  C_L <- runif(n, 2, 8)  # censoring for long event
  C_S <- runif(n, 1, 4)  # censoring for short event

  # Apply censoring
  X_L <- pmin(X_L_raw, C_L)
  D_L <- as.numeric(X_L_raw <= C_L)  # 1 if event, 0 if censored

  X_S <- pmin(X_S_raw, C_S)
  D_S <- as.numeric(X_S_raw <= C_S)  # 1 if event, 0 if censored

  # For old code format (column-based)
  old_format <- data.frame(
    X1i = log(X_S),  # log-transformed short event time
    X2i = X_L,       # long event time
    D1i = D_S,       # short event indicator
    D2i = D_L,       # long event indicator
    Z                # covariates
  )

  # For new code format (structured object)
  Z_df <- as.data.frame(Z)
  colnames(Z_df) <- paste0("Z", 1:p)

  new_format <- list(
    X_L = cbind(time = X_L, status = D_L),
    X_S = cbind(time = X_S, status = D_S),
    Z = Z_df,
    names = list(
      covariates = colnames(Z_df),
      x_s_name = "time"
    )
  )

  return(list(
    old_format = old_format,
    new_format = new_format,
    true_params = list(beta_long = beta_long, beta_short = beta_short)
  ))
}


test_that("Data generation works", {
  test_data <- generate_test_data(200, p=3)
  old_data <- test_data$old_format
  new_data <- test_data$new_format

  expect_equal(old_data$X2i, new_data$X_L[, "time"])
  expect_equal(exp(old_data$X1i), new_data$X_S[, "time"])
})

# Unit tests using testthat
test_that("Weight functions are equivalent", {
  test_data <- generate_test_data(n = 100, p = 3)
  old_data <- test_data$old_format
  new_data <- test_data$new_format

  old_weights <- Wi.FUN(old_data$X2i, data = cbind(old_data$X2i, old_data$D2i), t0 = t0, tau = tau)
  new_weights <- w_i(new_data$X_L[,"time"], new_data$X_L[,"status"], t0 = t0, tau = tau)

  expect_equal(new_weights, old_weights, tolerance = 1e-6)
})

test_that("GLM coefficients match between old and new approaches", {
  test_data <- generate_test_data(n = 200, p = 3)
  old_data <- test_data$old_format
  new_data <- test_data$new_format

  # Old approach
  old_weights <- Wi.FUN(old_data$X2i, data = cbind(old_data$X2i, old_data$D2i), t0 = t0, tau = tau)
  old_past <- which(old_data$X2i > t0)
  covariate_cols <- 5:ncol(old_data)
  old_formula <- paste("I(X2i < t0 + tau) ~", paste(names(old_data)[covariate_cols], collapse = " + "))
  old_glm <- glm(
    as.formula(old_formula),
    data = old_data[old_past, ],
    family = "binomial",
    weights = old_weights[old_past]
  )

  # New approach
  new_glm <- fit_glm_normal(new_data, t0, tau)

  expect_equal(coef(new_glm), coef(old_glm), tolerance = 1e-6)
})

