library(landpred)
library(survival)

# Load example data
data(data_example_landpred)

# Define landmark time and prediction window
t0 <- 2
tau <- 8

# Create a landpred object using the formula interface
# The formula specifies: Long-term survival ~ Short-term survival + Covariates
# Note: The short-term covariate must be a Surv object
obj <- landpred(
  Surv(XL, DL) ~ Surv(XS, DS) + Z,
  data = data_example_landpred,
  discrete = FALSE
)

# 1. Optimize bandwidth (Optional but recommended)
# This uses cross-validation to find the optimal bandwidth for the short-term covariate
# We use log transformation for the time variable as it's often more appropriate
bw <- optimize_bandwidth(
  landpred_obj = obj,
  t0 = t0,
  tau = tau,
  lower = 0.5,
  upper = 5,
  transform = log
)

print(paste("Optimal bandwidth:", bw))

# 2. Fit the model
# We pass the optimized bandwidth and the transformation used
model <- get_model(
  landpred_obj = obj,
  t0 = t0,
  tau = tau,
  bw = bw,
  transform = log
)

print(model)

# 3. Predict on new data
# For demonstration, we use the first 10 rows of the original data as "new data"
new_data <- data_example_landpred[1:10, ]

# The predict function expects a data frame with the same column names as used in the formula
probs <- predict(model, newdata = new_data)

print("Predicted probabilities:")
print(probs)
