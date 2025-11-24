# Create a Landpred Object

Parses the formula and data to create a landpred object used for
landmark prediction. Call \`?landpred.pacakge\` for more information on
the legacy API.

## Usage

``` r
landpred(formula, data, discrete = FALSE)
```

## Arguments

- formula:

  A formula object with a Surv object on the LHS and covariates on the
  RHS.

- data:

  The data frame.

- discrete:

  Logical, whether to use the discrete method (legacy).

## Value

A landpred_object.

## Examples

``` r
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
#> [1] "Optimal bandwidth: 1.94012006548541"

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
#> 
#> Continuous Landpred Model:
#> Call coef() or summary() with t_s to see coefficients.
#> 
#> Call:
#> landpred(formula = Surv(XL, DL) ~ Surv(XS, DS) + Z)
#> 
#> t0: 2.000      tau: 8.000     

# 3. Predict on new data
# For demonstration, we use the first 10 rows of the original data as "new data"
new_data <- data_example_landpred[1:10, ]

# The predict function expects a data frame with the same column names as used in the formula
probs <- predict(model, newdata = new_data)

print("Predicted probabilities:")
#> [1] "Predicted probabilities:"
print(probs)
#>  [1] 0.3946154 0.3946154 0.3447831 0.2360992 0.2360992 0.3641601 0.4010378
#>  [8] 0.3998490 0.3606193 0.4339085
```
