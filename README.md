
<!-- README.md is generated from README.Rmd. Please edit that file -->

# landpred

<!-- badges: start -->

[![R-CMD-check](https://github.com/baolong281/landpred/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/baolong281/landpred/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/baolong281/landpred/graph/badge.svg)](https://app.codecov.io/gh/baolong281/landpred)
<!-- badges: end -->

## Introduction

This landpred package provides nonparametric models for landmark
prediction of long-term survival outcomes, incorporating covariate and
short-term event information. The package supports the construction of
flexible varying-coefficient models that use discrete covariates, as
well as multiple continuous covariates. The goal is to improve
prediction accuracy when censored short-term events are available as
predictors, using robust nonparametric procedures that don’t require
correct model specification and avoid restrictive parametric assumptions
found in existing multistate survival methods. More information on these
models can be found in Parast et al. (2012, Journal of the American
Statistical Association, <doi:10.1080/01621459.2012.721281>), and Parast
et al. (2011, Biometrical Journal,
<https://doi.org/10.1002/bimj.201000150>).

# Tutorial

## Generating Data

We will generate a dataframe with two continuous covariates, $Z_1,Z_2$,
and one discrete covariate, $B$. We will also have a censored short
event $X_S$ and a censored long event, our outcome, $X_L$.

``` r
n <- 500
# Generate covariates
Z1 <- rnorm(n)
Z2 <- rnorm(n)
B <- rbinom(n, 1, 0.5)  # Binary discrete covariate

# Generate event times based on exponential model
X_L_raw <- rexp(n, rate = exp(-0.5 * Z1 + 0.3 * Z2 + 0.4 * B))
X_S_raw <- rexp(n, rate = exp(0.2 * Z1 - 0.3 * Z2 - 0.3 * B))

# Generate censoring times
C_L <- runif(n, 2, 8)  # censoring for long event
C_S <- runif(n, 1, 4)  # censoring for short event

# Apply censoring
X_L <- pmin(X_L_raw, C_L)
D_L <- as.numeric(X_L_raw <= C_L)  # 1 if event, 0 if censored

X_S <- pmin(X_S_raw, C_S)
D_S <- as.numeric(X_S_raw <= C_S)  # 1 if event, 0 if censored

# Return simple data frame
df <- data.frame(
  X_L = X_L,       # long event time
  D_L = D_L,       # long event indicator
  X_S = X_S,       # short event time
  D_S = D_S,       # short event indicator
  Z1 = Z1,         # continuous covariate 1
  Z2 = Z2,         # continuous covariate 2
  B = B            # discrete covariate
)
```

## Workflow

The package exports the `landpred` function, which will construct a
landpred object given a formula and data. We can supply any number of
continuous covariates to this, or a singular discrete covariate, as
demonstrated below. Note that short term information needs to be wrapped
in a `Surv` from the `survival` package.

``` r
library(landpred)
library(survival)

# Create model with continuous covariates and short term information
obj1 <- landpred(Surv(X_L, D_L) ~ Surv(X_S, D_S) + Z1 + Z2, data=df)

# Create model with discrete marker and short term information
# Notice the discrete flag is now enabled, which is false by default.
obj2 <- landpred(Surv(X_L, D_L) ~ Surv(X_S, D_S) + B, data=df, discrete=TRUE)

# Create model with discrete marker and no short term information
# Notice the discrete flag is now enabled, which is false by default.
obj3 <- landpred(Surv(X_L, D_L) ~  B, data=df, discrete=TRUE)
```

We will proceed with the object with continuous covariates, and
short-term information. We can call `summary` on this landpred object to
get more information from it. Note this landpred object returned by the
`landpred` call serves as a baseline object. To get a model for a
specific time ande delta, we must call the `get_model` function on this
object as specified in the `summary` function. Workflows / functions
will be the same in the discrete and continuous case.

``` r
summary(obj1)
#> 
#> Landpred Object Summary
#> Call get_model() to get time-specific model for t0 + tau
#> 
#> Call:
#> landpred(formula = Surv(X_L, D_L) ~ Surv(X_S, D_S) + Z1 + Z2)
#> 
#> Discrete: FALSE    Short Covariate: TRUE     N: 500
```

Our `get_model` function accepts a landpred object as input, and three
required parameters, `t0`, `tau`, and `bw`. This returns a landmark
prediction model for that specific time and delta. For the coefficients
and standard errors of the coefficeints, we call `summary` on the
returned model to get coefficients when no information in the short
covariate is provided (has not occured yet). We can also provide `t_s`
to this call to get coefficients for if the short event has occured yet.

We must first choose our bandwidth value. We provide a utility to select
this optimal value using K-fold cross validations

``` r
# t0 <- 0.5
# tau <- 1.5
# t_s <- 1
# bw <- optimize_bw(obj1, tau, t_s)
# bw
```

``` r
# Get model
model <- get_model(obj1, t0=1, tau=1.5, bw=0.05)

# Summary with no short covariate
summary(model)
#> 
#> Continuous Landpred Model:
#> 
#> Coefficients (No short covariate):
#> Warning in summary.glm(model$glm_noinfo): observations with zero weight not
#> used for calculating dispersion
#>             Estimate Std. Error
#> (Intercept)  1.84640     0.3022
#> Z1          -1.03234     0.2438
#> Z2           0.59962     0.2115
#> ---
#> Fit on n=146 observations.
#> 
#> t0: 1.000      tau: 1.500
```

``` r
# Summary with short covariate
summary(model, t_s=0.8)
#> 
#> Continuous Landpred Model:
#> 
#> Coefficients (t_s=0.800000):
#>             Estimate Std. Error
#> (Intercept)  12.8095     811.67
#> Z1           -6.0911     345.42
#> Z2            7.3664     618.72
#> ---
#> Fit on n=102 observations.
#> 
#> t0: 1.000      tau: 1.500
```

## Prediction

We can also do prediction wiih this model to get survival probabilities
for a given observation.

``` r
probs <- predict(model, newdata=df[1:5, , drop=FALSE])
probs
#> [1] 0.9968329 0.8711052 0.5689068 0.9175203 0.7586372
```
