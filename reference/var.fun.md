# Estimate Variance of Coefficients

Estimates the variance of the coefficients for the short-term GLM using
a perturbation resampling method.

## Usage

``` r
var.fun(t, data.v, tau, s, h, vmat, Ainv, weight = NULL, transform = identity)
```

## Arguments

- t:

  The target time for the short-term covariate (usually t_s).

- data.v:

  The data frame used for estimation.

- tau:

  The landmark time.

- s:

  The prediction window.

- h:

  The bandwidth.

- vmat:

  A matrix of perturbation weights.

- Ainv:

  The inverse information matrix from the model fit.

- weight:

  Optional weights.

- transform:

  Transformation function.

## Value

A list containing the estimated standard errors for the intercept and
slopes.
