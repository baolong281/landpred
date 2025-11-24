# Calculate Standard Errors for Coefficients

Calculates standard errors for the coefficients of the landpred model.
If `t_s` is provided, it uses the perturbation resampling method.
Otherwise, it returns the standard errors from the GLM.

## Usage

``` r
coefficient_se(model, t_s = NULL, samples = 200)
```

## Arguments

- model:

  A landpred_model_continuous object.

- t_s:

  The time of the short-term covariate measurement.

- samples:

  The number of resampling iterations.

## Value

A named vector of standard errors.

A named vector of standard errors.
