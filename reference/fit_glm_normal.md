# Fit GLM with Normal Weights (No Short Covariate Info)

Fits a GLM for the probability of the event occurring before `t0 + tau`,
given survival up to `t0`, using only baseline covariates.

## Usage

``` r
fit_glm_normal(landpred_obj, t0, tau)
```

## Arguments

- landpred_obj:

  A landpred object containing the data.

- t0:

  The landmark time.

- tau:

  The prediction window.

## Value

A fitted glm object.
