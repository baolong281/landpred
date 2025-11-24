# Fit GLM with Kernel Weights (Short Covariate Info)

Fits a GLM for the probability of the event occurring before `t0 + tau`,
given survival up to `t0` and information on a short-term covariate.
Uses kernel weighting based on the short-term covariate value.

## Usage

``` r
fit_short_glm(landpred_obj, t0, tau, t_s, bw, transform, indices = NULL)
```

## Arguments

- landpred_obj:

  A landpred object containing the data.

- t0:

  The landmark time.

- tau:

  The prediction window.

- t_s:

  The time of the short-term covariate measurement.

- bw:

  The bandwidth for kernel weighting.

- transform:

  A transformation function for the time variable (e.g., log).

- indices:

  Optional indices to subset the data.

## Value

A fitted glm object.

A fitted glm object.
