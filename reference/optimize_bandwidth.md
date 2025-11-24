# Optimize Bandwidth for Continuous Landpred Models

Selects the optimal bandwidth by minimizing the Mean Squared Error (MSE)
using cross-validation.

## Usage

``` r
optimize_bandwidth(
  landpred_obj,
  t0,
  tau,
  lower = 0.05,
  upper = 5,
  transform = identity,
  reps = 50,
  train_prop = 0.66
)
```

## Arguments

- landpred_obj:

  A landpred object.

- t0:

  The landmark time.

- tau:

  The prediction window.

- lower:

  Lower bound for bandwidth search.

- upper:

  Upper bound for bandwidth search.

- transform:

  Transformation function for the short-term covariate (e.g., log).
  Default is identity.

- reps:

  Number of cross-validation repetitions. Default is 50.

- train_prop:

  Proportion of data used for training in each fold. Default is 0.66.

## Value

The optimal bandwidth.
