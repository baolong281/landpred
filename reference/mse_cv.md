# Calculate MSE for Bandwidth Selection using Cross-Validation

Calculate MSE for Bandwidth Selection using Cross-Validation

## Usage

``` r
mse_cv(
  bw,
  landpred_obj,
  t0,
  tau,
  transform = identity,
  reps = 50,
  train_prop = 0.66
)
```

## Arguments

- bw:

  The bandwidth to test.

- landpred_obj:

  The landpred object.

- t0:

  The landmark time.

- tau:

  The prediction window.

- transform:

  Transformation function for short-term covariate.

- reps:

  Number of repetitions.

- train_prop:

  Proportion of data to use for training.

## Value

The Mean Squared Error.
