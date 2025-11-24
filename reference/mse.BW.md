# Optimize Bandwidth for Kernel Smoothing

Calculates the Mean Squared Error (MSE) for a given bandwidth to help
select the optimal bandwidth.

## Usage

``` r
mse.BW(data, t0, tau, h, folds = 3, reps = 2)
```

## Arguments

- data:

  The data frame.

- t0:

  The landmark time.

- tau:

  The prediction window.

- h:

  The bandwidth to test.

- folds:

  Number of cross-validation folds.

- reps:

  Number of repetitions.

## Value

The MSE.

The MSE.
