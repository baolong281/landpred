# Optimize Bandwidth (Legacy)

Optimize Bandwidth (Legacy)

## Usage

``` r
optimize.mse.BW(
  data,
  t0,
  tau,
  h.grid = seq(0.01, 2, length = 50),
  folds = 3,
  reps = 2
)
```

## Arguments

- data:

  Data frame.

- t0:

  Landmark time.

- tau:

  Prediction window.

- h.grid:

  Grid of bandwidths.

- folds:

  Number of folds.

- reps:

  Number of repetitions.
