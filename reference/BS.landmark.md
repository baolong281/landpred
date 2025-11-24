# Calculate Brier Score for Landmark Prediction

Calculates the Brier Score to evaluate the performance of the landmark
prediction model.

## Usage

``` r
BS.landmark(t0, tau, data, short = TRUE, weight = NULL)
```

## Arguments

- t0:

  The landmark time.

- tau:

  The prediction window.

- data:

  The data frame containing predictions.

- short:

  Logical, whether short-term covariate info was used.

- weight:

  Optional weights.

## Value

A list containing the estimated Brier Score (AUC.est - note: function
name says BS but return says AUC.est, likely BS).

A list containing the estimated Brier Score (AUC.est - note: function
name says BS but return says AUC.est, likely BS).
