# Calculate Probability with No Information

Calculates the probability of the event occurring before `t0 + tau`,
given survival up to `t0`, without using any covariate information.

## Usage

``` r
Prob.Null(t0, tau, data, weight = NULL, newdata = NULL)
```

## Arguments

- t0:

  The landmark time.

- tau:

  The prediction window.

- data:

  The data frame.

- weight:

  Optional weights.

- newdata:

  Optional new data for prediction.

## Value

A landpred_result object.
