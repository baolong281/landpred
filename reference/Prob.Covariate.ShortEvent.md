# Calculate Probability with Short Event Information

Calculates the probability of the event occurring before `t0 + tau`,
given survival up to `t0`, using information on a short-term event.

## Usage

``` r
Prob.Covariate.ShortEvent(
  t0,
  tau,
  data,
  weight = NULL,
  bandwidth = NULL,
  newdata = NULL
)
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

- bandwidth:

  Bandwidth for kernel smoothing.

- newdata:

  Optional new data for prediction.

## Value

A landpred_result object.
