# Calculate Probability with Covariate Information

Calculates the probability of the event occurring before `t0 + tau`,
given survival up to `t0`, using a single covariate.

## Usage

``` r
Prob.Covariate(t0, tau, data, weight = NULL, short = TRUE, newdata = NULL)
```

## Arguments

- t0:

  The landmark time.

- tau:

  The prediction window.

- data:

  The data frame for training.

- weight:

  Optional weights.

- short:

  Logical, whether the covariate is short-term.

- newdata:

  Dataframe of new data for prediction.

## Value

A landpred_result object.
