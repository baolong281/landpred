# Estimates P(TL \<t0+tau \| TL \> t0, Z, TS\>t0).

This function calculates the probability that the an individual has the
event of interest before t0 + tau given the discrete covariate, given
the short term event has not yet occurred by t0, and given the long term
event has not yet occurred and the individual is still at risk at time
t0. This function is called by Prob.Covariate.ShortEvent; this function
should not be called on its own.

## Usage

``` r
Prob2(t0, tau, data, covariate.value, weight = NULL)
```

## Arguments

- t0:

  the landmark time.

- tau:

  the residual survival time for which probabilities are calculated.
  Specifically, this function estimates the probability that the an
  individual has the event of interest before t0 + tau given the event
  has not yet occurred and the individual is still at risk at time t0.

- data:

  n by 5 matrix. A data matrix where the first column is XL = min(TL, C)
  where TL is the time of the long term event, C is the censoring time,
  and the second column is DL =1\*(TL\<C), the third column is log(XS) =
  log(min(TS, C)) where TS is the time of the short term event, C is the
  censoring time, the fourth column is DS =1\*(TS\<C), and the fifth
  column is the covariate. These are the data used to calculate the
  estimated probability.

- covariate.value:

  the discrete covariate value at which to calculate the estimated
  probability.

- weight:

  an optional weight to be incorporated in all estimation.

## Value

Estimated probability = P(TL \<t0+tau \| TL \> t0, Z, TS\>t0).

## References

Parast, Layla, Su-Chun Cheng, and Tianxi Cai. Incorporating short-term
outcome information to predict long-term survival with discrete markers.
Biometrical Journal 53.2 (2011): 294-307.

## Author

Layla Parast
