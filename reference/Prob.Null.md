# Estimates P(TL \<t0+tau \| TL \> t0).

This function calculates the probability that an individual has the
event of interest before t0 + tau given the event has not yet occurred
and the individual is still at risk at time t0; this estimated
probability does not incorporate any information about the covariate or
short term event information.

## Usage

``` r
Prob.Null(t0, tau, data, weight = NULL, newdata=NULL)
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

  n by k matrix, where k \>=2. A data matrix where the first column is
  XL = min(TL, C) where TL is the time of the long term event, C is the
  censoring time, and the second column is DL =1\*(TL\<C). These are the
  data used to calculate the estimated probability.

- weight:

  an optional weight to be incorporated in all estimation.

- newdata:

  an optional n by k matrix, where k \>=2. A data matrix where the first
  column is XL = min(TL, C) where TL is the time of the long term event,
  C is the censoring time, and the second column is DL =1\*(TL\<C).
  Predicted probabilities are estimated for these data.

## Value

- Prob:

  Estimated probability that the an individual has the event of interest
  before t0 + tau given the event has not yet occurred and the
  individual is still at risk at time t0; this estimated probability
  does not incorporate any information about the covariate or short term
  event information.

- data:

  the data matrix with an additional column with the estimated
  individual probabilities; note that the predicted probability is NA if
  TL \<t0 since it is only defined for individuals with TL\> t0

- newdata:

  the newdata matrix with an additional column with the estimated
  individual probabilities; note that the predicted probability is NA if
  TL \<t0 since it is only defined for individuals with TL\> t0; if
  newdata is not supplied then this returns NULL

## References

Parast, Layla, Su-Chun Cheng, and Tianxi Cai. Incorporating short-term
outcome information to predict long-term survival with discrete markers.
Biometrical Journal 53.2 (2011): 294-307.

## Author

Layla Parast

## Examples

``` r
data(data_example_landpred)
t0=2
tau = 8
Prob.Null(t0=t0,tau=tau,data=data_example_landpred)
#> Error in Prob.Null(t0 = t0, tau = tau, data = data_example_landpred): could not find function "Prob.Null"

out = Prob.Null(t0=t0,tau=tau,data=data_example_landpred)
#> Error in Prob.Null(t0 = t0, tau = tau, data = data_example_landpred): could not find function "Prob.Null"
out$Prob
#> Error: object 'out' not found
out$data
#> Error: object 'out' not found

newdata = matrix(c(1,1,3,0,4,1,10,1,11,0), ncol = 2, byrow=TRUE)
out = Prob.Null(t0=t0,tau=tau,data=data_example_landpred,newdata=newdata)
#> Error in Prob.Null(t0 = t0, tau = tau, data = data_example_landpred, newdata = newdata): could not find function "Prob.Null"
out$Prob
#> Error: object 'out' not found
out$newdata
#> Error: object 'out' not found
```
