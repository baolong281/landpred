# Estimates P(TL \<t0+tau \| TL \> t0, Z), i.e. given discrete covariate.

This function calculates the probability that the an individual has the
event of interest before t0 + tau given the discrete covariate and given
the event has not yet occurred and the individual is still at risk at
time t0; this estimated probability does not incorporate any information
about the short term event information.

## Usage

``` r
Prob.Covariate(t0, tau, data, weight = NULL, short  = TRUE, newdata = NULL)
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

  n by k matrix, where k =3 or k=5. A data matrix where the first column
  is XL = min(TL, C) where TL is the time of the long term event, C is
  the censoring time, and the second column is DL =1\*(TL\<C). If short
  term event information is included in this dataset then the third
  column is XS = min(TS, C) where TS is the time of the short term
  event, C is the censoring time, and the fourth column is DS
  =1\*(TS\<C), and the fifth column is the covariate. If short term
  event information is not included then the third column is the
  covariates (see "short" parameter). These are the data used to
  calculate the estimated probabilities.

- weight:

  an optional weight to be incorporated in all estimation.

- short:

  logical value indicating whether data includes short term event
  information. Should be TRUE if short term XS and DS are includes as
  third and fourth columns of data matrix meaning that the covariates is
  in the fifth column, FALSE if not meaning that the covariate is in the
  third column. Default is TRUE.

- newdata:

  n by k matrix, where k =3 or k=5. A data matrix where the first column
  is XL = min(TL, C) where TL is the time of the long term event, C is
  the censoring time, and the second column is DL =1\*(TL\<C), and the
  last column (either 3rd or 5th) contains covariate values. Predicted
  probabilities are estimated for these data.

## Value

- Prob:

  matrix of estimated probability for each value of the covariate; first
  column shows all covariate values and second column contains predicted
  probability at that covariate value

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
Prob.Covariate(t0=t0,tau=tau,data=data_example_landpred)
#> Error in Prob.Covariate(t0 = t0, tau = tau, data = data_example_landpred): could not find function "Prob.Covariate"

out = Prob.Covariate(t0=t0,tau=tau,data=data_example_landpred)
#> Error in Prob.Covariate(t0 = t0, tau = tau, data = data_example_landpred): could not find function "Prob.Covariate"
out$Prob
#> Error: object 'out' not found
out$data
#> Error: object 'out' not found

newdata = matrix(c(1,1,1, 3,0,1, 4,1,1, 10,1,0, 11,0,1), ncol = 3, byrow=TRUE)
out = Prob.Covariate(t0=t0,tau=tau,data=data_example_landpred,newdata=newdata)
#> Error in Prob.Covariate(t0 = t0, tau = tau, data = data_example_landpred,     newdata = newdata): could not find function "Prob.Covariate"
out$Prob
#> Error: object 'out' not found
out$newdata
#> Error: object 'out' not found
```
