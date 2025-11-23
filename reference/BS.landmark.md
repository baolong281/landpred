# Estimates the Brier score.

This function calculates the Brier score given the data (truth) and
corresponding estimated probabilities.

## Usage

``` r
BS.landmark(t0, tau, data, short = TRUE, weight=NULL)
```

## Arguments

- t0:

  the landmark time.

- tau:

  the residual survival time of interest.

- data:

  n by k matrix, where k = 4 or 6. A data matrix where the first column
  is XL = min(TL, C) where TL is the time of the long term event, C is
  the censoring time, and the second column is DL =1\*(TL\<C), the
  second to last column is the covariate vector (can be NULL) and the
  last column is the estimated probability P(TL\<t0+tau \| TL\>t0).

- short:

  logical value indicating whether data includes short term event
  information. Should be TRUE if short term XS and DS are includes as
  third and fourth columns of data matrix, FALSE if not. Default is
  TRUE.

- weight:

  an optional weight to be incorporated in all estimation.

## Value

- Brier.score:

  Estimated Brier score

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

BS.landmark(t0=t0,tau=tau, data = out$data)
#> Error: object 'out' not found
```
