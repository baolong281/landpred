# Computes the inverse probability of censoring weights for a specific t0 and tau

Computes the inverse probability of censoring weights for a specific t0
and tau i.e. this computes I(t0 \< XL \< t0+tau)\*DL/G(XL) +
I(XL\>t0+tau)/G(t0+tau) where XL = min(TL, C), TL is the time of the
long term event, C is the censoring time, DL =1\*(TL\<C) and G() is the
estimate survival probability for censoring estimated using the Kaplan
Meier estimator (see Ghat.FUN)

## Usage

``` r
Wi.FUN(data, t0, tau, weight.given = NULL)
```

## Arguments

- data:

  n by k matrix, where k\>= 2. A data matrix where the first column is
  XL = min(TL, C) where TL is the time of the long term event, C is the
  censoring time, and the second column is DL =1\*(TL\<C)

- t0:

  the landmark time..

- tau:

  the residual survival time for which probabilities are calculated.

- weight.given:

  an optional weight to be incorporated in estimation of this weight

## Value

Inverse probability of censoring weight.

## Author

Layla Parast

## Examples

``` r
data(data_example_landpred)
t0=2
tau = 8

W2i <- Wi.FUN(data_example_landpred[,1],data = data_example_landpred[,c(1:2)],t0=t0,tau=tau)
#> Error in Wi.FUN(data_example_landpred[, 1], data = data_example_landpred[,     c(1:2)], t0 = t0, tau = tau): could not find function "Wi.FUN"

```
