# Calculates initial optimal bandwidth.

Calculates initial optimal bandwidth with respect to mean squared error
using K-fold cross-validation.

## Usage

``` r
optimize.mse.BW(data, t0,tau,h.grid=seq(.01,2,length=50), folds=3, reps=2)
```

## Arguments

- data:

  n by 5 matrix. A data matrix where the first column is XL = min(TL, C)
  where TL is the time of the long term event, C is the censoring time,
  and the second column is DL =1\*(TL\<C), the third column is XS =
  min(TS, C) where TS is the time of the short term event, C is the
  censoring time, the fourth column is DS =1\*(TS\<C), and the fifth
  column is the covariate. These are the data used to calculate the
  estimated probability.

- t0:

  the landmark time.

- tau:

  the residual survival time of interest.

- h.grid:

  The grid of possible bandwidths that the user would like the function
  to search through. Default is h.grid = seq(.01,2,length=50).

- folds:

  Number of folds wanted for K-fold cross-validation. Default is 3.

- reps:

  Number of repitions wanted of K-fold cross-validation. Default is 2.

## Value

- h:

  Selected bandwidth.

## References

Parast, Layla, Su-Chun Cheng, and Tianxi Cai. Incorporating short-term
outcome information to predict long-term survival with discrete markers.
Biometrical Journal 53.2 (2011): 294-307.

## Author

Layla Parast
