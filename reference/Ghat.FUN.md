# Calculates the Kaplan Meier survival probability for censoring

Calculates the survival probability for censoring i.e. P(C \> tt) where
C is censoring; used in inverse probability of censoring weights (IPCW).
This function is called by Wi.FUN; this function should not be called on
its own.

## Usage

``` r
Ghat.FUN(tt, data, type = "fl", weight.given)
```

## Arguments

- tt:

  the time (or vector of times) at which the survival probability should
  be estimated.

- data:

  n by k matrix, where k\>=2. A data matrix where the first column is XL
  = min(TL, C) where TL is the time of the long term event, C is the
  censoring time, and the second column is DL =1\*(TL\<C)

- type:

  type sent to survfit function, default is "fl".

- weight.given:

  a weight to be used in estimation.

## Value

survival probability for censoring at time tt

## Author

Layla Parast
