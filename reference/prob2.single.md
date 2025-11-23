# Estimates P(TL \<t0+tau \| TL \> t0, Z, TS==ts) for a single t.

Helper function for Prob2.k.t; should not be called directly.

## Usage

``` r
prob2.single(K, W2i, Xi.long, tau, Di.short, Xi.short, Zi, t0, covariate.value)
```

## Arguments

- K:

  Kernel matrix.

- W2i:

  inverse probability of censoring weights.

- Xi.long:

  XL = min(TL, C) where TL is the time of the long term event, C is the
  censoring time.

- tau:

  the residual survival time for which probabilities are calculated.
  Specifically, this function estimates the probability that the an
  individual has the event of interest before t0 + tau given the event
  has not yet occurred and the individual is still at risk at time t0.

- Di.short:

  DS =1\*(TS\<C), where TS is the time of the short term event, C is the
  censoring time.

- Xi.short:

  log(XS) = log(min(TS, C)) where TS is the time of the short term
  event, C is the censoring time.

- Zi:

  covariate vector.

- t0:

  landmark time.

- covariate.value:

  specific covariate at which to estimate the conditional probability.

## Value

returns estimated probability for values corresponding to the kernel
matrix at the specified covariate value;

## Author

Layla Parast
