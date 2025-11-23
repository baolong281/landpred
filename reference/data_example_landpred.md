# Hypothetical data to be used in examples.

Hypothetical data to be used in examples.

## Usage

``` r
data(data_example_landpred)
```

## Format

A data frame with 4868 observations on the following 5 variables.

- `XL`:

  a numeric vector. XL = min(TL, C) where TL is the time of the long
  term event, C is the censoring time.

- `DL`:

  a 0/1 vector. DL =1\*(TL\<C) where TL is the time of the long term
  event, C is the censoring time.

- `XS`:

  a numeric vector. XS = min(TS, C) where TS is the time of the long
  term event, C is the censoring time.

- `DS`:

  a 0/1 vector. DS =1\*(TS\<C) where TS is the time of the long term
  event, C is the censoring time.

- `Z`:

  a 0/1 vector of discrete covariate values.

## Examples

``` r
data(data_example_landpred)
```
