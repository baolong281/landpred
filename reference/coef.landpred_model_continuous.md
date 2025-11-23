# Extract Coefficients from Landpred Continuous Model

Extracts coefficients. If `t_s` is provided, it fits the short-term GLM
and returns its coefficients.

## Usage

``` r
# S3 method for class 'landpred_model_continuous'
coef(object, t_s = NULL, ...)
```

## Arguments

- object:

  A landpred_model_continuous object.

- t_s:

  Optional short-term covariate time.

- ...:

  Additional arguments.

## Value

A named vector of coefficients.
