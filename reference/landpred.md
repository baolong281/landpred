# Create a Landpred Object

Parses the formula and data to create a landpred object used for
landmark prediction.

## Usage

``` r
landpred(formula, data, discrete = FALSE)
```

## Arguments

- formula:

  A formula object with a Surv object on the LHS and covariates on the
  RHS.

- data:

  The data frame.

- discrete:

  Logical, whether to use the discrete method (legacy).

## Value

A landpred_object.
