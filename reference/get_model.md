# Get Landpred Model

Fits the base GLM (no short covariate info) and creates a landpred model
object.

Creates a landpred model object for a specific landmark time and
prediction window. Dispatches to continuous or discrete model creation
based on the landpred object type.

## Usage

``` r
get_model(landpred_obj, t0, tau, bw = NULL, transform = identity)

get_model(landpred_obj, t0, tau, bw = NULL, transform = identity)
```

## Arguments

- landpred_obj:

  A landpred object.

- t0:

  The landmark time.

- tau:

  The prediction window.

- bw:

  The bandwidth (for continuous models).

- transform:

  Transformation function (for continuous models).

## Value

A landpred_model_continuous object.

A landpred_model object (continuous or discrete).
