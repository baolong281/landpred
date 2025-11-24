# Predict Method for Landpred Continuous Model

Predicts the probability of the event occurring given new data.

## Usage

``` r
# S3 method for class 'landpred_model_continuous'
predict(object, newdata = NULL, type = "response", ...)
```

## Arguments

- object:

  A landpred_model_continuous object.

- newdata:

  New data frame containing covariates and short-term event info.

- type:

  Type of prediction (default "response").

- ...:

  Additional arguments

## Value

A vector of predicted probabilities.
