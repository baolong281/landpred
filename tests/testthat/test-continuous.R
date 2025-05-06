data <- data.frame(
  a = runif(1000),
  b = runif(1000),
  c = runif(1000),
  discrete = rep(TRUE, 1000),
  discrete2 = rep(TRUE, 1000),
  d = runif(1000) * 2,
  a_d = rep(1, 1000),
  b_d = rep(0, 1000)
)
test_that("Getting continuous predictions works correctly with short covariate", {
  landpred_obj <- landpred(Surv(a, a_d) ~ Surv(b, b_d) + c, data=data)
  model <- get_model(landpred_obj, 0.5, 0.2, bw=0.3)
  preds <- predict(model, newdata=data[1:200, , drop=FALSE])
  expect_false(any(is.na(preds)))
})
