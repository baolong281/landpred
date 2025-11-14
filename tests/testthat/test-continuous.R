data <- data.frame(
  a = runif(1000),
  b = runif(1000),
  c = runif(1000),
  e = runif(1000),
  discrete = rep(TRUE, 1000),
  discrete2 = rep(TRUE, 1000),
  d = runif(1000) * 2,
  a_d = sample(c(0, 1), size=1000, replace=TRUE),
  b_d = sample(c(0, 1), size=1000, replace=TRUE)
)
test_that("Getting continuous predictions works correctly with short covariate", {
  landpred_obj <- landpred(Surv(a, a_d) ~ Surv(b, b_d) + c, data=data)
  model <- get_model(landpred_obj, 0.5, 0.2, bw=0.3)
  preds <- predict(model, newdata=data[1:200, , drop=FALSE])
  expect_false(any(is.na(preds)))
})

test_that("Getting standard errors of coefficients works", {
  landpred_obj <- landpred(Surv(a, a_d) ~ Surv(b, b_d) + c + e, data=data)
  model <- get_model(landpred_obj, 0.5, 0.2, bw=0.3)
  expect_false(any(is.na(coefficient_se(model))))
})

test_that("Model printing out works", {
  landpred_obj <- landpred(Surv(a, a_d) ~ Surv(b, b_d) + c + e, data=data)
  model <- get_model(landpred_obj, t0=0.5, tau=0.2, bw=0.3, transform=log)
  expect_output(print(model))

  # Summary works with each choice of t_s
  expect_output(summary(model))
  expect_output(summary(model, t_s=0.20))
  expect_output(summary(model, t_s=0.6))
})

