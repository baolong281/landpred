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

test_that("creating landpred objects works as expected", {

  obj_1 <- landpred(Surv(a, a_d) ~ c, data=data)
  expect_s3_class(obj_1, "landpred_model")

  obj_2 <- landpred(Surv(a, a_d) ~ c + Surv(b, b_d), data=data)
  expect_s3_class(obj_2, "landpred_model")

  obj_3 <- landpred(Surv(a, a_d) ~  Surv(b, b_d), data=data)
  expect_s3_class(obj_3, "landpred_model")

  obj_discrete <- landpred(Surv(a, a_d) ~  Surv(b, b_d) + discrete, discrete=TRUE, data=data)
  expect_s3_class(obj_3, "landpred_model")

  # Response needs to be survival object
  expect_error(landpred(a ~ b, data=data))

  # Only singular short-term covariate allowed
  expect_error(landpred(Surv(a, a_d) ~ Surv(b, b_d) + Surv(c, c_d), data=data))

  # Only singular covariate allowed when discrete = TRUE
  expect_error(landpred(Surv(a, a_d) ~ discrete + discrete2, discrete=TRUE, data=data))
})
