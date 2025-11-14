data <- data.frame(
  a = runif(1000),
  b = runif(1000),
  c = runif(1000),
  discrete = sample(c(TRUE, FALSE), size = 1000, replace = TRUE),
  discrete2 = sample(c(TRUE, FALSE), size = 1000, replace = TRUE),
  d = runif(1000) * 2,
  a_d = rep(1, 1000),
  b_d = rep(0, 1000)
)

test_that("creating landpred objects works as expected", {

  obj_1 <- landpred(Surv(a, a_d) ~ c, data=data, discrete=TRUE)
  expect_s3_class(obj_1, "landpred_object")

  expect_error(landpred(Surv(a, a_d) ~ c, data=data, discrete=FALSE))

  obj_2 <- landpred(Surv(a, a_d) ~ c + Surv(b, b_d), data=data)
  expect_s3_class(obj_2, "landpred_object")

  obj_discrete <- landpred(Surv(a, a_d) ~  Surv(b, b_d) + discrete, discrete=TRUE, data=data)
  expect_s3_class(obj_discrete, "landpred_object")

  obj_discrete_2 <- landpred(Surv(a, a_d) ~ discrete, discrete=TRUE, data=data)
  expect_s3_class(obj_discrete_2, "landpred_object")

  # Response needs to be survival object
  expect_error(landpred(a ~ b, data=data))

  # Only singular short-term covariate allowed
  expect_error(landpred(Surv(a, a_d) ~ Surv(b, b_d) + Surv(c, c_d), data=data))

  # Only singular covariate allowed when discrete = TRUE
  expect_error(landpred(Surv(a, a_d) ~ discrete + discrete2, discrete=TRUE, data=data))

  # Normal covariate must be supplied with short-term one
  expect_error(landpred(Surv(a, a_d) ~ Surv(b, b_d), data=data, discrete=TRUE))
})

test_that("getting predictions works with discrete model", {
  # Predictions with just discrete covariate
  obj_1 <- landpred(Surv(a, a_d) ~ discrete, data=data, discrete=TRUE)
  model_1 <- get_model(obj_1, 0.50, 0.20)
  expect_true(is.numeric(predict(model_1, newdata=data[1:50, , drop=FALSE])))

  # Predictions just short covariate + normal
  obj_3 <- landpred(Surv(a, a_d) ~ Surv(b, b_d) + discrete, data=data, discrete=TRUE)
  model_3 <- get_model(obj_3, 0.50, 0.20)
  expect_true(is.numeric(predict(model_3, newdata=data[1:50, , drop=FALSE])))
})
