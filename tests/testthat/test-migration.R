library(MASS)
library(splines)
library(sm)
library(quantreg)
library(survival)

# Global test values
t0 <- 1.0
tau <- 1.5
t_s <- 0.7
bw <- 0.5
set.seed(42)

# ---------- Old functions to test ------------------
loc.fun.ex.OLD <- function(t, data,tau, s, h, weight = NULL) {
  X1i = data[,1]; X2i = data[,2]; D1i = data[,3]; D2i = data[,4]; Zi = data[,-c(1:4)]
  if(is.null(weight))	{W2i <- Wi.FUN(X2i,data = cbind(X2i,D2i,Zi),t0=tau,tau=s)}
  else{W2i=weight}
  index.sub = data[,2] > tau & (data[,1] < log (tau)) & (data[,3] == 1)
  K = Kern.FUN(X1i[index.sub],t,h)
  est.mat = matrix(nrow=length(t), ncol = dim(as.matrix(Zi))[2] + 1)
  invinf.mat = matrix(nrow=length(t), ncol = (dim(as.matrix(Zi))[2] + 1)^2)
  tmpfm <- paste("1*(X2i< tau+s) ~ ", paste(names(Zi),collapse="+"))
  for(i in 1:length(t)) {
    m = glm(as.formula(tmpfm), data=data[index.sub,],  family = "binomial", weights = K[i,]*W2i[index.sub])
    est.mat[i,] = m$coeff
    invinf.mat[i,] = as.vector(vcov(m))
  }
  return(list("est.mat" = est.mat, "invinf.mat" = invinf.mat))
}

var.fun.ex9b <- function(t, data.v,tau, s, h, vmat, Ainv, weight=NULL) {
  length.t = length(t)
  X1i = data.v[,1]; X2i = data.v[,2]; D1i = data.v[,3]; D2i = data.v[,4]; Zi = data.v[,-c(1:4)]
  index.sub = which(data.v[,2] > tau & (data.v[,1] < log (tau)) & (data.v[,3] == 1)==TRUE)
  if(is.null(weight))	{W2i <- Wi.FUN.CONT(X2i,data = cbind	(X2i,D2i,Zi),t0=tau,tau=s)}
  else{W2i=weight}
  loc.m = loc.fun.ex.OLD(t=t, data=data.v, tau=tau, s=s, h=h)
  size <- nrow(data.v)
  vmat.c = vmat - matrix(rep(1, 500*size), nrow = 500, ncol = size)
  diff = 1*(X2i > tau & X2i < tau+s & D2i == 1)[index.sub] - g.logit(apply(loc.m$est.mat* cbind(rep(1,length(X1i[index.sub])), Zi[index.sub,]),1,sum))
  index.sub = index.sub[which(is.na(diff) == FALSE)]
  diff = diff[which(is.na(diff) == FALSE)]
  K = Kern.FUN.CONT(X1i[index.sub],t,h)
  piece.1.int = t(t(K)* (W2i[index.sub] * rep(1, length(X1i[index.sub]))* diff)) %*% t(vmat.c[,index.sub])
  W2i.star <- apply(vmat, 1, Wi.FUN.CONT, tt = X2i,data = cbind(X2i,D2i,Zi),t0=tau, tau=s)
  piece.2.int = t(t(K)* (rep(1, length(X1i[index.sub]))* diff)) %*% (W2i.star[index.sub,] - matrix(W2i[index.sub], ncol=500, nrow=length(X1i[index.sub]), byrow=F))
  piece.3.int = piece.1.int + piece.2.int
  int.star = piece.3.int *matrix(Ainv[,1], nrow = length.t, ncol = 500, byrow = F)

  len.z = dim(Zi)[2]
  slope.mat = matrix(nrow = length.t*len.z, ncol = 500)
  for(j in 1:len.z) {
    piece.1.slope = t(t(K)* (W2i[index.sub] * Zi[index.sub,j]* diff)) %*% t(vmat.c[,index.sub])
    piece.2.slope = t(t(K)* (Zi[index.sub,j]* diff)) %*% (W2i.star[index.sub,] - matrix(W2i[index.sub], ncol=500, nrow=length(X1i[index.sub]), byrow=F))
    piece.3.slope = piece.1.slope + piece.2.slope
    slope.mat[c((1+length.t*(j-1)):(length.t*(j))),] = piece.3.slope
    #int.star = int.star + piece.3.slope *matrix(Ainv[,j+1], nrow = length.t, ncol = 500, byrow = F)
  }
  o.matrix = matrix(nrow = length.t,ncol=len.z+1)
  o.entire1 = matrix(nrow = length.t, ncol = 500)
  o.entire2 = matrix(nrow = length.t, ncol = 500)
  o.entire3 = matrix(nrow = length.t, ncol = 500)
  o.entire4 = matrix(nrow = length.t, ncol = 500)
  o.entire5 = matrix(nrow = length.t, ncol = 500)
  o.entire6 = matrix(nrow = length.t, ncol = 500)
  o.entire7 = matrix(nrow = length.t, ncol = 500)
  o.entire8 = matrix(nrow = length.t, ncol = 500)
  o.entire9 = matrix(nrow = length.t, ncol = 500)
  o.entire10 = matrix(nrow = length.t, ncol = 500)

  for(l in 1:length.t) {
    a = matrix(Ainv[l,],len.z+1,len.z+1)
    #coded for len.z=8 because I'm afraid I will mess it up otherwise
    o1 = piece.3.int[l,]*a[1,1] + slope.mat[l,]*a[1,2] + slope.mat[l+length.t,]*a[1,3] +slope.mat[l+length.t*2,]*a[1,4] + slope.mat[l+(length.t*3),]*a[1,5] + slope.mat[l+(length.t*4),]*a[1,6] + slope.mat[l+(length.t*5),]*a[1,7] + slope.mat[l+(length.t*6),]*a[1,8] + slope.mat[l+(length.t*7),]*a[1,9] + slope.mat[l+(length.t*8),]*a[1,10]

    o2 = piece.3.int[l,]*a[2,1] + slope.mat[l,]*a[2,2] + slope.mat[l+length.t,]*a[2,3] +slope.mat[l+length.t*2,]*a[2,4] + slope.mat[l+(length.t*3),]*a[2,5] + slope.mat[l+(length.t*4),]*a[2,6] + slope.mat[l+(length.t*5),]*a[2,7] +   slope.mat[l+(length.t*6),]*a[2,8] + slope.mat[l+(length.t*7),]*a[2,9] + slope.mat[l+(length.t*8),]*a[2,10]

    o3 = piece.3.int[l,]*a[3,1] + slope.mat[l,]*a[3,2] + slope.mat[l+length.t,]*a[3,3] +slope.mat[l+length.t*2,]*a[3,4] +  slope.mat[l+(length.t*3),]*a[3,5] + slope.mat[l+(length.t*4),]*a[3,6] + slope.mat[l+(length.t*5),]*a[3,7] +   slope.mat[l+(length.t*6),]*a[3,8] + slope.mat[l+(length.t*7),]*a[3,9]  + slope.mat[l+(length.t*8),]*a[3,10]

    o4 = piece.3.int[l,]*a[4,1] + slope.mat[l,]*a[4,2] + slope.mat[l+length.t,]*a[4,3] +slope.mat[l+length.t*2,]*a[4,4] +  slope.mat[l+(length.t*3),]*a[4,5] + slope.mat[l+(length.t*4),]*a[4,6] + slope.mat[l+(length.t*5),]*a[4,7] +  slope.mat[l+(length.t*6),]*a[4,8] + slope.mat[l+(length.t*7),]*a[4,9] + slope.mat[l+(length.t*8),]*a[4,10]

    o5 = piece.3.int[l,]*a[5,1] + slope.mat[l,]*a[5,2] + slope.mat[l+length.t,]*a[5,3] +slope.mat[l+length.t*2,]*a[5,4] +  slope.mat[l+(length.t*3),]*a[5,5] + slope.mat[l+(length.t*4),]*a[5,6] + slope.mat[l+(length.t*5),]*a[5,7]   + slope.mat[l+(length.t*6),]*a[5,8] + slope.mat[l+(length.t*7),]*a[5,9] + slope.mat[l+(length.t*8),]*a[5,10]

    o6 = piece.3.int[l,]*a[6,1] + slope.mat[l,]*a[6,2] + slope.mat[l+length.t,]*a[6,3] +slope.mat[l+length.t*2,]*a[6,4] +  slope.mat[l+(length.t*3),]*a[6,5] + slope.mat[l+(length.t*4),]*a[6,6] + slope.mat[l+(length.t*5),]*a[6,7]   + slope.mat[l+(length.t*6),]*a[6,8] + slope.mat[l+(length.t*7),]*a[6,9] + slope.mat[l+(length.t*8),]*a[6,10]

    o7 = piece.3.int[l,]*a[7,1] + slope.mat[l,]*a[7,2] + slope.mat[l+length.t,]*a[7,3] +slope.mat[l+length.t*2,]*a[7,4] +  slope.mat[l+(length.t*3),]*a[7,5] + slope.mat[l+(length.t*4),]*a[7,6] + slope.mat[l+(length.t*5),]*a[7,7] +   slope.mat[l+(length.t*6),]*a[7,8] + slope.mat[l+(length.t*7),]*a[7,9]  + slope.mat[l+(length.t*8),]*a[7,10]

    o8 = piece.3.int[l,]*a[8,1] + slope.mat[l,]*a[8,2] + slope.mat[l+length.t,]*a[8,3] +slope.mat[l+length.t*2,]*a[8,4] +  slope.mat[l+(length.t*3),]*a[8,5] + slope.mat[l+(length.t*4),]*a[8,6] + slope.mat[l+(length.t*5),]*a[8,7] +   slope.mat[l+(length.t*6),]*a[8,8] + slope.mat[l+(length.t*7),]*a[8,9]  + slope.mat[l+(length.t*8),]*a[8,10]

    o9 = piece.3.int[l,]*a[9,1] + slope.mat[l,]*a[9,2] + slope.mat[l+length.t,]*a[9,3] +slope.mat[l+length.t*2,]*a[9,4] +  slope.mat[l+(length.t*3),]*a[9,5] + slope.mat[l+(length.t*4),]*a[9,6] + slope.mat[l+(length.t*5),]*a[9,7] +  slope.mat[l+(length.t*6),]*a[9,8] + slope.mat[l+(length.t*7),]*a[9,9] +  + slope.mat[l+(length.t*8),]*a[9,10]

    o10 = piece.3.int[l,]*a[10,1] + slope.mat[l,]*a[10,2] + slope.mat[l+length.t,]*a[10,3] +slope.mat[l+length.t*2,]*a[10,4] +  slope.mat[l+(length.t*3),]*a[10,5] + slope.mat[l+(length.t*4),]*a[10,6] + slope.mat[l+(length.t*5),]*a[10,7] +  slope.mat[l+(length.t*6),]*a[10,8] + slope.mat[l+(length.t*7),]*a[10,9] +  + slope.mat[l+(length.t*8),]*a[10,10]

    o.matrix[l,1] = sd(o1)
    o.matrix[l,2] = sd(o2)
    o.matrix[l,3] = sd(o3)
    o.matrix[l,4] = sd(o4)
    o.matrix[l,5] = sd(o5)
    o.matrix[l,6] = sd(o6)
    o.matrix[l,7] = sd(o7)
    o.matrix[l,8] = sd(o8)
    o.matrix[l,9] = sd(o9)
    o.matrix[l,10] = sd(o10)
    o.entire1[l,] = o1
    o.entire2[l,] = o2
    o.entire3[l,] = o3
    o.entire4[l,] = o4
    o.entire5[l,] = o5
    o.entire6[l,] = o6
    o.entire7[l,] = o7
    o.entire8[l,] = o8
    o.entire9[l,] = o9
    o.entire10[l,] = o10
  }
  return(list("int" = o.matrix[,1], "sl" = o.matrix[,-1], "o1" = o.entire1, "o2" = o.entire2, "o3" = o.entire3, "o4" = o.entire4, "o5" = o.entire5, "o6" = o.entire6, "o7" = o.entire7, "o8" = o.entire8, "o9" = o.entire9, "o10" = o.entire9))
}

# ------------------------------------------------

# Function to generate synthetic landmark prediction data
generate_test_data <- function(n = 500, p = 5) {
  # Generate covariates
  Z <- matrix(rnorm(n * p), nrow = n, ncol = p)
  colnames(Z) <- paste0("Z", 1:p)

  # True coefficients for survival models
  beta_long <- rnorm(p, mean = 0, sd = 0.3)
  beta_short <- rnorm(p, mean = 0, sd = 0.3)

  # Set some specific values for reproducibility
  if(p >= 1) beta_long[1] <- -0.5
  if(p >= 2) beta_long[2] <- 0.3
  if(p >= 3) beta_long[3] <- -0.2
  if(p >= 1) beta_short[1] <- 0.2
  if(p >= 2) beta_short[2] <- -0.3
  if(p >= 3) beta_short[3] <- 0.25

  # Generate event times based on exponential model
  linear_pred_long <- Z %*% beta_long
  X_L_raw <- rexp(n, rate = exp(linear_pred_long))

  linear_pred_short <- Z %*% beta_short
  X_S_raw <- rexp(n, rate = exp(linear_pred_short))

  # Generate censoring times
  C_L <- runif(n, 2, 8)  # censoring for long event
  C_S <- runif(n, 1, 4)  # censoring for short event

  # Apply censoring
  X_L <- pmin(X_L_raw, C_L)
  D_L <- as.numeric(X_L_raw <= C_L)  # 1 if event, 0 if censored

  X_S <- pmin(X_S_raw, C_S)
  D_S <- as.numeric(X_S_raw <= C_S)  # 1 if event, 0 if censored

  # For old code format (column-based)
  old_format <- data.frame(
    X1i = log(X_S),  # log-transformed short event time
    X2i = X_L,       # long event time
    D1i = D_S,       # short event indicator
    D2i = D_L,       # long event indicator
    Z                # covariates
  )

  # For new code format (structured object)
  Z_df <- as.data.frame(Z)
  colnames(Z_df) <- paste0("Z", 1:p)

  new_format <- list(
    X_L = cbind(time = X_L, status = D_L),
    X_S = cbind(time = X_S, status = D_S),
    Z = Z_df,
    names = list(
      covariates = colnames(Z_df),
      x_s_name = "time"
    )
  )

  return(list(
    old_format = old_format,
    new_format = new_format,
    true_params = list(beta_long = beta_long, beta_short = beta_short)
  ))
}


test_that("Data generation works", {
  test_data <- generate_test_data(200, p=3)
  old_data <- test_data$old_format
  new_data <- test_data$new_format

  expect_equal(old_data$X2i, new_data$X_L[, "time"])
  expect_equal(exp(old_data$X1i), new_data$X_S[, "time"])
})

test_that("GLM coefficients match between old and new approaches with short-term covariate (with kernel weights)", {
  test_data <- generate_test_data(n = 200, p = 5)
  old_data <- test_data$old_format
  new_data <- test_data$new_format

  old_data$X1i <- exp(old_data$X1i)

  old_coef = suppressWarnings({loc.fun.ex(
    t = t_s,
    data = old_data,
    tau = t0,
    s = tau,
    h = bw,
    transform=log
  )$est.mat[1, ]})

  new_glm <- fit_short_glm(new_data, t0, tau, t_s, bw=bw, transform=log)

  expect_equal(as.vector(coef(new_glm)), old_coef, tolerance = 1e-6)
})


test_that("GLM coefficients match between old and new approaches for no short-term info (no kernel weights)", {
  test_data <- generate_test_data(n = 200, p = 9)
  old_data <- test_data$old_format
  new_data <- test_data$new_format

  # Old approach
  old_weights <- Wi.FUN.CONT(old_data$X2i, data = cbind(old_data$X2i, old_data$D2i), t0 = t0, tau = tau)
  old_past <- which(old_data$X2i > t0)
  covariate_cols <- 5:ncol(old_data)
  old_formula <- paste("I(X2i < t0 + tau) ~", paste(names(old_data)[covariate_cols], collapse = " + "))
  old_glm <- glm(
    as.formula(old_formula),
    data = old_data[old_past, ],
    family = "quasibinomial",
    weights = old_weights[old_past]
  )

  # New approach
  new_glm <- fit_glm_normal(new_data, t0, tau)
  expect_equal(coef(new_glm), coef(old_glm), tolerance = 1e-6)
})

test_that("Variance estimation matches", {
  test_data <- generate_test_data(n = 200, p = 9)
  old_data <- test_data$old_format
  new_data <- test_data$new_format

  fit = suppressWarnings({loc.fun.ex(
    t = t_s,
    data = old_data,
    tau = t0,
    s = tau,
    h = bw,
    transform = log
  )})

  size = dim(old_data)[1]
  vmat = matrix(rexp(500 * size, 1), nrow = 500, ncol = size)
  var_old <- suppressWarnings({var.fun.ex9b(
    t = t_s,
    data.v = old_data,
    tau = t0,
    s = tau,
    h = bw,
    vmat = vmat,
    Ainv = fit$invinf
  )})

  old_se_intercept <- var_old$int
  old_se_slopes <- var_old$sl
  old_se <- c(old_se_intercept, old_se_slopes)

  var_new <- suppressWarnings({var.fun(
    t = t_s,
    data.v = old_data,
    tau = t0,
    s = tau,
    h = bw,
    vmat = vmat,
    Ainv = fit$invinf,
  )})

  new_se_intercept <- var_new$int
  new_se_slopes <- var_new$sl
  new_se <- c(new_se_intercept, new_se_slopes)

  expect_equal(new_se, old_se, tolerance = 1e-6)
})

test_that("Variance estimation using landpred object is relatively the same", {
  test_data <- generate_test_data(n = 200, p = 9)
  old_data <- test_data$old_format
  new_data <- test_data$new_format

  old_data$X1i <- exp(old_data$X1i)

  fit = suppressWarnings({loc.fun.ex(
    t = t_s,
    data = old_data,
    tau = t0,
    s = tau,
    h = bw,
    transform = log
  )})

  old_data$X1i <- log(old_data$X1i)

  size = dim(old_data)[1]
  vmat = matrix(rexp(500 * size, 1), nrow = 500, ncol = size)
  var_old <- suppressWarnings({var.fun.ex9b(
    t = t_s,
    data.v = old_data,
    tau = t0,
    s = tau,
    h = bw,
    vmat = vmat,
    Ainv = fit$invinf
  )})

  old_se_intercept <- var_old$int
  old_se_slopes <- var_old$sl
  old_se <- c(old_se_intercept, old_se_slopes)

  old_data$X1i <- exp(old_data$X1i)

  obj <-
    landpred(Surv(X2i, D2i) ~ Surv(X1i, D1i) + Z1 + Z2 + Z3 + Z4 + Z5 + Z6 + Z7 + Z8 + Z9, data = old_data)

  model <- get_model(obj, t0, tau, bw, transform=log)
  new_se <- coefficient_se(model, t_s=t_s, samples=500)

  print(old_se)
  print(new_se)

  rel_diff <- abs(new_se - old_se) / pmax(abs(old_se), 1e-8)
  print(rel_diff)
  expect_true(all(rel_diff <= 0.15),
              info = paste0("Max relative diff: ", max(rel_diff)))

})

