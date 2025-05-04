kaplan_maier <- function(times, X, deltas, type="fl") {
  fit <- survfit(Surv(X, deltas) ~ 1, se.fit = F, type = type)
  survival_probs <- summary(fit, times=times)$surv
  survival_probs
}

# weight function
w_i <- function(x_l, x_d, t0, tau) {
  output <- rep(0, length(x_l))

  term_1_indexes <-
    (x_l > t0) &
    (x_l <= t0 + tau)

  term_2_indexes <-
    (x_l > t0 + tau)

  kaplan_probs <- kaplan_maier(
    c(x_l[term_1_indexes], t0 + tau),
    x_l, x_d
  )

  output[term_1_indexes] <- (1 * x_d[term_1_indexes]) /
    kaplan_probs[-length(kaplan_probs)]
  output[term_2_indexes] <- 1 / kaplan_probs[length(kaplan_probs)]
  output
}

subsetted_df <- function(indexes, X_L, X_D, X_S, Z) {
  subsetted_data <- cbind(data.frame(
    X_L=X_L[indexes],
    X_D=X_D[indexes],
    X_S=X_S[indexes]
  ), Z[indexes, , drop=FALSE])

  subsetted_data
}

kernel_func <- function(x, bw) {
  dnorm(x / bw) / bw
}

# Linear model for t0 + tau where our given t_s < t0
# no information on short covariate
fit_glm_normal <-
  function(model, t0, tau) {
    X_L <- model$t_l[, "time"]
    X_S <- model$t_s[, "time"]
    X_D <- model$t_l[, "status"]
    Z <- model$Z

    ts_formula <-
      paste("X_L < t0 + tau ~", paste(names(Z), collapse = "+"))

    if (!is.null(X_S)) {
      ts_gt_subset <- pmin(X_L, X_S) > t0
    } else {
      ts_gt_subset <- X_L > t0
    }

    subset_gt_t0 <- subsetted_df(ts_gt_subset, X_L, X_D, X_S, Z)


    # fit as quasibinomial to suppress warnings about non-integer successes
    model <- glm(
      as.formula(ts_formula),
      data = subset_gt_t0,
      family = "quasibinomial",
      weights = w_i(subset_gt_t0$X_L, subset_gt_t0$X_D, t0, tau)
    )

    model
  }

# fit glm with kernel weights and information on short covariate
fit_short_glm <-
  function(model, t0, tau, t_s, bw, transform = identity) {
    X_L <- model$t_l[, "time"]
    X_S <- model$t_s[, "time"]
    X_D <- model$t_l[, "status"]
    Z <- model$Z

    ts_formula <-
      paste("X_L < t0 + tau ~", paste(names(Z), collapse = "+"))

    # maybe have to check if it was also censored?
    ts_lt_subset <- X_S < t0 & X_L > t0

    subset_lt_t0 <- subsetted_df(ts_lt_subset, X_L, X_D, X_S, Z)

    model <- glm(
      as.formula(ts_formula),
      data = subset_lt_t0,
      family = "quasibinomial",
      weights = kernel_func(transform(subset_lt_t0$X_S) - transform(t_s), bw =
                              bw) * w_i(subset_lt_t0$X_L, subset_lt_t0$X_D, t0, tau)
    )

    model
  }


handle_continuous_pred <- function(landpred_model, newdata, transform=identity) {
  model <- landpred_model$model
  t0 <- landpred_model$t0
  tau <- landpred_model$tau
  bw <- landpred_model$bw
  glm_noinfo <- landpred_model$glm_noinfo

  formatted_data <- newdata[, model$names[["covariates"]], drop=FALSE]


  X_S <- newdata[, model$name[["x_s_name"]], drop=TRUE]
  t_s_values <- unique(X_S)

  response <- numeric(0)

  for(s in t_s_values) {
    data_subset <- formatted_data[X_S==s, , drop=FALSE]

    model_specified <- NULL
    if(s < t0)  {
      model_specified <- glm_noinfo
    } else {
      glm_shortinfo <- fit_short_glm(
        model, t0, tau, s, bw, transform
      )
      model_specified <- glm_shortinfo
    }

    preds <- predict(
      model_specified, newdata=data_subset,
      type="response"
    )

    response[X_S == s] <- preds
  }

 response
}

get_model <- function(model, t0, tau, bw) {
  glm_noinfo <- fit_glm_normal(model, t0, tau)
  new_landpred_model_continuous(
    model, glm_noinfo, t0, tau, bw
  )
}

new_landpred_model_continuous <- function(model, glm_noinfo, t0, tau, bw) {
  structure(
    list(
      model=model,
      glm_noinfo=glm_noinfo,
      t0=t0,
      tau=tau,
      bw=bw
    ),
    class = "landpred_model_continuous"
  )
}

predict.landpred_model_continuous <- function(object, newdata=NULL, type="response", transform=identity) {
  handle_continuous_pred(object, newdata, transform)
}

coef.landpred_model_continuous <- function(object, t_s=NULL, transform=identity, ...) {
  if(is.null(t_s) || t_s > model$t0) {
    return(coef(object$glm_noinfo))
  } else {
    glm_shortinfo <- fit_short_glm(
      object$model, object$t0, object$tau, t_s, object$bw, transform
    )
    return(coef(glm_shortinfo))
  }
}




















