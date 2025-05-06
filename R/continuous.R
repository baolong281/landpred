kaplan_maier <- function(times, X, deltas, type="fl", weights=NULL) {

  # Default weights if not passed
  if(!is.null(weights)) {
    fit <- survfit(Surv(X, deltas) ~ 1, se.fit = F, type = type, weights=weights)
  } else {
    fit <- survfit(Surv(X, deltas) ~ 1, se.fit = F, type = type)
  }

  survival_probs <- summary(fit, times=times)$surv
  survival_probs
}

# weight function
w_i <- function(x_l, x_d, t0, tau, weights=NULL) {
  output <- rep(0, length(x_l))

  term_1_indexes <-
    (x_l > t0) &
    (x_l <= t0 + tau)

  term_2_indexes <-
    (x_l > t0 + tau)

  kaplan_probs <- kaplan_maier(
    c(x_l[term_1_indexes], t0 + tau),
    x_l, x_d, weights=weights
  )

  output[term_1_indexes] <- (1 * x_d[term_1_indexes]) /
    kaplan_probs[-length(kaplan_probs)]
  output[term_2_indexes] <- 1 / kaplan_probs[length(kaplan_probs)]
  output
}

subset_and_format_df <- function(landpred_obj, indexes) {
  X_L <- landpred_obj$X_L[, "time"]
  X_S <- landpred_obj$X_S[, "time"]
  X_D <- landpred_obj$X_L[, "status"]
  Z <- landpred_obj$Z

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

# Subset of the data used to fit model with no info on short covariate
get_subset_indexes_noinfo <- function(landpred_obj, t0) {
  if (!is.null(landpred_obj$X_S)) {
    subset_indexes <- pmin(landpred_obj$X_L[, "time"], landpred_obj$X_S[, "time"]) > t0
  } else {
    subset_indexes <- landpred_obj$X_L[, "time"] > t0
  }
  subset_indexes
}

# Subset of the data used to fit model with information on short covariate
# maybe have to check if it was also censored?
get_subset_indexes_short <- function(landpred_obj, t0) {
  subset_indexes <- landpred_obj$X_S[, "time"] < t0 & landpred_obj$X_L[, "time"] > t0
  subset_indexes
}

# Linear model for t0 + tau where our given t_s < t0
# no information on short covariate
fit_glm_normal <-
  function(landpred_obj, t0, tau) {
    ts_formula <-
      paste("X_L < t0 + tau ~", paste(names(landpred_obj$Z), collapse = "+"))

    ts_gt_subset <- get_subset_indexes_noinfo(landpred_obj, t0)
    subset_gt_t0 <- subset_and_format_df(landpred_obj, ts_gt_subset)

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
  function(landpred_obj, t0, tau, t_s, bw, transform) {
    ts_formula <-
      paste("X_L < t0 + tau ~", paste(names(landpred_obj$Z), collapse = "+"))

    ts_lt_subset <- get_subset_indexes_short(landpred_obj, t0)
    subset_lt_t0 <- subset_and_format_df(landpred_obj, ts_lt_subset)

    model <- glm(
      as.formula(ts_formula),
      data = subset_lt_t0,
      family = "quasibinomial",
      weights = kernel_func(transform(subset_lt_t0$X_S) - transform(t_s), bw =
                              bw) * w_i(subset_lt_t0$X_L, subset_lt_t0$X_D, t0, tau)
    )

    model
  }


handle_continuous_pred <- function(model, newdata) {
  landpred_obj <- model$landpred_obj
  t0 <- model$t0
  tau <- model$tau
  bw <- model$bw
  glm_noinfo <- model$glm_noinfo
  transform <- model$transform

  formatted_data <- newdata[, landpred_obj$names[["covariates"]], drop=FALSE]


  X_S <- newdata[, landpred_obj$name[["x_s_name"]], drop=TRUE]
  t_s_values <- unique(X_S)

  response <- numeric(0)

  for(s in t_s_values) {
    data_subset <- formatted_data[X_S==s, , drop=FALSE]

    model_specified <- NULL
    if(s < t0)  {
      model_specified <- glm_noinfo
    } else {
      glm_shortinfo <- fit_short_glm(
        landpred_obj, t0, tau, s, bw, transform
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

# confidence interval for short info model
estimate_coefficient_variance <-
  function(model,
           specified_glm,
           data_indexes,
           samples,
           kernel_weight = NULL) {

  landpred_obj <- model$landpred_obj

  baseline_coef <- coef(specified_glm)

  # number of observations
  N <- nobs(specified_glm)

  # bootstrap weights
  V = matrix(rexp(N * samples), nrow=N)

  data <- subset_and_format_df(landpred_obj, data_indexes)
  Z <- cbind(1, data[, colnames(landpred_obj$Z), drop=FALSE])

  # Set default kernel weight if not specified, i.e. we are estimating
  # coefficients for the model with no short info
  if(is.null(kernel_weight)) {
    kernel_weight <- rep(1, N)
  }

  diff_term <- 1 * (data$X_L  <= model$t0 + model$tau) - predict(
    specified_glm, newdata=data, type="response"
  )

  A <- vcov(specified_glm)

  pertubations_S <- apply(V, MARGIN = 2, function(V_vec) {
    W_L <- w_i(data$X_L, data$X_D, model$t0, model$tau, weights=V_vec)

    final_weighting <- W_L * kernel_weight * diff_term * V_vec
    weighted_vectors <- Z * final_weighting
    colSums(weighted_vectors)
  })

  pertubations_S <- cbind(1, pertubations_S)

  step_terms <- apply(pertubations_S, MARGIN=2, function(col) {
    A %*% col
  })

  boot_vectors <- apply(step_terms, MARGIN = 2, function(vec) {
    t(vec) + baseline_coef
  })

  t(boot_vectors)
}

coefficient_confint <-
  function(model,
           t_s,
           samples = 500
           ) {
    boot_vectors <- NULL
    if (t_s > model$t0) {
      subset_indexes <-
        get_subset_indexes_noinfo(model$landpred_obj, model$t0)
      boot_vectors <-
        estimate_coefficient_variance(model, model$glm_noinfo, subset_indexes,
                                      samples)
    } else {
      transform <- model$transform
      subset_indexes <-
        get_subset_indexes_short(model$landpred_obj, model$t0)
      glm_short <-
        fit_short_glm(model$landpred_obj, model$t0, model$tau, t_s, model$bw)
      kernel_weight <-
        kernel_func(transform(model$landpred_obj[["X_S"]][subset_indexes, "time"]) - transform(t_s), bw  = model$bw)
      boot_vectors <-
        estimate_coefficient_variance(model, glm_short, subset_indexes,
                                      samples, kernel_weight = kernel_weight)
    }
    boot_vectors
  }

get_model <- function(landpred_obj, t0, tau, bw, transform=identity) {
  glm_noinfo <- fit_glm_normal(landpred_obj, t0, tau)
  new_landpred_model_continuous(
    landpred_obj, glm_noinfo, t0, tau, bw,
    transform
  )
}

new_landpred_model_continuous <- function(landpred_obj, glm_noinfo, t0, tau, bw, transform) {
  structure(
    list(
      landpred_obj=landpred_obj,
      glm_noinfo=glm_noinfo,
      t0=t0,
      tau=tau,
      bw=bw,
      transform=transform
    ),
    class = "landpred_model_continuous"
  )
}

predict.landpred_model_continuous <- function(object, newdata=NULL, type="response") {
  handle_continuous_pred(object, newdata)
}

coef.landpred_model_continuous <- function(object, t_s=NULL, ...) {
  if(is.null(t_s) || t_s > model$t0) {
    return(coef(object$glm_noinfo))
  } else {
    glm_shortinfo <- fit_short_glm(
      object$landpred_obj, object$t0, object$tau, t_s, object$bw, object$transform
    )
    return(coef(glm_shortinfo))
  }
}




















