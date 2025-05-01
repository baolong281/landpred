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

handle_continuous_pred <- function(model, time, tau, t_s = NULL, bw = NULL, newdata) {
  X_L <- model$t_l[, "time"]
  X_S <- model$t_s[, "time"]
  X_D <- model$t_l[, "status"]
  Z <- model$Z

  if (!is.null(X_S)) {
    ts_gt_subset <- pmin(X_L, X_S) > time
  } else {
    ts_gt_subset <- X_L > time
  }

  subset_gt_t0 <- subsetted_df(ts_gt_subset, X_L, X_D, X_S, Z)

  ts_formula <-
    paste("X_L < time + tau ~", paste(names(Z), collapse = "+"))
  model_gt <- glm(
    as.formula(ts_formula),
    data = subset_gt_t0,
    family = "binomial",
    weights = w_i(subset_gt_t0$X_L, subset_gt_t0$X_D, time, tau)
  )


  model_lt <- NULL
  if (!is.null(X_S)) {
    ts_lt_subset <- pmin(X_L, X_S) < time

    subset_lt_t0 <- subsetted_df(ts_lt_subset, X_L, X_D, X_S, Z)

    model_lt <- glm(
      as.formula(ts_formula),
      data = subset_lt_t0,
      family = "binomial",
      weights = kernel_func(subset_lt_t0$X_S - t_s, bw=bw) * w_i(subset_lt_t0$X_L, subset_lt_t0$X_D, time, tau)
    )

  }

  return(list(gt_model = model_gt, lt_model = model_lt))

}
