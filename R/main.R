# Main landpred function
# Take a formula parse then return landpred model object we can call predict, etc... on
# CHANGE ALL T_S TO X_L LATEr
landpred <- function(formula, data, discrete=FALSE) {

  tf <- terms(formula, specials=c("Surv"))
  surv_terms <- attr(tf, "specials")$Surv

  # Get survival terms of right by not including the one for response
  rhs_survival_terms <- surv_terms[surv_terms > attr(tf, "response")]

  # Throw error if more than one short covariate, and get the column of it
  short_cov <- NULL
  if(length(rhs_survival_terms) > 1) {
    stop("Only a singular short-term covariate can be included.")
  } else if(length(rhs_survival_terms) == 1) {
    short_cov = attr(tf, "variables")[[rhs_survival_terms[[1]] + 1]]
    short_cov <- deparse(short_cov)
  }

  mf <- model.frame(formula, data)

  t_l <- model.response(mf)
  response_expr <- attr(tf, "variables")[[attr(tf, "response") + 1]]
  t_l_name <- deparse(response_expr[[2]])
  t_d_name <- deparse(response_expr[[3]])

  t_s <- if (!is.null(short_cov)) mf[[short_cov]] else NULL
  short_expr <- attr(tf, "variables")[[rhs_survival_terms[[1]] + 1]]
  t_s_name <- deparse(short_expr[[2]])

  if(!inherits(t_l, "Surv") || (!is.null(t_s) && !inherits(t_s, "Surv"))) {
    stop("Response variable and short-term covariate must a survival object.")
  }

  covariates <- attr(tf, "term.labels")
  covariates <- covariates[!(covariates %in% c(short_cov))]

  Z <- mf[, covariates, drop=FALSE]

  if(discrete == TRUE && ncol(Z) > 1) {
    stop("Only a singular covariate is allowed if discrete=TRUE")
  }

  names <- list(
    x_l_name = t_l_name,
    x_d_name = t_d_name,
    x_s_name = t_s_name,
    covariates=covariates
  )

  new_landpred_model(
    t_l,
    t_s,
    Z,
    names=names,
    formula = formula,
    discrete = discrete
  )
}

# Create new landpred model
new_landpred_model <- function(t_l, t_s, Z, formula, names, discrete=FALSE) {
  structure(
    list(
      t_l = t_l, t_s = t_s, Z = Z, formula = formula, discrete = discrete,
      names=names
    ),
    class="landpred_model"
  )
}

print.landpred_model <- function(x, ...) {
  cat("\nCall:\n")
  cat("landpred(formula = ", deparse(x$formula), ")\n", sep="")
}

predict.landpred_model <- function(object, t0, tau, ...) {
  if(object$discrete == TRUE) {
      return(handle_discrete_pred(object, t0, tau))
  }
  stop("CONTINUOUS NOT IMPLEMENTED YET!!!!")
}

handle_discrete_pred <- function(model, t0, tau) {
  # format data to pass into old landpred functions
  t_l <- model$t_l
  t_s <- model$t_s
  Z <- model$Z

  formatted_data <- data.frame(XL = as.numeric(t_l[, "time"]), DL =  as.numeric(t_l[, "status"]) )

  if(!is.null(Z)) {
    Z_df <- data.frame(Z=Z)
  }

  if(!is.null(t_s)) {
    ts_df <- data.frame(
      XS = as.numeric(t_s[, "time"]),
      DL = as.numeric(t_s[, "status"])
    )
  }

  if(is.null(t_s) && is.null(Z)) {
    return(Prob.Null(t0, tau, formatted_data))
  } else if (is.null(t_s) && !is.null(Z)) {
    return(
      Prob.Covariate(
        t0, tau, cbind(formatted_data, Z_df), short=FALSE
      )
    )
  } else if(!is.null(t_s) && is.null(Z)) {
    return(
      Prob.Covariate.ShortEvent(
        t0, tau, cbind(formatted_data, ts_df, Z_df)
      )
    )
  }

  stop("We are not supposed to be here.")
}
