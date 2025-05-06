new_landpred_result <- function(
    Prob=numeric(), data=numeric(), newdata=NA,
    t0=numeric(), tau=numeric()
    ) {

  stopifnot(is.numeric(t0))
  stopifnot(is.numeric(tau))

  structure(
    list(
      Prob=Prob,
      data=data,
      newdata=newdata,
      t0=t0,
      tau=tau
    ),
    class = "landpred_result"
  )
}

print.landpred_result <- function(x, ...) {
  cat("\nLandpred Results:\n\n")
  if(is.matrix(x$Prob)) {
    apply(x$Prob, 1, function(row) {
      cat(sprintf("P(TL < t0+tau|Z=%d): %.3f\n", row[1], row[2]))
    })
  } else {
    cat(sprintf("P(TL < t0+tau): %.3f", x$Prob))
  }

  cat("\n\n")
  cat(sprintf("t0: %-10.3f tau: %-10.3f", x$t0, x$tau))
}
