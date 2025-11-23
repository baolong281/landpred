# Package index

## All functions

- [`AUC.landmark()`](https://baolong281.github.io/landpred/reference/AUC.landmark.md)
  : Estimates the area under the ROC curve (AUC).
- [`BS.landmark()`](https://baolong281.github.io/landpred/reference/BS.landmark.md)
  : Estimates the Brier score.
- [`Ghat.FUN()`](https://baolong281.github.io/landpred/reference/Ghat.FUN.md)
  : Calculates the Kaplan Meier survival probability for censoring
- [`Kern.FUN()`](https://baolong281.github.io/landpred/reference/Kern.FUN.md)
  : Calculates kernel matrix
- [`Prob.Covariate()`](https://baolong281.github.io/landpred/reference/Prob.Covariate.md)
  : Estimates P(TL \<t0+tau \| TL \> t0, Z), i.e. given discrete
  covariate.
- [`Prob.Covariate.ShortEvent()`](https://baolong281.github.io/landpred/reference/Prob.Covariate.ShortEvent.md)
  : Estimates P(TL \<t0+tau \| TL \> t0, Z, min(TS, t0), I(TS\<=t0)),
  i.e. given discrete covariate and TS information.
- [`Prob.Null()`](https://baolong281.github.io/landpred/reference/Prob.Null.md)
  : Estimates P(TL \<t0+tau \| TL \> t0).
- [`Prob2()`](https://baolong281.github.io/landpred/reference/Prob2.md)
  : Estimates P(TL \<t0+tau \| TL \> t0, Z, TS\>t0).
- [`Prob2.k.t()`](https://baolong281.github.io/landpred/reference/Prob2.k.t.md)
  : Estimates P(TL \<t0+tau \| TL \> t0, Z, TS==ts).
- [`VTM()`](https://baolong281.github.io/landpred/reference/VTM.md) :
  Helper function, repeats a row.
- [`Wi.FUN()`](https://baolong281.github.io/landpred/reference/Wi.FUN.md)
  : Computes the inverse probability of censoring weights for a specific
  t0 and tau
- [`coef(`*`<landpred_model_continuous>`*`)`](https://baolong281.github.io/landpred/reference/coef.landpred_model_continuous.md)
  : Extract Coefficients from Landpred Continuous Model
- [`coefficient_se()`](https://baolong281.github.io/landpred/reference/coefficient_se.md)
  : Calculate Standard Errors for Coefficients
- [`cumsum2()`](https://baolong281.github.io/landpred/reference/cumsum2.md)
  : Helper function
- [`data_example_landpred`](https://baolong281.github.io/landpred/reference/data_example_landpred.md)
  : Hypothetical data to be used in examples.
- [`fit_glm_normal()`](https://baolong281.github.io/landpred/reference/fit_glm_normal.md)
  : Fit GLM with Normal Weights (No Short Covariate Info)
- [`fit_short_glm()`](https://baolong281.github.io/landpred/reference/fit_short_glm.md)
  : Fit GLM with Kernel Weights (Short Covariate Info)
- [`get_model()`](https://baolong281.github.io/landpred/reference/get_model.md)
  : Get Landpred Model
- [`helper.si()`](https://baolong281.github.io/landpred/reference/helper.si.md)
  : Helper function for AUC.landmark
- [`landpred-package`](https://baolong281.github.io/landpred/reference/landpred-package.md)
  [`landpred`](https://baolong281.github.io/landpred/reference/landpred-package.md)
  : Landmark Prediction of a Survival Outcome
- [`landpred()`](https://baolong281.github.io/landpred/reference/landpred.md)
  : Create a Landpred Object
- [`mse.BW()`](https://baolong281.github.io/landpred/reference/mse.BW.md)
  : Helper function for optimize.mse.BW.
- [`optimize.mse.BW()`](https://baolong281.github.io/landpred/reference/optimize.mse.BW.md)
  : Calculates initial optimal bandwidth.
- [`predict(`*`<landpred_model_continuous>`*`)`](https://baolong281.github.io/landpred/reference/predict.landpred_model_continuous.md)
  : Predict Method for Landpred Continuous Model
- [`predict(`*`<landpred_model_discrete>`*`)`](https://baolong281.github.io/landpred/reference/predict.landpred_model_discrete.md)
  : Predict Method for Discrete Landpred Model
- [`print(`*`<landpred_model_discrete>`*`)`](https://baolong281.github.io/landpred/reference/print.landpred_model_discrete.md)
  : Print Method for Discrete Landpred Model
- [`print(`*`<landpred_object>`*`)`](https://baolong281.github.io/landpred/reference/print.landpred_object.md)
  : Print Method for Landpred Object
- [`prob2.single()`](https://baolong281.github.io/landpred/reference/prob2.single.md)
  : Estimates P(TL \<t0+tau \| TL \> t0, Z, TS==ts) for a single t.
- [`summary(`*`<landpred_model_continuous>`*`)`](https://baolong281.github.io/landpred/reference/summary.landpred_model_continuous.md)
  : Summary Method for Landpred Continuous Model
- [`summary(`*`<landpred_object>`*`)`](https://baolong281.github.io/landpred/reference/summary.landpred_object.md)
  : Summary Method for Landpred Object
- [`var.fun()`](https://baolong281.github.io/landpred/reference/var.fun.md)
  : Estimate Variance of Coefficients
