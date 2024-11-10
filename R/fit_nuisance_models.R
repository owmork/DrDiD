#' Title
#'
#' @param preprocessed_list List of pre-processed data object/list as returned
#' by "preprocess()".
#'
#' @return List of four models is returned: outcome model for control condition,
#' one outcome model for treatment condition for each pre- and post treatment,
#' and exposure/propensity model.
#'
#' @export
fit_nuisance_models <- function(preprocessed_list) {
  dp <- preprocessed_list
  dta <- dp$data

  # Pre-step: model fixed effects
  pre_mod <- feols(
    fml = dp$pre_fmla,
    data = dta[dta$D == 0, ],
    weights = ~wname,
    combine.quick=FALSE
  )

  # Subtract from Y (residualize)
  Y_FE <- predict(pre_mod, dp$data)
  dp$data$Y <- dp$data$Y - Y_FE

  # Estimate nuisance parameters

  # Exposure model (yields propensity scores)
  exp_mod <- feglm(
    fml = dp$exp_fmla,
    family = dp$exp_link,
    data = dta,
    weights = ~wname,
    combine.quick = FALSE
  )

  # Outcome models
  ## (1) Control for both pre and post, as in multi-period case there is no "post"
  out_ctr_mod <- feols(
    fml = dp$out_ctr_fmla,
    data = dta[dta$D == 0, ],
    weights = ~wname,
    combine.quick=FALSE
  )

  ## (2) Treatment (pre)
  out_trt_pre_mod <- feols(
    fml = dp$out_trt_fmla,
    data = dta[dta$D == 1 & dta$post == 0, ],
    weights = ~wname
  )

  ## (3) Treatment (post)
  out_trt_post_mod <- feols(
    fml = dp$out_trt_fmla,
    data = dta[dta$D == 1 & dta$post == 1, ],
    weights = ~wname
  )

  mods_lst <- list(
    "dp" = dp,
    "mods" = list(
      "exp" = exp_mod,
      "out_ctr" = out_ctr_mod,
      "out_trt_pre" = out_trt_pre_mod,
      "out_trt_post" = out_trt_post_mod
    )
  )

  return(mods_lst)
}
