#' Title
#'
#' @param yname Outcome variable
#' @param dname Treatment variable
#' @param gname Treatment cohort, the period a unit is first treated
#' @param tname Periods
#' @param idname Unit id
#' @param data Raw dataset
#' @param exp_link Link of the exposure/propensity model, can be e.g. "logit" or "identity"
#' @param wname Weights
#' @param xformla Covariates formula
#' @param base_period Whether to estimate pre-treatment ATTs. "universal" sets it to treatment - 1,
#' "varying" selects the prior period and returns ATT for treatment - 1 instead of treating it as
#' reference value. Post-treatment ATTs are unaffected.
#' @param FE_1 Two-way fixed effects, e.g. time and region.
#' @param FE_2 One-way fixed effects, e.g. region. Here, you cannot use same as "idname" and "tname" as
#' those are assigned to either treatment and control group (idname) or in some applications, do not occur
#' in post period (tname).
#' @param B Number of boostrap samples in order to calculate standard errors
#' @param cores Number of cores for parallelization
#'
#' @return Estimated ATT with boostrap standard errors
#'
#' @export
DrDiD <- function(
    yname,
    dname,
    gname,
    tname,
    idname,
    data,
    exp_link = c("logit", "gaussian"),
    wname = NULL,
    xformla = ~0,
    FE_1 = ~0,
    FE_2 = ~0,
    base_period = c("varying", "universal"),
    B = 30,
    cores = 4
) {

  # Count time
  start_time <- Sys.time()

  # (a) Data pre-processing
  dp <- preprocess(
    yname = yname,
    dname = dname,
    gname = gname,
    tname = tname,
    idname = idname,
    data  = data,
    exp_link = exp_link,
    wname = wname,
    xformla = xformla,
    FE_1 <- FE_1,
    FE_2 <- FE_2
  )

  # (b) Fit (global) nuisance parameters
  mods <- fit_nuisance_models(dp)

  # (c) Extract local group-time ATTs and long table of gt tables
  att_obj <- get_attgt(mods, dp$data, base_period)
  att <- att_obj$att

  # Add long table to "dp" for bootstraping
  dp$long <- att_obj$long

  # (d) Bootstrap inference
  att_B <- bootstrap(dp, B, workers = cores) |> rbindlist()

  # Print operation time
  end_time <- Sys.time()
  time_taken <- as.numeric(difftime(end_time, start_time, units = "mins"))
  #message(cat("Time taken for operation:", time_taken, "\n"))

  # Return list
  ret_lst <- list(
    "main" = att,
    "bootstraps" = att_B
  )

  return(ret_lst)
}
