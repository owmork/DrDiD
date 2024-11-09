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
#' @param FE_1 Two-way fixed effects, e.g. time and region.
#' @param FE_2 One-way fixed effects, e.g. region. Here, you cannot use same as "idname" and "tname" as
#' those are assigned to either treatment and control group (idname) or in some applications, do not occur
#' in post period (tname).
#'
#' @return Pre-processed data object/list.
#'
#' @export
preprocess <- function(
    yname,
    dname,
    gname,
    tname,
    idname,
    data,
    exp_link,
    wname = NULL,
    xformla = ~0,
    FE_1 = ~0,
    FE_2 = ~0
) {

  # Data frame without missing observations
  data <- as.data.frame(data)

  # Select columns
  data <- cbind(
    id = data[, idname],
    D = data[, dname],
    G = data[, gname],
    P = data[, tname], # to avoid confusion with "TRUE" (instead P for period)
    Y = data[, yname],
    data[, c(all.vars(xformla), all.vars(FE_1))]
  )

  # Remove observations with NAs
  NAs  <- !complete.cases(data)
  data <- data[!NAs, ]
  message(sprintf("Dropping %i (%.1f %%) observations at fitting stage.", sum(NAs), 100*mean(NAs)))

  # Create "post" variable
  data$post <- (data$P >= data$G & data$D == 1)

  # Check for weight vector, else create
  if (is.null(wname)) data$wname <- rep(1, nrow(data))

  # Create formulas for outcome and propensity models
  setFixest_fml(
    ..covariates = xformla,
    ..FE_time_state = FE_1,
    ..FE_state = FE_2
  )

  exp_link          <- if (exp_link == "identity") {gaussian(link = "identity")} else {binomial(link = "logit")}
  exp_fmla          <- xpd(~ ..covariates | ..FE_time_state, lhs = "D")
  out_ctr_fmla      <- xpd(~ ..covariates | ..FE_time_state, lhs = "Y")
  out_trt_fmla      <- xpd(~ ..covariates | ..FE_state, lhs = "Y")

  return_lst <- list(
    "data" = data,
    "exp_link" = exp_link,
    "exp_fmla" = exp_fmla,
    "out_ctr_fmla" = out_ctr_fmla,
    "out_trt_fmla" = out_trt_fmla
  )

  return(return_lst)
}
