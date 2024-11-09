#' Title
#'
#' @param att_0 Main estimates of ATT
#' @param att_b Bootstrap estimates of ATT
#'
#' @return ATT along with bootstrap standard errors and more statistics.
#'
#' @export
statistics <- function(att_0, att_b) {
  se <- stats::IQR((att_0 - att_b)) / (stats::qnorm(0.75) - stats::qnorm(0.25))
  cv <- unname(stats::quantile(abs((att_0 - att_b)/se), probs = 0.95))
  uci <- att_0 + cv * se
  lci <- att_0 - cv * se
  statistic <- att_0 / se
  pval <- 2 * (1 - pnorm(abs(statistic)))
  stars <- gtools::stars.pval(pval)

  #ret_tbl <- data.frame(estimate = att_0, std.error = se, lb = lci, ub = uci, p.value = pval, star = stars)
  ret_tbl <- data.frame(estimate = att_0, std.error = se, statistic = statistic, p.value = pval, star = stars)
  return(ret_tbl)
}
