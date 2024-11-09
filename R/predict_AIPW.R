# Predict unique combinations, extend and compute AIPW
#' Title
#'
#' @param mods List of four nuisance models
#' @param df (Long) pre-processed data
#'
#' @return Summary table of AIPW ATT estimates for each group-time in df.
#'
#' @export
predict_AIPW <- function(mods, df, print_info = TRUE) {
  # Predict unique covariate combinations
  covars <- unique(unlist(map(mods, ~all.vars(.x$fml))))
  FEs <- unique(unlist(map(mods, ~.x$fixef_vars)))
  FEs <- FEs[!grepl("\\^", FEs)] # remove interactions
  lhss <- c(covars, FEs)

  # Unique combinations
  df_gt_unq <- unique(df[, ..lhss])

  # Predict
  df_gt_unq[, prop := predict(mods$exp, df_gt_unq)]
  df_gt_unq[, y_d0 := predict(mods$out_ctr, df_gt_unq)]
  df_gt_unq[, y_d10 := predict(mods$out_trt_pre, df_gt_unq)]
  df_gt_unq[, y_d11 := predict(mods$out_trt_post, df_gt_unq)]

  # Merge back to full table and remove covariates and NAs
  df_gt_pred <- merge(df, df_gt_unq, by = lhss, all.x = TRUE)
  NAs <- !complete.cases(df_gt_pred[ , c("prop", "y_d0", "y_d10", "y_d11")])
  if (print_info) {message(sprintf("Dropping %i (%.1f %%) observations at prediction stage.", sum(NAs), 100*mean(NAs)))}
  df_gt_pred <- df_gt_pred[!NAs, ]
  rm_lhs <- lhss[!lhss %in% c("D", "Y", "group_size")]
  df_gt_pred[, (rm_lhs) := NULL]

  # Compute AIPW estimate per group (gt)
  # Weights
  df_gt_pred[, `:=` (
    w_d10 = wname * D * (1 - post),
    w_d11 = wname * D * post,
    w_d00 = wname * prop * (1 - D) * (1 - post) / (1 - prop),
    w_d01 = wname * prop * (1 - D) * post / (1 - prop)
  )]

  df_gt_pred[, `:=` (
    w_d = wname * D,
    w_dt0 = wname * w_d10,
    w_dt1 = wname * w_d11
  )]

  # ATT components
  df_gt_AIPW <- df_gt_pred[, .(
    att_d10 = mean(w_d10 * (Y - y_d0)) / mean(w_d10),
    att_d11 = mean(w_d11 * (Y - y_d0)) / mean(w_d11),
    att_d00 = mean(w_d00 * (Y - y_d0)) / mean(w_d00),
    att_d01 = mean(w_d01 * (Y - y_d0)) / mean(w_d01),

    att_d_0 = mean(w_d * (y_d10 - y_d0)) / mean(w_d),
    att_dt00 = mean(w_dt0 * (y_d10 - y_d0)) / mean(w_dt0),
    att_d_1 = mean(w_d * (y_d11 - y_d0)) / mean(w_d),
    att_dt11 = mean(w_dt1 * (y_d11 - y_d0)) / mean(w_dt1),

    group_size = mean(group_size)
  ), by = gt]

  df_gt_AIPW[, att_AIPW := (att_d11 - att_d10) - (att_d01 - att_d00) + (att_d_1 - att_dt11) - (att_d_0 - att_dt00)]

  # Extract group and period
  df_gt_AIPW[, c("group", "period") := tstrsplit(gsub("[()]", "", gt), ", ")]
  df_gt_AIPW <- df_gt_AIPW[, .(group = as.numeric(group), period = as.numeric(period), att = att_AIPW, group_size)]

  return(df_gt_AIPW)
}
