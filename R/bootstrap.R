#' Title
#'
#' @param dp Pre-processed data object
#' @param B Number of bootstrap samples
#' @param workers Number of workers for parallelization
#'
#' @return List of estimated group-time average treatment effects
#'
#' @export
bootstrap <- function(dp, B = 30, workers) { # TODO: higher default

  # Get "original" data and "long" data
  df <- copy(dp$data)
  df_gt_long <- copy(dp$long)

  att_lst <- mclapply(1:B, \(b) {

    # Clustered normalized bootstrap weights (by id)
    df <- df[, wname := rexp(1), by = id]
    df <- df[, wname := wname / mean(wname)]

    # Fit (bootstrap) model
    mods  <- fit_nuisance_models(dp)

    # Predict
    # TODO: do not know why. should always exist
    if ("wname" %in% names(df_gt_long)) {df_gt_long[, wname := NULL]}
    df_gt_long <- merge(df_gt_long, unique(df[, .(id, wname)]), by = "id")
    att_b <- predict_AIPW(mods$mods, df_gt_long, print_info = FALSE)
    att_b[, b := b]

    return(att_b)
  }, mc.cores = workers)

  return(att_lst)
}
