#' Title
#'
#' @param mods List of four nuisance models
#' @param df Pre-processed data
#'
#' @return A two-element object/list. "att" contains the group-time average
#' treatment effects in table. "df_long" is the table combines the single
#' group-time specific tables, i.e. treatment and control group pre and post
#' treatment with regard to the base period and specific treatment date of
#' a group-time.
#'
#' @export
get_attgt <- function(mods, df) {
  setDT(df) # data.table

  GP <- df |> count(group = G, period = P) |> filter(group != 0)
  # TODO: more logical choice
  GP <- GP[GP$n > 1, ]

  # Create list of tables for each group (treatment + control, pre + post)
  df_gt_lst <- map(1:nrow(GP), \(i) {
    t_base <- GP[i, ]$group - 1
    t_post <- GP[i, ]$period

    # only valid for pre-treatment periods
    # t_base <- GP[i, ]$period - 1
    # t_post <- GP[i, ]$period

    # Filter rows in df directly
    df_gt <- df[P %in% c(t_post, t_base) & G %in% c(0, GP[i, ]$group)]

    # Overwrite post
    df_gt[, post := ifelse(P == t_post, 1, 0)]

    # Pre-treatment covariates adjustment
    covars <- all.vars(mods$exp$fml)
    covars <- covars[-1]  # Remove the response variable
    # Set pre-treatment covariates to their first value by 'id'
    df_gt[, (covars) := lapply(.SD, function(x) x[1]), by = id, .SDcols = covars]

    # Indicator what ATT(g,t) the data belongs to
    df_gt[, gt := sprintf("(%s, %s)", GP[i,]$group, GP[i,]$period)]
    df_gt[, group_size := GP[i,]$n]

    return(df_gt)
  })

  # Store in one table
  df_gt_long <- bind_rows(df_gt_lst)

  # Compute AIPW
  df_gt_AIPW <- predict_AIPW(mods, df_gt_long)

  # Return
  ret_lst <- list(
    "att" = df_gt_AIPW,
    "long" = df_gt_long
  )

  return(ret_lst)
}
