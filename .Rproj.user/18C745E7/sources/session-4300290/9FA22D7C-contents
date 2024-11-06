#' Title
#'
#' @param att_raw Raw object as returned by DrDiD.
#' @param type Type of aggregation. "simple" returns the overall ATT, "dynamic"
#' returns event study type, "calendar" returns ATT aggregated by post-treatment
#' period, and "custom" returns aggregated as specified with argument "e".
#' @param e When type is "calendar", specify the range of event time to
#' aggregate. Returns a single value for this range.
#'
#' @return Summary table of estimated ATT.
#'
#' @export
aggregate_att <- function(
    att_raw,
    type = c("simple", "dynamic", "calendar", "custom"),
    e = NULL) {

  # Drop NAs and add event time
  att_0 <- att_raw$main[complete.cases(att_raw$main), ]
  att_0$event_time <- att_0$period - att_0$group
  att_0 |> setDT()
  att_0 <- att_0[order(event_time),]

  att_b <- att_raw$bootstraps[complete.cases(att_raw$bootstraps), ]
  att_b$event_time <- att_b$period - att_b$group
  att_b |> setDT()
  att_b <- att_b[order(event_time),]

  # Aggregate weighted average of all non-negative ATTs
  if (type == "simple") {
    att_0_simple <- att_0[event_time >= 0,]
    att_0_simple <- weighted.mean(att_0_simple$att, att_0_simple$group_size)

    att_b_simple <- att_b[event_time >= 0,]
    att_b_simple <- att_b_simple[, .(att = sum(att * group_size) / sum(group_size)), by = .(b)]$att

    res <- statistics(att_0_simple, att_b_simple)
  } else if (type == "dynamic") {
    e_range <- sort(unique(att_0$event_time))

    att_0_dyn <- att_0[, .(att = sum(att * group_size) / sum(group_size), group_size = sum(group_size)), by = event_time]
    att_b_dyn <- att_b[, .(att = sum(att * group_size) / sum(group_size)), by = .(event_time, b)]

    att_dyn <- map(e_range, \(e) statistics(att_0_dyn[event_time == e, ]$att, att_b_dyn[event_time == e, ]$att))
    att_dyn <- do.call(rbind, att_dyn)
    att_dyn$e <- e_range
    att_dyn$n <- att_0_dyn$group_size
    att_dyn$p <- att_0_dyn$group_size / sum(att_0_dyn$group_size)
    res <- att_dyn
  } else if (type == "calendar") {
    att_0_calendar <- att_0[event_time >= 0,]
    att_b_calendar <- att_b[event_time >= 0,]

    p_range <- sort(unique(att_0_calendar$period))

    att_0_calendar <- att_0_calendar[, .(att = sum(att * group_size) / sum(group_size), group_size = sum(group_size)), by = period]
    att_b_calendar <- att_b_calendar[, .(att = sum(att * group_size) / sum(group_size)), by = .(period, b)]

    att_calendar <- map(p_range, \(p) statistics(att_0_calendar[period == p, ]$att, att_b_calendar[period == p, ]$att))
    att_calendar <- do.call(rbind, att_calendar)
    att_calendar$period <- p_range
    att_calendar$n <- att_0_calendar$group_size
    att_calendar$p <- att_0_calendar$group_size / sum(att_0_calendar$group_size)
    res <- att_calendar
  } else if (type == "custom") {
    e_min <- min(e)
    e_max <- max(e)

    att_0_custom <- att_0[event_time >= e_min & event_time <= e_max, ]
    att_0_custom <- weighted.mean(att_0_custom$att, att_0_custom$group_size)

    att_b_custom <- att_b[event_time >= e_min & event_time <= e_max, ]
    att_b_custom <- att_b_custom[, .(att = sum(att * group_size) / sum(group_size)), by = .(b)]$att

    res <- statistics(att_0_custom, att_b_custom)
  }

  # Prettify
  res[sapply(res, is.numeric)] <- lapply(res[sapply(res, is.numeric)], round, 3)

  return(res)
}
