#' Get last non-missing value
#'
#' Returns the last non-NA value in a vector.
#'
#' @param x A vector.
#' @return A single value or NA if all values are missing.
#' @export
last_non_na <- function(x) {
  vals <- x[!is.na(x)]
  if (length(vals) == 0) NA else dplyr::last(vals)
}

#' Summarize first and last numeric values by group
#'
#' Computes the first, last, and change in a numeric variable within groups.
#'
#' @param data A data frame.
#' @param group_col Column to group by.
#' @param order_col Column defining order (e.g. time or session).
#' @param value_col Numeric column to summarize.
#'
#' @return A data frame with first, last, and change values.
#' @export
first_last_numeric_summary <- function(data, group_col, order_col, value_col) {
  data %>%
    dplyr::group_by({{ group_col }}) %>%
    dplyr::arrange({{ order_col }}, .by_group = TRUE) %>%
    dplyr::summarise(
      first_value = dplyr::first({{ value_col }}),
      last_value = last_non_na({{ value_col }}),
      change = last_value - first_value,
      .groups = "drop"
    )
}


#' Summarize first and last logical values by group
#'
#' Tracks boolean changes over time within groups.
#'
#' @param data A data frame.
#' @param group_col Column to group by.
#' @param order_col Column defining order.
#' @param value_col Logical column.
#'
#' @return A data frame with first and last logical values.
#' @export
first_last_logical_summary <- function(data, group_col, order_col, value_col) {
  data %>%
    dplyr::group_by({{ group_col }}) %>%
    dplyr::arrange({{ order_col }}, .by_group = TRUE) %>%
    dplyr::summarise(
      first_value = {{ value_col }}[1],
      last_value = last_non_na({{ value_col }}),
      .groups = "drop"
    )
}


#' Count TRUE values by group
#'
#' Counts how many TRUE values occur within each group.
#'
#' @param data A data frame.
#' @param group_col Column to group by.
#' @param flag_col Logical column.
#' @param name Name of output column.
#'
#' @return A data frame with counts per group.
#' @export
count_true_by_group <- function(data, group_col, flag_col, name) {
  data %>%
    dplyr::group_by({{ group_col }}) %>%
    dplyr::summarise(
      "{name}" := sum({{ flag_col }}, na.rm = TRUE),
      .groups = "drop"
    )
}