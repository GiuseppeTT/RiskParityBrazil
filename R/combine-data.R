#' @title
#' Combine data
#'
#' @description
#' Bind downloaded data and arrange by asset and date.
#'
#' @param ...
#' The downloaded data as data frames.
#'
#' @return
#' A data frame containing the following columns:
#' - asset
#' - date
#' - return
#'
#' and arranged by asset and date.
#'
#' @export
combine_data <- function(
    ...
) {
    message("Combining downloaded data")

    NULL %>%
        dplyr::bind_rows(...) %>%
        dplyr::arrange(.data$asset, .data$date) %>%
        return()
}
