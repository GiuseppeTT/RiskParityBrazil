clean_downloaded_data <- function(
    data
) {
    data %>%
        dplyr::select(.data$asset, .data$date, .data$return) %>%
        dplyr::arrange(.data$asset, .data$date) %>%
        return()
}

clean_packed_data <- function(
    data
) {
    data %>%
        dplyr::select(.data$type, .data$asset, .data$date, .data$return) %>%
        dplyr::arrange(.data$type, .data$asset, .data$date) %>%
        return()
}
