convert_percent_return_to_return <- function(
    data
) {
    data %>%
        dplyr::mutate(return = .data$return / 100) %>%
        return()
}

convert_price_to_return <- function(
    data
) {
    data <-
        data %>%
        dplyr::group_by(.data$asset) %>%
        dplyr::arrange(.data$date) %>%
        dplyr::mutate(return = compute_return(.data$price)) %>%
        dplyr::filter(.data$date != min(.data$date)) %>%  # First return is NA
        dplyr::ungroup()

    data <-
        data %>%
        dplyr::select(! .data$price)

    return(data)
}
