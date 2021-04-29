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

convert_return_to_price <- function(
    data
) {
    data <-
        data %>%
        dplyr::group_by(.data$asset) %>%
        dplyr::arrange(.data$date) %>%
        dplyr::mutate(price = compute_price(.data$return)) %>%
        dplyr::ungroup()

    data <-
        data %>%
        dplyr::select(! .data$return)

    return(data)
}

convert_return_to_dca_multiple <- function(
    data
) {
    data <-
        data %>%
        dplyr::group_by(.data$asset) %>%
        dplyr::arrange(.data$date) %>%
        dplyr::mutate(DCA_multiple = compute_dca_multiple(.data$return)) %>%
        dplyr::ungroup()

    data <-
        data %>%
        dplyr::select(! .data$return)

    return(data)
}


convert_return_to_rolling_cagr <- function(
    data,
    window_size
) {
    data <-
        data %>%
        dplyr::group_by(.data$asset) %>%
        dplyr::arrange(.data$date) %>%
        dplyr::mutate(rolling_CAGR = compute_rolling_cagr(
            .data$return,
            .data$date,
            window_size
        )) %>%
        dplyr::ungroup()

    data <-
        data %>%
        dplyr::select(! .data$return)

    return(data)
}

convert_return_to_smooth_volatility <- function(
    data,
    smoothing_factor
) {
    data <-
        data %>%
        dplyr::group_by(.data$asset) %>%
        dplyr::arrange(.data$date) %>%
        dplyr::mutate(smooth_volatility = compute_smooth_volatility(
            .data$return,
            smoothing_factor
        )) %>%
        dplyr::ungroup()

    data <-
        data %>%
        dplyr::select(! .data$return)

    return(data)
}

convert_return_to_drawdown <- function(
    data
) {
    data <-
        data %>%
        dplyr::group_by(.data$asset) %>%
        dplyr::arrange(.data$date) %>%
        dplyr::mutate(drawdown = compute_drawdown(.data$return)) %>%
        dplyr::ungroup()

    data <-
        data %>%
        dplyr::select(! .data$return)

    return(data)
}

convert_return_to_smooth_correlation <- function(
    data,
    smoothing_factor
) {
    data <-
        data %>%
        dplyr::full_join(data, by = "date")

    data <-
        data %>%
        dplyr::filter(.data$asset.x != .data$asset.y) %>%
        dplyr::mutate(pair = pair(.data$asset.x, .data$asset.y, joiner = " / ")) %>%
        dplyr::mutate(type = asset_types$simple_asset) %>%
        dplyr::select(.data$type, .data$pair, .data$date, .data$return.x, .data$return.y) %>%
        dplyr::distinct(.data$pair, .data$date, .keep_all = TRUE)

    data <-
        data %>%
        dplyr::group_by(.data$pair) %>%
        dplyr::arrange(.data$date) %>%
        dplyr::mutate(smooth_correlation = compute_smooth_correlation(
            .data$return.x,
            .data$return.y,
            smoothing_factor
        )) %>%
        dplyr::ungroup()

    data <-
        data %>%
        dplyr::select(! c(.data$return.x, .data$return.y))

    return(data)
}

convert_return_to_theoretical_weight <- function(
    data,
    window_size
) {
    data <-
        data %>%
        dplyr::group_by(.data$asset) %>%
        dplyr::arrange(.data$date) %>%
        dplyr::mutate(inverse_volatility = compute_rolling_inverse_volatility(
            .data$return,
            .data$date,
            window_size
        )) %>%
        dplyr::ungroup()

    data <-
        data %>%
        dplyr::group_by(.data$date) %>%
        dplyr::mutate(theoretical_weight = .data$inverse_volatility / sum(.data$inverse_volatility)) %>%
        dplyr::ungroup()

    data <-
        data %>%
        dplyr::mutate(type = asset_types$simple_asset) %>%
        dplyr::select(.data$type, .data$asset, .data$date, .data$theoretical_weight)

    return(data)
}
