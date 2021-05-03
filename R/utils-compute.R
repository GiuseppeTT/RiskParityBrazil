compute_smoothing_factor <- function(
    half_life
) {
    smoothing_factor <- exp(- log(2) / half_life)

    return(smoothing_factor)
}

compute_return <- function(
    price
) {
    return_ <- price / dplyr::lag(price) - 1

    return(return_)
}

compute_price <- function(
    return_
) {
    price <- cumprod(1 + zero_first(return_))

    return(price)
}

compute_dca_multiple <- function(
    return_
) {
    dca_multiple <- cumulative_dca(1 + zero_first(return_)) / seq_along(return_)

    return(dca_multiple)
}

compute_rolling_cagr <- function(
    return_,
    window_size
) {
    return_ %>%
        slider::slide_dbl(
            ~ trading_days_per$year * expm1(mean(log1p(.x))),
            .before = window_size,
            .complete = TRUE
        ) %>%
        return()
}

compute_smooth_volatility <- function(
    return_,
    smoothing_factor
) {
    # Assumes 0 mean, which works great for daily returns
    return_ %>%
        purrr::map_dbl(~ (.x - 0)^2) %>%
        smooth(smoothing_factor) %>%
        magrittr::multiply_by(trading_days_per$year) %>%
        sqrt() %>%
        return()
}

compute_drawdown <- function(
    return_
) {
    price <-
        return_ %>%
        compute_price()

    drawdown <- (price - cummax(price))  / cummax(price)

    return(drawdown)
}

compute_smooth_correlation <- function(
    x_return,
    y_return,
    smoothing_factor
) {
    x_smooth_volatility <-
        x_return %>%
        compute_smooth_volatility(smoothing_factor)

    y_smooth_volatility <-
        y_return %>%
        compute_smooth_volatility(smoothing_factor)

    # Assumes 0 mean, which works great for daily returns
    smooth_covariance <-
        list(x_return, y_return) %>%
        purrr::pmap_dbl(~ (.x - 0) * (.y - 0)) %>%
        smooth(smoothing_factor) %>%
        magrittr::multiply_by(trading_days_per$year)

    smooth_correlation <-
        list(smooth_covariance, x_smooth_volatility, y_smooth_volatility) %>%
        purrr::pmap_dbl(~ ..1  / (..2 * ..3))

    return(smooth_correlation)
}

compute_rolling_inverse_volatility <- function(
    return_,
    window_size
) {
    return_ %>%
        slider::slide_dbl(
            ~ 1 / stats::sd(.x, na.rm = TRUE),
            .before = window_size,
            .complete = TRUE
        ) %>%
        return()
}
