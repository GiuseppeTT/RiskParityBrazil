#' @title
#' Analyze data
#'
#' @description
#' TODO
#'
#' @param data
#' A data frame containing type, asset, date and return columns.
#'
#' @return
#' TODO
#'
#' @export
analyze_data <- function(
    data
) {
    message("Analyzing data")

    window_size <- trading_days_per$year
    smoothing_factor <- compute_smoothing_factor(half_life = trading_days_per$month)

    summary <- list(
        metrics_table = table_metrics(data)
    )

    gain <- list(
        price_plot        = plot_price(data),
        rolling_cagr_plot = plot_rolling_cagr(data, window_size)
    )

    risk <- list(
        smooth_volatility_plot = plot_smooth_volatility(data, smoothing_factor),
        drawdown_plot          = plot_drawdown(data)
    )

    relation <- list(
        smooth_correlation_plot = plot_smooth_correlation(data, smoothing_factor),
        theoretical_weight_plot = plot_theoretical_weight(data, window_size)
    )

    analysis <- list(
        summary = summary,
        gain = gain,
        risk = risk,
        relation = relation
    )

    return(analysis)
}

table_metrics <- function(
    data,
    relative = FALSE
) {
    table <-
        data %>%
        dplyr::group_by(.data$asset) %>%
        dplyr::summarise(
            CAGR               = prod(1 + .data$return) ^ (trading_days_per$year / length(.data$date)) - 1,
            .mean              = trading_days_per$year * mean(.data$return),
            standard_deviation = sqrt(trading_days_per$year) * stats::sd(.data$return),
            sharpe             = .data$.mean / .data$standard_deviation,
            worst_drawdown     = min(compute_drawdown(.data$return))
        ) %>%
        dplyr::select(! tidyselect::starts_with(".")) %>%
        dplyr::ungroup()

    table <-
        table %>%
        humanize_column_names()

    return(table)
}

plot_price <- function(
    data,
    relative = FALSE
) {
    data <-
        data %>%
        convert_return_to_price()

    plot <-
        data %>%
        ggplot2::ggplot(ggplot2::aes(x = .data$date, y = .data$price, color = .data$asset)) +
        ggplot2::geom_line(ggplot2::aes(size = .data$type, linetype = .data$type)) +
        ggplot2::scale_y_log10() +
        base_theme()

    plot <-
        plot %>%
        humanize_plot_labels()

    return(plot)
}

plot_rolling_cagr <- function(
    data,
    window_size,
    relative = TRUE
) {
    data <-
        data %>%
        convert_return_to_rolling_cagr(window_size) %>%
        tidyr::drop_na(.data$rolling_CAGR)

    plot <-
        data %>%
        ggplot2::ggplot(ggplot2::aes(x = .data$date, y = .data$rolling_CAGR, color = .data$asset)) +
        ggplot2::geom_line(ggplot2::aes(size = .data$type, linetype = .data$type)) +
        ggplot2::scale_y_continuous(labels = scales::percent) +
        base_theme()

    plot <-
        plot %>%
        humanize_plot_labels()

    return(plot)
}

plot_smooth_volatility <- function(
    data,
    smoothing_factor
) {
    data <-
        data %>%
        convert_return_to_smooth_volatility(smoothing_factor)

    plot <-
        data %>%
        ggplot2::ggplot(ggplot2::aes(x = .data$date, y = .data$smooth_volatility, color = .data$asset)) +
        ggplot2::geom_line(ggplot2::aes(size = .data$type, linetype = .data$type)) +
        ggplot2::scale_y_continuous(labels = scales::percent) +
        base_theme()

    plot <-
        plot %>%
        humanize_plot_labels()

    return(plot)
}

plot_drawdown <- function(
    data,
    relative = FALSE
) {
    data <-
        data %>%
        convert_return_to_drawdown()

    plot <-
        data %>%
        ggplot2::ggplot(ggplot2::aes(x = .data$date, y = .data$drawdown, color = .data$asset)) +
        ggplot2::geom_line(ggplot2::aes(size = .data$type, linetype = .data$type)) +
        ggplot2::scale_y_continuous(labels = scales::percent, limits = c(-1, 0)) +
        base_theme()

    plot <-
        plot %>%
        humanize_plot_labels()

    return(plot)
}

plot_smooth_correlation <- function(
    data,
    smoothing_factor
) {
    asset_count <-
        data %>%
        dplyr::filter(.data$type == asset_types$simple_asset) %>%
        dplyr::pull(.data$asset) %>%
        unique()

    if (length(asset_count) == 1) {
        return(NULL)
    }

    data <-
        data %>%
        dplyr::filter(.data$type == asset_types$simple_asset) %>%
        dplyr::select(! .data$type)

    data <-
        data %>%
        convert_return_to_smooth_correlation(smoothing_factor)

    plot <-
        data %>%
        ggplot2::ggplot(ggplot2::aes(x = .data$date, y = .data$smooth_correlation, color = .data$pair)) +
        ggplot2::geom_line(ggplot2::aes(size = .data$type, linetype = .data$type)) +
        ggplot2::scale_y_continuous(labels = scales::percent, limits = c(-1, 1)) +
        base_theme()

    plot <-
        plot %>%
        humanize_plot_labels()

    return(plot)
}

plot_theoretical_weight <- function(
    data,
    window_size
) {
    data_types <-
        data %>%
        dplyr::pull(.data$type) %>%
        unique()

    if (all(data_types != asset_types$portfolio)) {
        return(NULL)
    }

    # .data doest not work with tidyr::nesting
    # https://github.com/tidyverse/tidyr/issues/971
    data <-
        data %>%
        tidyr::complete(tidyr::nesting(type, asset), .data$date) %>%
        dplyr::filter(.data$type == asset_types$simple_asset) %>%
        dplyr::select(! .data$type)

    data <-
        data %>%
        convert_return_to_theoretical_weight(window_size) %>%
        tidyr::drop_na()

    plot <-
        data %>%
        ggplot2::ggplot(ggplot2::aes(x = .data$date, y = .data$theoretical_weight, color = .data$asset)) +
        ggplot2::geom_line(ggplot2::aes(size = .data$type, linetype = .data$type)) +
        ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
        base_theme()

    plot <-
        plot %>%
        humanize_plot_labels()

    return(plot)
}
