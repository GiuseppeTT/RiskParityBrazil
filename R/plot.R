plot_price <- function(
    data,
    y_scale = y_scales$linear
) {
    computed_variable <- y_variables$price

    data %>%
        preprocess_plot_base(computed_variable, compute_price) %>%
        plot_base(computed_variable, y_scale) %>%
        return()
}

plot_dca_multiple <- function(
    data,
    y_scale = y_scales$linear
) {
    computed_variable <- y_variables$dca_multiple

    data %>%
        preprocess_plot_base(computed_variable, compute_dca_multiple) %>%
        plot_base(computed_variable, y_scale) %>%
        return()
}

plot_rolling_cagr <- function(
    data,
    window_size,
    y_scale = y_scales$linear
) {
    computed_variable <- y_variables$rolling_cagr

    data %>%
        preprocess_plot_base(computed_variable, compute_rolling_cagr, window_size = window_size) %>%
        plot_base(computed_variable, y_scale) %>%
        return()
}

plot_smooth_volatility <- function(
    data,
    smoothing_factor,
    y_scale = y_scales$linear
) {
    computed_variable <- y_variables$smooth_volatility

    data %>%
        preprocess_plot_base(computed_variable, compute_smooth_volatility, smoothing_factor = smoothing_factor) %>%
        plot_base(computed_variable, y_scale) %>%
        return()
}

plot_drawdown <- function(
    data,
    y_scale = y_scales$linear
) {
    computed_variable <- y_variables$drawdown

    data %>%
        preprocess_plot_base(computed_variable, compute_drawdown) %>%
        plot_base(computed_variable, y_scale, limits = c(-1, 0)) %>%
        return()
}

plot_smooth_correlation <- function(
    data,
    smoothing_factor,
    y_scale = y_scales$linear
) {
    computed_variable <- y_variables$smooth_correlation

    data <-
        data %>%
        dplyr::filter(.data$type == asset_types$simple_asset)

    # TODO: implement count_asset
    asset_count <-
        data %>%
        dplyr::pull(.data$asset) %>%
        unique()

    if (length(asset_count) == 1) {
        return(NULL)
    }

    data <-
        data %>%
        pair_data()

    # TODO: include this case in preprocess_plot_base
    # Maybe use tidyselect
    data <-
        data %>%
        dplyr::group_by(.data$asset) %>%
        dplyr::arrange(.data$date) %>%
        dplyr::mutate("{computed_variable}" := compute_smooth_correlation(
            .data$return.x,
            .data$return.y,
            smoothing_factor = smoothing_factor
        )) %>%
        dplyr::ungroup()

    plot <-
        data %>%
        plot_base(computed_variable, y_scale, limits = c(-1, 1))

    return(plot)
}

pair_data <- function(
    data
) {
    data <-
        data %>%
        dplyr::full_join(data, by = "date")

    data <-
        data %>%
        dplyr::filter(.data$asset.x != .data$asset.y) %>%
        dplyr::mutate(type = factor(asset_types$simple_asset, levels = asset_types)) %>%
        dplyr::mutate(asset = pair(.data$asset.x, .data$asset.y, joiner = " / "))

    data <-
        data %>%
        dplyr::distinct(.data$asset, .data$date, .keep_all = TRUE) %>%
        dplyr::select(.data$type, .data$asset, .data$date, .data$return.x, .data$return.y) %>%
        dplyr::arrange(.data$type, .data$asset, .data$date)

    return(data)
}

pair <- function(
    x,
    y,
    joiner
) {
    x <-
        x %>%
        as.character()

    y <-
        y %>%
        as.character()

    list(x, y) %>%
        purrr::pmap(c) %>%
        purrr::map(sort) %>%
        purrr::map_chr(stringr::str_c, collapse = joiner) %>%
        return()
}

plot_theoretical_weight <- function(
    data,
    window_size,
    y_scale = y_scales$linear
) {
    computed_variable <- y_variables$theoretical_weight

    data <-
        data %>%
        dplyr::filter(.data$type == asset_types$simple_asset)

    data <-
        data %>%
        dplyr::group_by(.data$asset) %>%
        dplyr::arrange(.data$date) %>%
        dplyr::mutate(inverse_volatility = compute_rolling_inverse_volatility(.data$return, window_size)) %>%
        dplyr::ungroup()

    data <-
        data %>%
        dplyr::group_by(.data$date) %>%
        dplyr::mutate("{computed_variable}" := .data$inverse_volatility / sum(.data$inverse_volatility)) %>%
        dplyr::ungroup()

    plot <-
        data %>%
        plot_base(computed_variable, y_scale, limits = c(0, 1))

    return(plot)
}

# TODO: maybe rename to compute_by_asset
preprocess_plot_base <- function(
    data,
    computed_variable,
    compute_function,
    ...
) {
    data %>%
        dplyr::group_by(.data$asset) %>%
        dplyr::arrange(.data$date) %>%
        dplyr::mutate("{computed_variable}" := compute_function(.data$return, ...)) %>%
        dplyr::ungroup() %>%
        return()
}

plot_base <- function(
    data,
    computed_variable,
    y_scale,
    ...
) {
    dots <- list(...)

    labels <- dots$labels %||% scales::percent
    limits <- dots$limits %||% NULL

    scale_y <- tidy_switch(
        y_scale,
        "{y_scales$linear}" := ggplot2::scale_y_continuous,
        "{y_scales$log}" := ggplot2::scale_y_log10
    )

    plot <-
        data %>%
        ggplot2::ggplot(ggplot2::aes(x = .data$date, y = .data[[computed_variable]], color = .data$asset)) +
        ggplot2::geom_line(ggplot2::aes(size = .data$type, linetype = .data$type)) +
        scale_y(labels = labels, limits = limits) +
        base_theme()

    plot <-
        plot %>%
        humanize_plot_labels()

    return(plot)
}
