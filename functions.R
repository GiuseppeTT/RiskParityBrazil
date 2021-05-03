trading_days_per <- list(
    week = 5,
    month = 21,
    year = 248
)

asset_types <- list(
    benchmark = "Benchmark",
    simple_asset = "Simple asset",
    portfolio = "Portfolio"
)

return_types <- list(
    nominal = "Nominal",
    relative = "Relative"
)

y_variables <- list(
    price = "Price",
    dca_multiple = "DCA Multiple",
    rolling_cagr = "Rolling CAGR",
    smooth_volatility = "Smooth Volatility",
    drawdown = "Drawdown",
    smooth_correlation = "Smooth Correlation",
    theoretical_weight = "Theoretical Weight"
)

y_scales <- list(
    linear = "Linear",
    log = "Log"
)

list_available <- function(
    data,
    variable
) {
    data %>%
        dplyr::pull({{ variable }}) %>%
        unique() %>%
        sort() %>%
        return()
}

plot_base <- function(
    data,
    computed_variable,
    compute_function,
    ...,
    .complete = FALSE
) {
    # TODO: consider always tidyr::complete, maybe even do this in pack_data
    # TODO: change correlation pair column name to asset column name
    # TODO: think if computed_variable should be string (use y_variable and rename it to y_variable) or NSE (non standard evaluation)

    if (.complete) {
        data <-
            data %>%
            tidyr::complete(tidyr::nesting(type, asset), .data$date)
    }

    data <-
        data %>%
        dplyr::group_by(.data$asset) %>%
        dplyr::arrange(.data$date) %>%
        dplyr::mutate("{computed_variable}" := compute_function(.data$return, ...))

    plot <-
        data %>%
        ggplot2::ggplot(ggplot2::aes(x = .data$date, y = .data[[computed_variable]], color = .data$asset)) +
        ggplot2::geom_line(ggplot2::aes(size = .data$type, linetype = .data$type))

    return(plot)
}

relativize_return <- function(
    data
) {
    benchmark_data <-
        data %>%
        dplyr::filter(.data$type == asset_types$benchmark) %>%
        dplyr::select(.data$date, benchmark_return = .data$return)

    # TODO: Full join is not performing union
    data <-
        data %>%
        dplyr::full_join(benchmark_data, by = "date") %>%
        dplyr::mutate(return = (1 + .data$return) / (1 + .data$benchmark_return) - 1) %>%
        dplyr::select(! .data$benchmark_return)

    return(data)
}

base_theme <- function(
    # Empty
) {
    theme <- list(
        ggplot2::scale_size_manual(values = vctrs::vec_c(
            "{asset_types$benchmark}"    := 3,
            "{asset_types$simple_asset}" := 0.5,
            "{asset_types$portfolio}"    := 2
        )),
        ggplot2::scale_linetype_manual(values = vctrs::vec_c(
            "{asset_types$benchmark}"    := "dotted",
            "{asset_types$simple_asset}" := "solid",
            "{asset_types$portfolio}"    := "solid"
        )),
        ggplot2::guides(color = guide_legend(override.aes = list(size = 3))),
        ggplot2::theme_bw(base_size = 20),
        ggplot2::theme(
            legend.position = "bottom",
            legend.background = element_rect(color = "black", size = 0.5)
        )
    )

    return(theme)
}

tidy_switch <- function(
    expression_,
    ...
) {
    expression_ <- list(expression_)
    dots <- rlang::list2(...)

    result <- do.call(switch, c(expression_, dots))

    return(result)
}
