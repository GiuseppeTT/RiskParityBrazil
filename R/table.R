# TODO: include DCA something
table_metrics <- function(
    data
) {
    table <-
        data %>%
        dplyr::group_by(.data$asset) %>%
        dplyr::summarise(
            CAGR               = expm1(trading_days_per$year * mean(log1p(.data$return), na.rm = TRUE)),
            .mean              = trading_days_per$year * mean(.data$return, na.rm = TRUE),
            standard_deviation = sqrt(trading_days_per$year) * stats::sd(.data$return, na.rm = TRUE),
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
