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

    yearly_window_size <- days_per$year
    monthly_smoothing_factor <- compute_smoothing_factor(half_life = days_per$month)
    weekly_smoothing_factor <- compute_smoothing_factor(half_life = days_per$week)

    summary <- list(
        metrics_table = table_metrics(data)
    )

    gain <- list(
        price_plot        = plot_price(data, y_scale = y_scales$log),
        rolling_cagr_plot = plot_rolling_cagr(data, yearly_window_size)
    )

    risk <- list(
        smooth_volatility_plot = plot_smooth_volatility(data, weekly_smoothing_factor, y_scale = y_scales$log),
        drawdown_plot          = plot_drawdown(data)
    )

    relation <- list(
        smooth_correlation_plot = plot_smooth_correlation(data, monthly_smoothing_factor)
    )

    analysis <- list(
        summary = summary,
        gain = gain,
        risk = risk,
        relation = relation
    )

    return(analysis)
}
