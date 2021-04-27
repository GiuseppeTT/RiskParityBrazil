#' @title
#' Pack data
#'
#' @description
#' TODO
#'
#' @param data
#' A data frame containing asset, date and return columns.
#' @param benchmark
#' Benchmark name.
#' @param assets
#' Asset names.
#' @param simulate_portfolio
#' TRUE if should include a risk parity portfolio into the packed data.
#'
#' @return
#' A data frame containing the following columns:
#' - type
#' - asset
#' - date
#' - return
#'
#' and arranged by type, asset and date.
#'
#' @export
pack_data <- function(
    data,
    benchmark,
    assets,
    simulate_portfolio = FALSE
) {
    collapsed_assets <-
        assets %>%
        stringr::str_c(collapse = ", ")

    message(glue("Packing assets `{collapsed_assets}` with benchmark `{benchmark}`"))

    if (simulate_portfolio) {
        message("    Simulating portfolio")

        portfolio <- "Risk Parity"

        portfolio_data <-
            data %>%
            filter_assets(assets) %>%
            simulate_portfolio_data(portfolio)

        data <-
            data %>%
            dplyr::bind_rows(portfolio_data)

    } else {
        portfolio <- NA
    }

    data <-
        data %>%
        filter_assets(c(benchmark, assets, portfolio))

    data <-
        data %>%
        dplyr::mutate(type = dplyr::case_when(
            asset ==   benchmark ~ asset_types$benchmark,
            asset %in% assets    ~ asset_types$simple_asset,
            asset ==   portfolio ~ asset_types$portfolio
        )) %>%
        dplyr::mutate(asset = factor(.data$asset, levels = c(benchmark, assets, portfolio))) %>%
        dplyr::mutate(type = factor(
            .data$type,
            levels = c(asset_types$benchmark, asset_types$simple_asset, asset_types$portfolio))
        )

    data <-
        data %>%
        clean_packed_data()

    return(data)
}

simulate_portfolio_data <- function(
    data,
    portfolio
) {
    data <-
        data %>%
        tidyr::pivot_wider(names_from = .data$asset, values_from = .data$return) %>%
        dplyr::mutate(dplyr::across(where(is.numeric), zero_na)) %>%
        xts::as.xts(index = date)

    rebalance_weights <-
        data %>%
        compute_rebalance_weights(window_size = trading_days_per$year, rebalance_period = trading_days_per$year)

    portfolio_data <-
        data %>%
        PerformanceAnalytics::Return.portfolio(rebalance_weights) %>%
        tibble::as_tibble() %>%
        dplyr::rename(date = .data$index, return = .data$portfolio.returns) %>%
        dplyr::mutate(asset = portfolio, .before = dplyr::everything())

    return(portfolio_data)
}

compute_rebalance_weights <- function(
    data,
    window_size,
    rebalance_period
) {
    rolling_volatilities <- zoo::rollapply(data, window_size, stats::sd)

    rebalance_weights <- 1 / rolling_volatilities / rowSums(1 / rolling_volatilities)
    rebalance_weights <- rebalance_weights[seq(window_size, nrow(rebalance_weights), by = rebalance_period)]

    return(rebalance_weights)
}
