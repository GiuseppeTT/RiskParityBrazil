#' @title
#' Complement data
#'
#' @description
#' Complement the data with the following functionalities:
#' - Proxy: Add "proxied" asset that is a copy of "base" asset
#' - Spread: Spread non-daily returns to daily returns of "spreaded" asset
#' - Regress: Extend "regressed" asset returns as a linear function of "base"
#' asset returns
#' - Mix: Add "mixed" asset whose returns are the average of "base" assets
#' returns for each day.
#'
#' @param data
#' A data frame containing asset, date and return columns.
#' @param config
#' A list of lists containing the complement methods and arguments. They are
#' `method` (method name, one of `proxy`, `spread`, `regress` or `mix`) and
#' `arguments` (method arguments).
#'
#' @return
#' A data frame containing the following columns:
#' - asset
#' - date
#' - return
#'
#' and arranged by asset and date.
#'
#' @export
complement_data <- function(
    data,
    config
) {
    message("Complementing data")

    data <-
        config %>%
        purrr::reduce(complement_each_asset, .init = data)

    data <-
        data %>%
        clean_downloaded_data()

    return(data)
}

complement_each_asset <- function(
    data,
    complement
) {
    method    <- complement$method
    arguments <- complement$arguments

    method %>%
        switch(
            proxy   = proxy_asset(data, arguments$proxied, arguments$base),
            spread  = spread_asset(data, arguments$spreaded),
            regress = regress_asset(data, arguments$regressed, arguments$base),
            mix     = mix_asset(data, arguments$mixed, arguments$base),
            stop(glue("Method `{method}` is not supported"))
        ) %>%
        return()
}

proxy_asset <- function(
    data,
    proxied,
    base
) {
    message(glue("    Proxying asset `{proxied}` based on asset `{base}`"))

    data_complement <-
        data %>%
        dplyr::filter(.data$asset == base) %>%
        dplyr::mutate(asset = proxied)

    data <-
        data %>%
        dplyr::bind_rows(data_complement)

    return(data)
}

spread_asset <- function(
    data,
    spreaded
) {
    message(glue("    Spreading asset `{spreaded}`"))

    full_dates <-
        data %>%
        dplyr::filter(.data$asset == spreaded) %>%
        dplyr::pull(.data$date) %>%
        build_full_dates()

    data_complement <-
        tidyr::expand_grid(asset = spreaded, date = full_dates) %>%
        dplyr::left_join(data, by = c("asset", "date"))

    data_complement <-
        data_complement %>%
        dplyr::mutate(return = spread_return(.data$return))

    data <-
        data %>%
        dplyr::filter(.data$asset != spreaded) %>%
        dplyr::bind_rows(data_complement)

    return(data)
}

build_full_dates <- function(
    date
) {
    date_range <-
        date %>%
        range()

    full_dates <- seq(date_range[1], date_range[2] + months(1) - lubridate::days(1), by = "day")

    return(full_dates)
}

spread_return <- function(
    return_
) {
    return_ %>%
        split_on_non_na() %>%
        purrr::map(spread_first) %>%
        purrr::simplify() %>%
        return()
}

split_on_non_na <- function(
    x
) {
    x %>%
        split(cumsum(!is.na(x))) %>%
        return()
}

spread_first <- function(
    return_
) {
    first <- return_[1]
    size <- length(return_)
    geometric_mean <- expm1(log1p(first) / size)

    return_ %>%
        rlang::rep_along(geometric_mean) %>%
        return()
}

regress_asset <- function(
    data,
    regressed,
    base
) {
    message(glue("    Regressing asset `{regressed}` based on asset `{base}`"))

    min_regressed_date <-
        data %>%
        dplyr::filter(.data$asset == regressed) %>%
        dplyr::pull(.data$date) %>%
        min()

    regression_coefficient <-
        data %>%
        filter_assets(c(regressed, base)) %>%
        dplyr::group_by(.data$date) %>%
        dplyr::summarise(ratio = .data$return[.data$asset == regressed] / .data$return[.data$asset == base]) %>%
        dplyr::pull(.data$ratio) %>%
        mean(trim = 0.05)

    data_complement <-
        data %>%
        dplyr::filter(.data$asset == base) %>%
        dplyr::filter(.data$date < min_regressed_date) %>%
        dplyr::mutate(return = regression_coefficient * .data$return) %>%
        dplyr::mutate(asset = regressed)

    data_complement <-
        data %>%
        dplyr::filter(.data$asset == regressed) %>%
        dplyr::bind_rows(data_complement) %>%
        dplyr::arrange(.data$date)

    data <-
        data %>%
        dplyr::filter(.data$asset != regressed) %>%
        dplyr::bind_rows(data_complement)

    return(data)
}

mix_asset <- function(
    data,
    mixed,
    base
) {
    collapsed_assets <-
        base %>%
        stringr::str_c(collapse = ", ")

    message(glue("    Mixing asset `{mixed}` based on asset(s) `{collapsed_assets}`"))

    data_complement <-
        data %>%
        filter_assets(base) %>%
        dplyr::group_by(.data$date) %>%
        dplyr::summarise(asset = mixed, return = mean(.data$return)) %>%
        dplyr::ungroup()

    data <-
        data %>%
        dplyr::bind_rows(data_complement)

    return(data)
}
