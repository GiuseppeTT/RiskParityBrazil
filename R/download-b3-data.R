#' @title
#' Download data from the Brazil Stock Exchange (B3)
#'
#' @description
#' Download index data from the Brazil Stock Exchange (B3).
#'
#' @param config
#' A list of lists containing the B3 data arguments. They are `asset` (asset
#' name), `index` (index ID, as per B3 site) and `first_year` (first year with
#' available data, as per B3 site).
#'
#' @details
#' This function downloads the data directly from an internal API of B3 that
#' feeds some tables in the B3 site (e.g. \href{http://www.b3.com.br/pt_br/market-data-e-indices/indices/indices-amplos/indice-ibovespa-ibovespa-estatisticas-historicas.html}{IBOV data table}).
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
download_b3_data <- function(
    config
) {
    message("Downloading B3 data")

    config <-
        config %>%
        purrr::map(expand_year) %>%
        dplyr::bind_rows()

    data <-
        config %>%
        purrr::pmap(download_each_b3_data, language = "en-us") %>%
        dplyr::bind_rows()

    data <-
        data %>%
        convert_price_to_return() %>%
        clean_downloaded_data()

    return(data)
}

expand_year <- function(
    config
) {
    first_year <- config$first_year
    config$first_year <- NULL

    this_year <-
        lubridate::today() %>%
        lubridate::year()

    config$year <- seq(first_year, this_year)
    config$asset <- rlang::rep_along(config$year, config$asset)
    config$index <- rlang::rep_along(config$year, config$index)

    return(config)
}

download_each_b3_data <- function(
    asset,
    index,
    year,
    language
) {
    message(glue("    Downloading asset `{asset}`, year `{year}`"))

    response_content <-
        request_each_b3_data(index, year, language)

    data <-
        response_content %>%
        read_each_b3_data(asset, index, year)

    return(data)
}

request_each_b3_data <- function(
    index,
    year,
    language
) {
    json <-
        list(index = index, language = language, year = as.character(year)) %>%
        jsonlite::toJSON(auto_unbox = TRUE) %>%
        jsonlite::base64_enc()

    url <- glue(
        "https://sistemaswebb3-listados.b3.com.br/indexStatisticsProxy/IndexCall/GetDownloadPortfolioDay/{json}"
    )

    response <-
        url %>%
        httr::GET(httr::config(ssl_verifypeer = FALSE))

    response_content <-
        response %>%
        httr::content() %>%
        jsonlite::base64_dec() %>%
        rawToChar()

    return(response_content)
}

read_each_b3_data <- function(
    response_content,
    asset,
    index,
    year
) {
    data <-
        response_content %>%
        stringr::str_remove_all("\\r") %>%
        readr::read_delim(delim = ";", skip = 1, n_max = 31)

    data <-
        data %>%
        dplyr::rename(day = .data$Day) %>%
        tidyr::pivot_longer(! .data$day, names_to = "month", values_to = "price") %>%
        tidyr::drop_na(.data$price)  # Non-trading dates span NA price values

    data <-
        data %>%
        dplyr::mutate(asset = {{ asset }}) %>%
        dplyr::mutate(date = robust_make_date(year, .data$month, .data$day)) %>%
        dplyr::select(.data$asset, .data$date, .data$price)

    return(data)
}
