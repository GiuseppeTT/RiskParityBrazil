#' @title
#' Download data from the Central Bank of Brazil (BCB)
#'
#' @description
#' Download macro and index data from the Central Bank of Brazil (BCB).
#'
#' @param config
#' A list of lists containing the BCB data arguments. They are `asset` (asset
#' name), `series` (series number, as per sgs api) and `unit` (asset unit,
#' either return or price).
#'
#' @details
#' This function downloads the data directly from the [sgs api](
#' https://dadosabertos.bcb.gov.br/dataset). You can check the available series
#' manually at [sgs site](https://www3.bcb.gov.br/sgspub/).
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
download_bcb_data <- function(
    config
) {
    message("Downloading BCB data")

    config <-
        config %>%
        dplyr::bind_rows()

    data <-
        config %>%
        purrr::pmap(download_each_bcb_data) %>%
        dplyr::bind_rows()

    data <-
        data %>%
        clean_downloaded_data()

    return(data)
}

download_each_bcb_data <- function(
    asset,
    series,
    unit
) {
    message(glue("    Downloading asset `{asset}`"))

    url <- glue("http://api.bcb.gov.br/dados/serie/bcdata.sgs.{series}/dados?formato=json")

    response <-
        url %>%
        httr::RETRY("GET", .)

    data <-
        response %>%
        httr::content() %>%
        dplyr::bind_rows()

    data <-
        data %>%
        dplyr::rename(date = .data$data, "{unit}" := .data$valor) %>%
        dplyr::mutate(asset = {{ asset }}) %>%
        dplyr::mutate(date = lubridate::dmy(.data$date)) %>%
        dplyr::mutate("{unit}" := as.numeric(.data[[unit]]))

    data <-
        data %>%
        standardize_each_bcb_data(unit)

    return(data)
}

standardize_each_bcb_data <- function(
    data,
    unit
) {
    unit %>%
        switch(
            return = convert_percent_return_to_return(data),
            price  = convert_price_to_return(data),
            stop(glue("Unit `{unit}` is not supported"))
        ) %>%
        return()
}
