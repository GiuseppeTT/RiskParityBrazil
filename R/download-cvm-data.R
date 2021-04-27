#' @title
#' Download data from the Securities and Exchange Commission of Brazil (CVM)
#'
#' @description
#' Download fund data from the Securities and Exchange Commission of Brazil
#' (CVM).
#'
#' @param config
#' A list of lists containing the CVM data arguments. They are `asset` (asset
#' name) and `cnpj` (fund ID, as per daily report api).
#'
#' @details
#' This function downloads the data directly from the [daily report api](
#' http://dados.cvm.gov.br/dataset/fi-doc-inf_diario).
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
download_cvm_data <- function(
    config
) {
    message("Downloading CVM data")

    urls <- list_all_cvm_data_urls()

    config <-
        config %>%
        dplyr::bind_rows()

    cnpjs <-
        config %>%
        dplyr::pull(.data$cnpj)

    data <-
        urls %>%
        purrr::map(download_each_cvm_data, cnpjs = cnpjs) %>%
        dplyr::bind_rows()

    data <-
        data %>%
        add_asset_names(config) %>%
        convert_price_to_return() %>%
        clean_downloaded_data()

    return(data)
}

list_all_cvm_data_urls <- function(
    # Empty
) {
    hist_base_url <- "http://dados.cvm.gov.br/dados/FI/DOC/INF_DIARIO/DADOS/HIST/"

    hist_urls <-
        hist_base_url %>%
        list_cvm_data_urls()

    recent_base_url <- "http://dados.cvm.gov.br/dados/FI/DOC/INF_DIARIO/DADOS/"

    recent_urls <-
        recent_base_url %>%
        list_cvm_data_urls()

    urls <- c(
        hist_urls,
        recent_urls
    )

    return(urls)
}

list_cvm_data_urls <- function(
    base_url
) {
    urls <-
        base_url %>%
        rvest::read_html() %>%
        rvest::html_nodes(".indexcolname") %>%
        rvest::html_node("a") %>%
        rvest::html_attr("href")

    urls <-
        urls %>%
        stringr::str_subset("\\.(csv|zip)$") %>%
        stringr::str_c(base_url, .)

    return(urls)
}

download_each_cvm_data <- function(
    url,
    cnpjs
) {
    period <-
        url %>%
        extract_url_period()

    message(glue("    Downloading period `{period}`"))

    path <- fs::file_temp()

    response <-
        url %>%
        httr::GET(httr::write_disk(path))

    file_type <-
        url %>%
        extract_url_file_type()

    data <-
        file_type %>%
        switch(
            zip = unzip_and_read_each_cvm_data(path, cnpjs),
            csv = read_each_cvm_data(path, cnpjs),
            stop(glue("File type `{file_type}` is not supported"))
        )

    return(data)
}

extract_url_period <- function(
    url
) {
    url %>%
        stringr::str_match("(\\d+)\\.(zip|csv)$") %>%
        magrittr::extract(2) %>%
        return()
}

extract_url_file_type <- function(
    url
) {
    url %>%
        stringr::str_match("\\.(zip|csv)$") %>%
        magrittr::extract(2) %>%
        return()
}

unzip_and_read_each_cvm_data <- function(
    path,
    cnpjs
) {
    path %>%
        utils::unzip(exdir = tempdir()) %>%
        purrr::map(read_each_cvm_data, cnpjs = cnpjs) %>%
        return()
}

read_each_cvm_data <- function(
    path,
    cnpjs
) {
    column_types <- readr::cols_only(
        CNPJ_FUNDO = readr::col_character(),
        DT_COMPTC  = readr::col_date("%Y-%m-%d"),
        VL_QUOTA   = readr::col_double()
    )

    path %>%
        readr::read_delim(delim = ";", col_types = column_types, progress = FALSE) %>%
        dplyr::rename(cnpj = .data$CNPJ_FUNDO, date = .data$DT_COMPTC, price = .data$VL_QUOTA) %>%
        dplyr::filter(.data$cnpj %in% cnpjs) %>%
        return()
}

add_asset_names <- function(
    data,
    config
) {
    data %>%
        dplyr::left_join(config, by = "cnpj") %>%
        return()
}
