as.xts.data.frame <- function(
    data,
    index
) {
    index_values <-
        data %>%
        dplyr::pull({{ index }})

    data %>%
        dplyr::select(! {{ index }}) %>%
        xts::xts(order.by = index_values) %>%
        return()
}

as_tibble.xts <- function(
    data,
    index
) {
    data %>%
        zoo::fortify.zoo() %>%
        tibble::as_tibble() %>%
        dplyr::rename(index = .data$Index) %>%
        return()
}
