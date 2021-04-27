filter_assets <- function(
    data,
    assets
) {
    data %>%
        dplyr::filter(.data$asset %in% assets) %>%
        enforce_common_range(date, by = .data$asset) %>%
        return()
}

enforce_common_range <- function(
    data,
    variable,
    by
) {
    common_range <-
        data %>%
        dplyr::group_by({{ by }}) %>%
        dplyr::summarise(min_var = min({{ variable }}), max_var = max({{ variable }})) %>%
        dplyr::summarise(min = max(.data$min_var), max = min(.data$max_var)) %>%
        dplyr::ungroup() %>%
        as.list()

    data <-
        data %>%
        dplyr::filter(common_range$min <= {{ variable }}, {{ variable }} <= common_range$max)

    return(data)
}
