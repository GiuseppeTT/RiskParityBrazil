#' @title
#' Relativize return
#'
#' @description
#' TODO
#'
#' @param data
#' TODO
#'
#' @return
#' TODO
#'
#' @export
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
