smooth <- function(
    x,
    old_weight
) {
    backcast <-
        x %>%
        rev() %>%
        purrr::reduce(~ old_weight * .x + (1 - old_weight) * .y) %>%
        return()

    x %>%
        purrr::accumulate(~ old_weight * .x + (1 - old_weight) * .y, .init = backcast) %>%
        magrittr::extract(-1) %>%
        return()
}
