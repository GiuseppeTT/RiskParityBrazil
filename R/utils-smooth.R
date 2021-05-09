smooth <- function(
    x,
    old_weight
) {
    backcast <-
        x %>%
        rev() %>%
        purrr::reduce(update_smooth, old_weight = old_weight) %>%
        return()

    x %>%
        purrr::accumulate(update_smooth, old_weight = old_weight, .init = backcast) %>%
        magrittr::extract(-1) %>%
        return()
}

update_smooth <- function(
    old_smooth,
    current_value,
    old_weight
) {
    if (is.na(current_value))
        current_smooth <-  old_smooth
    else
        current_smooth <- old_weight * old_smooth + (1 - old_weight) * current_value

    return(current_smooth)
}
