zero_na <- function(
    x
) {
    x %>%
        tidyr::replace_na(0) %>%
        return()
}
