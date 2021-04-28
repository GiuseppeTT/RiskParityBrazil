pair <- function(
    x,
    y,
    joiner = ""
) {
    x <-
        x %>%
        as.character()

    y <-
        y %>%
        as.character()

    list(x, y) %>%
        purrr::pmap(c) %>%
        purrr::map(sort) %>%
        purrr::map_chr(stringr::str_c, collapse = joiner) %>%
        return()
}
