zero_first <- function(
    x
) {
    x %>%
        magrittr::inset2(1, 0) %>%
        return()
}
