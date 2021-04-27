compute_return <- function(
    price
) {
    return_ <- price / dplyr::lag(price) - 1

    return(return_)
}
