tidy_switch <- function(
    expression_,
    ...
) {
    expression_ <- list(expression_)
    dots <- rlang::list2(...)

    result <- do.call(switch, c(expression_, dots))

    return(result)
}
