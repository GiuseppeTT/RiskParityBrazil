#' @title
#' Cumulative DCA
#'
#' @description
#' Compute the cumulative DCA of a sequence X.
#'
#' @param xs
#' The sequence of values.
#'
#' @return
#' The cumulative DCA of the sequence X.
#'
#' @details
#' The cumulative DCA is described by the follwing equations.
#'
#' DCA\[1\] = X\[1\]
#'
#' DCA\[2\] = X\[1\] • X\[2\] + X\[2\]
#'
#' DCA\[3\] = X\[1\] • X\[2\] • X\[3\] + X\[2\] • X\[3\] + X\[3\]
#'
#' DCA\[N\] = X\[1\] • ... • X\[N\] + X\[2\] • ... • X\[N\] + ... + X\[N-1\] • X\[N\] + X\[N\]
#'
#' DCA\[n\] = sum(i = 1 to n) prod(j = i to n) X\[j\]
#'
#' DCA\[n\] = DCA\[n-1\] * X\[n\] + X\[n\]
cumulative_dca <- function(
    xs
) {
    dcas <- numeric(length(xs))
    dca <- 0
    for (i in seq_along(xs)) {
        x <- xs[i]

        dca <- (dca + 1) * x
        dcas[i] <- dca
    }

    return(dcas)
}
