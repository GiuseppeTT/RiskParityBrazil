months.numeric <- function(
    x
) {
    x %>%
        lubridate::period(month = .) %>%
        return()
}

robust_make_date <- function(
    year,
    month,
    day
) {
    date <- stringr::str_c(year, month, day, sep = "-")

    date %>%
        lubridate::ymd() %>%
        return()
}
