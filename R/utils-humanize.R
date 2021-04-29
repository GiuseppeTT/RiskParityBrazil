humanize_column_names <- function(
    data
) {
    humane_column_names <-
        data %>%
        colnames() %>%
        humanize_string()

    data %>%
        magrittr::set_colnames(humane_column_names) %>%
        return()
}

humanize_plot_labels <- function(
    plot
) {
    humane_plot_labels <-
        plot %>%
        magrittr::extract2("labels") %>%
        purrr::map(humanize_string)

    plot %>%
        magrittr::inset2("labels", humane_plot_labels) %>%
        return()
}

humanize_string <- function(
    string
) {
    string %>%
        stringr::str_split(r"([_\. ])") %>%
        purrr::map(fix_case) %>%
        purrr::map(stringr::str_c, collapse = " ") %>%
        purrr::simplify() %>%
        return()
}

fix_case <- function(
    string
) {
    string %>%
        purrr::map_if(~ .x != stringr::str_to_upper(.x), stringr::str_to_sentence) %>%
        return()
}
