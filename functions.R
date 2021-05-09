list_available <- function(
    data,
    variable
) {
    data %>%
        dplyr::pull({{ variable }}) %>%
        unique() %>%
        sort() %>%
        return()
}

base_theme <- function(
    # Empty
) {
    theme <- list(
        ggplot2::scale_size_manual(values = vctrs::vec_c(
            "{asset_types$benchmark}"    := 3,
            "{asset_types$simple_asset}" := 0.5,
            "{asset_types$portfolio}"    := 2
        )),
        ggplot2::scale_linetype_manual(values = vctrs::vec_c(
            "{asset_types$benchmark}"    := "dotted",
            "{asset_types$simple_asset}" := "solid",
            "{asset_types$portfolio}"    := "solid"
        )),
        ggplot2::guides(color = guide_legend(override.aes = list(size = 3))),
        ggplot2::theme_bw(base_size = 20),
        ggplot2::theme(
            legend.position = "bottom",
            legend.background = element_rect(color = "black", size = 0.5)
        )
    )

    return(theme)
}
