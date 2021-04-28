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
        ggplot2::theme_bw(base_size = 18),
        ggplot2::theme(legend.position = "none")
    )

    return(theme)
}
