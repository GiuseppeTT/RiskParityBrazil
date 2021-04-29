library(targets)
library(tarchetypes)

tar_option_set(
    packages = "RiskParityBrazil",
    imports = "RiskParityBrazil",
    format = "qs",
    memory = "transient",
    garbage_collection = TRUE
)

download_targets <- list(
    list(
        tar_file(
            bcb_config_path,
            here::here("config/download-bcb.yaml")
        ),
        tar_target(
            bcb_config,
            yaml::read_yaml(bcb_config_path)
        ),
        tar_target(
            bcb_data,
            download_bcb_data(bcb_config)
        )
    ),
    list(
        tar_file(
            cvm_config_path,
            here::here("config/download-cvm.yaml")
        ),
        tar_target(
            cvm_config,
            yaml::read_yaml(cvm_config_path)
        ),
        tar_target(
            cvm_data,
            download_cvm_data(cvm_config)
        )
    ),
    list(
        tar_file(
            b3_config_path,
            here::here("config/download-b3.yaml")
        ),
        tar_target(
            b3_config,
            yaml::read_yaml(b3_config_path)
        ),
        tar_target(
            b3_data,
            download_b3_data(b3_config)
        )
    )
)

process_targets <- list(
    tar_target(
        combined_data,
        combine_data(
            bcb_data,
            cvm_data,
            b3_data
        )
    ),
    list(
        tar_file(
            complement_config_path,
            here::here("config/complement.yaml")
        ),
        tar_target(
            complement_config,
            yaml::read_yaml(complement_config_path)
        ),
        tar_target(
            complemented_data,
            complement_data(combined_data, complement_config)
        )
    ),
    list(
        tar_file(
            pack_config_path,
            here::here("config/pack.yaml")
        ),
        tar_target(
            pack_config,
            yaml::read_yaml(pack_config_path),
            iteration = "list"
        ),
        tar_target(
            packed_data,
            pack_data(
                complemented_data,
                pack_config$benchmark,
                pack_config$assets,
                pack_config$simulate_portfolio
            ),
            pattern = map(pack_config),
            iteration = "list"
        )
    )
)

analysis_targets <- list(
    tar_target(
        analysis,
        analyze_data(packed_data),
        pattern = map(packed_data),
        iteration = "list"
    )
)

dashboard_targets <- list(
    tar_render(
        index,
        here::here("Rmd/index.Rmd"),
        output_file = here::here("output/github-pages/index.html")
    ),
    tar_target(
        output_file,
        here::here("output/github-pages/dashboard", name_dashboard_file(pack_config$name)),
        pattern = map(pack_config),
        iteration = "vector"
    ),
    tar_render_rep(
        dashboard,
        here::here("Rmd/dashboard.Rmd"),
        params = tibble::tibble(
            analysis = analysis,
            output_file = output_file
        )
    )
)

list(
    download_targets,
    process_targets,
    analysis_targets,
    dashboard_targets
)
