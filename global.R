data <- tar_read(complemented_data)

available_assets <-
    data %>%
    list_available(asset)

window_size <- trading_days_per$year
smoothing_factor <- RiskParityBrazil:::compute_smoothing_factor(half_life = trading_days_per$month)
