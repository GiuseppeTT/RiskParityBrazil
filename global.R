complemented_data <- tar_read(complemented_data)

available_assets <-
    complemented_data %>%
    list_available(asset)

yearly_window_size <- days_per$year
monthly_smoothing_factor <- compute_smoothing_factor(half_life = days_per$month)
weekly_smoothing_factor <- compute_smoothing_factor(half_life = days_per$week)
