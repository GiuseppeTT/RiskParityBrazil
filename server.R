server <- function(
    input,
    output,
    session
) {
    data_pack <- reactive({
        data %>%
            RiskParityBrazil::pack_data(input$benchmark, input$assets, input$simulate_portfolio) %>%
            return()
    })

    observeEvent(data_pack(), {
        dates <-
            data_pack() %>%
            pull(date)

        updateDateRangeInput(
            session = session,
            inputId = "period",
            start = min(dates),
            end = max(dates),
            min = min(dates),
            max = max(dates)
        )
    })

    analysis_data <- reactive({
        analysis_data <-
            data_pack() %>%
            filter(input$period[1] <= date, date <= input$period[2])

        analysis_data <- tidy_switch(
            input$return_type,
            "{return_types$nominal}" := analysis_data,
            "{return_types$relative}" := relativize_return(analysis_data),
        )

        return(analysis_data)
    })

    output$summary_table <- render_gt({
        table_data <-
            analysis_data()

        table <-
            table_data %>%
            RiskParityBrazil:::table_metrics()

        # TODO: refactor columns argument once {tidyselect} is available for {gt}
        table <-
            table %>%
            gt() %>%
            fmt_percent(columns = colnames(table)[-1]) %>%
            cols_align(align = "left", columns = "Asset") %>%
            tab_options(table.width = "100%")

        return(table)
    })

    output$analysis_plot <- renderPlot({
        plot_data <-
            analysis_data()

        plot <- tidy_switch(
            input$y_variable,
            "{y_variables$price}" := plot_base(plot_data, input$y_variable, RiskParityBrazil:::compute_price),
            "{y_variables$dca_multiple}" := plot_base(plot_data, input$y_variable, RiskParityBrazil:::compute_dca_multiple),
            "{y_variables$rolling_cagr}" := plot_base(plot_data, input$y_variable, RiskParityBrazil:::compute_rolling_cagr, window_size = window_size),
            "{y_variables$smooth_volatility}" := plot_base(plot_data, input$y_variable, RiskParityBrazil:::compute_smooth_volatility, smoothing_factor = smoothing_factor),
            "{y_variables$drawdown}" := plot_base(plot_data, input$y_variable, RiskParityBrazil:::compute_drawdown)
        )

        labels <- scales::percent

        limits <- tidy_switch(
            input$y_variable,
            "{y_variables$drawdown}" := c(-1, 0),
            "{y_variables$smooth_correlation}" := c(-1, 1),
            "{y_variables$theoretical_weight}" := c(0, 1),
            NULL
        )

        scale_y <- tidy_switch(
            input$y_scale,
            "{y_scales$linear}" := scale_y_continuous,
            "{y_scales$log}" := scale_y_log10
        )

        plot <-
            plot +
            scale_y(labels = labels, limits = limits) +
            base_theme()

        plot <-
            plot %>%
            RiskParityBrazil:::humanize_plot_labels()

        return(plot)
    })
}
