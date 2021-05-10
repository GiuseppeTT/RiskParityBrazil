server <- function(
    input,
    output,
    session
) {
    data_pack <- reactive({
        complemented_data %>%
            pack_data(input$benchmark, input$assets, input$simulate_portfolio) %>%
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

        analysis_data <- tidy_switch(
            input$investment_type,
            "{investment_types$lump_sum}" := analysis_data,
            "{investment_types$dca}" := dca_return(analysis_data),
        )

        return(analysis_data)
    })

    output$summary_table <- render_gt({
        table_data <-
            analysis_data()

        table <-
            table_data %>%
            table_metrics()

        # TODO: refactor columns argument once {tidyselect} is available for {gt}
        table <-
            table %>%
            gt(rowname_col = "Asset") %>%
            fmt_percent(everything()) %>%
            tab_style(
                style = cell_text(weight = "bold"),
                locations = cells_column_labels(everything())
            ) %>%
            tab_options(
                table.border.top.style = "hidden",
                table.width = "100%"
            )

        return(table)
    })

    output$analysis_plot <- renderPlot({
        plot_data <-
            analysis_data()

        # TODO: this is computing everything!!
        plot <- tidy_switch(
            input$y_variable,
            "{y_variables$price}" := plot_price(
                plot_data,
                y_scale = input$y_scale
            ),
            "{y_variables$dca_multiple}" := plot_dca_multiple(
                plot_data,
                y_scale = input$y_scale
            ),
            "{y_variables$rolling_cagr}" := plot_rolling_cagr(
                plot_data,
                window_size = yearly_window_size,
                y_scale = input$y_scale
            ),
            "{y_variables$smooth_volatility}" := plot_smooth_volatility(
                plot_data,
                smoothing_factor = weekly_smoothing_factor,
                y_scale = input$y_scale
            ),
            "{y_variables$drawdown}":= plot_drawdown(
                plot_data,
                y_scale = input$y_scale
            ),
            "{y_variables$smooth_correlation}" := plot_smooth_correlation(
                plot_data,
                smoothing_factor = monthly_smoothing_factor,
                y_scale = input$y_scale
            ),
            "{y_variables$theoretical_weight}" := plot_theoretical_weight(
                plot_data,
                window_size = yearly_window_size,
                y_scale = input$y_scale
            )
        )

        return(plot)
    })
}
