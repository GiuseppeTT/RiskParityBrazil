header <- dashboardHeader(
    title = "RiskParityBrazil"
)

sidebar <- dashboardSidebar(
    sidebarMenu(id = "side_bar_menu",
        menuItem(tabName = "summary",
            text = "Summary",
            icon = icon("table"),
            selected = TRUE
        ),
        menuItem(tabName = "analysis",
            text = "Analysis",
            icon = icon("chart-line")
        )
    ),
    # Tight input layout
    tags$style(type = "text/css", ".form-group, .selectize-control {margin-bottom: 0; padding-bottom: 0 !important;}"),
    selectInput(inputId = "assets",
        label = "Assets:",
        choices = available_assets,
        multiple = TRUE,
        selected = c("IMA", "IBrA")
    ),
    selectInput(inputId = "benchmark",
        label = "Benchmark:",
        choices = available_assets,
        selected = "CDI"
    ),
    dateRangeInput(inputId = "period",
        label = "Period:"
    ),
    radioButtons(inputId = "return_type",
        label = "Return:",
        choices = unname(return_types),
        selected = return_types$nominal,
        inline = TRUE
    ),
    checkboxInput(inputId = "simulate_portfolio",
        label = "Simulate portfolio",
        value = FALSE
    ),
    conditionalPanel("input.side_bar_menu == 'analysis'",
        selectInput(inputId = "y_variable",
            label = "Y variable:",
            choices = unname(y_variables),
            selected = y_variables$price
        ),
        radioButtons(inputId = "y_scale",
            label = "Y scale:",
            choices = unname(y_scales),
            selected = y_scales$linear,
            inline = TRUE
        )
    )
)

body <- dashboardBody(tabItems(
    tabItem(tabName =  "summary",
        gt_output(outputId = "summary_table")
    ),
    tabItem(tabName = "analysis",
        # https://stackoverflow.com/questions/36469631/how-to-get-leaflet-for-r-use-100-of-shiny-dashboard-height
        tags$style(type = "text/css", "#analysis_plot {height: calc(100vh - 80px) !important;}"),
        plotOutput(outputId = "analysis_plot")
    )
))

ui <- dashboardPage(
    header,
    sidebar,
    body
)
