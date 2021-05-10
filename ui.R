header <- dashboardHeader(title = "Risk Parity Brazil"
)

sidebar <- dashboardSidebar(collapsed = TRUE, minified = FALSE, expandOnHover = FALSE, sidebarMenu(
    menuItem(tabName = "summary",
        text = "Summary",
        icon = icon("table"),
        selected = TRUE
    ),
    menuItem(tabName = "analysis",
        text = "Analysis",
        icon = icon("chart-line")
    )
))

control_bar <- dashboardControlbar(controlbarMenu(
    controlbarItem(title = "Data",
        icon = icon("database"),
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
        radioButtons(inputId = "investment_type",
            label = "Return:",
            choices = unname(investment_types),
            selected = investment_types$lump_sum,
            inline = TRUE
        ),
        checkboxInput(inputId = "simulate_portfolio",
            label = "Simulate portfolio",
            value = FALSE
        )
    ),
    controlbarItem(title = "Plot",
        icon = icon("chart-line"),
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
))

body <- dashboardBody(tabItems(
    tabItem(tabName =  "summary", fluidRow(box(width = 12,
        tags$style(".card-header {display: none !important}"),
        gt_output(outputId = "summary_table"
        )
    ))),
    tabItem(tabName = "analysis", fluidRow(box(width = 12,
        tags$style(".card-header {display: none !important}"),
        # https://stackoverflow.com/questions/36469631/how-to-get-leaflet-for-r-use-100-of-shiny-dashboard-height
        # 56 header, 30 body, 40 box, 17 ???
        tags$style(type = "text/css", "#analysis_plot {height: calc(100vh - 56px - 30px - 40px - 17px) !important}"),
        plotOutput(outputId = "analysis_plot"
        )
    )))
))

footer <- NULL

ui <- dashboardPage(
    header = header,
    sidebar = sidebar,
    controlbar = control_bar,
    body = body,
    footer = footer,
    dark = NULL
)
