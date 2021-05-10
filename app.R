library(tidyverse)
library(targets)
library(gt)

library(shiny)
# library(shinydashboard)
# library(shinydashboardPlus)
library(bs4Dash)

devtools::load_all()

# TODO: shiny modules
source("functions.R")
source("global.R")
source("ui.R")
source("server.R")

app <- shinyApp(ui, server)

runApp(app, port = 8080)
