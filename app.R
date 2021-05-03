library(tidyverse)
library(targets)
library(gt)

library(shiny)
library(shinydashboard)

# TODO: tidyr::complete and use 365 days for window_size

# TODO: shiny modules
source("functions.R")
source("global.R")
source("ui.R")
source("server.R")

app <- shinyApp(ui, server)

runApp(app, port = 8080)
