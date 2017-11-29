# 10/29/2016. Author: Manu Garcia-Quismondo

#if (interactive()) {
#    shinyApp(ui, server)
#}
library(shiny)
library(shinyjs)
source("AppRoutes.R")
setwd(source.route)
# source("AppRoutes.R")
# source("createHistogramTable.R")
setwd(source.route)
source("loadTables.R")
setwd(source.route)
source("checkOrProcessTable.R")
setwd(source.route)
source("ui.R")
source("server.R")
setwd(source.route)
app=shinyApp(ui=ui, server=server)
runApp(app, launch.browser = TRUE)
setwd(source.route)