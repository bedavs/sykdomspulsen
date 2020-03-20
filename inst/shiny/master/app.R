## app.R ##
library(shiny)

shinyOptions(cache = diskCache("/tmp/", max_size = 50e6))

source("global.R")
source("norsyss.R")
source("covid19.R")

ui <- navbarPage(
  "Sykdomspulsen",
  tabPanel("NorSySS",
    norsyss_ui("norsyss", config=config)
  ),
  tabPanel("COVID-19",
    covid19_ui("covid19", config=config)
  )
)

server <- function(input, output) {
  callModule(norsyss_server, "norsyss", config=config)
  callModule(covid19_server, "covid19", config=config)
}


shinyApp(ui, server)

#  shiny::runApp('inst/shiny/corona', port = 4989, host = "0.0.0.0")



