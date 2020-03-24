## app.R ##
library(shiny)

shinyOptions(cache = diskCache("/tmp/", max_size = 50e6, max_age = 5))

assign("dev_invalidate_cache", lubridate::now(), envir = .GlobalEnv)

source("global.R")
source("norsyss.R")
source("covid19.R")

ui <- navbarPage(
  "Sykdomspulsen intern",
  tabPanel("NorSySS",
    norsyss_ui("norsyss", config=config)
  ),
  tabPanel("COVID-19",
    covid19_ui("covid19", config=config)
  )
)

ui <- tagList(
  tags$style("
  .container{
    width:100%;
    min-width: 1300px;
    }
 "),
  tags$div(class="container",
           navbarPage(
             title = "Sykdomspulsen intern",
             tabPanel("NorSySS",
                      norsyss_ui("norsyss", config=config)
             ),
             tabPanel("COVID-19",
                      covid19_ui("covid19", config=config)
             ),
             theme = "fhi.css"
           )
  )
)

server <- function(input, output) {
  callModule(norsyss_server, "norsyss", config=config)
  callModule(covid19_server, "covid19", config=config)
}


shinyApp(ui, server)

#  shiny::runApp('inst/shiny/corona', port = 4989, host = "0.0.0.0")





















