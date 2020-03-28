## app.R ##
library(shiny)
library(shinyjs)

if (.Platform$OS.type == "windows"){
  assign("dev_invalidate_cache", lubridate::now(), envir = .GlobalEnv)
} else {
  shinyOptions(cache = diskCache("/tmp/", max_size = 50e6, max_age = 60*60)) # 1 hour
  assign("dev_invalidate_cache", 1, envir = .GlobalEnv)
}


source("global.R")
source("dagsrapport.R")

ui <- tagList(
  useShinyjs(),
  tags$style("
  .container{
    width: 1200px;
    }
 "),
  tags$div(class="container",
           navbarPage(
             title = div(img(id="logo",src="fhi.svg", height="40px"), "Sykdomspulsen"),
             tabPanel("Dagsrapport",
                      dagsrapport_ui("dagsrapport", config=config)
             ),
             theme = "fhi.css"
           )
  )
)

server <- function(input, output) {
  callModule(dagsrapport_server, "dagsrapport", config=config)
}


shinyApp(ui, server)

#  shiny::runApp('inst/shiny/corona', port = 4989, host = "0.0.0.0")




