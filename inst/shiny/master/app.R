## app.R ##
library(shiny)
library(shinyjs)

if (.Platform$OS.type == "windows"){
} else {
  shinyOptions(cache = diskCache("/tmp/", max_size = 50e6, max_age = 60*60)) # 1 hour
}

assign("dev_invalidate_cache", lubridate::now(), envir = .GlobalEnv)
#assign("dev_invalidate_cache", 1, envir = .GlobalEnv)

source("global.R")
source("covid19.R")
source("norsyss.R")
source("norsyss_overview.R")
source("norsyss_purpose.R")

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
  useShinyjs(),
  tags$style("
  .container{
    width: 1200px;
    }
 "),
  tags$div(class="container",
           navbarPage(
             title = div(img(id="logo",src="fhi.svg", height="40px"), "Sykdomspulsen"),
             tabPanel("COVID-19",
                      covid19_ui("covid19", config=config)
             ),
             tabPanel("NorSySS",
                      norsyss_ui("norsyss", config=config)
             ),
             theme = "fhi.css"
           )
  )
)

server <- function(input, output) {
  callModule(covid19_server, "covid19", config=config)

  callModule(norsyss_server, "norsyss", config=config)
  callModule(norsyss_overview_server, "norsyss_overview", config=config)
  callModule(norsyss_purpose_server, "norsyss_purpose", config=config)
}


shinyApp(ui, server)

#  shiny::runApp('inst/shiny/corona', port = 4989, host = "0.0.0.0")




