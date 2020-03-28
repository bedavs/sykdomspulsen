

norsyss_ui <- function(id, config) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width=12, align="left",
        p(
          id="toptext",
          strong("NorSySS (Norwegian Syndromic Surveillance System)"),
          " er et overvåkningssystem basert på",
          br(),
          "diagnosekoder (ICPC-2 koder) satt på legekontor og legevakter i hele Norge."
          )
      )
    ),
    tabsetPanel(
      tabPanel(title="Oversikt",
               norsyss_overview_ui("norsyss_overview", config=config)
      ),
      tabPanel(
        title="Formålet",
        norsyss_purpose_ui("norsyss_purpose", config = config)
      )
    )
  )
}

norsyss_server <- function(input, output, session, config) {
}

