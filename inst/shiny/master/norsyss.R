

norsyss_ui <- function(id, config) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width=12, align="left",
        p(
          id="toptext",
          strong("NorSySS er forkortelsen for Norwegian Syndromic Surveillance System som er en del av Sykdomspulsen."), br(),
          "Dette er et overvåkningssystem basert på diagnosekoder (ICPC-2 koder) satt på legekontor og legevakt i hele Norge.", br(), br(),

          "Formålet med NorSySS er å se trender og utbredelse av smittsomme sykdommer slik at utbrudd oppdages så tidlig som mulig. ",
          "I tillegg kan overvåkningen brukes til å vurdere effekt av folkehelsetiltak.", br(), br(),

          "Foreløpig er det mulig å se på to forskjellige symptomer/syndromer gjennom NorSySS, men ",
          "dette vil utvides til å inkludere flere etter hvert.", br(), br(),

          "Mage-tarminfeksjoner som er en samlebetegnelse for ICPC-2 kodene Diare (D11), Tarminfeksjon (D70) ",
          "og Gastroenteritt antatt infeksiøs (D73).", br(), br(),

          "Luftveisinfeksjoner som er en samlebetegnelse for Hoste (R05), Akutt øvre luftveisinfeksjon (R74), ",
          "Akutt bronkitt/bronkiolitt (R78) og Luftveisinfeksjon IKA (R83).", br(), br(),

          "Under kan du velge blant tre faner som gir deg forskjellig informasjon:", br(),
          "- Oversikt fanen vil gi deg en rekke grafer or tabeller hvor du kan velge det geografiske området du er interessert i", br(),
          "- Dashboard fanen vil gi deg en interaktiv graf der du kan velge geografisk område og der du kan zoome inn på spesifikke områder av tidslinjen.", br(),
          "- Informasjon fanen gir deg litt mer informasjon om dataene vi bruker", br()
          )
      )
    ),
    tabsetPanel(
      tabPanel(
        title="Oversikt",
        norsyss_overview_ui("norsyss_overview", config=config)
      ),
      tabPanel(
        title="Dashboard",
        norsyss_weekly_ui("norsyss_weekly", config=config)
      ),
      # tabPanel(
      #   title="Daglig",
      #   norsyss_daily_ui("norsyss_daily", config=config)
      # ),
      tabPanel(
        title="Informasjon",
        norsyss_purpose_ui("norsyss_purpose", config = config)
      )
    )
  )
}

norsyss_server <- function(input, output, session, config) {

}

