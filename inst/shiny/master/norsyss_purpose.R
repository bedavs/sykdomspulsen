

norsyss_purpose_ui <- function(id, config) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width=2,
        p("")
      ),
      column(
        width=8, align="center",
        br(),br(),br(),
        p("NorSySS (Norwegian Syndromic Surveillance System) er et overvåkningssystem basert på diagnosekoder (ICPC-2 koder) satt på legekontor og legevakter i hele Norge."),
        p("Formålet med NorSySS er å se trender og utbredelse av smittsomme sykdommer slik at utbrudd oppdages så tidlig som mulig. I tillegg kan overvåkningen brukes til å vurdere effekt av folkehelsetiltak."),
        p("Diagnosekoder som registreres hos lege eller legevakt sendes til Helsedirektoratet som en del av legenes refusjonskrav (KUHR-systemet). Folkehelseinstituttet mottar daglig oppdatert KUHR-data til Sykdomspulsen. Dataene er anonyme uten pasientidentifikasjon, men med informasjon om kjønn, aldersgruppe, konsultasjonsdato og sted for konsultasjon."),
        p("Geografisk område basert på stedet for legekonsultasjon, ikke pasientens bosted."),
        p("Fra 08.03.2020 begynte helsevesnet å bruke ICPC-2 koden R99.1 til COVID-19."),
        br()
      ),
      column(
        width=2,
        p("")
      )
    )
  )
}

norsyss_purpose_server <- function(input, output, session, config) {

}

