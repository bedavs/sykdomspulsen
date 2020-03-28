

norsyss_purpose_ui <- function(id, config) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width=2,
        p("")
      ),
      column(
        width=8, align="left",
        p(
          "Vi får data til denne overvåkingen via Sykdomspulsen. ",
          "Diagnosekoder som registreres hos lege eller legevakt sendes ",
          "til Helsedirektoratet som en del av legenes refusjonskrav (KUHR-systemet). ",
          "Folkehelseinstituttet mottar daglig oppdatert KUHR-data til Sykdomspulsen. ",
          "Dataene er anonyme når vi mottar dem, uten pasientidentifikasjon, men med ",
          "informasjon om kjønn, aldersgruppe, konsultasjonsdato og sted for konsultasjon.", br(), br(),

          strong("Informasjon om dataene vi bruker i NorSySS:"), br(),
          "- Både telefon og legekontakt er inkludert", br(),
          "- Legekontor og legevakt er inkludert", br(),
          "- Geografisk område basert på stedet for legekonsultasjon, ikke pasientens bosted", br(),
          "- De kommunene som ikke har legevakt eller legekontor er ikke med i listen ",
          "der man velger geografisk område da vi ikke har noe data om disse. ",
          "Personene som bor i kommuner uten lege og legevakt benytter legekontor ",
          "og legevakt i andre kommuner", br(),
          "- Antallet konsultasjoner er vanligvis lavere i ferier og på helligdager. ",
          "Dette er spesielt tydelig rundt jul/nyttår og påske, men også i ",
          "sommerferieukene.", br(),
          "- Det kan være 14 dager forsinkelse i dataene da de kommer fra KUHR ",
          "systemet. Dersom det for noen datoer ikke er registrert noen ",
          "konsultasjoner fra et geografisk område vil dette vises som røde ",
          "stiplede linjer i grafene.", br(), br(),

          strong("Fargekoder i grafene:"), br(),
          "- Bakgrunnsfargen er laget ut fra beregninger fra de foregående 5 ",
          "årene i samme geografiske område og samme sykdom/syndrom og ",
          "aldersgruppe (for årene 2006-2010 er 5 fremtidige år brukt).", br(),
          "- Blå bakgrunnsfarge viser at antallet konsultasjoner er som forventet", br(),
          "- Gul bakgrunnsfarge viser at antallet konsultasjoner er høyere enn forventet", br(),
          "- Rød bakgrunnsfarge viser at antall konsultasjoner er betydelig høyere enn forventet", br(),
          "- I grafer der det er en svart strek viser det antallet faktiske konsultasjoner. ",
          "Dersom denne streken er i det blå feltet er antallet konsultasjoner er som forventet, ",
          "om den er i det gule feltet er antallet konsultasjoner høyere enn forventet og om ",
          "den er i det røde feltet er antallet konsultasjoner betydelig høyere enn forventet ",
          "for gitt tidsrom, alder og geografisk område.", br(), br(),

          strong("Kommunereformen: "), "Kommuner som har blitt slått sammen og fått ",
          "et nytt navn vil ikke finnes i oversiktene. Kommuner som har blitt slått ",
          "sammen med en annen kommune men beholdt navnet vil vises i oversiktene, ",
          "og beregningene tar hensyn til sammenslåingen. Det samme gjelder ",
          "sammenslåtte kommuner som får nytt kommunenavn.", br(), br(),

          strong("Små kommuner: "), "Kommuner med under 500 innbyggere vil ikke ",
          "kunne se grafer for aldersgrupperinger, men bare 'totalt antall'. ",
          "Dette er av hensyn til personvern.", br(), br(),

          strong("Interkommunalt samarbeid om legekontor/legevakt: "),
          "I Sykdomspulsen er geografisk område basert på stedet for legekonsultasjon, ",
          "ikke pasientens bosted. Derfor vil legekontorets/legevaktens postadresse ",
          "si hvilken kommune som vises i Sykdomspulsen. De andre kommunene som er ",
          "med på det interkommunale samarbeidet vil ikke vises i Sykdomspulsen.", br(), br(),

          strong("Ved tekniske feil, spørsmål eller tilbakemeldinger "),
          "vennligst send en mail til sykdomspulsen@fhi.no"
        ),
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

