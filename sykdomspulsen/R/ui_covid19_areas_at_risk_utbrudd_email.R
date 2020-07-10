#' ui_covid19_areas_at_risk_utbrudd_email
#' @param data a
#' @param argset a
#' @param schema a
#' @export
ui_covid19_areas_at_risk_utbrudd_email <- function(data, argset, schema) {
  # tm_run_task("ui_covid19_areas_at_risk_docx")
  # tm_run_task("ui_covid19_areas_at_risk_utbrudd_email")

  if(plnr::is_run_directly()){
    data <- sc::tm_get_data("ui_covid19_areas_at_risk_utbrudd_email", index_plan=1)
    argset <- sc::tm_get_argset("ui_covid19_areas_at_risk_utbrudd_email", index_plan=1, index_argset = 1)
    schema <- sc::tm_get_schema("ui_covid19_areas_at_risk_utbrudd_email")
  }
  file <- glue::glue(argset$file)
  folder <- sc::path("output",argset$folder, create_dir = T)

  filepath <- fs::path(folder, file)
  if(!fs::file_exists(filepath)) return()

  html <- glue::glue(
    "Vedlagt er dagens signaler for covid-19 fordelt på geografisk område og aldersgrupper.<br>",
    "Dataene er basert på antall tilfeller fra MSIS og antall konsultasjoner hos lege og legevakt fra NorSySS.<br><br>",

    "Signalsystemet bruker gjennomsnittet og 95% konfidensintervall for de to foregående ukene som en terskel for et signal.",
    " For eksempel brukes uke 2020-26 og 2020-27 som en basis for å beregne terskelverdi for uke 2020-28.<br><br>",

    "Hilsen,<br><br>",
    "Sykdompulsen teamet (Gry, Richard, Beatriz, Gunnar og Yusman)"
  )

  mailr(
      subject = glue::glue("Covid-19 areas at risk {lubridate::today()}"),
      html = html,
      to = e_emails(
        "covid19_areas_at_risk"
      ),
      attachments = filepath
    )
}

