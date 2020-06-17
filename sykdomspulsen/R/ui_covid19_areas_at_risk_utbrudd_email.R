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
    "Please find attached the current week's results.<br><br>",
    "Sincerely,<br><br>",
    "Sykdompulsen Team"
  )

  mailr(
      subject = glue::glue("Covid-19 areas at risk {lubridate::today()}"),
      html = html,
      to = e_emails(
        "utbrudd"
      ),
      attachments = filepath
    )
}

