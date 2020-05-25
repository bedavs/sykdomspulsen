#' ui_normomo_email_internal
#' @param data a
#' @param argset a
#' @param schema a
#' @export
ui_normomo_email_internal <- function(data, argset, schema) {
  if(FALSE){
    tm_run_task("ui_normomo_thresholds_1yr_5yr")
    tm_run_task("ui_normomo_overview_by_location")
    tm_run_task("ui_normomo_overview_by_age")
    tm_run_task("ui_normomo_table_overview")
    tm_run_task("ui_normomo_table_excess_only")

    tm_run_task("ui_normomo_email_internal")

    data <- tm_get_data("ui_normomo_email_internal", index_plan=1)
    argset <- tm_get_argset("ui_normomo_email_internal", index_plan=1, index_argset = 1)
    schema <- tm_get_schema("ui_normomo_email_internal")
  }

  tab1 <- glue::glue(argset$tab1)
  tab2 <- glue::glue(argset$tab2)
  fig1 <- glue::glue(argset$fig1)
  fig2 <- glue::glue(argset$fig2)
  fig3 <- glue::glue(argset$fig3)

  tab1_filepath <- sc::path("output", argset$tab1_filepath)
  tab2_filepath <- sc::path("output", argset$tab2_filepath)
  fig1_filepath <- sc::path("output", argset$fig1_filepath)
  fig2_filepath <- sc::path("output", argset$fig2_filepath)
  fig3_filepath <- sc::path("output", argset$fig3_filepath)


  html <- glue::glue(
    "<html>",
    "Resultater fra overv{fhi::nb$aa}kingssystemet for d{fhi::nb$oe}delighet (NorMOMO) er tilgjengelig p{fhi::nb$aa} <a href='file:///N:/sykdomspulsen_normomo_restricted_output/{argset$today}'>N:/sykdomspulsen_normomo_restricted_output/{argset$today}</a> (tilgangsbegrenset)<br><br>",
    "Her er nye resultater fra overv{fhi::nb$aa}kingssystemet for generell d{fhi::nb$oe}delighet i Norge (<a href='https://www.fhi.no/sv/influensa/influensaovervaking/overvakingssystem-for-dodelighet-eu/'>NorMOMO</a>).<br><br>",
    "NorMOMO er basert p{fhi::nb$aa} ukentlig oppdaterte anonyme data fra Folkeregisteret og analyseres ved bruk av <a href='http://www.euromomo.eu/methods/methods.html'>EuroMOMO-modellen</a>.<br><br>",
    "Under f{fhi::nb$oe}lger en oppsummering av forrige ukes resultater. Resultatene <span style='color:red'>er til intern bruk</span>, m{fhi::nb$aa} tolkes med varsomhet og kan justeres noe grunnet forsinkelse i rapporteringen av d{fhi::nb$oe}dsfall.<br><br><br>",
    "<b>Tabell 1.</b> Antall registrerte d{fhi::nb$oe}dsfall de 8 og 4 siste ukene og niv{fhi::nb$aa} av d{fhi::nb$oe}delighet.<br><br>",
    "<img src='cid:{tab1}' width='800' align='middle' style='display:block;width:100%;max-width:800px' alt=''><br><br>",
    "<b>Tabell 2.</b> Geografiske omr{fhi::nb$aa}de og aldersgrupper med h{fhi::nb$oe}yere enn forventet d{fhi::nb$oe}dsfall de siste 8 ukene.<br><br>",
    "<img src='cid:{tab2}' width='800' align='middle' style='display:block;width:100%;max-width:800px' alt=''><br><br>",
    "<b>Figur 1.</b> Totalt antall d{fhi::nb$oe}dsfall per uke det siste {fhi::nb$aa}ret ({fhi::nb$oe}verst) og de siste 5 {fhi::nb$aa}rene (nederst), alle aldersgrupper.<br><br>",
    "<img src='cid:{fig1}' width='800' align='middle' style='display:block;width:100%;max-width:800px' alt=''><br><br>",
    "<b>Figur 2.</b> Antall d{fhi::nb$oe}dsfall per uke det siste {fhi::nb$aa}ret fordelt p{fhi::nb$aa} fylke.<br><br>",
    "<img src='cid:{fig2}' width='800' align='middle' style='display:block;width:100%;max-width:800px' alt=''><br><br>",
    "<b>Figur 3.</b> Antall d{fhi::nb$oe}dsfall per uke det siste {fhi::nb$aa}ret fordelt p{fhi::nb$aa} aldersgruppe.<br><br>",
    "<img src='cid:{fig3}' width='800' align='middle' style='display:block;width:100%;max-width:800px' alt=''><br><br>",
    "</html>"
  )

  if(sc::config$permissions$ui_normomo_email_internal$has_permission()){
    mailr(
      subject = glue::glue("Resultater fra NorMOMO {argset$today}"),
      html = html,
      to = e_emails(
        "ui_normomo_results",
        is_final = sc::config$permissions$ui_normomo_email_internal$is_final()
      ),
      inlines = c(tab1_filepath, tab2_filepath, fig1_filepath, fig2_filepath, fig3_filepath),
      is_final = sc::config$permissions$ui_normomo_email_internal$is_final()
    )

    sc::config$permissions$ui_normomo_email_internal$revoke_permission()
  }
}
