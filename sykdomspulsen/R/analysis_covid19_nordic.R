#' analysis_covid19_nordic
#' @param data a
#' @param argset a
#' @param schema a
#' @export
analysis_covid19_nordic <- function(data, argset, schema) {
  # tm_run_task("analysis_covid19_nordic")

  if(plnr::is_run_directly()){
    sc::tm_update_plans("analysis_covid19_nordic")

    index_plan <- 1
    data <- sc::tm_get_data("analysis_covid19_nordic", index_plan=index_plan)
    argset <- sc::tm_get_argset("analysis_covid19_nordic", index_plan=index_plan, index_argset = 1)
    schema <- sc::tm_get_schema("analysis_covid19_nordic")
  }

  # cases ----
  d <- data$data[tag_outcome=="cases"]
  d[, average_2wks_pr100 := as.numeric(NA)]
  d[, average_2wks_pr100000 := as.numeric(NA)]
  d[, average_2wks_status := as.character(NA)]

  setorder(d, location_code, yrwk)
  d[ !is.na(n), n_status := "normal"]
  d[ pr100000 > 20, n_status := "high"]

  d[, average_2wks_pr100000 :=  (pr100000 + shift(pr100000)) / 2, by=.(location_code)]
  d[ !is.na(average_2wks_pr100000), average_2wks_status := "normal"]
  d[ average_2wks_pr100000 > 20, average_2wks_status := "high"]

  schema$output$db_upsert_load_data_infile(d)

  # tests ----
  d <- data$data[tag_outcome=="tests"]
  d[, average_2wks_pr100 := as.numeric(NA)]
  d[, average_2wks_pr100000 := as.numeric(NA)]
  d[, average_2wks_status := as.character(NA)]

  setorder(d, location_code, yrwk)
  d[ !is.na(n), n_status := "normal"]
  d[ pr100 > 5, n_status := "high"]

  d[, average_2wks_pr100 :=  (pr100 + shift(pr100)) / 2, by=.(location_code)]
  d[ !is.na(average_2wks_pr100), average_2wks_status := "normal"]
  d[ average_2wks_pr100 > 5, average_2wks_status := "high"]

  schema$output$db_upsert_load_data_infile(d)

  # icu ----
  d <- data$data[tag_outcome=="icu"]
  d[, average_2wks_pr100 := as.numeric(NA)]
  d[, average_2wks_pr100000 := as.numeric(NA)]
  d[, average_2wks_status := as.character(NA)]

  setorder(d, location_code, yrwk)
  d[, n_status := NA]

  schema$output$db_upsert_load_data_infile(d)

}

