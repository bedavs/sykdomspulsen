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

    schema$output$db_field_types
  }

  # cases ----
  d <- data$data[tag_outcome %in% c("cases","tests")]

  setnames(d,"n","n_lag0")
  setorder(d, location_code, tag_outcome, yrwk)
  d[, n_lag1 := shift(n_lag0), by=.(location_code, tag_outcome)]
  d[, n_lag0_1 := n_lag0 + n_lag1]

  d <- dcast.data.table(
    d,
    location_code+yrwk+pop ~ tag_outcome,
    value.var = c("n_lag0", "n_lag1", "n_lag0_1")
  )

  d[, n_cases_lag0 := n_lag0_cases]
  d[, pr100000_cases_lag0 := 100000 * n_lag0_cases / pop]
  d[, pr100000_cases_lag1 := 100000 * n_lag1_cases / pop]
  d[, pr100000_cases_lag0_1 := 100000 * n_lag0_1_cases / pop]

  d[, denom_tests_lag0 := n_lag0_tests]
  d[, pr100_tests_lag0 := 100 * n_lag0_cases / n_lag0_tests]
  d[, pr100_tests_lag1 := 100 * n_lag1_cases / n_lag1_tests]
  d[, pr100_tests_lag0_1 := 100 * n_lag0_1_cases / n_lag0_1_tests]

  d[, granularity_time := "week"]

  fill_in_missing(d)

  schema$output$db_upsert_load_data_infile(d)
}

