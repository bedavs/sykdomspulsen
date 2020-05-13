
#' data_covid19_model
#'
#' Get and clean model data
#'
#' @param data a
#' @param argset a
#' @param schema a
#' @export
data_covid19_model <- function(data, argset, schema){
  # tm_run_task("data_covid19_model")
  # data <- tm_get_data("data_covid19_model")
  # argset <- tm_get_argset("data_covid19_model")
  # schema <- tm_get_schema("data_covid19_model")

  if(!fs::dir_exists(sc::path("input","sykdomspulsen_covid19_dagsrapport_input"))){
    fs::dir_create(sc::path("input","sykdomspulsen_covid19_dagsrapport_input"))
  }
  file <- fs::dir_ls(sc::path("input","sykdomspulsen_covid19_dagsrapport_input"), regexp="modelling_[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9].csv")
  file <- max(file)
  d <- fread(file)

  setnames(
    d,
    old = c(
      "prev_mean",
      "prev_l95",
      "prev_u95",

      "incidence",
      "incidence_l95",
      "incidence_u95",

      "hosp_prev_mean",
      "hosp_prev_l95",
      "hosp_prev_u95",

      "icu_prev_mean",
      "icu_prev_l95",
      "icu_prev_u95"
    ),
    new = c(
      "infectious_prev_est",
      "infectious_prev_thresholdl0",
      "infectious_prev_thresholdu0",

      "incidence_est",
      "incidence_thresholdl0",
      "incidence_thresholdu0",

      "hosp_prev_est",
      "hosp_prev_thresholdl0",
      "hosp_prev_thresholdu0",

      "icu_prev_est",
      "icu_prev_thresholdl0",
      "icu_prev_thresholdu0"
    )
  )

  d[, granularity_time := "day"]
  fill_in_missing(d)

  schema$output$db_drop_table()
  schema$output$db_connect()
  schema$output$db_drop_constraint()
  schema$output$db_load_data_infile(d)
  schema$output$db_add_constraint()
}




