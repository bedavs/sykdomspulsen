fill_in_missing <- function(d){
  if(!"granularity_geo" %in% names(d)){
    d[, granularity_geo:=dplyr::case_when(
      stringr::str_detect(location_code,"municip") ~ "municip",
      stringr::str_detect(location_code,"county") ~ "county",
      stringr::str_detect(location_code,"norge") ~ "nation",
    )]
  }
  if(!"border" %in% names(d)){
    d[, border:=config$border]
  }
  if(!"age" %in% names(d)){
    d[, age := "totalt"]
  }
  if(!"sex" %in% names(d)){
    d[,sex := "totalt"]
  }
  if(!"yrwk" %in% names(d)){
    d[,yrwk := fhi::isoyearweek(date)]
  }
  if(!"date" %in% names(d)){
    d[
      fhidata::days,
      on="yrwk",
      date:=sun
    ]
  }
  if(!"season" %in% names(d)){
    d[, season := fhi::season(yrwk)]
  }
  if(!"year" %in% names(d)){
    d[, year := fhi::isoyear_n(date)]
  }
  if(!"week" %in% names(d)){
    d[, week := fhi::isoweek_n(date)]
  }
  if(!"x" %in% names(d)){
    d[, x := fhi::x(week)]
  }
}

# data_covid19_model
#
# Get and clean model data
#
#  @import data.table
#
data_covid19_model <- function(data, argset, schema){
  # tm_run_task("data_covid19_model")
  # data <- tm_get_data("data_covid19_model")
  # argset <- tm_get_argset("data_covid19_model")
  # schema <- tm_get_schema("data_covid19_model")

  file <- fs::dir_ls("/input/covid19/", regexp="modelling_[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9].csv")
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




