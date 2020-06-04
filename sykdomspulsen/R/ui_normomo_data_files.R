#' ui_normomo_data_files
#' @param data a
#' @param argset a
#' @param schema a
#' @export
ui_normomo_data_files <- function(data, argset, schema) {
  if(plnr::is_run_directly()){
    # tm_run_task("ui_normomo_data_files")
    sc::tm_update_plans("ui_normomo_data_files")
    data <- sc::tm_get_data("ui_normomo_data_files", index_plan=1)
    argset <- sc::tm_get_argset("ui_normomo_data_files", index_plan=1, index_argset = 1)
    schema <- sc::tm_get_schema("ui_normomo_data_files")
  }

  d <- copy(data$data)

  # folder
  folder <- sc::path("output",argset$folder, create_dir = T)
  filename <- glue::glue(argset$filename)
  filepath <- fs::path(folder,filename)

  writexl::write_xlsx(
    d,
    filepath
  )

  d <- copy(data$data)
  d[,is_less_than_5:=n_obs<5]
  xtabs(~is_less_than_5+age,data=d)
  xtabs(~is_less_than_5+age,data=d[location_code %in% c(
    "county03",
    "county30"
  )])
  xtabs(~is_less_than_5+age,data=d[location_code %in% c(
    "county03",
    "county30"
  )])
  d[age=="total" & is_less_than_5==T]
  d[date=="2019-12-29" & age=="total"]
  mean(d[location_code=="county38" & age=="total"]$n_obs)
  d[,is_less_than_5:=NULL]

  d[]

  location_codes <- c("norge","county03","county38")
  tag_location <- "norge_oslo_viken"
  filename <- glue::glue(argset$filename_public)
  filepath <- fs::path(folder,filename)

  writexl::write_xlsx(
    normomo_censor(
      d,
      location_codes = location_codes,
      ages = c("15-64","65+","total")
    ),
    filepath
  )

  location_codes <- unique(norway_locations()$county_code)
  location_codes <- location_codes[!location_codes %in% c("norge","county03","county38")]
  tag_location <- "andre"
  filename <- glue::glue(argset$filename_public)
  filepath <- fs::path(folder,filename)

  writexl::write_xlsx(
    normomo_censor(
      d,
      location_codes = location_codes,
      ages = c("65+","total")
    ),
    filepath
  )

  org::write_text(
    txt = glue::glue(
      "This documentation was last updated on 2020-06-03.\n\n",

      "This is the documentation for the files:\n",
      "- offentliggjort_data_norge_oslo_viken_YYYY-MM-DD.xlsx\n",
      "- offentliggjort_data_andre_YYYY-MM-DD.csv\n\n",

      "offentliggjort_data_norge_oslo_viken_YYYY-MM-DD.xlsx includes the country Norway and the counties Oslo and Viken for ages 15-64, 65+, and total.\n",
      "offentliggjort_data_andre_YYYY includes the other counties for ages 65+ and total.\n",
      "We have implemented censoring to prevent back-calculation of small values from row/column totals.\n",
      "If any file/location/age/week combination records less than 5 deaths, the following values will be set to -99 for all ages except total (for the appropriate file/week combination):\n",
      "- n_obs\n",
      "- ncor_est\n",
      "- ncor_thresholdl0\n",
      "- ncor_thresholdu0\n",
      "- ncor_zscore\n",
      "- ncor_baseline_expected\n",
      "- ncor_baseline_thresholdl0\n",
      "- ncor_baseline_thresholdu0\n",
      "- ncor_baseline_thresholdu1\n",
      "\n",

      "The variables available are listed below:\n",

      "granularity_time: Temporal granularity\n",
      "granularity_geo: Geographical granularity\n",
      "location_code: The geographical location\n",
      "border: The borders (kommunesammensl{fhi::nb$aa}ing) that location_code represents\n",
      "age: Age in years\n",
      "sex: Sex\n",
      "year: Isoyear\n",
      "week: Isoweek\n",
      "yrwk: YYYY-WW\n",
      "season: Seasons run week 30 -> week 29\n",
      "x: Week within season\n",
      "date: Sunday of the week\n",

      "n_obs: Number of recorded deaths\n",
      "ncor_est: Number of estimated deaths\n",
      "ncor_thresholdl0: Lower bound of 95% prediction interval for estimated number of deaths\n",
      "ncor_thresholdu0: Upper bound of 95% prediction interval for estimated number of deaths\n",
      "ncor_zscore: Zscore for estimated number of deaths\n",
      "ncor_status: normal, medium, high\n",
      "ncor_excess: max(0, ncor_thresholdu0-ncor_est)\n",
      "ncor_baseline_expected: Expected number of estimated deaths\n",
      "ncor_baseline_thresholdl0: Lower bound of prediction interval (2 zscores) for expected estimated number of deaths\n",
      "ncor_baseline_thresholdu0: Upper bound of prediction interval (2 zscores) for expected estimated number of deaths\n",
      "ncor_baseline_thresholdu1: Upper bound of prediction interval (4 zscores) for expected estimated number of deaths\n",
      "forecast: Is ncor_est a prediction\n",

      "location_name: The name of the geographical location\n",

      "\n\n"
    ),
    file = fs::path(
      folder,
      "_DOCUMENTATION_offentliggjort_data.txt"
    ),
    header = NULL
  )


}

normomo_censor <- function(d, location_codes="county03", ages=c("15-64","65+","total")){
  dx <- copy(d[
    location_code %in% location_codes &
    age %in% ages
  ])

  dx[,keep := TRUE]
  dx[n_obs < 5, keep := FALSE]
  dx[, keep := as.logical(min(keep)), by=.(yrwk)]
  dx[age=="total", keep := T]
  dx[keep==F]

  for(i in c(
    "n_obs",
    "ncor_est",
    "ncor_thresholdl0",
    "ncor_thresholdu0",
    "ncor_zscore",
    "ncor_baseline_expected",
    "ncor_baseline_thresholdl0",
    "ncor_baseline_thresholdu0",
    "ncor_baseline_thresholdu1"
  )){
    dx[keep==F, (i) := -99]
  }
  dx[,keep:=NULL]
  dx[, location_name := get_location_name(location_code)]

  setorder(dx, location_code, age, -yrwk)
  return(dx)
}
