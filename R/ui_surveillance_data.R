ui_surveillance_data <- function(data, argset, schema) {
  # tm_run_task("ui_surveillance_data")
  # data <- tm_get_data("ui_surveillance_data", index_plan=1)
  # argset <- tm_get_argset("ui_surveillance_data", index_plan=1, index_argset = 1)
  # schema <- tm_get_schema("ui_surveillance_data")

  folder <- path("output", "surveillance_data")
  if(!fs::dir_exists(folder)){
    fs::dir_create(folder)
    git2r::clone(
      url = "https://github.com/folkehelseinstituttet/surveillance_data",
      local_path = folder,
      credentials = config$git_cred
    )
  }
  fs::dir_create(fs::path(folder, "covid19"))

  d <- schema$data_covid19_msis_by_time_location$dplyr_tbl() %>%
    dplyr::filter(granularity_time=="day") %>%
    dplyr::filter(location_code=="norge") %>%
    dplyr::collect()
  setDT(d)

  writexl::write_xlsx(
    d,
    fs::path(
      folder,
      "covid19",
      glue::glue("data_covid19_msis_by_time_location_{lubridate::today()}.xlsx")
    )
  )

  fwrite(
    d,
    fs::path(
      folder,
      "covid19",
      glue::glue("data_covid19_msis_by_time_location_{lubridate::today()}.csv")
    )
  )

  org::write_text(
    txt = glue::glue(
      "This documentation was last updated on 2020-04-24.\n\n",

      "This is the documentation for the files:\n",
      "- data_covid19_msis_by_time_location_YYYY-MM-DD.xlsx\n",
      "- data_covid19_msis_by_time_location_YYYY-MM-DD.csv\n\n",

      "If you notice anything wrong with these datasets, or have any questions relating to the technical aspects of the dataset, please email RichardAubrey.White@fhi.no\n\n",
      "These files are generated automatically each day through an extraction from MSIS.\n\n",

      "The variables available are listed below:\n",

      "granularity_time: Temporal granularity\n",
      "granularity_geo: Geographical granularity\n",
      "location_code: The geographical location\n",
      "border: The borders (kommunesammensl{fhi::nb$aa}ing) that location_code represents\n",
      "age: Age in years\n",
      "sex: Sex\n",
      "season: Seasons run week 30 -> week 29\n",
      "year: Isoyear (pr{fhi::nb$oe}vetakingsdato)\n",
      "week: Isoweek (pr{fhi::nb$oe}vetakingsdato)\n",
      "yrwk: YYYY-WW (pr{fhi::nb$oe}vetakingsdato)\n",
      "x: Week within season\n",
      "date: Date of the data (pr{fhi::nb$oe}vetakingsdato)\n",

      "n: Number of confirmed cases\n\n"
    ),
    file = fs::path(
      folder,
      "covid19",
      "_DOCUMENTATION_data_covid19_msis_by_time_location.txt"
    ),
    header = NULL
  )
}
