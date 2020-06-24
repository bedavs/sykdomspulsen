#' ui_surveillance_data
#' @param data a
#' @param argset a
#' @param schema a
#' @export
ui_surveillance_data <- function(data, argset, schema) {

  if(plnr::is_run_directly()){
    # tm_run_task("ui_surveillance_data")

    sc::tm_update_plans("ui_surveillance_data")
    data <- sc::tm_get_data("ui_surveillance_data", index_plan=1)
    argset <- sc::tm_get_argset("ui_surveillance_data", index_plan=1, index_argset = 1)
    schema <- sc::tm_get_schema("ui_surveillance_data")
  }

  folder <- sc::path("output", "sykdomspulsen_data_output", create_dir = T)
  folder <- sc::path("output", "sykdomspulsen_data_output", "surveillance_data")
  if(!fs::dir_exists(folder)){
    system(glue::glue("cd {sc::path('output', 'sykdomspulsen_data_output')}; sudo git clone https://github.com/folkehelseinstituttet/surveillance_data.git"))
  }
  repo <- git2r::repository(folder)
  git2r::config(repo = repo, user.name="Sykdomspulsen",user.email="sykdomspulsen@fhi.no")
  git2r::pull(repo)

  fs::dir_create(fs::path(folder, "covid19"))

  d1 <- schema$data_covid19_msis_by_time_location$dplyr_tbl() %>%
    dplyr::filter(granularity_time=="day") %>%
    dplyr::filter(granularity_geo %in% c("nation","county")) %>%
    dplyr::collect()
  setDT(d1)

  d2 <- schema$data_covid19_msis_by_time_location$dplyr_tbl() %>%
    dplyr::filter(granularity_time=="day") %>%
    dplyr::group_by(location_code) %>%
    dplyr::summarize(n=sum(n)) %>%
    dplyr::collect()
  setDT(d2)
  d2[,granularity_time := "total"]
  fill_in_missing(d2)

  d <- rbind(d1,d2)

  d_pop <- norway_population()
  d_pop <- d_pop[,.(
    pop = sum(pop)
  ),keyby=.(
    year,location_code
  )]
  d_pop <- d_pop[year>=2000]
  d_pop_x <- d_pop[year==2020]
  d_pop_x[,year:=1900]
  d_pop <- rbind(d_pop_x,d_pop)
  d[
    d_pop,
    on=c("location_code","year"),
    pop:=pop
  ]
  d[,pr100000 := 100000*n/pop]

  setorder(d, -granularity_time, location_code, date)

  d[, location_name := get_location_name(location_code)]

  # data_covid19_msis_by_time_location ----
  writexl::write_xlsx(
    d[granularity_time=="day"],
    fs::path(
      folder,
      "covid19",
      glue::glue("data_covid19_msis_by_time_location_{lubridate::today()}.xlsx")
    )
  )

  fwrite(
    d[granularity_time=="day"],
    fs::path(
      folder,
      "covid19",
      glue::glue("data_covid19_msis_by_time_location_{lubridate::today()}.csv")
    )
  )

  org::write_text(
    txt = glue::glue(
      "This documentation was last updated on 2020-06-04.\n\n",

      "This is the documentation for the files:\n",
      "- data_covid19_msis_by_time_location_YYYY-MM-DD.xlsx\n",
      "- data_covid19_msis_by_time_location_YYYY-MM-DD.csv\n\n",

      "These files contain daily COVID-19 epicurves for Norway and the counties of Norway\n\n",

      "If something does not appear to be correct, or more documentation is needed in a particular area, or you have suggestions about the way the data is presented/formatted, please email RichardAubrey.White@fhi.no\n\n",
      "These files are generated automatically each day through an extraction from MSIS.\n\n",

      "The variables available are listed below:\n",

      "granularity_time: Temporal granularity (day)\n",
      "granularity_geo: Geographical granularity (nation and county)\n",
      "location_code: The geographical location\n",
      "border: The borders (kommunesammensl{fhi::nb$aa}ing) that location_code represents\n",
      "age: Age in years\n",
      "sex: Sex\n",
      "year: Isoyear (pr{fhi::nb$oe}vetakingsdato)\n",
      "week: Isoweek (pr{fhi::nb$oe}vetakingsdato)\n",
      "yrwk: YYYY-WW (pr{fhi::nb$oe}vetakingsdato)\n",
      "season: Seasons run week 30 -> week 29\n",
      "x: Week within season\n",
      "date: Date of the data (pr{fhi::nb$oe}vetakingsdato)\n",

      "n: Number of confirmed cases\n",
      "pop: Population obtained from SSB\n",
      "pr100000: 100000*n/pop\n",

      "location_name: The name of the geographical location\n",

      "\n"
    ),
    file = fs::path(
      folder,
      "covid19",
      "_DOCUMENTATION_data_covid19_msis_by_time_location.txt"
    ),
    header = NULL
  )

  # data_covid19_msis_by_location ----

  writexl::write_xlsx(
    d[granularity_time=="total"],
    fs::path(
      folder,
      "covid19",
      glue::glue("data_covid19_msis_by_location_{lubridate::today()}.xlsx")
    )
  )

  fwrite(
    d[granularity_time=="total"],
    fs::path(
      folder,
      "covid19",
      glue::glue("data_covid19_msis_by_location_{lubridate::today()}.csv")
    )
  )

  org::write_text(
    txt = glue::glue(
      "This documentation was last updated on 2020-06-04.\n\n",

      "This is the documentation for the files:\n",
      "- data_covid19_msis_by_location_YYYY-MM-DD.xlsx\n",
      "- data_covid19_msis_by_location_YYYY-MM-DD.csv\n\n",

      "These files contain the total number of reported COVID-19 cases for Norway, the counties of Norway, and the municipalities of Norway.\n\n",

      "If something does not appear to be correct, or more documentation is needed in a particular area, or you have suggestions about the way the data is presented/formatted, please email RichardAubrey.White@fhi.no\n\n",
      "These files are generated automatically each day through an extraction from MSIS.\n\n",

      "Note: granularity_time='total' means the cumulative total for the entire time period.\n\n",

      "The variables available are listed below:\n",

      "granularity_time: Temporal granularity (total)\n",
      "granularity_geo: Geographical granularity (nation, county, and municip)\n",
      "location_code: The geographical location\n",
      "border: The borders (kommunesammensl{fhi::nb$aa}ing) that location_code represents\n",
      "age: Age in years\n",
      "sex: Sex\n",
      "year: N/A\n",
      "week: N/A\n",
      "yrwk: N/A\n",
      "season: N/A\n",
      "x: N/A\n",
      "date: N/A\n",

      "n: Total number of confirmed cases\n",
      "pop: Population obtained from SSB\n",
      "pr100000: 100000*n/pop\n",

      "location_name: The name of the geographical location\n",

      "\n"
    ),
    file = fs::path(
      folder,
      "covid19",
      "_DOCUMENTATION_data_covid19_msis_by_location.txt"
    ),
    header = NULL
  )

  # data_covid19_msis_by_time_sex_age ----

  d <- schema$data_covid19_msis_by_time_sex_age$dplyr_tbl() %>%
    dplyr::filter(granularity_time=="week") %>%
    dplyr::filter(granularity_geo=="nation") %>%
    dplyr::collect()
  setDT(d)

  writexl::write_xlsx(
    d,
    fs::path(
      folder,
      "covid19",
      glue::glue("data_covid19_msis_by_time_sex_age_{lubridate::today()}.xlsx")
    )
  )

  fwrite(
    d,
    fs::path(
      folder,
      "covid19",
      glue::glue("data_covid19_msis_by_time_sex_age_{lubridate::today()}.csv")
    )
  )

  org::write_text(
    txt = glue::glue(
      "This documentation was last updated on 2020-06-23.\n\n",

      "This is the documentation for the files:\n",
      "- data_covid19_msis_by_time_sex_age_YYYY-MM-DD.xlsx\n",
      "- data_covid19_msis_by_time_sex_age_YYYY-MM-DD.csv\n\n",

      "These files contain weekly COVID-19 epicurves for Norway by age and sex\n\n",

      "If something does not appear to be correct, or more documentation is needed in a particular area, or you have suggestions about the way the data is presented/formatted, please email RichardAubrey.White@fhi.no\n\n",
      "These files are generated automatically each day through an extraction from MSIS.\n\n",

      "The variables available are listed below:\n",

      "granularity_time: Temporal granularity (week)\n",
      "granularity_geo: Geographical granularity (nation)\n",
      "location_code: The geographical location\n",
      "border: The borders (kommunesammensl{fhi::nb$aa}ing) that location_code represents\n",
      "age: Age in years\n",
      "sex: Sex\n",
      "year: Isoyear (pr{fhi::nb$oe}vetakingsdato)\n",
      "week: Isoweek (pr{fhi::nb$oe}vetakingsdato)\n",
      "yrwk: YYYY-WW (pr{fhi::nb$oe}vetakingsdato)\n",
      "season: Seasons run week 30 -> week 29\n",
      "x: Week within season\n",
      "date: Date corresponding to Sunday of the week\n",

      "n: Number of confirmed cases\n",

      "\n"
    ),
    file = fs::path(
      folder,
      "covid19",
      "_DOCUMENTATION_data_covid19_msis_by_time_sex_age.txt"
    ),
    header = NULL
  )

  # data_covid19_lab_by_time ----

  d <- schema$data_covid19_lab_by_time$dplyr_tbl() %>%
    dplyr::filter(granularity_time=="day") %>%
    dplyr::filter(granularity_geo=="nation") %>%
    dplyr::collect()
  setDT(d)
  d[, cum_n_tested := NULL]

  writexl::write_xlsx(
    d,
    fs::path(
      folder,
      "covid19",
      glue::glue("data_covid19_lab_by_time_{lubridate::today()}.xlsx")
    )
  )

  fwrite(
    d,
    fs::path(
      folder,
      "covid19",
      glue::glue("data_covid19_lab_by_time_{lubridate::today()}.csv")
    )
  )

  org::write_text(
    txt = glue::glue(
      "This documentation was last updated on 2020-06-23.\n\n",

      "This is the documentation for the files:\n",
      "- data_covid19_lab_by_time_YYYY-MM-DD.xlsx\n",
      "- data_covid19_lab_by_time_YYYY-MM-DD.csv\n\n",

      "These files contain daily numbers of people tested for COVID-19 in Norway\n\n",

      "If something does not appear to be correct, or more documentation is needed in a particular area, or you have suggestions about the way the data is presented/formatted, please email RichardAubrey.White@fhi.no\n\n",
      "These files are generated automatically each day through an extraction from databases.\n\n",

      "The variables available are listed below:\n",

      "granularity_time: Temporal granularity (day)\n",
      "granularity_geo: Geographical granularity (nation)\n",
      "location_code: The geographical location\n",
      "border: The borders (kommunesammensl{fhi::nb$aa}ing) that location_code represents\n",
      "age: Age in years\n",
      "sex: Sex\n",
      "year: Isoyear\n",
      "week: Isoweek\n",
      "yrwk: YYYY-WW\n",
      "season: Seasons run week 30 -> week 29\n",
      "x: Week within season\n",
      "date: Date\n",

      "n_neg: Number of people who received a negative test\n",
      "n_pos: Number of people who received a positive test\n",
      "pr100_pos: 100*n_pos/(n_pos+n_neg)\n",

      "\n"
    ),
    file = fs::path(
      folder,
      "covid19",
      "_DOCUMENTATION_data_covid19_lab_by_time.txt"
    ),
    header = NULL
  )

  # data_covid19_hospital_by_time ----

  d <- schema$data_covid19_hospital_by_time$dplyr_tbl() %>%
    dplyr::filter(granularity_time=="day") %>%
    dplyr::filter(granularity_geo=="nation") %>%
    dplyr::collect()
  setDT(d)

  writexl::write_xlsx(
    d,
    fs::path(
      folder,
      "covid19",
      glue::glue("data_covid19_hospital_by_time_{lubridate::today()}.xlsx")
    )
  )

  fwrite(
    d,
    fs::path(
      folder,
      "covid19",
      glue::glue("data_covid19_hospital_by_time_{lubridate::today()}.csv")
    )
  )

  org::write_text(
    txt = glue::glue(
      "This documentation was last updated on 2020-06-23.\n\n",

      "This is the documentation for the files:\n",
      "- data_covid19_hospital_by_time_YYYY-MM-DD.xlsx\n",
      "- data_covid19_hospital_by_time_YYYY-MM-DD.csv\n\n",

      "These files contain daily numbers of people admitted to hospitals or ICUs due to COVID-19 in Norway\n\n",

      "If something does not appear to be correct, or more documentation is needed in a particular area, or you have suggestions about the way the data is presented/formatted, please email RichardAubrey.White@fhi.no\n\n",
      "These files are generated automatically each day through an extraction from databases.\n\n",

      "The variables available are listed below:\n",

      "granularity_time: Temporal granularity (day)\n",
      "granularity_geo: Geographical granularity (nation)\n",
      "location_code: The geographical location\n",
      "border: The borders (kommunesammensl{fhi::nb$aa}ing) that location_code represents\n",
      "age: Age in years\n",
      "sex: Sex\n",
      "year: Isoyear\n",
      "week: Isoweek\n",
      "yrwk: YYYY-WW\n",
      "season: Seasons run week 30 -> week 29\n",
      "x: Week within season\n",
      "date: Date\n",

      "n_icu: Number of people who were admitted to an ICU today due to COVID-19\n",
      "n_hospital_main_cause: Number of people who were admitted to a hospital with COVID-19 as the main cause\n",

      "\n"
    ),
    file = fs::path(
      folder,
      "covid19",
      "_DOCUMENTATION_data_covid19_hospital_by_time.txt"
    ),
    header = NULL
  )

  # data_covid19_demographics ----

  d <- schema$data_covid19_demographics$dplyr_tbl() %>%
    dplyr::filter(granularity_time=="total") %>%
    dplyr::filter(granularity_geo=="nation") %>%
    dplyr::collect()
  setDT(d)

  writexl::write_xlsx(
    d,
    fs::path(
      folder,
      "covid19",
      glue::glue("data_covid19_demographics_{lubridate::today()}.xlsx")
    )
  )

  fwrite(
    d,
    fs::path(
      folder,
      "covid19",
      glue::glue("data_covid19_demographics_{lubridate::today()}.csv")
    )
  )

  org::write_text(
    txt = glue::glue(
      "This documentation was last updated on 2020-06-24.\n\n",

      "This is the documentation for the files:\n",
      "- data_covid19_demographics_YYYY-MM-DD.xlsx\n",
      "- data_covid19_demographics_YYYY-MM-DD.csv\n\n",

      "These files contain demographic numbers related to COVID-19 in Norway\n\n",

      "If something does not appear to be correct, or more documentation is needed in a particular area, or you have suggestions about the way the data is presented/formatted, please email RichardAubrey.White@fhi.no\n\n",
      "These files are generated automatically each day through an extraction from databases.\n\n",

      "The variables available are listed below:\n",

      "granularity_time: Temporal granularity (day)\n",
      "granularity_geo: Geographical granularity (nation)\n",
      "location_code: The geographical location\n",
      "border: The borders (kommunesammensl{fhi::nb$aa}ing) that location_code represents\n",
      "age: Age in years\n",
      "sex: Sex\n",
      "year: N/A\n",
      "week: N/A\n",
      "yrwk: N/A\n",
      "season: N/A\n",
      "x: N/A\n",
      "date: N/A\n",

      "tag_outcome: The outcome that is being described (deaths)\n",

      "n: Total number of people corresponding to this location_code/age/sex/tag_outcome combination\n",

      "\n"
    ),
    file = fs::path(
      folder,
      "covid19",
      "_DOCUMENTATION_data_covid19_demographics.txt"
    ),
    header = NULL
  )

  org::write_text(
    txt = glue::glue(
      "This documentation was last updated on 2020-06-24.\n\n",

      "Please see file _DOCUMENTATION_data_covid19_demographics.txt\n",

      "\n"
    ),
    file = fs::path(
      folder,
      "covid19",
      "_DOCUMENTATION_data_covid19_deaths.txt"
    ),
    header = NULL
  )


  # finish up ----

  if(sc::config$is_production){
    system(glue::glue("sudo git -C {folder} add -A"))
    cmd <- glue::glue(
      'sudo git -C {folder} commit --author="Sykdomspulsen <sykdomspulsen@fhi.no>" -m "Updated {lubridate::now()}"'
    )
    system(cmd)

    git2r::push(
      object = repo,
      credentials = git2r::cred_token()
    )
  }
}
