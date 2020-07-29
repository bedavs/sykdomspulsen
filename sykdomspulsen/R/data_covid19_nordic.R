#' data_pre_covid19_nordic
#' @param data a
#' @param argset a
#' @param schema a
#' @export
data_pre_covid19_nordic <- function(data, argset, schema){
  # tm_run_task("data_pre_covid19_nordic")

  if(plnr::is_run_directly()){
    # rsync -avh --delete --no-times --exclude 'test' /input/sykdomspulsen_covid19_nordic_input/ /input/sykdomspulsen_covid19_nordic_input/test/
    data <- sc::tm_get_data("data_pre_covid19_nordic")
    argset <- sc::tm_get_argset("data_pre_covid19_nordic")
    schema <- sc::tm_get_schema("data_pre_covid19_nordic")
  }

  folder_base <- sc::path("input","sykdomspulsen_covid19_nordic_input", create_dir = T)
  folder_denmark <- fs::path(folder_base, "denmark")
  if(!fs::dir_exists(folder_denmark)) fs::dir_create(folder_denmark)
  folder_sweden <- fs::path(folder_base, "sweden", lubridate::today())
  if(!fs::dir_exists(folder_sweden)) fs::dir_create(folder_sweden)
  folder_finland <- fs::path(folder_base, "finland", lubridate::today())
  if(!fs::dir_exists(folder_finland)) fs::dir_create(folder_finland)

  # iceland
  html <- xml2::read_html("https://www.covid.is/data")
  html <- xml2::read_html("https://e.infogram.com/e3205e42-19b3-4e3a-a452-84192884450d?src=embed")

  d <- html %>%
    rvest::html_nodes("script") %>%
    rvest::html_text()
  d <- stringr::str_subset(d,"window\\.infographicData")
  d <- stringr::str_remove(d, "window\\.infographicData=")
  d_start <- stringr::str_locate_all(d, "\\{")[[1]][,"start"]
  d_end <- stringr::str_locate_all(d, "\\}")[[1]][,"start"]
  d <- stringr::str_sub(d, start=1,end=max(d_end))
  length(d_start)
  length(d_end)

  d <- jsonlite::fromJSON(d)
  names(d)
  d$elements$content$content$entities[[1]]

  # finland cases ----
  utils::download.file(
    url = "https://sampo.thl.fi/pivot/prod/en/epirapo/covid19case/fact_epirapo_covid19case.csv?row=hcdmunicipality2020-445222&column=dateweek2020010120201231-443686&filter=measure-444833&",
    destfile = fs::path(folder_finland, "cases.csv")
  )

  # finland tests ----
  utils::download.file(
    url = "https://sampo.thl.fi/pivot/prod/en/epirapo/covid19case/fact_epirapo_covid19case.csv?row=hcdmunicipality2020-445222&column=dateweek2020010120201231-443686&filter=measure-445356&",
    destfile = fs::path(folder_finland, "tests.csv")
  )

  # sweden ----
  file <- fs::path(folder_sweden,"sweden.xlsx")
  utils::download.file(
    url = "https://www.arcgis.com/sharing/rest/content/items/b5e7488e117749c19881cce45db13f7e/data",
    destfile = file
  )

  d <- readxl::read_excel(
    file,
    sheet = "Antal per dag region"
  )

  d <- readxl::read_excel(
    file,
    sheet = glue::glue("Antal intensivv{fhidata::nb$aa}rdade per dag")
  )
  setDT(d)

  # sweden tests ----
  ht <- xml2::read_html("https://www.folkhalsomyndigheten.se/smittskydd-beredskap/utbrott/aktuella-utbrott/covid-19/antal-individer-som-har-testats-for-covid-19/")
  d <- ht %>%
    rvest::html_nodes("table") %>%
    magrittr::extract(1) %>%
    rvest::html_table()
  writexl::write_xlsx(d[[1]], fs::path(folder_sweden, "tests_week_number.xlsx"))

  ht <- xml2::read_html("https://www.folkhalsomyndigheten.se/smittskydd-beredskap/utbrott/aktuella-utbrott/covid-19/antal-individer-som-har-testats-for-covid-19/")
  d <- ht %>%
    rvest::html_nodes("table") %>%
    magrittr::extract(2) %>%
    rvest::html_table()
  writexl::write_xlsx(d[[1]], fs::path(folder_sweden, "tests.xlsx"))

  # denmark ----

  danish_months <- c(
    "januar",
    "februar",
    "marts",
    "april",
    "maj",
    "juni",
    "juli",
    "august",
    "september",
    "oktober",
    "november",
    "december"
  )
  ht <- xml2::read_html("https://www.ssi.dk/sygdomme-beredskab-og-forskning/sygdomsovervaagning/c/covid19-overvaagning/arkiv-med-overvaagningsdata-for-covid19")
  d <- ht %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()
  d_which <- which(stringr::str_detect(d, "gningsdata den"))
  d_names <- d[d_which]
  d_names <- stringr::str_remove(d_names, glue::glue("Overv{fhidata::nb$aa}gningsdata den "))
  for(i in seq_along(danish_months)){
    d_names <- stringr::str_replace(d_names, glue::glue(". {danish_months[i]} "), glue::glue("-{formatC(i,flag=0,width=2)}-"))
  }
  d_names <- stringr::str_remove_all(d_names,"\n")
  d_names <- stringr::str_trim(d_names)
  d_dates <- as.Date(d_names, format="%d-%m-%Y")
  for(i in seq_along(d_dates)){
    folder_dest <- fs::path(folder_denmark, d_dates[i])
    if(fs::dir_exists(folder_dest)) next()

    url <- ht %>%
      rvest::html_nodes("a") %>%
      magrittr::extract(d_which[i]) %>%
      rvest::html_attr("href")

    destfile <- fs::path(tempdir(), "temp.zip")
    utils::download.file(
      url,
      destfile
    )

    utils::unzip(zipfile = destfile, exdir = folder_dest)
    #fs::dir_ls(fs::path(folder_dest, "xl"))
  }
  folder_denmark


}

#' data_covid19_nordic
#' @param data a
#' @param argset a
#' @param schema a
#' @export
data_covid19_nordic <- function(data, argset, schema){
  # tm_run_task("data_covid19_nordic")

  if(plnr::is_run_directly()){
    # rm -rf /input/sykdomspulsen_covid19_nordic_input/test; cp -r /input/sykdomspulsen_covid19_nordic_input /tmp/test; mv /tmp/test /input/sykdomspulsen_covid19_nordic_input/
    # rsync -avh --delete --no-times --exclude 'test' /input/sykdomspulsen_covid19_nordic_input/ /input/sykdomspulsen_covid19_nordic_input/test/
    data <- sc::tm_get_data("data_covid19_nordic")
    argset <- sc::tm_get_argset("data_covid19_nordic")
    schema <- sc::tm_get_schema("data_covid19_nordic")

    schema$output$db_field_types
  }
  schema$output$db_drop_all_rows()

  folder_base <- sc::path("input","sykdomspulsen_covid19_nordic_input", create_dir = T)

  # denmark ----
  folder_denmark <- fs::path(folder_base, "denmark")

  files <- fs::dir_ls(folder_denmark, regexp="[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]")
  min_val <- fs::path(folder_denmark, "2020-05-25")
  files <- files[files >= min_val]
  retval <- vector("list", length=length(files))
  for(i in seq_along(files)){
    x_date <- stringr::str_extract(files[i], "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]")
    d <- fread(
      fs::path(files[i], "Region_summary.csv"),
      dec=","
    )
    d[, date := as.Date(x_date)]
    retval[[i]] <- d
  }
  retval <- rbindlist(retval)
  retval[, n_tests := as.numeric(stringr::str_remove_all(Testede, "\\."))]
  retval[, n_cases := as.numeric(stringr::str_remove_all(Positive, "\\."))]
  setorder(retval, Region, date)
  retval[, n_tests := n_tests - shift(n_tests), by=.(Region)]
  retval[, n_cases := n_cases - shift(n_cases), by=.(Region)]

  retval[
    fhidata::denmark_locations_long_b2020,
    on="Region==location_name",
    location_code := location_code
  ]
  retval[Region=="I alt i Danmark", location_code := "DK"]

  retval[, yrwk := fhi::isoyearweek(date)]
  retval <- retval[, .(
    cases = sum(n_cases),
    tests = sum(n_tests)
  ), keyby=.(
    location_code, yrwk
  )]
  retval <- retval[!is.na(cases)]

  retval <- melt.data.table(
    retval,
    id.vars = c("location_code", "yrwk"),
    variable.name = "tag_outcome",
    variable.factor = FALSE,
    value.name = "n"
  )

  retval[
    fhidata::population_denmark_b2020[age=="total"],
    on="location_code",
    pop := pop
  ]
  retval[, granularity_time := "week"]
  retval[,manual_extraction:=FALSE]
  fill_in_missing(retval)

  schema$output$db_upsert_load_data_infile(retval)

  # finland cases ----
  folder_finland <- fs::path(folder_base, "finland")

  folder_finland <- fs::dir_ls(folder_finland, regexp="[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]")
  folder_finland <- max(folder_finland)

  d <- fread(fs::path(folder_finland, "cases.csv"))
  d <- d[!is.na(val)]
  d[, yrwk := paste0(
    stringr::str_extract(Time,"[0-9][0-9][0-9][0-9]"),
    "-",
    stringr::str_extract(Time,"[0-9][0-9]$")
    )
  ]
  d <- d[yrwk != "NA-NA"]

  d[
    fhidata::finland_locations_long_b2020,
    on="Area==location_name",
    location_code := location_code
  ]
  d[Area=="All areas", location_code := "FI"]

  d <- d[,.(
    location_code,
    yrwk,
    n = val
  )]

  d[
    fhidata::population_finland_b2020[age=="total"],
    on="location_code",
    pop := pop
  ]
  d[, tag_outcome := "cases"]
  d[, granularity_time := "week"]
  d[,manual_extraction:=FALSE]
  fill_in_missing(d)

  schema$output$db_upsert_load_data_infile(d)


  # finland tests ----
  folder_finland <- fs::path(folder_base, "finland")

  files <- fs::dir_ls(folder_finland, regexp="[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]")
  min_val <- fs::path(folder_finland, "2020-05-25")
  files <- files[files >= min_val]
  retval <- vector("list", length=length(files))
  for(i in seq_along(files)){
    x_date <- stringr::str_extract(files[i], "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]")
    d <- fread(
      fs::path(files[i], "tests.csv"),
      dec=","
    )
    d[, date := as.Date(x_date)]
    retval[[i]] <- d
  }
  retval <- rbindlist(retval)
  retval <- retval[Time=="Time"]
  setorder(retval, Area, date)
  setnames(retval, "val", "n_tests")
  retval[, n_tests := n_tests - shift(n_tests), by=.(Area)]
  retval <- retval[!is.na(n_tests)]

  retval[
    fhidata::finland_locations_long_b2020,
    on="Area==location_name",
    location_code := location_code
  ]
  retval[Area=="All areas", location_code := "FI"]

  retval[, yrwk := fhi::isoyearweek(date)]
  d <- retval[yrwk >= "2020-26", .(
    n = sum(n_tests)
  ), keyby=.(
    location_code, yrwk
  )]

  d[
    fhidata::population_finland_b2020[age=="total"],
    on="location_code",
    pop := pop
  ]
  d[, tag_outcome := "tests"]
  d[, granularity_time := "week"]
  d[,manual_extraction:=FALSE]
  fill_in_missing(d)

  schema$output$db_upsert_load_data_infile(d)

  # sweden cases ----
  folder_sweden <- fs::path(folder_base, "sweden")

  folder_sweden <- fs::dir_ls(folder_sweden, regexp="[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]")
  folder_sweden <- max(folder_sweden)

  file <- fs::path(folder_sweden,"sweden.xlsx")

  d <- readxl::read_excel(
    fs::path(folder_sweden,"sweden.xlsx"),
    sheet = "Antal per dag region"
  )
  setDT(d)
  d <- melt.data.table(
    d,
    id.vars = "Statistikdatum",
    variable.factor = F
  )
  d[variable==glue::glue("J{fhidata::se$ae}mtland_H{fhidata::se$ae}rjedalen"),
    variable := glue::glue("J{fhidata::se$ae}mtland")]
  d[,variable := stringr::str_replace_all(variable, "_", " ")]
  d[, yrwk := fhi::isoyearweek(Statistikdatum)]
  d[
    fhidata::sweden_locations_long_b2020,
    on="variable==location_name",
    location_code := location_code
  ]
  d[variable=="Totalt antal fall", location_code := "SE"]
  d[variable==glue::glue("S{fhidata::se$oe}rmland"), location_code := "SE122"]
  d <- d[,.(
    n = sum(value)
  ),keyby=.(
    location_code,
    yrwk
  )]

  d[
    fhidata::population_sweden_b2020[age=="total"],
    on="location_code",
    pop := pop
  ]
  d[, tag_outcome := "cases"]
  d[, granularity_time := "week"]
  d[,manual_extraction:=FALSE]
  fill_in_missing(d)

  schema$output$db_upsert_load_data_infile(d)

  # sweden icu ----
  d <- readxl::read_excel(
    fs::path(folder_sweden,"sweden.xlsx"),
    sheet = glue::glue("Antal intensivv{fhidata::nb$aa}rdade per dag")
  )
  setDT(d)
  setnames(d, glue::glue("Datum_v{fhidata::nb$aa}rdstart"), "date")
  setnames(d, glue::glue("Antal_intensivv{fhidata::nb$aa}rdade"), "n_icu")
  d[, location_code := "SE"]
  d[, yrwk := fhi::isoyearweek(date)]
  d <- d[,.(
    n = sum(n_icu)
  ),keyby=.(
    location_code,
    yrwk
  )]

  d[
    fhidata::population_sweden_b2020[age=="total"],
    on="location_code",
    pop := pop
  ]
  d[, tag_outcome := "icu"]
  d[, granularity_time := "week"]
  d[,manual_extraction:=FALSE]
  fill_in_missing(d)

  schema$output$db_upsert_load_data_infile(d)

  # sweden tests ----
  folder_sweden <- fs::path(folder_base, "sweden")

  files <- fs::dir_ls(folder_sweden, regexp="[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]")
  retval <- vector("list", length=length(files))
  for(i in seq_along(files)){
    # load in test data
    d <- readxl::read_excel(
      fs::path(files[i], "tests.xlsx")
    )
    setDT(d)

    # it is in two different formats
    if(file.exists(fs::path(files[i], "tests_week_number.xlsx"))){
      wk <- readxl::read_excel(
        fs::path(files[i], "tests_week_number.xlsx")
      )
      wk <- wk$Vecka[1]
      wk <- stringr::str_extract(wk, "[0-9][0-9]$")
      setnames(d, c("Region", "x_n_tests", "x"))
      d[,x:=NULL]
    } else {
      x_date <- stringr::str_extract(files[i], "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]")
      wk <- stringr::str_extract(names(d)[2], "[0-9][0-9]$")
      setnames(d, c("Region", "x_n_tests", "x"))
      d[,x:=NULL]
    }
    d[,yrwk := paste0("2020-",wk)]
    retval[[i]] <- d
  }
  retval <- rbindlist(retval)
  retval[, id:=.N:1, by=.(Region, yrwk)]
  retval <- retval[id==1]
  retval[,id:=NULL]
  retval[, n_tests := as.numeric(stringr::str_remove_all(x_n_tests, " "))]
  retval[, x_n_tests := NULL]

  retval[Region==glue::glue("J{fhidata::se$ae}mtland/H{fhidata::se$ae}rjedalen"),
         Region := glue::glue("J{fhidata::se$ae}mtland")]
  retval[Region==glue::glue("J{fhidata::se$ae}mtland/ H{fhidata::se$ae}rjedalen"),
         Region := glue::glue("J{fhidata::se$ae}mtland")]

  retval[
    fhidata::sweden_locations_long_b2020,
    on="Region==location_name",
    location_code := location_code
  ]
  retval[Region==glue::glue("S{fhidata::se$oe}rmland"), location_code := "SE122"]

  retval_se <- retval[,.(
    location_code="SE",
    n_tests=sum(n_tests)
  ),keyby=.(yrwk)]
  retval[,Region := NULL ]
  d <- na.omit(rbind(retval, retval_se))

  setnames(d, "n_tests", "n")

  d[
    fhidata::population_sweden_b2020[age=="total"],
    on="location_code",
    pop := pop
  ]
  d[, tag_outcome := "tests"]
  d[, granularity_time := "week"]
  d[,manual_extraction:=FALSE]
  fill_in_missing(d)

  schema$output$db_upsert_load_data_infile(d)

  # manual cases ----

  d <- readxl::read_excel(
    fs::path(folder_base, "Manual data collection.xlsx"),
    sheet = "Cases",
    skip = 1
  )
  setDT(d)
  d <- d[Region!="Iceland border screening"] ## maybe this should add to Iceland total cases
  d <- d[!is.na(location_code)]
  d[, Country := NULL]
  d[, Region := NULL]
  d <- melt.data.table(
    d,
    id.vars = "location_code",
    variable.factor = FALSE,
    variable.name = "yrwk",
    value.name = "n"
  )
  d <- d[!is.na(n)]
  d[, tag_outcome := "cases"]
  d[, granularity_time := "week"]

  pops <- rbind(
    fhidata::population_denmark_b2020,
    fhidata::population_sweden_b2020,
    fhidata::population_finland_b2020,
    fhidata::population_iceland_b2020
  )

  fill_in_missing(d)

  d[
    pops[age=="total"],
    on=c("year","location_code"),
    pop := pop
  ]

  d[, granularity_time := "week"]
  d[,manual_extraction:=TRUE]

  schema$output$db_upsert_load_data_infile(d)

  # manual tests ----

  d <- readxl::read_excel(
    fs::path(folder_base, "Manual data collection.xlsx"),
    sheet = "Tests",
    skip = 1
  )
  setDT(d)
  d <- d[Region!="Iceland border screening"] ## maybe this should add to Iceland total cases

  d <- d[!is.na(location_code)]
  d[, Country := NULL]
  d[, Region := NULL]
  d <- melt.data.table(
    d,
    id.vars = "location_code",
    variable.factor = FALSE,
    variable.name = "yrwk",
    value.name = "n"
  )
  d <- d[!is.na(n)]
  d[, tag_outcome := "tests"]
  d[, granularity_time := "week"]

  pops <- rbind(
    fhidata::population_denmark_b2020,
    fhidata::population_sweden_b2020,
    fhidata::population_finland_b2020,
    fhidata::population_iceland_b2020
  )

  fill_in_missing(d)

  d[
    pops[age=="total"],
    on=c("year","location_code"),
    pop := pop
  ]

  d[, granularity_time := "week"]
  d[,manual_extraction:=TRUE]

  schema$output$db_upsert_load_data_infile(d)

  # manual icu ----

  d <- readxl::read_excel(
    fs::path(folder_base, "Manual data collection.xlsx"),
    sheet = "ICU",
    skip = 1
  )
  setDT(d)

  d <- d[!is.na(location_code)]
  d[, Country := NULL]
  d[, Region := NULL]
  d <- melt.data.table(
    d,
    id.vars = "location_code",
    variable.factor = FALSE,
    variable.name = "yrwk",
    value.name = "n"
  )
  d <- d[!is.na(n)]
  d[, tag_outcome := "icu"]
  d[, granularity_time := "week"]

  pops <- rbind(
    fhidata::population_denmark_b2020,
    fhidata::population_sweden_b2020,
    fhidata::population_finland_b2020,
    fhidata::population_iceland_b2020
  )

  fill_in_missing(d)

  d[
    pops[age=="total"],
    on=c("year","location_code"),
    pop := pop
  ]

  d[, granularity_time := "week"]
  d[,manual_extraction:=TRUE]

  schema$output$db_upsert_load_data_infile(d)

  # filling in the blanks ----
  d <- schema$output$dplyr_tbl() %>%
    dplyr::collect()
  setDT(d)

  skeleton <- expand.grid(
    location_code = unique(d$location_code),
    yrwk = unique(d$yrwk),
    tag_outcome = c("cases", "tests", "icu"),
    stringsAsFactors = FALSE
  )
  setDT(skeleton)

  skeleton[
    d,
    on=c("location_code", "yrwk", "tag_outcome"),
    n := n
  ]

  skeleton[
    d,
    on=c("location_code", "yrwk", "tag_outcome"),
    manual_extraction := manual_extraction
  ]
  skeleton[is.na(n), n := NA]
  skeleton[is.na(manual_extraction), manual_extraction := FALSE]

  skeleton[, granularity_time := "week"]
  fill_in_missing(skeleton)

  skeleton[
    pops[age=="total"],
    on=c("year","location_code"),
    pop := pop
  ]

  schema$output$db_upsert_load_data_infile(skeleton)

}












