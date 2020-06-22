#' data_pre_covid19_nordic
#' @param data a
#' @param argset a
#' @param schema a
#' @export
data_pre_covid19_nordic <- function(data, argset, schema){
  # tm_run_task("data_pre_covid19_nordic")

  if(plnr::is_run_directly()){
    data <- sc::tm_get_data("data_pre_covid19_nordic")
    argset <- sc::tm_get_argset("data_pre_covid19_nordic")
    schema <- sc::tm_get_schema("data_pre_covid19_nordic")
  }

  folder_base <- sc::path("input","sykdomspulsen_covid19_dagsrapport_input", "nordic", "input", create_dir = T)
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
    fs::dir_ls(fs::path(destfolder, "xl"))
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
    data <- sc::tm_get_data("data_covid19_nordic")
    argset <- sc::tm_get_argset("data_covid19_nordic")
    schema <- sc::tm_get_schema("data_covid19_nordic")

    schema$output$db_field_types
  }

  folder_base <- sc::path("input","sykdomspulsen_covid19_dagsrapport_input", "nordic", "input", create_dir = T)

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

  # finland cases ----
  folder_finland <- fs::path(folder_base, "finland")

  folder_finland <- fs::dir_ls(folder_finland, regexp="[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]")
  folder_finland <- max(folder_finland)

  d <- fread(fs::path(folder_finland, "cases.csv"))

  d <- fread(fs::path(folder_finland, "tests.csv"))

  # sweden ----
  folder_sweden <- fs::path(folder_base, "sweden")

  folder_sweden <- fs::dir_ls(folder_sweden, regexp="[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]")
  folder_sweden <- max(folder_sweden)

  file <- fs::path(folder_sweden,"sweden.xlsx")

  d <- readxl::read_excel(
    fs::path(folder_sweden,"sweden.xlsx"),
    sheet = "Antal per dag region"
  )

  d <- readxl::read_excel(
    fs::path(folder_sweden,"sweden.xlsx"),
    sheet = glue::glue("Antal intensivv{fhidata::nb$aa}rdade per dag")
  )
  setDT(d)

  # sweden tests ----
  d <- readxl::read_excel(
    fs::path(folder_sweden,"tests.xlsx")
  )
  setDT(d)


}

