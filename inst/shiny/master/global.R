library(pool)
library(data.table)
library(magrittr)
library(ggplot2)

format_nor_perc <- function(x) paste0(fhiplot::format_nor(x, digits=1),"%")

if (.Platform$OS.type == "windows"){
  db_config <- list(
    driver = Sys.getenv("DB_DRIVER", "Sql Server"),
    server = Sys.getenv("DB_SERVER", "dm-prod"),
    db = Sys.getenv("DB_DB", "Sykdomspulsen_surv"),
    port = as.integer(Sys.getenv("DB_PORT", 1433)),
    user = Sys.getenv("DB_USER", "root"),
    password = Sys.getenv("DB_PASSWORD", "example")
  )
} else {
  system("/bin/authenticate.sh")
  db_config <- list(
    driver = Sys.getenv("DB_DRIVER", "MySQL"),
    server = Sys.getenv("DB_SERVER", "db"),
    db = Sys.getenv("DB_DB", "sykdomspuls"),
    port = as.integer(Sys.getenv("DB_PORT", 3306)),
    user = Sys.getenv("DB_USER", "root"),
    password = Sys.getenv("DB_PASSWORD", "example")
  )
}

if(db_config$driver %in% c("ODBC Driver 17 for SQL Server")){
  pool <- dbPool(
    drv = odbc::odbc(),
    driver = db_config$driver,
    server = db_config$server,
    database = db_config$db,
    port = db_config$port,
    uid = db_config$user,
    Pwd = db_config$password,
    trusted_connection = "yes"
  )
} else if(db_config$driver %in% c("Sql Server")){
  pool <- dbPool(
    drv = odbc::odbc(),
    driver = db_config$driver,
    server = db_config$server,
    database = db_config$db,
    port = db_config$port,
    trusted_connection = "yes"
  )
} else {
  pool <- dbPool(
    drv = odbc::odbc(),
    driver = db_config$driver,
    server = db_config$server,
    port = db_config$port,
    user = db_config$user,
    password = db_config$password,
    encoding = "utf8"
  )
}
DBI::dbExecute(pool, glue::glue({"USE {db_config$db};"}))



config <- new.env()
config$ages <- list(
  "Totalt",
  "0-4",
  "5-14",
  "15-19",
  "20-29",
  "30-64",
  "65+"
)

config$start_date_norsyss_standard_weekly <- as.Date("2018-01-01")
config$start_date <- as.Date("2020-03-06")
val <- pool %>% dplyr::tbl("data_norsyss") %>%
  dplyr::summarize(date = max(date)) %>%
  dplyr::collect()
config$max_date_uncertain <- as.Date(val$date[1])
config$min_date_uncertain <- config$max_date_uncertain-6

x <- pool %>% dplyr::tbl("data_norsyss") %>%
  dplyr::distinct(tag_outcome) %>%
  dplyr::collect()

# choices_location_code
x_choices <- fhidata::norway_locations_long_b2020[-1]
setorder(x_choices,location_code)
x_choices <- rbind(
  x_choices[location_code=="norge"],
  x_choices[stringr::str_detect(location_code, "^county")],
  x_choices[stringr::str_detect(location_code, "^municip")]
)

x_choices[
  fhidata::norway_locations_b2020,
  on="location_code==municip_code",
  county_name := county_name
  ]

x_choices[stringr::str_detect(location_code, "^county"), location_name := paste0(location_name, " (fylke)")]
x_choices[stringr::str_detect(location_code, "^municip"), location_name := paste0(location_name, " (kommune i ", county_name,")")]

choices_location <- x_choices$location_code
names(choices_location) <- x_choices$location_name

config$choices_location <- choices_location

choices_location_daily <- x_choices[is.na(county_name)]$location_code
names(choices_location_daily) <- x_choices[is.na(county_name)]$location_name
config$choices_location_daily <- choices_location_daily

config$choices_norsyss_tag <- list(
  "Luftveisinfeksjoner (R05, R74, R78, R83)" = "respiratoryexternal_lf_lt",
  "Magetarm (D11, D70, D73)" = "gastro_lf_lt"
)

get_granularity_geo <- function(location_code){
  if(location_code == "norge"){
    granularity_geo <- "nation"
  } else if(stringr::str_detect(location_code, "^county")){
    granularity_geo <- "county"
  } else if(stringr::str_detect(location_code, "^municip")){
    granularity_geo <- "municip"
  }
  return(granularity_geo)
}

get_dependent_location_codes <- function(location_code){
  granularity_geo <- get_granularity_geo(location_code = location_code)

  if(granularity_geo == "nation"){
    location_codes <- c("norge",unique(fhidata::norway_locations_b2020[,.(county_code)])$county_code)
  } else if(granularity_geo == "county"){
    location_codes <- c(location_code, fhidata::norway_locations_b2020[county_code==location_code]$municip_code)
  } else if(granularity_geo == "municip"){
    x_county_code <- fhidata::norway_locations_b2020[municip_code==location_code]$county_code
    location_codes <- unique(c(location_code, fhidata::norway_locations_b2020[county_code==x_county_code]$municip_code))
  }

  return(location_codes)
}
