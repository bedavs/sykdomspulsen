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

