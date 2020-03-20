library(pool)
library(data.table)
library(magrittr)
library(ggplot2)

system("/bin/authenticate.sh")

format_nor_perc <- function(x) paste0(fhiplot::format_nor(x, digits=1),"%")

db_config <- list(
  driver = Sys.getenv("DB_DRIVER", "MySQL"),
  server = Sys.getenv("DB_SERVER", "db"),
  db = Sys.getenv("DB_DB", "sykdomspuls"),
  port = as.integer(Sys.getenv("DB_PORT", 3306)),
  user = Sys.getenv("DB_USER", "root"),
  password = Sys.getenv("DB_PASSWORD", "example")
)

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

config$start_date <- as.Date("2020-03-08")
val <- pool %>% dplyr::tbl("data_norsyss") %>%
  dplyr::summarize(date = max(date)) %>%
  dplyr::collect()
config$max_date_uncertain <- val$date[1]
config$min_date_uncertain <- config$max_date_uncertain-6


