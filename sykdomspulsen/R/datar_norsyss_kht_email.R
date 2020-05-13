#' datar_norsyss_kht_email
#' @param data a
#' @param argset a
#' @param schema a
#' @export
datar_norsyss_kht_email <- function(data, argset, schema){
  if(plnr::is_run_directly()){
    tm_run_task("datar_norsyss_kht_email")
    data <- tm_get_data("datar_norsyss_kht_email")
    argset <- tm_get_argset("datar_norsyss_kht_email")
    schema <- tm_get_schema("datar_norsyss_kht_email")
  }

  folder <- sc::path("input", "sykdomspulsen_norsyss_input", create_dir = TRUE)

  files <- fs::dir_ls(folder, regexp="Sykdomspulsen-Abonnement-")
  file <- max(files)

  d <- fread(file)
  d[
    ,
    location_code :=
      dplyr::case_when(
        StedKode==0 ~ "norge",
        stringr::str_length(StedKode) %in% 1:2 ~ paste0("county",formatC(StedKode,width=2,flag=0)),
        stringr::str_length(StedKode) %in% 3:4 ~ paste0("municip",formatC(StedKode,width=4,flag=0)),
      )
    ]

  d <- melt.data.table(d, id.vars="location_code", measure.vars = c("UserName","Postmottak"))
  d[,variable:=NULL]
  setnames(d, "value", "email")
  d <- d[, email:=stringr::str_to_lower(email)]
  d <- d[!email %in% c(
    "post@fhi.no",
    "postmottak@tullemail.kommune.no",
    "postmottak@tullekommune.kommune.no"
  )]
  d <- unique(d)

  if(!sc::config$is_production){
    d <- d[
      email %in% c(
        "richardaubrey.white@fhi.no"
      )
    ]
  } else {
    extra <- data.table(
      location_code = norway_locations_long()$location_code,
      email = "sykdomspulsen@fhi.no"
    )
    d <- rbind(d, extra)

    extra <- data.table(
      location_code = norway_locations_long()$location_code,
      email = "utbrudd@fhi.no"
    )
    d <- rbind(d, extra)
  }

  schema$output$db_drop_table()
  schema$output$db_connect()
  schema$output$db_drop_constraint()
  schema$output$db_load_data_infile(d)
  schema$output$db_add_constraint()
}

#' datar_norsyss_kht_email_drop
#' @param data a
#' @param argset a
#' @param schema a
#' @export
datar_norsyss_kht_email_drop <- function(data, argset, schema){
  if(FALSE){
    tm_run_task("datar_norsyss_kht_email_drop")
    data <- tm_get_data("datar_norsyss_kht_email_drop")
    argset <- tm_get_argset("datar_norsyss_kht_email_drop")
    schema <- tm_get_schema("datar_norsyss_kht_email_drop")

  }

  schema$output$db_drop_table()
}
