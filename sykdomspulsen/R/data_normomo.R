#' data_pre_normomo
#' @param data a
#' @param argset a
#' @param schema a
#' @export
data_pre_normomo <- function(data, argset, schema){
  folder <- sc::path("input", "sykdomspulsen_normomo_input", create_dir = TRUE, trailing_slash = T)

  if(sc::config$is_production){
    data_grab <- glue::glue(
      'get -r "ut" {folder}\n',
      'rm ut/*'
    )
    data_grab <- glue::glue(
      'get -r "ut" {folder}\n'
    )
    data_grab_txt <- tempfile()
    cat(data_grab, file = data_grab_txt)

    cmd <- glue::glue(
      'sshpass -p{Sys.getenv("NORMOMO_EVRY_PW")} ',
      'sftp -o StrictHostKeyChecking=no -oBatchMode=no -b {data_grab_txt} {Sys.getenv("NORMOMO_EVRY_USER")}; ',
      'mv {folder}ut/* {folder}; ',
      'rmdir {folder}ut'
    )
    system(cmd)
  }
}


#' datar_normomo
#' @param data a
#' @param argset a
#' @param schema a
#' @export
datar_normomo <- function(data, argset, schema){
  if(plnr::is_run_directly()){
    # tm_run_task("datar_normomo")
    data <- sc::tm_get_data("datar_normomo")
    argset <- sc::tm_get_argset("datar_normomo")
    schema <- sc::tm_get_schema("datar_normomo")
  }

  folder <- sc::path("input", "sykdomspulsen_normomo_input", create_dir = TRUE)

  files <- fs::dir_ls(folder, regexp="FHIDOD2_[0-9]+.txt$")
  file <- max(files)
  date_extracted <- stringr::str_extract(file, "[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]")
  date_extracted <- glue::glue(
    "{yyyy}-{mm}-{dd}",
    yyyy = stringr::str_sub(date_extracted, 1, 4),
    mm = stringr::str_sub(date_extracted, 5, 6),
    dd = stringr::str_sub(date_extracted, 7, 8)
  )

  date_extracted <- as.Date(date_extracted)
  d <- data.table::fread(file)

  d[, DoD := as.Date(as.character(DODS_DATO), format = "%Y%m%d")]
  d[, DoR := as.Date(as.character(ENDR_DATO), format = "%Y%m%d")]
  d[, DoB := as.Date(as.character(FDATO_YYYYMMDD), format = "%Y%m%d")]
  d[, age := floor(as.numeric(difftime(DoD, DoB, units = "days")) / 365.25)]
  d[is.na(DoR), DoR := DoD + 1]
  d[DoR >= "2015-09-03", DoR := DoR + 1]

  d[, year := as.numeric(stringr::str_sub(DODS_DATO, 1, 4))]
  d[, county_code := paste0("county", formatC(FYLKE, width = 2, flag = "0"))]

  d <- merge(
    d,
    norway_county_merging(),
    by.x = c("county_code", "year"),
    by.y = c("county_code_original", "year"),
    all.x = T,
    allow.cartesian = TRUE
  )

  d[is.na(weighting), weighting := 1]
  d[, x := 1:.N]
  set.seed(4)
  d[, keep := sample(c(TRUE, FALSE), 1, replace = T, prob = c(weighting, 1 - weighting)), by = x]

  d <- d[keep == TRUE]

  d[, sex := dplyr::case_when(
    KJONN == "K" ~ "female",
    KJONN == "M" ~ "male"
  )]

  d[, county_code := NULL]
  d[, FYLKE := NULL]
  d[, weighting := NULL]
  d[, x := NULL]
  d[, keep := NULL]
  d[, year:=NULL]
  d[, STATUS:=NULL]
  d[, DODS_DATO := NULL]
  d[, FDATO_YYYYMMDD:=NULL]
  d[, KJONN := NULL]
  d[, ENDR_DATO := NULL]
  d[, LEVERINGSDATO := NULL]

  setnames(d, "county_code_current", "location_code")

  d[, date_extracted:=date_extracted]

  d2 <- copy(d)
  d2[, sex := "total"]
  retval <- rbind(d,d2)

  d <- copy(retval)
  d[, location_code := "norge"]
  retval <- rbind(retval,d)

  retval[,uuid:=1:.N]

  schema$output$db_drop_table()
  schema$output$db_connect()
  schema$output$db_drop_constraint()
  schema$output$db_load_data_infile(retval)
  schema$output$db_add_constraint()
}

#' datar_normomo_drop
#' @param data a
#' @param argset a
#' @param schema a
#' @export
datar_normomo_drop <- function(data, argset, schema){
  # tm_run_task("datar_normomo_drop")
  # data <- tm_get_data("datar_normomo_drop")
  # argset <- tm_get_argset("datar_normomo_drop")
  # schema <- tm_get_schema("datar_normomo_drop")

  schema$output$db_drop_table()
}
