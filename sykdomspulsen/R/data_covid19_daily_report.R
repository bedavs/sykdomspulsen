#' data_covid19_daily_report
#' @param data a
#' @param argset a
#' @param schema a
#' @export
data_covid19_daily_report <- function(data, argset, schema){
  # tm_run_task("data_covid19_daily_report")
  # tm_run_task("prelim_data_covid19_daily_report")

  if(plnr::is_run_directly()){
    data <- sc::tm_get_data("data_covid19_daily_report")
    argset <- sc::tm_get_argset("data_covid19_daily_report")
    schema <- sc::tm_get_schema("data_covid19_daily_report")
  }

  folder <- sc::path("input","sykdomspulsen_covid19_dagsrapport_input")
  if(!fs::dir_exists(folder)){
    fs::dir_create(folder)
  }
  file <- fs::dir_ls(folder, regexp="dagsrapport_[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9].RDS")
  file <- file[!stringr::str_detect(file,"processed")]
  file <- max(file)
  file_processed <- stringr::str_replace(
    file,
    sc::path("input","sykdomspulsen_covid19_dagsrapport_input","dagsrapport"),
    sc::path("input","sykdomspulsen_covid19_dagsrapport_input","processed_dagsrapport")
  )

  master <- readRDS(file)

  date_max <- as.Date(stringr::str_extract(file,"[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]"))-1
  date_min_msis <- min(master$data_covid19_msis_by_time_location_nation$date)

  names(schema)
  names(master)

  # data_covid19_msis_by_time_location ----
  # municip
  master$data_covid19_msis_by_time_location_municip

  skeleton <- expand.grid(
    location_code = fhidata::norway_locations_b2020$municip_code,
    date = seq.Date(
      date_min_msis,
      date_max,
      by = 1
    ),
    stringsAsFactors = FALSE
  )
  setDT(skeleton)

  d_municip <- merge(
    skeleton,
    master$data_covid19_msis_by_time_location_municip,
    by=c("location_code","date"),
    all.x = T
  )
  d_municip[is.na(n), n:=0]
  d_municip[, granularity_geo := "municip"]

  # county
  master$data_covid19_msis_by_time_location_county

  skeleton <- expand.grid(
    location_code = unique(fhidata::norway_locations_b2020$county_code),
    date = seq.Date(
      date_min_msis,
      date_max,
      by = 1
    ),
    stringsAsFactors = FALSE
  )
  setDT(skeleton)

  d_county <- merge(
    skeleton,
    master$data_covid19_msis_by_time_location_county,
    by=c("location_code","date"),
    all.x = T
  )
  d_county[is.na(n), n:=0]
  d_county[, granularity_geo := "county"]

  # norge
  master$data_covid19_msis_by_time_location_county

  skeleton <- expand.grid(
    location_code = "norge",
    date = seq.Date(
      date_min_msis,
      date_max,
      by = 1
    ),
    stringsAsFactors = FALSE
  )
  setDT(skeleton)

  d_norge <- merge(
    skeleton,
    master$data_covid19_msis_by_time_location_nation,
    by=c("location_code","date"),
    all.x = T
  )
  d_norge[is.na(n), n:=0]
  d_norge[, granularity_geo := "nation"]

  d <- rbind(d_municip, d_county, d_norge)

  d[, granularity_time := "day"]
  fill_in_missing(d)

  d_week <- d[,.(
    n=sum(n)
  ), keyby=.(
    location_code,
    granularity_geo,
    yrwk
  )]
  d_week[,granularity_time:="week"]
  fill_in_missing(d_week)
  d_week[,date:=as.Date(date)]

  setcolorder(d_week,names(d))
  retval <- rbind(d,d_week)

  schema$data_covid19_msis_by_time_location$db_drop_table()
  schema$data_covid19_msis_by_time_location$db_connect()
  schema$data_covid19_msis_by_time_location$db_drop_constraint()
  schema$data_covid19_msis_by_time_location$db_load_data_infile(retval)
  schema$data_covid19_msis_by_time_location$db_add_constraint()

  # data_covid19_msis_by_time_infected_abroad ----
  master$data_covid19_msis_by_time_infected_abroad
  master$data_covid19_msis_by_time_infected_abroad[,
                                                   tag_location_infected := stringr::str_to_lower(tag_location_infected)
                                                   ]

  skeleton <- expand.grid(
    tag_location_infected = c("utlandet", "norge", "ukjent"),
    date = seq.Date(
      date_min_msis,
      date_max,
      by = 1
    ),
    stringsAsFactors = FALSE
  )
  setDT(skeleton)
  skeleton[,yrwk := fhi::isoyearweek(date)]
  skeleton[,date:=NULL]
  skeleton <- unique(skeleton)

  skeleton[
    master$data_covid19_msis_by_time_infected_abroad,
    on=c("yrwk","tag_location_infected"),
    n:=n
    ]
  skeleton[is.na(n), n:=0]
  skeleton[, granularity_time := "week"]
  skeleton[,location_code:="norge"]

  fill_in_missing(skeleton)
  retval <- skeleton

  schema$data_covid19_msis_by_time_infected_abroad$db_drop_table()
  schema$data_covid19_msis_by_time_infected_abroad$db_connect()
  schema$data_covid19_msis_by_time_infected_abroad$db_drop_constraint()
  schema$data_covid19_msis_by_time_infected_abroad$db_load_data_infile(retval)
  schema$data_covid19_msis_by_time_infected_abroad$db_add_constraint()
  # tbl("data_covid19_msis_by_time_infected_abroad")

  # data_covid19_msis_by_time_sex_age ----
  master$data_covid19_msis_by_time_sex_age
  retval <- copy(master$data_covid19_msis_by_time_sex_age)

  retval[
    ,
    sex := dplyr::case_when(
      sex == "Kvinne" ~ "female",
      sex == "Mann" ~ "male"
    )
    ]

  skeleton <- expand.grid(
    yrwk = unique(retval$yrwk),
    sex = c("female","male"),
    age = c(
      "0-9",
      "10-19",
      "20-29",
      "30-39",
      "40-49",
      "50-59",
      "60-69",
      "70-79",
      "80-89",
      "90+"
    ),
    stringsAsFactors = F
  )
  setDT(skeleton)
  retval <- merge(
    skeleton,
    retval,
    by=c("yrwk","sex","age"),
    all=T
  )
  retval[is.na(n), n:=0]

  retval[, granularity_time := "week"]
  retval[,location_code:="norge"]
  fill_in_missing(retval)

  total <- retval[,.(
    n = sum(n)
  ), keyby=.(
    sex,age
  )]
  total[, granularity_time := "total"]
  total[,location_code:="norge"]
  fill_in_missing(total)

  retval <- rbind_standardize(retval,total)

  schema$data_covid19_msis_by_time_sex_age$db_drop_table()
  schema$data_covid19_msis_by_time_sex_age$db_connect()
  schema$data_covid19_msis_by_time_sex_age$db_drop_constraint()
  schema$data_covid19_msis_by_time_sex_age$db_load_data_infile(retval)
  schema$data_covid19_msis_by_time_sex_age$db_add_constraint()
  # tbl("data_covid19_msis_by_time_sex_age")

  # data_covid19_lab_by_time ----
  master$data_covid19_lab_by_time

  retval <- data.table(master$data_covid19_lab_by_time)
  retval[, granularity_time := "day"]
  retval[,location_code:="norge"]

  fill_in_missing(retval)

  schema$data_covid19_lab_by_time$db_drop_table()
  schema$data_covid19_lab_by_time$db_connect()
  schema$data_covid19_lab_by_time$db_drop_constraint()
  schema$data_covid19_lab_by_time$db_load_data_infile(retval)
  schema$data_covid19_lab_by_time$db_add_constraint()

  # data_covid19_hospital_by_time ----
  master$data_covid19_hospital_by_time

  retval <- copy(master$data_covid19_hospital_by_time)
  retval[,date:=as.Date(date)]
  retval[, granularity_time := "day"]
  retval[,location_code:="norge"]

  fill_in_missing(retval)

  # make sure we dont include data from the future
  retval <- retval[date <= date_max]

  schema$data_covid19_hospital_by_time$db_drop_table()
  schema$data_covid19_hospital_by_time$db_connect()
  schema$data_covid19_hospital_by_time$db_drop_constraint()
  schema$data_covid19_hospital_by_time$db_load_data_infile(retval)
  schema$data_covid19_hospital_by_time$db_add_constraint()
  sc::tbl("data_covid19_hospital_by_time")

  # data_covid19_deaths ----
  master$data_covid19_deaths

  retval <- master$data_covid19_deaths[, .(cum_n = ant_m)]
  retval[, granularity_time := "total"]
  retval[,location_code:="norge"]

  fill_in_missing(retval)

  schema$data_covid19_deaths$db_drop_table()
  schema$data_covid19_deaths$db_connect()
  schema$data_covid19_deaths$db_drop_constraint()
  schema$data_covid19_deaths$db_load_data_infile(retval)
  schema$data_covid19_deaths$db_add_constraint()

  # data_covid19_demographics ----
  master$data_covid19_demographics

  retval <- copy(master$data_covid19_demographics)
  retval[, granularity_time := "total"]
  retval[,location_code:="norge"]

  fill_in_missing(retval)

  schema$data_covid19_demographics$db_drop_table()
  schema$data_covid19_demographics$db_connect()
  schema$data_covid19_demographics$db_drop_constraint()
  schema$data_covid19_demographics$db_load_data_infile(retval)
  schema$data_covid19_demographics$db_add_constraint()
}




