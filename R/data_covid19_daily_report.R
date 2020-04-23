# data_covid19_daily_report
#
# Get and clean NorMOMO data
#
#  @import data.table
#
data_covid19_daily_report <- function(data, argset, schema){
  # tm_run_task("data_covid19_daily_report")
  # data <- tm_get_data("data_covid19_daily_report")
  # argset <- tm_get_argset("data_covid19_daily_report")
  # schema <- tm_get_schema("data_covid19_daily_report")

  file <- fs::dir_ls("/input/covid19/", regexp="dagsrapport_[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9].RDS")
  file <- max(file)
  master <- readRDS(file)

  date_max <- as.Date(stringr::str_extract(file,"[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]"))-1
  date_min_msis <- min(master$data_covid19_msis_by_time_location$date)

  names(schema)
  names(master)

  # data_covid19_msis_by_time_location ----
  master$data_covid19_msis_by_time_location

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
    master$data_covid19_msis_by_time_location,
    by=c("location_code","date"),
    all.x = T
  )
  d_municip[is.na(n), n:=0]
  d_municip[, granularity_geo := "municip"]

  # now generate county data
  d_county <- copy(d_municip)
  d_county[
    fhidata::norway_locations_b2020,
    on="location_code==municip_code",
    county_code := county_code
    ]
  d_county <- d_county[,.(
    n = sum(n)
  ), keyby=.(
    location_code = county_code,
    date
  )]
  d_county[, granularity_geo := "county"]

  # generate norge data
  d_norge <- d_county[,.(
    n=0
  ), keyby=.(
    date
  )]
  d_norge[,n:=NULL]
  d_norge[
    master$data_covid19_msis_by_time_location[,.(n=sum(n)),keyby="date"],
    on="date",
    n:=n
    ]
  d_norge[is.na(n),n:=0]
  d_norge[, location_code := "norge"]
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

  # data_covid19_msis_by_sex_age ----
  master$data_covid19_msis_by_sex_age
  master$data_covid19_msis_by_sex_age[
    ,
    sex := dplyr::case_when(
      sex == "Kvinne" ~ "kvinne",
      sex == "Mann" ~ "mann"
    )
    ]

  retval <- master$data_covid19_msis_by_sex_age
  retval[, granularity_time := "totalt"]
  retval[,location_code:="norge"]

  fill_in_missing(retval)

  schema$data_covid19_msis_by_sex_age$db_drop_table()
  schema$data_covid19_msis_by_sex_age$db_connect()
  schema$data_covid19_msis_by_sex_age$db_drop_constraint()
  schema$data_covid19_msis_by_sex_age$db_load_data_infile(retval)
  schema$data_covid19_msis_by_sex_age$db_add_constraint()

  # data_covid19_lab_by_time ----
  master$data_covid19_lab_by_time

  retval <- master$data_covid19_lab_by_time
  retval[, granularity_time := "day"]
  retval[,location_code:="norge"]

  fill_in_missing(retval)

  schema$data_covid19_lab_by_time$db_drop_table()
  schema$data_covid19_lab_by_time$db_connect()
  schema$data_covid19_lab_by_time$db_drop_constraint()
  schema$data_covid19_lab_by_time$db_load_data_infile(retval)
  schema$data_covid19_lab_by_time$db_add_constraint()

  # data_covid19_nir_by_time ----
  master$data_covid19_nir_by_time

  retval <- master$data_covid19_nir_by_time
  retval[, granularity_time := "day"]
  retval[,location_code:="norge"]

  fill_in_missing(retval)

  schema$data_covid19_nir_by_time$db_drop_table()
  schema$data_covid19_nir_by_time$db_connect()
  schema$data_covid19_nir_by_time$db_drop_constraint()
  schema$data_covid19_nir_by_time$db_load_data_infile(retval)
  schema$data_covid19_nir_by_time$db_add_constraint()

}




