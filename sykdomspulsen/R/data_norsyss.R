#' data_norsyss
#' @param data a
#' @param argset a
#' @param schema a
#' @export
data_norsyss <- function(data, argset, schema){
  # tm_run_task("data_norsyss")

  if(plnr::is_run_directly()){
    data <- sc::tm_get_data("data_norsyss")
    argset <- sc::tm_get_argset("data_norsyss")
    schema <- sc::tm_get_schema("data_norsyss")
  }

  schema$output$add_indexes()
  schema$output$list_indexes_db()

  syndromes <- argset$syndromes

  folder <- sc::path("input", "sykdomspulsen_norsyss_input", create_dir = TRUE)
  file <- fs::dir_ls(folder, regexp="norsyss_[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9].txt")
  file <- max(file)

  message(sprintf("Cleaning file %s", file))
  #EmailNotificationOfNewData(files$id)

  d <- fread(file)
  setnames(d,"date","x_date")
  dates <- unique(d[,"x_date"])
  dates[,date:=data.table::as.IDate(x_date)]
  dates[, isoyear := fhi::isoyear_n(date)]

  skeleton_date_max <- as.Date(stringr::str_extract(file, "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]"))-1
  skeleton_date_min <- as.Date(min(d$x_date))
  skeleton_year_min <- fhi::isoyear_n(skeleton_date_min)

  d[dates,on="x_date", date:=date]
  d[dates,on="x_date", isoyear:=isoyear]
  d[,x_date:=NULL]

  ###################
  lubridate::now()
  schema$output$db_keep_rows_where(glue::glue("year<{skeleton_year_min}"))
  lubridate::now()
  ###################

  master <- d

  for (i in 1:nrow(syndromes)) {
    conf <- syndromes[i]
    message(sprintf("Processing %s/%s: %s -> %s", i, nrow(syndromes), conf$tag_input, conf$tag_output))

    res <- CleanData(
      d = copy(master[
        Kontaktype %in% conf$contactType[[1]] &
          Praksis %in% conf$practice_type[[1]]
      ]),
      syndrome = conf$tag_input,
      skeleton_date_max = skeleton_date_max,
      skeleton_date_min = skeleton_date_min
    )
    res[, tag_outcome:=conf$tag_output]

    schema$output$db_load_data_infile(res)
  }
  message("Copying into data_norsyss_recent")

  sc::drop_table("data_norsyss_recent")
  sql <- glue::glue("SELECT * INTO data_norsyss_recent FROM {schema$output$db_table} WHERE year >= {fhi::isoyear_n()-1};")
  DBI::dbExecute(schema$output$conn, sql)

  message("New data is now formatted and ready")


  return(TRUE)
}


# Format the raw data
CleanData <- function(d,
                      syndrome,
                      skeleton_date_max = max(d$date),
                      skeleton_date_min = min(d$date),
                      population = norway_population(),
                      hellidager = fhidata::norway_dates_holidays,
                      testIfHelligdagIndikatorFileIsOutdated = TRUE,
                      removeMunicipsWithoutConsults = FALSE) {
  # variables used in data.table functions in this function
  . <- NULL
  municip <- NULL
  age <- NULL
  datex <- NULL
  yrwk <- NULL
  municipEnd <- NULL
  consult <- NULL
  consultWithInfluensa <- NULL
  consultWithoutInfluensa <- NULL
  influensa <- NULL
  pop <- NULL
  error <- NULL
  n <- NULL
  granularityGeo <- NULL
  HelligdagIndikator <- NULL
  county <- NULL
  location <- NULL
  # end

  CONFIG <- config

  population <- copy(population)

  # fix population age categories
  for (i in which(names(CONFIG$def$age$norsyss) != "total")) {
    population[age %in% CONFIG$def$age$norsyss[[i]], agex := names(CONFIG$def$age$norsyss)[i]]
  }
  population[, age := NULL]
  setnames(population, "agex", "age")

  population <- population[, .(
    pop = sum(pop)
  ), keyby = .(
    location_code, age, year
  )]

  total <- population[, .(
    pop = sum(pop)
  ), keyby = .(
    location_code, year
  )]
  total[, age := "total"]

  population <- rbind(population, total)
  # end population fix

  if (!"IDate" %in% class(d$date)) {
    d[, date := data.table::as.IDate(date)]
  }

  d[, consult_without_influenza := consult - influensa]
  setnames(d, "consult", "consult_with_influenza")

  syndromeAndConsult <- unique(c(
    syndrome,
    "consult_with_influenza",
    "consult_without_influenza"
  ))

  d <- d[location_code != "municip9999",
    lapply(.SD, sum),
    by = .(age, date, location_code),
    .SDcols = syndromeAndConsult
  ]

  # seperate out municip vs ward
  d_w <- d[stringr::str_detect(location_code,"^ward")]
  d_m <- d[stringr::str_detect(location_code,"^municip")]

  # create skeleton for wards
  dateMin <- min(skeleton_date_min)
  dateMax <- max(skeleton_date_max)
  if (removeMunicipsWithoutConsults) {
    d_w[, total := sum(consult_with_influenza, na.rm = T), by = municip]
    d_w <- d_w[is.finite(total)]
    d_w <- d_w[total > 0]
    d_w[, total := NULL]
    skeleton <-
      data.table(expand.grid(
        location_code = unique(norway_ward_merging()[
          ward_code_current %in% unique(d_w$location_code) |
          ward_code_original %in% unique(d_w$location_code)
        ]$municip_code_original),
        unique(d_w$age),
        seq.Date(dateMin, dateMax, 1)
      ))
  } else {
    skeleton <-
      data.table(expand.grid(
        location_code = unique(norway_ward_merging()$ward_code_original),
        unique(d_w$age),
        seq.Date(dateMin, dateMax, 1)
      ))
  }
  setnames(skeleton, c("location_code", "age", "date"))
  skeleton[, date := data.table::as.IDate(date)]
  data <-
    merge(skeleton,
          d_w,
          by = c("location_code", "age", "date"),
          all.x = TRUE
    )

  for (i in syndromeAndConsult) {
    data[is.na(get(i)), (i) := 0]
  }

  total <- data[,
                lapply(.SD, sum),
                keyby = .(date, location_code),
                .SDcols = syndromeAndConsult
  ]
  total[, age := "total"]
  data <- rbind(total, data[age != "ukjent"])

  data[, year := fhi::isoyear_n(date)]

  # BYDEL MERGING
  data <- merge(
    data,
    norway_ward_merging()[, c("ward_code_original", "year", "ward_code_current", "weighting")],
    by.x = c("location_code", "year"),
    by.y = c("ward_code_original", "year"),
    all.x = T,
    allow.cartesian = T
    )
  dim(data)
  data <- data[!is.na(ward_code_current)]

  # apply the weighting
  for (i in syndromeAndConsult) {
    data[, (i) := get(i) * weighting]
  }

  data_w <- data[!is.na(ward_code_current),
               lapply(.SD, sum),
               keyby = .(ward_code_current, year, age, date),
               .SDcols = c(syndromeAndConsult)
  ]

  # create skeleton for municips
  dateMin <- min(skeleton_date_min)
  dateMax <- max(skeleton_date_max)
  if (removeMunicipsWithoutConsults) {
    d_m[, total := sum(consult_with_influenza, na.rm = T), by = municip]
    d_m <- d_m[is.finite(total)]
    d_m <- d_m[total > 0]
    d_m[, total := NULL]
    skeleton <-
      data.table(expand.grid(
        location_code = unique(norway_municip_merging()[
          municip_code_current %in% unique(d_m$location_code) |
          municip_code_original %in% unique(d_m$location_code)
        ]$municip_code_original),
        unique(d_m$age),
        seq.Date(dateMin, dateMax, 1)
      ))
  } else {
    skeleton <-
      data.table(expand.grid(
        location_code = unique(norway_municip_merging()$municip_code_original),
        unique(d_m$age),
        seq.Date(dateMin, dateMax, 1)
      ))
  }
  setnames(skeleton, c("location_code", "age", "date"))
  skeleton[, date := data.table::as.IDate(date)]
  data <-
    merge(skeleton,
      d_m,
      by = c("location_code", "age", "date"),
      all.x = TRUE
    )

  for (i in syndromeAndConsult) {
    data[is.na(get(i)), (i) := 0]
  }

  total <- data[,
    lapply(.SD, sum),
    keyby = .(date, location_code),
    .SDcols = syndromeAndConsult
  ]
  total[, age := "total"]
  data <- rbind(total, data[age != "ukjent"])

  data[, year := fhi::isoyear_n(date)]

#
#   dates <- unique(data[, "date", with = F])
#   dates[, datex := date]
#   dates[, yrwk := format.Date(datex, "%G-%V")] # Week-based year, instead of normal year (%Y)
#   dates[, week := as.numeric(format.Date(datex, "%V"))]
#   dates[, year := as.numeric(format.Date(date, "%G"))]
#   dates[, month := as.numeric(format.Date(date, "%m"))]
#   dates[, season := fhi::season(yrwk)]
#   dates[, x := fhi::x(week)]
#   dates <- dates[year >= 2006]
#   dates[, datex := NULL]
#   #dates[, yrwk := NULL]
#   data <- merge(data, dates, by = "date")

  # KOMMUNE MERGING
  data <-
    merge(data,
      norway_municip_merging()[, c("municip_code_original", "year", "municip_code_current", "weighting")],
      by.x = c("location_code", "year"),
      by.y = c("municip_code_original", "year"),
      all.x = T,
      allow.cartesian = T
    )
  dim(data)
  data <- data[!is.na(municip_code_current)]

  # apply the weighting
  for (i in syndromeAndConsult) {
    data[, (i) := get(i) * weighting]
  }

  data_m <- data[!is.na(municip_code_current),
    lapply(.SD, sum),
    keyby = .(municip_code_current, year, age, date),
    .SDcols = c(syndromeAndConsult)
  ]

  # COMBINE BYDEL AND MUNICIP
  setnames(data_w,"ward_code_current","location_code")
  setnames(data_m,"municip_code_current","location_code")

  data_w[, granularity_geo := "ward"]
  data_m[, granularity_geo := "municip"]
  data <- rbind(data_w, data_m)

  # apply ceiling to ensure we have whole numbers after weighting
  for (i in syndromeAndConsult) {
    data[, (i) := ceiling(get(i))]
  }
  dim(data)

  # merge in population
  n1 <- nrow(data)
  data <- merge(data, population,
    by.x = c("location_code", "age", "year"),
    by.y = c("location_code", "age", "year")
  )
  n2 <- nrow(data)

  if (n1 != n2) {
    stop("Population file not merging correctly")
  }

  # merging in municipalitiy-fylke names
  data[norway_locations(), on = "location_code==municip_code", county := county_code]

  for (i in syndromeAndConsult) {
    data[is.na(get(i)), (i) := 0]
  }

  data <- data[date >= data.table::as.IDate("2006-01-01")]
  data[, location_code := as.character(location_code)]

  setnames(hellidager, c("date", "HelligdagIndikator"))
  hellidager[, date := data.table::as.IDate(date)]
  if (testIfHelligdagIndikatorFileIsOutdated &
    lubridate::today() > max(hellidager$date)) {
    stop("HELLIGDAGER NEEDS UPDATING")
  }
  data[hellidager, on = "date", HelligdagIndikator := HelligdagIndikator]
  data[is.na(HelligdagIndikator), HelligdagIndikator := FALSE]

  #data[, year := NULL]

  setnames(data, syndrome, "n")

  if (!"consult_with_influenza" %in% names(data)) {
    data[, consult_with_influenza := n]
  }
  if (!"consult_without_influenza" %in% names(data)) {
    data[, consult_without_influenza := n]
  }
  setcolorder(data, unique(
    c(
      "granularity_geo",
      "date",
      "HelligdagIndikator",
      "location_code",
      "age",
      "n",
      "consult_without_influenza",
      "consult_with_influenza",
      "pop",
      "year"
    )
  ))

  setorder(data, granularity_geo,location_code, age, date)
  setkey(data, granularity_geo, location_code, age, date)

  # create fylke
  fylke <- data[granularity_geo=="municip", .(
    HelligdagIndikator = mean(HelligdagIndikator),
    n = sum(n),
    consult_without_influenza = sum(consult_without_influenza),
    consult_with_influenza = sum(consult_with_influenza),
    pop = sum(pop)
  ), keyby = .(county, age, date, year)]

  fylke[, granularity_geo := "county"]
  fylke[, location_code:=county]
  fylke[, county:= NULL]

  # create national
  norge <- data[granularity_geo=="municip", .(
    HelligdagIndikator = mean(HelligdagIndikator),
    n = sum(n),
    consult_without_influenza = sum(consult_without_influenza),
    consult_with_influenza = sum(consult_with_influenza),
    pop = sum(pop)
  ), keyby = .(age, date, year)]

  data[, county:=NULL]
  norge[, location_code := "norge"]
  norge[, granularity_geo := "nation"]

  data <- rbind(data, fylke, norge)
  setcolorder(data, c("granularity_geo",  "location_code", "age", "date"))
  setorderv(data, c("granularity_geo", "location_code", "age", "date"))
  setkey(data, location_code, age)
  setnames(data, c(
    "granularity_geo",
    "location_code",
    "age",
    "date",
    "holiday",
    "n",
    "consult_without_influenza",
    "consult_with_influenza",
    "pop",
    "year"
  ))

  data[, sex:="total"]
  data[, border:=config$border]
  data[, granularity_time:="day"]
  fill_in_missing(data)

  return(data)
}
