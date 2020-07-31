#' datar_norsyss
#' @param data a
#' @param argset a
#' @param schema a
#' @export
datar_norsyss <- function(data, argset, schema){
  # tm_run_task("datar_norsyss")

  if(plnr::is_run_directly()){
    data <- sc::tm_get_data("datar_norsyss")
    argset <- sc::tm_get_argset("datar_norsyss")
    schema <- sc::tm_get_schema("datar_norsyss")
  }

  folder <- sc::path("input", "sykdomspulsen_norsyss_input", create_dir = TRUE)

  # get all files
  files <- fs::dir_ls(folder, regexp="norsyss_raw_[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]_[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]_[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9].RDS")

  # identify the ones with the latest dates
  files_dates <- stringr::str_extract_all(files, "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]")
  files_dates <- lapply(files_dates, function(x) x[1])
  files_dates <- unlist(unique(files_dates))
  date_max <- max(files_dates)

  # pull out all files that have the latest date
  files <- fs::dir_ls(folder, regexp=glue::glue("norsyss_raw_{date_max}_[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]_[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9].RDS"))

  file_min <- min(files)
  date_min <- stringr::str_extract_all(file_min, "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]")[[1]][2]
  schema$output$db_keep_rows_where(glue::glue("date<'{date_min}'"))

  for(i in seq_along(files)){
    file <- files[i]
    message(glue::glue("{i}/{length(files)} Cleaning {file}"))

    d <- readRDS(file)

    d[, granularity_time := "day"]
    fill_in_missing(d)

    schema$output$db_load_data_infile(d)
  }

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
