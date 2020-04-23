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

  d <- d[municip != "municip9999",
    lapply(.SD, sum),
    by = .(age, date, municip),
    .SDcols = syndromeAndConsult
  ]

  dateMin <- min(skeleton_date_min)
  dateMax <- max(skeleton_date_max)
  if (removeMunicipsWithoutConsults) {
    d[, total := sum(consult_with_influenza, na.rm = T), by = municip]
    d <- d[is.finite(total)]
    d <- d[total > 0]
    d[, total := NULL]
    skeleton <-
      data.table(expand.grid(
        municip = unique(norway_municip_merging()[municip_code_current %in% unique(d$municip) |
          municip_code_original %in% unique(d$municip)]$municip_code_original),
        unique(d$age),
        seq.Date(dateMin, dateMax, 1)
      ))
  } else {
    skeleton <-
      data.table(expand.grid(
        municip = unique(norway_municip_merging()$municip_code_original),
        unique(d$age),
        seq.Date(dateMin, dateMax, 1)
      ))
  }
  setnames(skeleton, c("municip", "age", "date"))
  skeleton[, date := data.table::as.IDate(date)]
  data <-
    merge(skeleton,
      d,
      by = c("municip", "age", "date"),
      all.x = TRUE
    )

  for (i in syndromeAndConsult) {
    data[is.na(get(i)), (i) := 0]
  }

  total <- data[municip != "municip9999",
    lapply(.SD, sum),
    keyby = .(date, municip),
    .SDcols = syndromeAndConsult
  ]
  total[, age := "total"]
  data <- rbind(total, data[age != "ukjent"])

  dates <- unique(data[, "date", with = F])
  dates[, datex := date]
  dates[, yrwk := format.Date(datex, "%G-%V")] # Week-based year, instead of normal year (%Y)
  dates[, week := as.numeric(format.Date(datex, "%V"))]
  dates[, year := as.numeric(format.Date(date, "%G"))]
  dates[, month := as.numeric(format.Date(date, "%m"))]
  dates[, season := fhi::season(yrwk)]
  dates[, x := fhi::x(week)]
  dates <- dates[year >= 2006]
  dates[, datex := NULL]
  #dates[, yrwk := NULL]
  data <- merge(data, dates, by = "date")

  # KOMMUNE MERGING
  data <-
    merge(data,
      norway_municip_merging()[, c("municip_code_original", "year", "municip_code_current", "weighting")],
      by.x = c("municip", "year"),
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

  data <- data[!is.na(municip_code_current),
    lapply(.SD, sum),
    keyby = .(municip_code_current, year, age, date, week, yrwk, month, season, x),
    .SDcols = c(syndromeAndConsult)
  ]

  # apply ceiling to ensure we have whole numbers after weighting
  for (i in syndromeAndConsult) {
    data[, (i) := ceiling(get(i))]
  }
  dim(data)
  setnames(data, "municip_code_current", "municip")

  # merge in population
  n1 <- nrow(data)
  data <- merge(data, population,
    by.x = c("municip", "age", "year"),
    by.y = c("location_code", "age", "year")
  )
  n2 <- nrow(data)

  if (n1 != n2) {
    msg("Population file not merging correctly", type = "err", slack = T)
  }

  # merging in municipalitiy-fylke names
  data[norway_locations(), on = "municip==municip_code", county := county_code]

  for (i in syndromeAndConsult) {
    data[is.na(get(i)), (i) := 0]
  }

  data <- data[date >= data.table::as.IDate("2006-01-01")]
  data[, municip := as.character(municip)]

  setnames(hellidager, c("date", "HelligdagIndikator"))
  hellidager[, date := data.table::as.IDate(date)]
  if (testIfHelligdagIndikatorFileIsOutdated &
    lubridate::today() > max(hellidager$date)) {
    msg("HELLIGDAGER NEEDS UPDATING", type = "err", slack = T)
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
      "date",
      "HelligdagIndikator",
      "municip",
      "age",
      "n",
      "consult_without_influenza",
      "consult_with_influenza",
      "pop",
      "yrwk",
      "year",
      "week",
      "month",
      "season",
      "x"
    )
  ))

  setorder(data, municip, age, date)
  setkey(data, municip, age, date)

  data[, granularityGeo := "municip"]
  setnames(data, "municip", "location")

  # create fylke
  fylke <- data[, .(
    HelligdagIndikator = mean(HelligdagIndikator),
    n = sum(n),
    consult_without_influenza = sum(consult_without_influenza),
    consult_with_influenza = sum(consult_with_influenza),
    pop = sum(pop)
  ), keyby = .(county, age, date, year, yrwk, week, month, season,x)]

  fylke[, granularityGeo := "county"]
  fylke[, location:=county]
  fylke[, county:= NULL]

  # create national
  norge <- data[, .(
    HelligdagIndikator = mean(HelligdagIndikator),
    n = sum(n),
    consult_without_influenza = sum(consult_without_influenza),
    consult_with_influenza = sum(consult_with_influenza),
    pop = sum(pop)
  ), keyby = .(age, date, year, yrwk, week, month, season,x)]

  data[, county:=NULL]
  norge[, location := "norge"]
  norge[, granularityGeo := "nation"]

  data <- rbind(data, fylke, norge)
  setcolorder(data, c("granularityGeo",  "location", "age", "date"))
  setorderv(data, c("granularityGeo", "location", "age", "date"))
  setkey(data, location, age)
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
    "yrwk",
    "year",
    "week",
    "month",
    "season",
    "x"

  ))

  data[, sex:="total"]
  data[, border:=config$border]
  data[, granularity_time:="day"]
  ## data[, yrwk := fhi::isoyearweek(date)]
  ## data[, year := fhi::isoyear_n(date)]
  ## data[, week := fhi::isoweek_n(date)]
  ## data[, season := fhi::season(yrwk)]

  ## if (!ValidateDataClean(data)) {
  ## }

  return(data)
}

# Identify the latest raw/clean datasets
# @param raw Folder containing raw data
# @param clean Folder containing clean data
# @import data.table
IdentifyDatasets <-
  function(raw = list.files(path("input", "norsyss"), "^partially_formatted_"))
  {
    print(raw)
    ## res <- IdentifyAllDatasets(raw = raw, clean = clean)
    ## print(res)
    ## if (nrow(res) > 0) {
    ##   res <- res[nrow(res)]
    ## }

    return(data.table(raw))
  }


# get_NorSySS_data
#
# Get and clean from file
#
#
# @import data.table
data_norsyss <- function(data, argset, schema){
  # tm_run_task("data_norsyss")
  # argset <- tm_get_argset("data_norsyss")
  # schema <- tm_get_schema("data_norsyss")
  syndromes <- argset$syndromes

  file <- fs::dir_ls("/input/norsyss/", regexp="norsyss_[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9].txt")
  file <- max(file)

  msg(sprintf("Cleaning file %s", file))
  #EmailNotificationOfNewData(files$id)

  d <- fread(file)
  setnames(d,"date","x_date")
  dates <- unique(d[,"x_date"])
  dates[,date:=data.table::as.IDate(x_date)]
  dates[, isoyear := fhi::isoyear_n(date)]

  skeleton_date_max <- as.Date(stringr::str_extract(file, "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]"))-1
  skeleton_date_min <- as.Date(min(d$x_date))

  d[dates,on="x_date", date:=date]
  d[dates,on="x_date", isoyear:=isoyear]
  d[,x_date:=NULL]

  # finding dates to run
  max_year_in_data <- fhi::isoyear_n(max(d$date))

  max_date <- schema$output$dplyr_tbl() %>%
    dplyr::group_by(tag_outcome) %>%
    dplyr::summarise(date = max(date, na.rm=T)) %>%
    dplyr::collect() %>%
    latin1_to_utf8()

  if(nrow(max_date)==0){
    max_year_in_db <- 2006
  } else {
    max_date <- min(max_date$date)
    max_year_in_db <- fhi::isoyear_n(max_date)-10
  }
  if(max_year_in_db <=2006) max_year_in_db <- 2006

  # if we aren't deleting all the data
  # check to see if the required syndromes are in the database for each year
  # if we don't have a perfect syndrome match, then delete all the data
  # and start again
  if(max_year_in_db > 2006){
    syndromes_in_db <- schema$output$dplyr_tbl() %>%
      dplyr::distinct(year, tag_outcome) %>%
      dplyr::collect() %>%
      latin1_to_utf8()

    delete_all_data <- FALSE
    for(y in unique(syndromes_in_db$year)){
      syndromes_in_db_in_year <- syndromes_in_db[year==y]$tag_outcome
      if(sum(!argset$syndromes$tag_output %in% syndromes_in_db_in_year)>0) delete_all_data <- TRUE
      if(sum(!syndromes_in_db_in_year %in% argset$syndromes$tag_output)>0) delete_all_data <- TRUE
    }
    if(delete_all_data) max_year_in_db <- 2006
  }

  schema$output$db_drop_rows_where(glue::glue("year>={max_year_in_db}"))
  #schema$output$db_drop_constraint()
  years_to_process <- max_year_in_db:max_year_in_data

  for (i in 1:nrow(syndromes)) {
    conf <- syndromes[i]
    msg(sprintf("Processing %s/%s: %s -> %s", i, nrow(syndromes), conf$tag_input, conf$tag_output))

    res <- CleanData(
      d = copy(d[
        isoyear %in% years_to_process &
        Kontaktype %in% conf$contactType[[1]] &
        Praksis %in% conf$practice_type[[1]]
        ]),
      syndrome = conf$tag_input,
      skeleton_date_max = skeleton_date_max,
      skeleton_date_min = skeleton_date_min
    )
    res[, tag_outcome:=conf$tag_output]
    res[, gender:="total"]
    # make sure there's nothing funny going on with week 53
    res <- res[year %in% years_to_process]

    schema$output$db_load_data_infile(res)
  }
  #schema$output$db_add_constraint()
  msg("New data is now formatted and ready")
  return(TRUE)
}
