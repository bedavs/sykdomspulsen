#' analysis_normomo
#' @param data a
#' @param argset a
#' @param schema a
#' @export
analysis_normomo <-  function(data, argset, schema){
  if(plnr::is_run_directly()){
    # tm_run_task("datar_normomo")
    # tm_run_task("datar_normomo_drop")
    # tm_run_task("analysis_normomo")

    sc::tm_update_plans("analysis_normomo")
    data <- sc::tm_get_data("analysis_normomo", index_plan=2)
    argset <- sc::tm_get_argset("analysis_normomo", index_plan=2, index_argset = 9)
    schema <- sc::tm_get_schema("analysis_normomo")
  }

  fs::dir_create(argset$wdir)

  d <- as.data.frame(data$raw[fhi::isoyear_n(DoR)<=argset$year_end])
  MOMO::SetOpts(
    DoA = argset$date_extracted,
    DoPR = as.Date("2012-1-1"),
    WStart = 1,
    WEnd = 52,
    country = argset$location_code,
    source = "FHI",
    MDATA = d,
    HDATA = analysis_normomo_hfile(),
    INPUTDIR = tempdir(),
    WDIR = argset$wdir,
    back = 7,
    WWW = 52*5,
    Ysum = argset$year_end,
    Wsum = 40,
    plotGraphs = FALSE,
    delayVersion = "richard",
    delayFunction = NULL,
    MOMOgroups = argset$momo_groups,
    MOMOmodels = argset$momo_models,
    Ydrop = argset$Ydrop,
    Wdrop = argset$Wdrop,
    verbose = FALSE
  )

  tryCatch({
    MOMO::RunMoMo()
  }, error=function(err){
    stop("ERROR HERE")
  })

  if(argset$upload){
    # upload results
    data_dirty <- rbindlist(MOMO::dataExport$toSave, fill = TRUE)
    data_clean <- clean_exported_momo_data(
      data_dirty,
      location_code = argset$location_code,
      sex = argset$sex
    )
    # only upload the requested parts
    data_clean <- data_clean[year >= argset$year_start_upload & year <= argset$year_end_upload]
    schema$output$db_upsert_load_data_infile(data_clean, verbose = T)

    # upload daily data
    temp <- list()
    for(i in seq_along(argset$momo_groups)){
      txt <- glue::glue("data$raw[{argset$momo_groups[[i]]}]")
      temp[[i]] <- eval(parse(text = txt))
      temp[[i]][, age := NULL]
      temp[[i]][ , GROUP := names(argset$momo_groups)[i]]
    }
    temp <- rbindlist(temp)
    temp[,
      age := dplyr::case_when(
        GROUP == "0to4" ~ "0-4",
        GROUP == "5to14" ~ "5-14",
        GROUP == "15to64" ~ "15-64",
        GROUP == "65to74" ~ "65-74",
        GROUP == "75to84" ~ "75-84",
        GROUP == "85P" ~ "85+",
        GROUP == "Total" ~ "total",
        GROUP == "65P" ~ "65+"
      )
    ]
    setnames(temp, "DoD", "date")

    d <- temp[,.(
      n_obs = .N
    ), keyby=.(
      age, location_code, sex, date
    )]

    skeleton <- expand.grid(
      location_code = unique(temp$location_code),
      sex = unique(temp$sex),
      age = unique(temp$age),
      date = seq.Date(
        from = fhidata::days[year==argset$year_start_upload]$mon[1],
        to = max(data_clean$date),
        by = 1
      ),
      stringsAsFactors = F
    )
    setDT(skeleton)
    skeleton[, date:=data.table::as.IDate(date)]
    skeleton[
      d,
      on=c("location_code","sex","age","date"),
      n_obs := n_obs
    ]
    skeleton[is.na(n_obs),n_obs := 0]
    skeleton[
      data_clean,
      on=c("location_code","sex","age","date"),
      ncor_est := as.double(ncor_est)
    ]
    skeleton[
      data_clean,
      on=c("location_code","sex","age","date"),
      forecast := forecast
    ]
    skeleton[, yrwk := fhi::isoyearweek(date)]
    skeleton[, forecast := as.logical(max(forecast, na.rm=T)), by=.(location_code, sex, age, yrwk)]
    skeleton[, n_wk := sum(n_obs), by=.(location_code, sex, age, yrwk)]
    skeleton[, ncor_est := mean(ncor_est, na.rm=T), by=.(location_code, sex, age, yrwk)]
    skeleton[, multiplier := ncor_est/n_wk]
    skeleton[is.infinite(multiplier) | is.nan(multiplier), multiplier := 1]
    skeleton[, ncor_est := as.double(n_obs)]
    skeleton[forecast==T, ncor_est := n_obs * multiplier]
    skeleton[, granularity_time := "day"]

    skeleton_week <- skeleton[,.(
      n_obs = sum(n_obs),
      ncor_est = sum(ncor_est)
    ),keyby=.(
      location_code,
      sex,
      age,
      yrwk
    )]
    skeleton_week[, granularity_time := "week"]
    fill_in_missing(skeleton_week)
    schema$data_normomo$db_upsert_load_data_infile(skeleton_week)

    fill_in_missing(skeleton)
    schema$data_normomo$db_upsert_load_data_infile(skeleton)
  }
}


analysis_normomo_hfile <- function() {
  hfile <- fhidata::norway_dates_holidays[is_holiday == TRUE]
  hfile[, closed := 1]
  hfile[, is_holiday := NULL]
  return(as.data.frame(hfile))
}

analysis_normomo_function_factory <- function(location_code, sex) {
  force(location_code)
  force(sex)
  function(){
    sc::tbl("datar_normomo") %>%
      dplyr::filter(location_code==!!location_code) %>%
      dplyr::filter(sex==!!sex) %>%
      dplyr::collect() %>%
      sc::latin1_to_utf8()
  }
}

analysis_normomo_plans <- function(){
  val <- sc::tbl("datar_normomo") %>%
    dplyr::summarize(date_extracted=max(date_extracted,na.rm=T)) %>%
    dplyr::collect()
  date_extracted <- val$date_extracted

  list_plan <- list()
  # For SSI ----
  list_plan[[length(list_plan)+1]] <- plnr::Plan$new()
  list_plan[[length(list_plan)]]$add_data(name = "raw", fn=function(){
    sc::tbl("datar_normomo") %>%
      dplyr::filter(location_code=="norge") %>%
      dplyr::filter(sex=="total") %>%
      dplyr::collect() %>%
      sc::latin1_to_utf8()
  })
  list_plan[[length(list_plan)]]$add_analysis(
    fn = analysis_normomo,
    location_code = "norway",
    year_end = fhi::isoyear_n(date_extracted),
    date_extracted = date_extracted,
    wdir = sc::path("output","sykdomspulsen_normomo_restricted_output",lubridate::today(),"ssi", create_dir = T),
    upload = FALSE,
    momo_groups = list(
      "0to4" =  "age >= 0 & age <=4",
      "5to14" = "age >= 5 & age <=14",
      "15to64" = "age >= 15 & age <=64",
      "65P" = "age >= 65 | is.na(age)",
      "Total" = "age >= 0 | is.na(age)",
      "65to74" = "65 <= age & age <= 74",
      "75to84" = "75 <= age & age <= 84",
      "85P" = "age >= 85 | is.na(age)",
      "0to14" = "age >= 0 & age <= 14",
      "15to44" = "age >= 15 & age <= 44",
      "45to64" = "age >= 45 & age <= 64"
    ),
    momo_models = c(
      "0to4" = "LINE",
      "5to14" = "LINE",
      "15to64" = "LINE_SIN",
      "65P" = "LINE_SIN",
      "Total" = "LINE_SIN",
      "65to74" = "LINE_SIN",
      "75to84" = "LINE_SIN",
      "85P" = "LINE_SIN",
      "0to14" = "LINE_SIN",
      "15to44" = "LINE_SIN",
      "45to64" = "LINE_SIN"
    ),
    Ydrop = 2020,
    Wdrop = 01
  )

  # FHI - Norge/County ----
  for(j in c("norge",unique(norway_locations()$county_code))) for(sex in c("total")) {
    list_plan[[length(list_plan)+1]] <- plnr::Plan$new()

    list_plan[[length(list_plan)]]$add_data(name = "raw", fn=analysis_normomo_function_factory(
      location_code = j,
      sex = sex
      ))
    for(i in 2012:fhi::isoyear_n(date_extracted)){
      if(i==fhi::isoyear_n(date_extracted)){
        date_extracted_year_specific <- date_extracted
      } else {
        date_extracted_year_specific <- fhidata::days[stringr::str_detect(yrwk, as.character(i))][.N]$sun
      }

      list_plan[[length(list_plan)]]$add_analysis(
        fn = analysis_normomo,
        location_code = j,
        sex = sex,
        year_start_upload = ifelse(i==2012, 2008, i-2),
        year_end_upload = ifelse(i==fhi::isoyear_n(date_extracted), i, i-1),
        year_end = i,
        date_extracted = date_extracted_year_specific,
        wdir = tempdir(),
        upload = TRUE,
        momo_groups = list(
          "0to4" =  "age >= 0 & age <=4",
          "5to14" = "age >= 5 & age <=14",
          "15to64" = "age >= 15 & age <=64",
          "65to74" = "65 <= age & age <= 74",
          "75to84" = "75 <= age & age <= 84",
          "85P" = "age >= 85 | is.na(age)",
          "Total" = "age >= 0 | is.na(age)",
          "65P" = "age >= 65 | is.na(age)"
        ),
        momo_models = c(
          "0to4" = "LINE",
          "5to14" = "LINE",
          "15to64" = "LINE_SIN",
          "65to74" = "LINE_SIN",
          "75to84" = "LINE_SIN",
          "85P" = "LINE_SIN",
          "Total" = "LINE_SIN",
          "65P" = "LINE_SIN"
        ),
        Ydrop = 2020,
        Wdrop = 01
      )
    }
  }

  return(list_plan)
}

clean_exported_momo_data <- function(
  data_dirty,
  location_code, sex
  ) {

  data_dirty <- data_dirty[
    !is.na(Pnb),
    c(
      "GROUP",
      "wk",
      "wk2",
      "YoDi",
      "WoDi",
      "Pnb",
      "nb",
      "nbc",
      "UPIb2",
      "UPIb4",
      "UPIc",
      "LPIc",
      "UCIc",
      "LCIc",
      "zscore"
    ),
    with = F]

  data_dirty[,forecast := nbc != nb]

  # prediction interval
  data_dirty[is.na(UPIc) | UPIc < nbc, UPIc := nbc]
  data_dirty[is.na(LPIc) | LPIc > nbc, LPIc := nbc]
  data_dirty[LPIc < 0, LPIc := 0]
  # prediction interval cant be below the real value!
  data_dirty[is.na(LPIc) | LPIc < nb, LPIc := nb]

  # remove prediction intervals before correction
  data_dirty[forecast==FALSE, UPIc := nbc]
  data_dirty[forecast==FALSE, LPIc := nbc]

  setnames(data_dirty,c("LPIc","UPIc"),c("ncor_thresholdl0","ncor_thresholdu0"))
  setnames(data_dirty, "nb", "n_obs")
  setnames(data_dirty, "nbc", "ncor_est")
  setnames(data_dirty, "Pnb", "ncor_baseline_expected")
  setnames(data_dirty, "wk2", "yrwk")

  data_dirty[,yrwk:=as.character(yrwk)]
  setnames(data_dirty, "YoDi", "year")
  setnames(data_dirty, "WoDi", "week")
  setnames(data_dirty, "zscore", "ncor_zscore")

  data_dirty[, location_code := location_code]
  data_dirty[location_code == "norway", location_code := "norge"]
  data_dirty[, age := dplyr::case_when(
    GROUP == "0to4" ~ "0-4",
    GROUP == "5to14" ~ "5-14",
    GROUP == "15to64" ~ "15-64",
    GROUP == "65to74" ~ "65-74",
    GROUP == "75to84" ~ "75-84",
    GROUP == "85P" ~ "85+",
    GROUP == "Total" ~ "total",
    GROUP == "65P" ~ "65+"
  )]

  # creating thesholds
  data_dirty[, ncor_baseline_thresholdl0 := ncor_baseline_expected - abs(UPIb2 - ncor_baseline_expected)]
  setnames(data_dirty, "UPIb2", "ncor_baseline_thresholdu0")
  setnames(data_dirty, "UPIb4", "ncor_baseline_thresholdu1")
  data_dirty[, ncor_excess := pmax(ncor_est - ncor_baseline_thresholdu0, 0)]
  data_dirty[, ncor_status := "normal"]
  data_dirty[ncor_est > ncor_baseline_thresholdu0, ncor_status := "medium"]
  data_dirty[ncor_est > ncor_baseline_thresholdu1, ncor_status := "high"]
  data_dirty[fhidata::days, on = "yrwk", date := sun]

  data_dirty[,granularity_time := "week"]
  data_dirty[,granularity_geo := dplyr::case_when(
    location_code == "norge" ~ "nation",
    TRUE ~ "county"
  )]
  data_dirty[,border := config$border]
  data_dirty[,sex:=sex]
  data_dirty[,season:=fhi::season(yrwk)]
  data_dirty[,x:=fhi::x(week)]

  data_dirty <- data_dirty[,c(
    "granularity_time",
    "granularity_geo",
    "location_code",
    "border",
    "age",
    "sex",
    "season",
    "year",
    "week",
    "yrwk",
    "x",
    "date",
    "n_obs",
    "ncor_est",
    "ncor_thresholdl0",
    "ncor_thresholdu0",
    "ncor_zscore",
    "ncor_status",
    "ncor_excess",
    "ncor_baseline_expected",
    "ncor_baseline_thresholdl0",
    "ncor_baseline_thresholdu0",
    "ncor_baseline_thresholdu1",
    "forecast"
  )]

  return(data_dirty)
}

