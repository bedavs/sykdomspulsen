#' analysis_covid19_areas_at_risk
#' @param data a
#' @param argset a
#' @param schema a
#' @export
analysis_covid19_metrics <- function(data, argset, schema) {
  # tm_run_task("analysis_covid19_metrics")
  # d <- sc::tbl("results_covid19_metrics") %>% dplyr::collect()

  if(plnr::is_run_directly()){
    sc::tm_update_plans("analysis_covid19_metrics")

    index_plan <- 1
    data <- sc::tm_get_data("analysis_covid19_metrics", index_plan=index_plan)
    argset <- sc::tm_get_argset("analysis_covid19_metrics", index_plan=index_plan, index_argset = 2)
    schema <- sc::tm_get_schema("analysis_covid19_metrics")
  }

  retval <- list()

  # hospital ----
  d <- data$data$d_hosp[location_code==argset$location_code]
  d[, yrwk := fhi::isoyearweek(date)]
  d <- d[, .(
    value_n_hospital_main_cause = sum(n_hospital_main_cause)
  ), keyby=.(location_code, yrwk)]
  d[, formatted_n_hospital_main_cause := fhiplot::format_nor(sum(value_n_hospital_main_cause))]
  d <- melt.data.table(
    d,
    id.vars = c("location_code", "yrwk"),
    measure = patterns("^value", "^formatted"), value.name = c("value", "formatted"),
    variable.name = "tag_outcome",
  )
  levels(d$tag_outcome) <- "n_hospital_main_cause"
  d[, tag_outcome := as.character(tag_outcome)]

  d[, censor := FALSE]

  retval[[length(retval)+1]] <- d

  # msis ----
  d <- data$data$d_cases[location_code==argset$location_code]
  setnames(d, "n", "value")
  d[, formatted := fhiplot::format_nor(value)]
  d[, tag_outcome := "n_msis"]

  d[, censor := FALSE]

  retval[[length(retval)+1]] <- d

  # lab ----
  d <- data$data$d_lab[location_code==argset$location_code]
  d[, yrwk := fhi::isoyearweek(date)]
  d <- d[, .(
    value_n_lab_tested = (sum(n_neg+n_pos)),
    value_pr100_lab_pos = (100*sum(n_pos)/sum(n_neg+n_pos))
  ), keyby=.(location_code, yrwk)]
  d[, formatted_n_lab_tested := fhiplot::format_nor(value_n_lab_tested)]
  d[, formatted_pr100_lab_pos := fhiplot::format_nor(value_pr100_lab_pos)]

  d <- melt.data.table(
    d,
    id.vars = c("location_code", "yrwk"),
    measure = patterns("^value", "^formatted"), value.name = c("value", "formatted"),
    variable.name = "tag_outcome"
  )
  levels(d$tag_outcome) <- c("pr100_lab_pos", "n_lab_tested")
  d[, tag_outcome := as.character(tag_outcome)]

  d[, censor := FALSE]

  retval[[length(retval)+1]] <- d

  # norsyss
  d <- data$data$d_norsyss[location_code==argset$location_code]
  d[, yrwk := fhi::isoyearweek(date)]
  d <- d[, .(
    value_n_norsyss = sum(n),
    value_pr100_norsyss = 100*sum(n)/sum(consult_with_influenza)
  ), keyby=.(location_code, yrwk)]
  d[, formatted_n_norsyss := fhiplot::format_nor(value_n_norsyss)]
  d[, formatted_pr100_norsyss := fhiplot::format_nor_perc_1(value_pr100_norsyss)]

  d[, censor := value_n_norsyss %in% 1:4]

  d <- melt.data.table(
    d,
    id.vars = c("location_code", "yrwk", "censor"),
    measure = patterns("^value", "^formatted"), value.name = c("value", "formatted"),
    variable.name = "tag_outcome"
  )
  levels(d$tag_outcome) <- c("n_norsyss", "pr100_norsyss")
  d[, tag_outcome := as.character(tag_outcome)]

  retval[[length(retval)+1]] <- d

  # sr ----
  d <- data$data$d_sr[location_code==argset$location_code]
  d[, yrwk := fhi::isoyearweek(date)]
  d <- d[, .(
    value_pr100_sr_symptoms = 100*sum(n_symps_yesfever_cough_or_breath)/sum(n)
  ), keyby=.(location_code, yrwk)]
  # remove once we get data ----
  d[, value_pr100_sr_symptoms := NA] # remove once we get data
  # remove once we get data ----
  d[is.nan(value_pr100_sr_symptoms), value_pr100_sr_symptoms:= NA]
  d[, formatted_pr100_sr_symptoms := fhiplot::format_nor_perc_1(value_pr100_sr_symptoms)]
  d[is.na(value_pr100_sr_symptoms), formatted_pr100_sr_symptoms:=NA]

  d[, tag_outcome := "pr100_sr_symptoms"]
  d[, tag_outcome := as.character(tag_outcome)]

  setnames(
    d,
    c(
      "value_pr100_sr_symptoms",
      "formatted_pr100_sr_symptoms"
    ),
    c(
      "value",
      "formatted"
    )
  )

  d[, censor := FALSE]

  retval[[length(retval)+1]] <- d

  # combine ----
  retval <- rbindlist(retval, use.names=T)

  yrwks <- which(fhidata::days$yrwk=="2020-10"):which(fhidata::days$yrwk==fhi::isoyearweek())
  yrwks <- fhidata::days[yrwks]$yrwk

  skeleton <- data.table(
    expand.grid(
      yrwk = yrwks,
      tag_outcome = c(
        "n_hospital_main_cause",
        "n_msis",
        "n_lab_tested",
        "pr100_lab_pos",
        "n_norsyss",
        "pr100_norsyss",
        "pr100_sr_symptoms"
      )
    )
  )
  skeleton <- merge(
    skeleton,
    retval,
    by=c("yrwk","tag_outcome"),
    all.x=T
  )
  skeleton[is.na(value), formatted:="IK"]
  skeleton[is.na(censor), censor := FALSE]

  skeleton[, location_code := argset$location_code]
  skeleton[, granularity_time := "week"]

  fill_in_missing(skeleton)

  schema$output$db_upsert_load_data_infile(skeleton)
  #return(skeleton)
}


analysis_covid19_metrics_function_factory <- function(loc){
  # snapshots the variable 'loc' and fixes in the following
  # function
  force(loc)

  function(){
    retval <- list()

    # hospital ----
    d <- sc::tbl("data_covid19_hospital_by_time") %>%
      dplyr::filter(granularity_time == "day") %>%
      dplyr::filter(location_code %in% !!loc) %>%
      dplyr::select(location_code, date, n_hospital_main_cause) %>%
      dplyr::collect()
    setDT(d)
    retval$d_hosp <- d

    # cases ----
    d <- sc::tbl("data_covid19_msis_by_time_location") %>%
      dplyr::filter(granularity_time == "week") %>%
      dplyr::filter(location_code %in% !!loc) %>%
      dplyr::select(location_code, yrwk, n) %>%
      dplyr::collect()
    setDT(d)
    retval$d_cases <- d

    # tested ----
    d <- sc::tbl("data_covid19_lab_by_time") %>%
      dplyr::filter(granularity_time == "day") %>%
      dplyr::filter(location_code %in% !!loc) %>%
      dplyr::select(location_code, date, n_neg, n_pos) %>%
      dplyr::collect()
    setDT(d)
    retval$d_lab <- d

    # norsyss ----
    d <- sc::tbl("data_norsyss") %>%
      dplyr::filter(granularity_time == "day") %>%
      dplyr::filter(location_code %in% !!loc) %>%
      dplyr::filter(tag_outcome == "covid19_vk_ote") %>%
      dplyr::filter(age == "total") %>%
      dplyr::filter(year >= 2020) %>%
      dplyr::select(location_code, date, n, consult_with_influenza) %>%
      dplyr::collect()
    setDT(d)
    retval$d_norsyss <- d

    # self reporting ----
    d <- sc::tbl("data_covid19_self_reporting") %>%
      dplyr::filter(granularity_time == "day") %>%
      dplyr::filter(location_code %in% !!loc) %>%
      dplyr::select(location_code, date, n_symps_yesfever_cough_or_breath, n) %>%
      dplyr::collect()
    setDT(d)
    retval$d_sr <- d

    retval
  }
}

analysis_covid19_metrics_plans <- function(){

  # these are the areas we are interested in
  locs <- norway_locations_long()$location_code
  #locs <- "municip0301"

  list_plan <- list()
  list_plan[[length(list_plan)+1]] <- plnr::Plan$new()

  # add in the data
  list_plan[[length(list_plan)]]$add_data(
    name = "data",
    fn=analysis_covid19_metrics_function_factory(
      loc = locs
    )
  )
  for(loc in locs){
    # create a new plan


    # what do you want to do with the data?
    list_plan[[length(list_plan)]]$add_analysis(
      fn = analysis_covid19_metrics,
      location_code = loc # giving us information that is available through 'argset'
    )
  }

  return(list_plan)
}
