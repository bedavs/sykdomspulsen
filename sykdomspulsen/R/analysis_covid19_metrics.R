#' analysis_covid19_areas_at_risk
#' @param data a
#' @param argset a
#' @param schema a
#' @export
analysis_covid19_metrics <- function(data, argset, schema) {
  # tm_run_task("analysis_covid19_metrics")

  if(plnr::is_run_directly()){
    sc::tm_update_plans("analysis_covid19_metrics")

    index_plan <- 1
    data <- sc::tm_get_data("analysis_covid19_metrics", index_plan=index_plan)
    argset <- sc::tm_get_argset("analysis_covid19_metrics", index_plan=index_plan, index_argset = 1)
    schema <- sc::tm_get_schema("analysis_covid19_metrics")
  }

  retval <- list()
  retval[[length(retval)+1]] <- melt.data.table(
    data$data$d_hosp,
    id.vars = "yrwk",
    variable.name = "tag_outcome",
    variable.factor = FALSE
  )

  retval[[length(retval)+1]] <- melt.data.table(
    data$data$d_cases,
    id.vars = "yrwk",
    variable.name = "tag_outcome",
    variable.factor = FALSE
  )

  retval[[length(retval)+1]] <- melt.data.table(
    data$data$d_lab,
    id.vars = "yrwk",
    variable.name = "tag_outcome",
    variable.factor = FALSE
  )

  retval[[length(retval)+1]] <- melt.data.table(
    data$data$d_norsyss,
    id.vars = "yrwk",
    variable.name = "tag_outcome",
    variable.factor = FALSE
  )

  retval[[length(retval)+1]] <- melt.data.table(
    data$data$d_sr,
    id.vars = "yrwk",
    variable.name = "tag_outcome",
    variable.factor = FALSE
  )

  retval <- rbindlist(retval)

  retval[, location_code := argset$location_code]
  retval[, granularity_time := "week"]

  fill_in_missing(retval)

  return(retval)
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
      dplyr::filter(location_code== !!loc) %>%
      dplyr::select(date, n_hospital_main_cause) %>%
      dplyr::collect()
    setDT(d)
    d[, yrwk := fhi::isoyearweek(date)]
    retval$d_hosp <- d[, .(
      n_hospital_main_cause = fhiplot::format_nor(sum(n_hospital_main_cause))
    ), keyby=.(yrwk)]

    # cases ----
    d <- sc::tbl("data_covid19_msis_by_time_location") %>%
      dplyr::filter(granularity_time == "week") %>%
      dplyr::filter(location_code== !!loc) %>%
      dplyr::select(yrwk, n_msis=n) %>%
      dplyr::collect()
    setDT(d)
    d[, n_msis := fhiplot::format_nor(n_msis)]
    retval$d_cases <- d

    # tested ----
    d <- sc::tbl("data_covid19_lab_by_time") %>%
      dplyr::filter(granularity_time == "day") %>%
      dplyr::filter(location_code== !!loc) %>%
      dplyr::select(date, n_neg, n_pos) %>%
      dplyr::collect()
    setDT(d)
    d[, yrwk := fhi::isoyearweek(date)]
    retval$d_lab <- d[, .(
      n_lab_tested = fhiplot::format_nor(sum(n_neg+n_pos)),
      pr100_lab_pos = fhiplot::format_nor_perc_1(100*sum(n_pos)/sum(n_neg+n_pos))
    ), keyby=.(yrwk)]

    # norsyss ----
    d <- sc::tbl("data_norsyss") %>%
      dplyr::filter(granularity_time == "day") %>%
      dplyr::filter(location_code== !!loc) %>%
      dplyr::filter(tag_outcome == "covid19_vk_ote") %>%
      dplyr::filter(age == "total") %>%
      dplyr::select(date, n, consult_with_influenza) %>%
      dplyr::collect()
    setDT(d)
    d[, yrwk := fhi::isoyearweek(date)]
    retval$d_norsyss <- d[, .(
      n_norsyss = fhiplot::format_nor(sum(n)),
      pr100_norsyss = fhiplot::format_nor_perc_1(100*sum(n)/sum(consult_with_influenza))
    ), keyby=.(yrwk)]

    # self reporting ----
    d <- sc::tbl("data_covid19_self_reporting") %>%
      dplyr::filter(granularity_time == "day") %>%
      #dplyr::filter(location_code== !!location_code) %>%
      dplyr::select(date, n_symps_yesfever_cough_or_breath, n) %>%
      dplyr::collect()
    setDT(d)
    d[, yrwk := fhi::isoyearweek(date)]
    retval$d_sr <- d[, .(
      pr100_sr_symptoms = fhiplot::format_nor_perc_1(100*sum(n_symps_yesfever_cough_or_breath)/sum(n))
    ), keyby=.(yrwk)]

    retval
  }
}

analysis_covid19_metrics_plans <- function(){

  # these are the areas we are interested in
  locs <- "norge"

  list_plan <- list()
  for(loc in locs){
    # create a new plan
    list_plan[[length(list_plan)+1]] <- plnr::Plan$new()

    # add in the data
    list_plan[[length(list_plan)]]$add_data(
      name = "data",
      fn=analysis_covid19_metrics_function_factory(
        loc = loc
      )
    )

    # what do you want to do with the data?
    list_plan[[length(list_plan)]]$add_analysis(
      fn = analysis_covid19_metrics,
      location_code = loc # giving us information that is available through 'argset'
    )
  }

  return(list_plan)
}
