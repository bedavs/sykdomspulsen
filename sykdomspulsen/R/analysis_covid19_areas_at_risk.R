#' analysis_covid19_areas_at_risk
#' @param data a
#' @param argset a
#' @param schema a
#' @export
analysis_covid19_areas_at_risk <- function(data, argset, schema) {
  # tm_run_task("analysis_covid19_areas_at_risk")

  if(plnr::is_run_directly()){
    sc::tm_update_plans("analysis_covid19_areas_at_risk")

    index_plan <- 1
    data <- sc::tm_get_data("analysis_covid19_areas_at_risk", index_plan=index_plan)
    argset <- sc::tm_get_argset("analysis_covid19_areas_at_risk", index_plan=index_plan, index_argset = 1)
    schema <- sc::tm_get_schema("analysis_covid19_areas_at_risk")
  }


  # this code
  # a) only works for "total age groups" (change it to all age groups) # total & age groups?
  # start changing the data
  # MSIS
  d <- copy(data$covid19$msis)

  d[, n_lag1 := shift(n, type="lag")]
  d[, n_lag2 := shift(n, n=2L, type="lag")]


  yrwks <- unique(d$yrwk)

  d[,baseline := pmax(1,round((n_lag1+n_lag2)/2))]
  d[,threshold := qpois(0.975, lambda = baseline)]

  d[,n_msis_status:=ifelse(n > threshold,"high","normal")]

  d_msis <- d[,c("location_code","yrwk","age", "n","baseline","threshold","n_msis_status" )]
  setnames(d_msis, c("n",
                     "baseline",
                     "threshold"),
           c("n_msis",
             "n_msis_baseline_expected",
             "n_msis_baseline_thresholdu0"))



  # NORSYSS
  d <- copy(data$covid19$norsyss)
    d[,pr100:=100*n/consult_with_influenza]

    d[is.nan(pr100), pr100:=0]
    d[, n_lag1 := shift(n, type="lag"), by=age]
    d[, n_lag2 := shift(n, n=2L, type="lag"), by=age]
    d[, consult_with_influenza_lag1 := shift(consult_with_influenza, type="lag"), by=age]
    d[, consult_with_influenza_lag2 := shift(consult_with_influenza, n=2L, type="lag"), by=age]


    d[,pr100_baseline := pmax(0.01,100*(n_lag1+n_lag2)/
                               (consult_with_influenza_lag1+consult_with_influenza_lag2))]
    d[, n_norsyss_baseline_expected := round(pr100_baseline*consult_with_influenza/100)]

    d[,n_threshold := qpois(0.975, lambda=n_norsyss_baseline_expected)]
    d[,pr100_threshold := 100*n_threshold/consult_with_influenza]
    d[is.nan(pr100_threshold),pr100_threshold:=0]



    d[,n_norsyss_status:=ifelse(pr100 > pr100_threshold,"high","normal")]

    d_norsyss <- d[,c("location_code",
                      "yrwk",
                      "age",
                      "n",
                      "consult_with_influenza",
                      "n_norsyss_baseline_expected",
                      "n_threshold",
                      "pr100",
                      "pr100_baseline",
                      "pr100_threshold",
                      "n_norsyss_status")]

    setnames(d_norsyss,
             c("n",
               "consult_with_influenza",
               "n_threshold",
               "pr100",
               "pr100_baseline",
               "pr100_threshold"),
             c("n_norsyss",
               "n_norsyss_denominator",
               "n_norsyss_baseline_thresholdu0",
               "pr100_norsyss",
               "pr100_norsyss_baseline_expected",
               "pr100_norsyss_baseline_thresholdu0"))



  retval <- merge(
    d_msis,
    d_norsyss,
    by=c("location_code","yrwk", "age"),
    all=T
  )

  retval[, granularity_time := "week"]

  #############################
  # b) it doesn't provide the output in the way we want it
  schema$output$db_field_types

  # this will fill in a lot of standard columns
    fill_in_missing(retval)
    return(retval)
}


analysis_covid19_areas_at_risk_function_factory <- function(loc){
  # snapshots the variable 'loc' and fixes in the following
  # function
  force(loc)

  function(){
    retval <- list()

    retval$msis <- sc::tbl("data_covid19_msis_by_time_location") %>%
      dplyr::filter(location_code %in% !!loc) %>%
      dplyr::filter(date >= "2020-03-09") %>%
      dplyr::group_by(location_code, age, yrwk) %>%
      dplyr::summarize(n=sum(n)) %>%
      dplyr::collect() %>%
      sc::latin1_to_utf8()

    retval$norsyss <- sc::tbl("data_norsyss") %>%
      dplyr::filter(granularity_time=="day") %>%
      dplyr::filter(location_code %in% !!loc) %>%
      dplyr::filter(date >= "2020-03-09") %>%
      dplyr::filter(tag_outcome %in% "covid19_vk_ote") %>%
      dplyr::select(location_code, age, yrwk, n, consult_with_influenza) %>%
      dplyr::group_by(location_code, age, yrwk) %>%
      dplyr::summarize(n=sum(n), consult_with_influenza=sum(consult_with_influenza)) %>%
      dplyr::collect() %>%
      sc::latin1_to_utf8()

    # make sure they both have the same yrwks
    yrwks <- intersect(unique(retval$msis$yrwk), unique(retval$norsyss$yrwk))
    retval$msis <- retval$msis[yrwk %in% yrwks]
    retval$norsyss <- retval$norsyss[yrwk %in% yrwks]

    # sort them
    setorder(retval$msis, location_code, age, yrwk)
    setorder(retval$norsyss, location_code, age, yrwk)

    retval
  }
}

analysis_covid19_areas_at_risk_plans <- function(){

  # these are the areas we are interested in
  locs <- norway_locations_long()$location_code
  locs <- locs[!locs %in% "norway"]
  locs <- locs[!stringr::str_detect(locs,"^ward")]

  list_plan <- list()
  for(loc in locs){
    # create a new plan
    list_plan[[length(list_plan)+1]] <- plnr::Plan$new()

    # add in the data
    list_plan[[length(list_plan)]]$add_data(
      name = "covid19",
      fn=analysis_covid19_areas_at_risk_function_factory(
        loc = loc
      )
    )

    # what do you want to do with the data?
    list_plan[[length(list_plan)]]$add_analysis(
      fn = analysis_covid19_areas_at_risk,
      location_code = loc # giving us information that is available through 'argset'
    )
  }

  return(list_plan)
}
