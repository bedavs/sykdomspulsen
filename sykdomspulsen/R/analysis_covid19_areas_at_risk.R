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
  # a) only works for "total age groups" (change it to all age groups)
  # b) it doesn't provide the output in the way we want it
  schema$output$db_field_types

  # start changing the data

  d <- copy(data$covid19$norsyss_combined)
  if(nrow(d)==0) return(NULL)

  setorder(d,location_code,yrwk)
  d[,pr100:=100*n/consult_with_influenza]
  d[is.nan(pr100), pr100:=0]
  d[,yrwk_id := paste0("yrwk",1:.N), by=.(location_code)]

  yrwks <- unique(d$yrwk)

  d_wide <- dcast.data.table(d, location_code ~ yrwk_id, value.var = c("n","consult_with_influenza","pr100"))

  d_wide[,baseline := pmax(0.01,100*(n_yrwk1+n_yrwk2)/(consult_with_influenza_yrwk1+consult_with_influenza_yrwk2))]

  d_wide[,threshold_yrwk_1 := 0]
  d_wide[,threshold_yrwk_2 := 0]

  d_wide[,threshold_yrwk_3 := 100*qpois(0.975, lambda=baseline*consult_with_influenza_yrwk3/100)/consult_with_influenza_yrwk3]
  d_wide[is.nan(threshold_yrwk_3),threshold_yrwk_3:=0]

  d_wide[,threshold_yrwk_4 := 100*qpois(0.975, lambda=baseline*consult_with_influenza_yrwk4/100)/consult_with_influenza_yrwk4]
  d_wide[is.nan(threshold_yrwk_4),threshold_yrwk_4:=0]

  d_wide_norsyss <- d_wide

  location_code_norsyss <- d_wide_norsyss[pr100_yrwk3 > threshold_yrwk_3 | pr100_yrwk4 > threshold_yrwk_4]$location_code

  d <- copy(data$covid19$msis)
  setorder(d,location_code,yrwk)
  d[,yrwk_id := paste0("yrwk",1:.N), by=.(location_code)]

  d_wide <- dcast.data.table(d, location_code ~ yrwk_id, value.var = "n")
  d_wide[,baseline := pmax(1,round((yrwk1+yrwk2)/2))]
  d_wide[,threshold := qpois(0.975, lambda = baseline)]

  d_wide_msis <- d_wide
  location_code_msis <- d_wide_msis[yrwk3 > threshold | yrwk4 > threshold]$location_code

  location_codes <- unique(c(location_code_norsyss, location_code_msis))

  tab_norsyss <- melt.data.table(
    d_wide_norsyss[location_code %in% location_codes],
    id="location_code",
    measure = patterns("^n_", "^pr100_","^threshold_"),
    value.name = c("norsyss_n","norsyss_pr100","norsyss_pr100_threshold")
  )

  tab_norsyss

  tab_msis <- melt.data.table(
    d_wide_msis[location_code %in% location_codes],
    id.vars =c("location_code","threshold"),
    measure.vars = c("yrwk1","yrwk2","yrwk3","yrwk4")
  )
  levels(tab_msis$variable) <- 1:4

  tab <- merge(
    tab_msis,
    tab_norsyss,
    by=c("location_code","variable")
  )

  setnames(tab,c("threshold","value"),c("msis_threshold","msis_n"))

  tab[, pretty_msis_threshold:=fhiplot::format_nor(msis_threshold)]
  tab[, pretty_msis_n:=fhiplot::format_nor(msis_n)]
  tab[, pretty_norsyss_n:=fhiplot::format_nor(norsyss_n)]
  tab[, pretty_norsyss_pr100:=fhiplot::format_nor_perc_1(norsyss_pr100)]
  tab[, pretty_norsyss_pr100_threshold:=fhiplot::format_nor_perc_1(norsyss_pr100_threshold)]

  tab[variable %in% 1:2, pretty_msis_threshold:=""]
  tab[variable %in% 1:2, pretty_norsyss_pr100_threshold:=""]

  tab[,location_name := get_location_name(location_code)]
  tab[location_name=="Bergen"]

  tab[variable %in% 3:4,msis_difference := msis_n-msis_threshold]
  tab[variable %in% 3:4,norsyss_difference := norsyss_pr100-norsyss_pr100_threshold]

  tab[, yrwk := variable]
  levels(tab$yrwk) <- yrwks

  # get the ordering of locations right
  ordering_msis <- na.omit(tab[,c("location_name","location_code","msis_difference","norsyss_difference")])
  setorder(ordering_msis, -msis_difference, -norsyss_difference)
  ordering_msis <- unique(ordering_msis$location_code)

  ordering_norsyss <- na.omit(tab[,c("location_name","location_code","msis_difference","norsyss_difference")])
  setorder(ordering_norsyss, -norsyss_difference, -msis_difference)
  ordering_norsyss <- unique(ordering_norsyss$location_code)

  location_codes_1 <- ordering_msis[ordering_msis %in% location_code_msis]
  location_codes_2 <- ordering_norsyss[!ordering_norsyss %in% location_code_msis]
  location_codes <- c(location_codes_1, location_codes_2)

  tab[,location_code:=factor(location_code, levels = location_codes)]
  setorder(tab,location_code,variable)

  # this will be automatically upserted to schema = "output" because of
  # upsert_at_end_of_each_plan = TRUE
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
