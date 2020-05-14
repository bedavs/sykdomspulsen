#' ui_covid19_areas_at_risk
#' @param data a
#' @param argset a
#' @param schema a
#' @export
ui_covid19_areas_at_risk <- function(data, argset, schema) {
  # tm_run_task("ui_covid19_areas_at_risk")

  if(plnr::is_run_directly()){
    sc::tm_update_plans("ui_covid19_areas_at_risk")
    length(config$tasks$list_task$ui_norsyss_kht_email$plans)

    index_plan <- 1
    data <- sc::tm_get_data("ui_covid19_areas_at_risk", index_plan=index_plan)
    argset <- sc::tm_get_argset("ui_covid19_areas_at_risk", index_plan=index_plan, index_argset = 1)
    schema <- sc::tm_get_schema("ui_covid19_areas_at_risk")
  }

  d <- copy(data$covid19$norsyss)
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

  # render it

  file <- glue::glue("covid19_areas_at_risk_{lubridate::today()}.pdf")
  folder <- sc::path("output","sykdomspulsen_norsyss_restricted_output",lubridate::today(), create_dir = T)
  tempdir <- tempdir()

  rmarkdown::render(
    input = system.file("rmd/ui_covid19_areas_at_risk.Rmd", package="sykdomspulsen"),
    output_dir = tempdir,
    output_file = file,
    intermediates_dir = tempdir
  )

  sc::mv(
    fs::path(tempdir, file),
    fs::path(folder, file)
  )
}

areas_at_risk_ht <- function(tab, yrwks){
  msis_index_hig <- which(tab$msis_n > tab$msis_threshold & tab$variable %in% 3:4)
  norsyss_index_hig <- which(tab$norsyss_pr100 > tab$norsyss_pr100_threshold & tab$variable %in% 3:4)

  levels(tab$variable) <- yrwks

  ht <- huxtable::hux(
    "Geo"=tab$location_name,
    "Uke"=tab$variable,
    "Tilfeller"=tab$pretty_msis_n,
    "Terskel"=tab$pretty_msis_threshold,
    "Konsultasjoner"=tab$pretty_norsyss_n,
    "Andel"=tab$pretty_norsyss_pr100,
    "Terskel"=tab$pretty_norsyss_pr100_threshold
  )%>%
    huxtable::add_colnames() %>%
    fhiplot::huxtable_theme_fhi_basic()
  ht <- huxtable::set_background_color(ht, huxtable::evens, huxtable::everywhere, "#FFFFFF")

  if (length(msis_index_hig) > 0) huxtable::background_color(ht)[msis_index_hig+1, 3] <- fhiplot::warning_color[["hig"]]
  if (length(norsyss_index_hig) > 0) huxtable::background_color(ht)[norsyss_index_hig+1, 6] <- fhiplot::warning_color[["hig"]]


  ht <- huxtable::add_rows(ht, ht[1, ], after = 0)

  ht <- huxtable::merge_cells(ht, 1:2, 1)
  ht <- huxtable::merge_cells(ht, 1:2, 2)

  ht <- huxtable::merge_cells(ht, 1, 3:4)
  ht[1, 3] <- "MSIS"

  ht <- huxtable::merge_cells(ht, 1, 5:7)
  ht[1, 5] <- "NorSySS"

  huxtable::left_border(ht)[, c(3, 5)] <- 2
  #huxtable::left_border_style(ht)[, c(3, 5)] <- "double"

  ht <- huxtable::merge_repeated_rows(ht, huxtable::everywhere, 1)
  huxtable::width(ht) <- 1
  huxtable::tabular_environment(ht) <- "longtable"
  ht
}

areas_at_risk_ft <- function(tab, yrwks){

  msis_index_hig <- which(tab$msis_n > tab$msis_threshold & tab$variable %in% 3:4)
  norsyss_index_hig <- which(tab$norsyss_pr100 > tab$norsyss_pr100_threshold & tab$variable %in% 3:4)

  levels(tab$variable) <- yrwks

  ft <- flextable::flextable(
    data.frame(
      "Geo"=tab$location_name,
      "Uke"=tab$variable,
      "Tilfeller"=tab$pretty_msis_n,
      "Terskel"=tab$pretty_msis_threshold,
      "Konsultasjoner"=tab$pretty_norsyss_n,
      "Andel"=tab$pretty_norsyss_pr100,
      "Terskel"=tab$pretty_norsyss_pr100_threshold
    )
  )
  labs <- as.list(c(
    "Geo",
    "Uke",
    "Tilfeller",
    "Terskel",
    "Konsultasjoner",
    "Andel",
    "Terskel"
  ))
  names(labs) <- ft$col_keys
  ft <- flextable::set_header_labels(ft,values =labs)
  ft <- flextable::autofit(ft)
  ft <- flextable::merge_v(ft, j = ~ Geo )

  ft <- flextable::add_header_row(
    ft,
    values = c(
      "",
      "MSIS",
      "NorSySS"
    ),
    colwidths = c(
      2,
      2,
      3
    )
  )
  ft <- flextable::theme_box(ft)
  ft <- flextable::align(ft, align = "center", part = "all")

  if (length(msis_index_hig) > 0) ft <- flextable::bg(
    ft,
    i = msis_index_hig,
    j = 3,
    bg=fhiplot::warning_color[["hig"]]
  )
  if (length(norsyss_index_hig) > 0) ft <- flextable::bg(
    ft,
    i = norsyss_index_hig,
    j = 6,
    bg=fhiplot::warning_color[["hig"]]
  )

  return(ft)
}

ui_covid19_areas_at_risk_function_factory <- function(yrwk){
  force(yrwk)
  function(){
    retval <- list()

    retval$msis <- sc::tbl("data_covid19_msis_by_time_location") %>%
      dplyr::filter(granularity_time == "week") %>%
      dplyr::filter(yrwk %in% !!yrwk) %>%
      dplyr::collect() %>%
      sc::latin1_to_utf8()

    retval$norsyss <- sc::tbl("data_norsyss") %>%
      dplyr::filter(granularity_time=="day") %>%
      dplyr::filter(age=="total") %>%
      dplyr::filter(yrwk %in% !!yrwk) %>%
      dplyr::filter(tag_outcome %in% "covid19_vk_ote") %>%
      dplyr::select(location_code, yrwk, n, consult_with_influenza) %>%
      dplyr::group_by(location_code,yrwk) %>%
      dplyr::summarize(n=sum(n), consult_with_influenza=sum(consult_with_influenza)) %>%
      dplyr::collect() %>%
      sc::latin1_to_utf8()

    retval$norsyss_norge <- sc::tbl("data_norsyss") %>%
      dplyr::filter(granularity_time=="day") %>%
      dplyr::filter(age=="total") %>%
      dplyr::filter(date >= "2020-03-09") %>%
      dplyr::filter(tag_outcome %in% "covid19_vk_ote") %>%
      dplyr::filter(location_code=="norge") %>%
      dplyr::select(location_code, yrwk, date, n, consult_with_influenza) %>%
      dplyr::group_by(location_code,yrwk, date) %>%
      dplyr::summarize(n=sum(n), consult_with_influenza=sum(consult_with_influenza)) %>%
      dplyr::collect() %>%
      sc::latin1_to_utf8()

    retval
  }
}

ui_covid19_areas_at_risk_plans <- function(){
  x_tags <- c("respiratoryexternal_vk_ot", "gastro_vk_ot")
  yrwk <- fhi::isoyearweek(lubridate::today()-seq(0,21,7)-1)

  list_plan <- list()
  list_plan[[length(list_plan)+1]] <- plnr::Plan$new()

  list_plan[[length(list_plan)]]$add_data(
    name = "covid19",
    fn=ui_covid19_areas_at_risk_function_factory(
      yrwk = yrwk
    )
  )
  list_plan[[length(list_plan)]]$add_analysis(
    fn = ui_covid19_areas_at_risk,
    yrwk = yrwk
  )

  return(list_plan)
}
