ui_covid19_areas_at_risk <- function(data, argset, schema) {
  if(FALSE){
    tm_run_task("ui_covid19_areas_at_risk")
  }

  if(plnr::is_run_directly()){
    tm_update_plans("ui_covid19_areas_at_risk")
    length(config$tasks$list_task$ui_norsyss_kht_email$plans)

    index_plan <- 1
    data <- tm_get_data("ui_covid19_areas_at_risk", index_plan=index_plan)
    argset <- tm_get_argset("ui_covid19_areas_at_risk", index_plan=index_plan, index_argset = 1)
    schema <- tm_get_schema("ui_covid19_areas_at_risk")
  }

  file <- glue::glue("covid19_areas_at_risk_{lubridate::today()}.pdf")
  folder <- fs::path("/output","covid19",lubridate::today())
  fs::dir_create(folder)

  rmarkdown::render(
    input = system.file("rmd/ui_covid19_areas_at_risk.Rmd", package="sykdomspulsen"),
    output_dir = folder,
    output_file = file,
    intermediates_dir = tempdir()
  )
}

xnorsyss_kht_obs_table <- function(results, tag_outcome) {
  r_long <- copy(results)

  tag_pretty <- config$def$norsyss$long_names[[tag_outcome]]

  if (nrow(r_long) == 0) {
    return(sprintf("<b>%s:</b> <span style='color:red;text-decoration:underline;'>Ingen varsler registrert</span><br><br><br>", tag_pretty))
  }

  setorder(r_long, tag_outcome, yrwk)
  r_long[age=="total",age:="Totalt"]
  r_long[, week_id := 1:.N,by=.(location_code,age,sex)]
  r_long[, tag_pretty := tag_pretty]
  r_long[, excessp := fhiplot::format_nor(ceiling(pmax(0, n - n_baseline_thresholdu0)),0)]
  r_long[, zscorep := fhiplot::format_nor(n_zscore, 1)]
  r_long[, n := fhiplot::format_nor(n, 0)]

  r_wide <- dcast.data.table(
    r_long,
    tag_pretty + location_code + age ~ week_id,
    value.var = c("n", "excessp", "n_baseline_thresholdu0", "n_zscore", "zscorep", "n_status")
  )
  r_wide <- r_wide[!(n_status_1=="normal" & n_status_2=="normal" & n_status_3=="normal" & n_status_4=="normal")]
  setorder(r_wide, -n_zscore_4)
  r_wide[,location_name := get_location_name(location_code)]

  yrwks <- unique(r_long[, c("week_id", "yrwk")])
  setorder(yrwks, week_id)

  tab <- huxtable::huxtable(
    geo = r_wide$location_name,
    Alder = r_wide$age,
    `n_1` = r_wide$n_1,
    `n_2` = r_wide$n_2,
    `n_3` = r_wide$n_3,
    `n_4` = r_wide$n_4,
    `excess_1` = r_wide$excessp_1,
    `excess_2` = r_wide$excessp_2,
    `excess_3` = r_wide$excessp_3,
    `excess_4` = r_wide$excessp_4,
    `zscore_1` = r_wide$zscorep_1,
    `zscore_2` = r_wide$zscorep_2,
    `zscore_3` = r_wide$zscorep_3,
    `zscore_4` = r_wide$zscorep_4
  ) %>%
    huxtable::add_colnames() %>%
    fhiplot::huxtable_theme_fhi_basic()

  # coloring in
  for (i in 1:4) {
    z <- glue::glue("n_status_{i}")
    column_to_color <- c(2, 6, 10) + i
    index_low <- which(r_wide[[z]] == "normal") + 1
    index_med <- which(r_wide[[z]] == "medium") + 1
    index_hig <- which(r_wide[[z]] == "high") + 1

    if (length(index_low) > 0) huxtable::background_color(tab)[index_low, column_to_color] <- fhiplot::warning_color[["low"]]
    if (length(index_med) > 0) huxtable::background_color(tab)[index_med, column_to_color] <- fhiplot::warning_color[["med"]]
    if (length(index_hig) > 0) huxtable::background_color(tab)[index_hig, column_to_color] <- fhiplot::warning_color[["hig"]]
  }

  tab[1, ] <- c(
    "Geografisk omr\u00E5de",
    "Alder",
    yrwks$yrwk,
    yrwks$yrwk,
    yrwks$yrwk
  )

  tab <- huxtable::add_rows(tab, tab[1, ], after = 0)

  huxtable::escape_contents(tab)[, 1] <- FALSE

  tab <- huxtable::merge_cells(tab, 1:2, 1)
  tab <- huxtable::merge_cells(tab, 1:2, 2)

  tab <- huxtable::merge_cells(tab, 1, 3:6)
  tab[1, 3] <- "Antall konsultasjoner"

  tab <- huxtable::merge_cells(tab, 1, 7:10)
  tab[1, 7] <- "Flere enn normalt<sup>1</sup>"

  tab <- huxtable::merge_cells(tab, 1, 11:14)
  tab[1, 11] <- "Z-verdi<sup>2</sup>"

  huxtable::left_border(tab)[, c(3, 7, 11)] <- 5
  huxtable::left_border_style(tab)[, c(3, 7, 11)] <- "double"

  huxtable::align(tab) <- "center"

  nr0 <- nrow(tab) + 1
  tab <- huxtable::add_footnote(tab, glue::glue(
    "<sup>1</sup>Differansen mellom antall registrete og {fhi::nb$oe}vre grense for normalt antall (95% prediksjonsintervall)<br>",
    "<sup>2</sup>Z-verdi: antall ganger standardavvik ut fra forventet antall konsultasjoner<br>",
    "Bl{fhi::nb$aa}tt felt: Antall konsultasjoner er som forventet (Z-verdi < 2)<br>",
    "Gult felt: Antall konsultasjoner er h{fhi::nb$oe}yere enn forventet (Z-verdi mellom 2 og 4 og minst 3 konsultasjoner)<br>",
    "R{fhi::nb$oe}dt felt: Antall konsultasjoner er betydelig h{fhi::nb$oe}yere enn forventet (Z-verdi >= 4 og minst 4 konsultasjoner)<br>",
  ), border = 0)
  nr1 <- nrow(tab)

  huxtable::escape_contents(tab)[1, c(7, 11)] <- F
  huxtable::escape_contents(tab)[nr0:nr1, ] <- F

  huxtable::left_padding(tab) <-  5
  huxtable::right_padding(tab) <-  5

  # return(tab)
  return(huxtable::to_html(tab))
}

xnorsyss_kht_covid19_table <- function(data){
  tab <- copy(data$covid19$norsyss)
  setnames(tab, "n", "n_norsyss")
  tab[
    data$covid19$msis,
    on=c("location_code","yrwk"),
    n_msis := fhiplot::format_nor(n)
  ]

  tab[,pr100_norsyss := fhiplot::format_nor_perc_1(100*n_norsyss/consult_with_influenza)]
  tab[consult_with_influenza==0, pr100_norsyss := "0,0%"]
  tab[,n_norsyss:=fhiplot::format_nor(n_norsyss)]

  setorder(tab,location_code,yrwk)
  tab[,week_id := 1:.N,by=.(location_code)]

  tab_wide <- dcast.data.table(
    tab,
    location_code ~ week_id,
    value.var = c("pr100_norsyss","n_norsyss","n_msis")
  )
  tab_wide <- rbind(tab_wide[location_code=="norge"],tab_wide[location_code!="norge"])
  tab_wide[,location_name := get_location_name(location_code)]
  tab_wide[, location_code := NULL]
  tab_wide <- unique(tab_wide)
  setcolorder(tab_wide, "location_name")

  yrwks <- unique(tab[, c("week_id", "yrwk")])
  setorder(yrwks, week_id)

  ht <- huxtable::as_hux(tab_wide) %>%
    huxtable::add_colnames() %>%
    fhiplot::huxtable_theme_fhi_basic()

  ht[1, ] <- c(
    "Geografisk omr\u00E5de",
    yrwks$yrwk,
    yrwks$yrwk,
    yrwks$yrwk
  )

  ht <- huxtable::add_rows(ht, ht[1, ], after = 0)

  huxtable::escape_contents(ht)[, 2] <- FALSE

  ht <- huxtable::merge_cells(ht, 1:2, 1)

  ht <- huxtable::merge_cells(ht, 1, 2:5)
  ht[1, 2] <- "NorSySS<sup>12</sup> andel"

  ht <- huxtable::merge_cells(ht, 1, 6:9)
  ht[1, 6] <- "NorSySS<sup>2</sup> antall"

  ht <- huxtable::merge_cells(ht, 1, 10:13)
  ht[1, 10] <- "MSIS antall"

  huxtable::left_border(ht)[, c(2, 6, 10)] <- 5
  huxtable::left_border_style(ht)[, c(2, 6, 10)] <- "double"

  huxtable::align(ht) <- "center"

  nr0 <- nrow(ht) + 1
  ht <- huxtable::add_footnote(ht, glue::glue(
    "<sup>1</sup>Nevneren til andelen er totalt antall konsultasjoner i det samme geografiske omr{fhi::nb$aa}det.<br>",
    "<sup>2</sup>NorSySS er forkortelsen for Norwegian Syndromic Surveillance System og her refererer til ICPC-2 koden R991: covid-19 (mistenkt eller bekreftet)<br>",
  ), border = 0)
  nr1 <- nrow(ht)

  huxtable::escape_contents(ht)[1, ] <- F
  huxtable::escape_contents(ht)[nr0:nr1, ] <- F

  huxtable::left_padding(ht) <-  5
  huxtable::right_padding(ht) <-  5

  return(huxtable::to_html(ht))
}

xui_norsyss_kht_email_alert_function_factory <- function(location_codes, x_tags, yrwk, n_status = c("medium", "high")){
  force(location_codes)
  force(x_tags)
  force(yrwk)
  force(n_status)
  function(){
    retval <- list()
    for(tag in x_tags){
      x_location_codes <- tbl("results_norsyss_standard") %>%
        dplyr::filter(granularity_time == "week") %>%
        dplyr::filter(location_code %in% !!location_codes) %>%
        dplyr::filter(tag_outcome %in% !!tag) %>%
        dplyr::filter(yrwk %in% !!yrwk) %>%
        dplyr::filter(n_status %in% !!n_status) %>%
        dplyr::distinct(location_code) %>%
        dplyr::collect() %>%
        sc::latin1_to_utf8()

      x_location_codes <- x_location_codes$location_code

      if(length(x_location_codes)==0){
        retval[[tag]] <- data.table()
      } else {
        retval[[tag]] <- tbl("results_norsyss_standard") %>%
          dplyr::filter(granularity_time == "week") %>%
          dplyr::filter(location_code %in% !!x_location_codes) %>%
          dplyr::filter(tag_outcome %in% !!tag) %>%
          dplyr::filter(yrwk %in% !!yrwk) %>%
          dplyr::collect() %>%
          sc::latin1_to_utf8()
      }
    }

    retval
  }
}

ui_covid19_areas_at_risk_function_factory <- function(yrwk){
  force(yrwk)
  function(){
    retval <- list()

    retval$msis <- tbl("data_covid19_msis_by_time_location") %>%
      dplyr::filter(granularity_time == "week") %>%
      dplyr::filter(yrwk %in% !!yrwk) %>%
      dplyr::collect() %>%
      sc::latin1_to_utf8()

    retval$norsyss <- tbl("data_norsyss") %>%
      dplyr::filter(granularity_time=="day") %>%
      dplyr::filter(age=="total") %>%
      dplyr::filter(yrwk %in% !!yrwk) %>%
      dplyr::filter(tag_outcome %in% "covid19_vk_ote") %>%
      dplyr::select(location_code, yrwk, n, consult_with_influenza) %>%
      dplyr::group_by(location_code,yrwk) %>%
      dplyr::summarize(n=sum(n), consult_with_influenza=sum(consult_with_influenza)) %>%
      dplyr::collect() %>%
      sc::latin1_to_utf8()

    retval$norsyss_norge <- tbl("data_norsyss") %>%
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
