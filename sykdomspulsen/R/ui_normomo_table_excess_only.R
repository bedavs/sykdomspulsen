#' ui_normomo_table_excess_only
#' @param data a
#' @param argset a
#' @param schema a
#' @export
ui_normomo_table_excess_only <- function(data, argset, schema) {
  if(FALSE){
    sc::tm_update_plans("ui_normomo_table_excess_only_sort_location")

    data <- sc::tm_get_data("ui_normomo_table_excess_only_sort_location", index_plan=1)
    argset <- sc::tm_get_argset("ui_normomo_table_excess_only_sort_location", index_plan=1, index_argset = 1)
    schema <- sc::tm_get_schema("ui_normomo_table_excess_only_sort_location")

    sc::tm_update_plans("ui_normomo_table_excess_only_sort_age")

    data <- sc::tm_get_data("ui_normomo_table_excess_only_sort_age", index_plan=1)
    argset <- sc::tm_get_argset("ui_normomo_table_excess_only_sort_age", index_plan=1, index_argset = 1)
    schema <- sc::tm_get_schema("ui_normomo_table_excess_only_sort_age")
  }

  d <- copy(data$data)
  d <- d[n_obs > 0]

  # folder
  folder <- sc::path("output",argset$folder, create_dir = T)
  file <- glue::glue(argset$file)
  filepath <- fs::path(folder,file)

  # if no excess mortality, then make empty graph
  if(nrow(d)==0){
    q <- no_data_plot()
    fhiplot::save_a4(
      q,
      filepath
    )
    return()
  }


  yrwks <- rev(unique(sort(d$yrwk)))[1:8]
  d <- d[ncor_status != "normal" & yrwk %in% yrwks]
  d[,location_name := get_location_name(location_code)]

  d[
    ,
    age := factor(
      age,
      levels = rev(c("0-4", "5-14", "15-64", "65+", "65-74", "75-84", "85+", "total")),
      labels = rev(c("0-4", "5-14", "15-64", "65+", "65-74", "75-84", "85+", "Totalt"))
    )
    ]

  setorder(d,location_name, age, -yrwk)
  d <- rbind(d[location_code=="norge"],d[location_code!="norge"])
  if(argset$sort=="age"){
    location_code_order <- unique(d$location_code)
    setorder(d,age, -yrwk, -location_code)
  }

  if(argset$sort=="location"){
    tab <- huxtable::hux(
      "Omr\u00E5de" = d$location_name,
      "Alder" = d$age,
      "\u00C5r-uke" = d$yrwk,
      "Registrert\\textsuperscript{1}" = d$n_obs,
      "Korrigert\\textsuperscript{2}" = round(d$ncor_est),
      "Z-score\\textsuperscript{3}" = fhiplot::format_nor(d$ncor_zscore, 2),
      "Overd\u00F8delighet\\textsuperscript{4}" = ceiling(d$ncor_excess),
      "Normalt\\textsuperscript{5}" = glue::glue("{round(d$ncor_baseline_thresholdl0)} - {round(d$ncor_baseline_thresholdu0)}"),
      "Forh\u00F8yet" = glue::glue("{round(d$ncor_baseline_thresholdu0)} - {round(d$ncor_baseline_thresholdu1)}"),
      "Betydelig forh\u00F8yet" = glue::glue(">{round(d$ncor_baseline_thresholdu1)}")
    )
  } else if(argset$sort=="age"){
    tab <- huxtable::hux(
      "Alder" = d$age,
      "\u00C5r-uke" = d$yrwk,
      "Omr\u00E5de" = d$location_name,
      "Registrert\\textsuperscript{1}" = d$n_obs,
      "Korrigert\\textsuperscript{2}" = round(d$ncor_est),
      "Z-score\\textsuperscript{3}" = fhiplot::format_nor(d$ncor_zscore, 2),
      "Overd\u00F8delighet\\textsuperscript{4}" = ceiling(d$ncor_excess),
      "Normalt\\textsuperscript{5}" = glue::glue("{round(d$ncor_baseline_thresholdl0)} - {round(d$ncor_baseline_thresholdu0)}"),
      "Forh\u00F8yet" = glue::glue("{round(d$ncor_baseline_thresholdu0)} - {round(d$ncor_baseline_thresholdu1)}"),
      "Betydelig forh\u00F8yet" = glue::glue(">{round(d$ncor_baseline_thresholdu1)}")
    )
  }

  tab %<>%
    huxtable::add_colnames() %>%
    fhiplot::huxtable_theme_fhi_basic() %>%
    huxtable::set_align(huxtable::everywhere, huxtable::everywhere, "center") %>%
    huxtable::set_top_padding(huxtable::everywhere, huxtable::everywhere, 0.1) %>%
    huxtable::set_bottom_padding(huxtable::everywhere, huxtable::everywhere, 0.1) %>%
    huxtable::set_left_padding(huxtable::everywhere, huxtable::everywhere, 0.1) %>%
    huxtable::set_right_padding(huxtable::everywhere, huxtable::everywhere, 0.1)

  index_low <- which(d$ncor_status == "normal") + 1
  index_med <- which(d$ncor_status == "medium") + 1
  index_hig <- which(d$ncor_status == "high") + 1

  for (col in 1:7) {
    huxtable::background_color(tab)[-1, col] <- fhiplot::warning_color["low"]
    huxtable::background_color(tab)[index_med, col] <- fhiplot::warning_color["med"]
    huxtable::background_color(tab)[index_hig, col] <- fhiplot::warning_color["hig"]
  }

  huxtable::background_color(tab)[index_low, 8] <- fhiplot::warning_color["low"]
  huxtable::background_color(tab)[index_med, 9] <- fhiplot::warning_color["med"]
  huxtable::background_color(tab)[index_hig, 10] <- fhiplot::warning_color["hig"]

  tab <- huxtable::add_rows(tab, tab[1, ], after = 0)

  tab[1, 1] <- " "
  tab[1, 2] <- " "
  tab[1, 3] <- " "
  tab[1, 7] <- " "

  tab <- huxtable::merge_cells(tab, 1, 4:6)
  tab[1, 4] <- "Antall d\u00F8dsfall"

  tab <- huxtable::merge_cells(tab, 1, 8:10)
  tab[1, 8] <- "D\u00F8delighetsniv\u00E5"

  # first column
  tab <- huxtable::merge_repeated_rows(tab, huxtable::everywhere, 1)
  tab <- huxtable::merge_repeated_rows(tab, huxtable::everywhere, 2)

  huxtable::width(tab) <- 0.9

  nr0 <- nrow(tab) + 1
  tab <- huxtable::add_footnote(tab, glue::glue(
    "\\textsuperscript{1}Antall registrerte d{fhi::nb$oe}dsfall\\\\*",
    "\\textsuperscript{2}Antall registrerte d{fhi::nb$oe}dsfall korrigert for registreringsforsinkelse\\\\*",
    "\\textsuperscript{3}Standardavvik (z-score $\\ge$ 2,0 indikerer at det er et h{fhi::nb$oe}yere antall d{fhi::nb$oe}dsfall enn normalt)\\\\*",
    "\\textsuperscript{4}Differansen mellom antall korrigerte d{fhi::nb$oe}dsfall og {fhi::nb$oe}vre grense for normalt antall d{fhi::nb$oe}dsfall\\textsuperscript{5}\\\\*",
    "\\textsuperscript{5}95\\% prediksjonsintervall"
  ), border = 0)
  nr1 <- nrow(tab)

  huxtable::escape_contents(tab)[nr0:nr1, ] <- F
  huxtable::escape_contents(tab)[1:2, ] <- F

  huxtable::left_border_style(tab)[1:(nr0 - 1), 3] <- "double"
  huxtable::left_border_style(tab)[1:(nr0 - 1), 4] <- "double"
  huxtable::left_border_style(tab)[1:(nr0), 7] <- "double"
  huxtable::left_border_style(tab)[1:(nr0), 8] <- "double"

  # add a header
  tab <- huxtable::insert_row(tab, glue::glue(""), fill = "", colspan = ncol(tab))

  # tab
  huxtable_to_png(tab, file = filepath)

}
