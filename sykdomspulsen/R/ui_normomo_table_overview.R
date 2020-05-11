
ui_normomo_table_overview <- function(data, argset, schema) {
  if(FALSE){
    tm_update_plans("ui_normomo_table_overview")

    data <- tm_get_data("ui_normomo_table_overview", index_plan=1)
    argset <- tm_get_argset("ui_normomo_table_overview", index_plan=1, index_argset = 1)
    schema <- tm_get_schema("ui_normomo_table_overview")
  }

  d <- copy(data$data)

  # folder
  folder <- sc::path("output",glue::glue(argset$folder))
  fs::dir_create(folder)
  file <- glue::glue(argset$file)
  filepath <- fs::path(folder,file)

  yrwks <- rev(unique(sort(d$yrwk)))[1:8]
  d <- d[(age=="total" & yrwk %in% yrwks) | yrwk %in% yrwks[1:4]]

  d[
    ,
    age := factor(
      age,
      levels = rev(c("0-4", "5-14", "15-64", "65+", "65-74", "75-84", "85+", "total")),
      labels = rev(c("0-4", "5-14", "15-64", "65+", "65-74", "75-84", "85+", "Totalt"))
    )
    ]
  setorder(d, age, -yrwk)

  tab <- huxtable::hux(
    "Alder" = d$age,
    "\u00C5r-uke" = d$yrwk,
    "Registrert\\textsuperscript{1}" = d$n_obs,
    "Korrigert\\textsuperscript{2}" = round(d$ncor_est),
    "Z-score\\textsuperscript{3}" = fhiplot::format_nor(d$ncor_zscore, 2),
    "Overd\u00F8delighet\\textsuperscript{4}" = ceiling(d$ncor_excess),
    "Normalt\\textsuperscript{5}" = glue::glue("{round(d$ncor_baseline_thresholdl0)} - {round(d$ncor_baseline_thresholdu0)}"),
    "Forh\u00F8yet" = glue::glue("{round(d$ncor_baseline_thresholdu0)} - {round(d$ncor_baseline_thresholdu1)}"),
    "Betydelig forh\u00F8yet" = glue::glue(">{round(d$ncor_baseline_thresholdu1)}")
  ) %>%
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

  for (col in 1:6) {
    huxtable::background_color(tab)[-1, col] <- fhiplot::warning_color["low"]
    huxtable::background_color(tab)[index_med, col] <- fhiplot::warning_color["med"]
    huxtable::background_color(tab)[index_hig, col] <- fhiplot::warning_color["hig"]
  }

  huxtable::background_color(tab)[index_low, 7] <- fhiplot::warning_color["low"]
  huxtable::background_color(tab)[index_med, 8] <- fhiplot::warning_color["med"]
  huxtable::background_color(tab)[index_hig, 9] <- fhiplot::warning_color["hig"]

  tab <- huxtable::add_rows(tab, tab[1, ], after = 0)

  tab[1, 1] <- " "
  tab[1, 2] <- " "
  tab[1, 6] <- " "

  tab <- huxtable::merge_cells(tab, 1, 3:5)
  tab[1, 3] <- "Antall d\u00F8dsfall"

  tab <- huxtable::merge_cells(tab, 1, 7:9)
  tab[1, 7] <- "D\u00F8delighetsniv\u00E5"

  # first column
  tab <- huxtable::merge_cells(tab, 3:10, 1)
  tab <- huxtable::merge_cells(tab, 11:14, 1)
  tab <- huxtable::merge_cells(tab, 15:18, 1)
  tab <- huxtable::merge_cells(tab, 19:22, 1)
  tab <- huxtable::merge_cells(tab, 23:26, 1)
  tab <- huxtable::merge_cells(tab, 27:30, 1)
  tab <- huxtable::merge_cells(tab, 31:34, 1)
  tab <- huxtable::merge_cells(tab, 35:38, 1)

  huxtable::top_border_style(tab)[3, ] <- "double"
  huxtable::top_border_style(tab)[11, ] <- "double"
  huxtable::top_border_style(tab)[15, ] <- "double"
  huxtable::top_border_style(tab)[19, ] <- "double"
  huxtable::top_border_style(tab)[23, ] <- "double"
  huxtable::top_border_style(tab)[27, ] <- "double"
  huxtable::top_border_style(tab)[31, ] <- "double"
  huxtable::top_border_style(tab)[35, ] <- "double"

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

  huxtable::left_border_style(tab)[1:(nr0 - 1), 2] <- "double"
  huxtable::left_border_style(tab)[1:(nr0 - 1), 3] <- "double"
  huxtable::left_border_style(tab)[1:(nr0), 6] <- "double"
  huxtable::left_border_style(tab)[1:(nr0), 7] <- "double"

  # add a header
  tab <- huxtable::insert_row(tab, glue::glue("{get_location_name(argset$location_code)}"), fill = "", colspan = ncol(tab))

  # tab
  huxtable_to_png(tab, file = filepath)

}
