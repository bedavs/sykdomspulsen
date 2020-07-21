#' ui_covid19_nordic
#' @param data a
#' @param argset a
#' @param schema a
#' @export
ui_covid19_nordic <- function(data, argset, schema) {
  # tm_run_task("ui_covid19_nordic")

  if(plnr::is_run_directly()){
    sc::tm_update_plans("ui_covid19_nordic")

    index_plan <- 1
    data <- sc::tm_get_data("ui_covid19_nordic", index_plan=index_plan)
    argset <- sc::tm_get_argset("ui_covid19_nordic", index_plan=index_plan, index_argset = 1)
    schema <- sc::tm_get_schema("ui_covid19_nordic")
  }

  master <- copy(data$data)
  setorder(master, location_code, yrwk)
  master[
    nordic_locations(),
    on="location_code",
    iso3:=iso3
  ]

  folder <- sc::path("output",argset$folder, create_dir = T)
  filename <- glue::glue(argset$filename)
  filepath <- fs::path(folder, filename)

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "decisions")
  openxlsx::addWorksheet(wb, "cases pr100000")
  openxlsx::addWorksheet(wb, "tests (positive) pr100")
  #openxlsx::addWorksheet(wb, "icu n")
  openxlsx::addWorksheet(wb, "cases n")
  openxlsx::addWorksheet(wb, "tests (total) n")

  #negStyle <- openxlsx::createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
  style_title <- openxlsx::createStyle(fontSize = 20)
  style_red <- openxlsx::createStyle(fgFill = "peachpuff")
  style_perc <- openxlsx::createStyle(numFmt="0.0%")
  style_1_decimal <- openxlsx::createStyle(numFmt="0.0")

  # decisions main ----
  yrwks <- fhi::isoyearweek(lubridate::today()-c(14,7))
  yrwkx <- fhi::isoyearweek(lubridate::today()-c(7))
  yrwks_combined <- paste0(yrwks,collapse=" and ")

  openxlsx::freezePane(wb, "decisions", firstActiveRow=3, firstActiveCol=4)
  openxlsx::setColWidths(wb, "decisions", cols=c(1:3,6,10), width="auto", hidden=c(F,T,F,F,F))
  # openxlsx::setColWidths(wb, "decisions", cols=c(4:10), width="auto")

  openxlsx::writeData(
    wb,
    "decisions",
    x = c("Cases/100 000"),
    startCol = 4,
    startRow = 1
  )

  openxlsx::writeData(
    wb,
    "decisions",
    x = c("Positive tests (%)"),
    startCol = 8,
    startRow = 1
  )

  openxlsx::addStyle(
    wb,
    sheet = "decisions",
    style = style_title,
    rows = 1,
    cols = 1:10,
    gridExpand = T,
    stack = T
  )

  # decisions cases ----
  tab <- master[
    yrwk %in% yrwkx,
    .(
      iso3,
      location_code,
      pr100000_cases_lag1,
      pr100000_cases_lag0,
      pr100000_cases_lag0_1
    )
  ]
  tab[, location_name := get_nordic_location_name(location_code)]
  setcolorder(tab, c("iso3","location_code","location_name"))

  tab1 <- tab

  openxlsx::writeData(
    wb,
    sheet = "decisions",
    x = tab1,
    startCol = 1,
    startRow = 2
  )
  openxlsx::addStyle(
    wb,
    sheet = "decisions",
    style = style_1_decimal,
    rows = 1:100,
    cols = 4:6,
    gridExpand = T,
    stack = T
  )

  # decisions test ----
  tab <- master[
    yrwk %in% yrwkx,
    .(
      pr1_tests_lag1 = pr100_tests_lag1 / 100,
      pr1_tests_lag0 = pr100_tests_lag0 / 100,
      pr1_tests_lag0_1 = pr100_tests_lag0_1 /100
    )
  ]
  tab2 <- tab

  openxlsx::writeData(
    wb,
    sheet = "decisions",
    x = tab2,
    startCol = 8,
    startRow = 2
  )
  openxlsx::addStyle(
    wb,
    sheet = "decisions",
    style = style_perc,
    rows = 1:100,
    cols = 8:10,
    gridExpand = T,
    stack = T
  )

  # decisions titles ----

  openxlsx::writeData(
    wb,
    "decisions",
    x = t(c(yrwks, yrwks_combined, "     ", yrwks, yrwks_combined)),
    colNames = F,
    startCol = 4,
    startRow = 2
  )

  # cases_pr100000 ----
  tab <- master[
    yrwk >= "2020-20",
    .(
      iso3,
      location_code,
      yrwk,
      pr100000_cases_lag0
    )
  ]
  tab <- dcast.data.table(
    tab,
    iso3+location_code ~ yrwk,
    value.var = "pr100000_cases_lag0"
  )
  tab[, location_name := get_nordic_location_name(location_code)]
  setcolorder(tab, c("iso3","location_code","location_name"))

  openxlsx::freezePane(wb, "cases pr100000", firstActiveRow=2, firstActiveCol=4)
  openxlsx::setColWidths(wb, "cases pr100000", cols=c(1:3), width="auto", hidden=c(F,T,F))
  openxlsx::writeData(
    wb,
    sheet = "cases pr100000",
    x = tab
  )
  openxlsx::addStyle(
    wb,
    sheet = "cases pr100000",
    style = style_1_decimal,
    rows = 1:100,
    cols = 1:40,
    gridExpand = T,
    stack = T
  )

  # tests_pr100 ----
  tab <- master[
    yrwk >= "2020-20",
    .(
      iso3,
      location_code,
      yrwk,
      pr100_tests_lag0
    )
  ]
  tab[, prop := pr100_tests_lag0/100]
  tab <- dcast.data.table(
    tab,
    iso3+location_code ~ yrwk,
    value.var = "prop"
  )
  tab[, location_name := get_nordic_location_name(location_code)]
  setcolorder(tab, c("iso3","location_code","location_name"))

  openxlsx::freezePane(wb, "tests (positive) pr100", firstActiveRow=2, firstActiveCol=4)
  openxlsx::setColWidths(wb, "tests (positive) pr100", cols=c(1:3), width="auto", hidden=c(F,T,F))
  openxlsx::writeData(
    wb,
    sheet = "tests (positive) pr100",
    x = tab
  )
  openxlsx::addStyle(
    wb,
    sheet = "tests (positive) pr100",
    style = style_perc,
    rows = 1:100,
    cols = 1:40,
    gridExpand = T,
    stack = T
  )

  # cases_n ----
  tab <- master[
    yrwk >= "2020-20",
    .(
      iso3,
      location_code,
      yrwk,
      n_cases_lag0
    )
  ]

  tab <- dcast.data.table(
    tab,
    iso3+location_code ~ yrwk,
    value.var = "n_cases_lag0"
  )
  tab[, location_name := get_nordic_location_name(location_code)]
  setcolorder(tab, c("iso3","location_code","location_name"))

  openxlsx::freezePane(wb, "cases n", firstActiveRow=2, firstActiveCol=4)
  openxlsx::setColWidths(wb, "cases n", cols=c(1:3), width="auto", hidden=c(F,T,F))
  openxlsx::writeData(
    wb,
    sheet = "cases n",
    x = tab
  )



  # tests_n ----
  tab <- master[
    yrwk >= "2020-20",
    .(
      iso3,
      location_code,
      yrwk,
      denom_tests_lag0
    )
  ]

  tab <- dcast.data.table(
    tab,
    iso3+location_code ~ yrwk,
    value.var = "denom_tests_lag0"
  )
  tab[, location_name := get_nordic_location_name(location_code)]
  setcolorder(tab, c("iso3","location_code","location_name"))

  openxlsx::freezePane(wb, "tests (total) n", firstActiveRow=2, firstActiveCol=4)
  openxlsx::setColWidths(wb, "tests (total) n", cols=c(1:3), width="auto", hidden=c(F,T,F))
  openxlsx::writeData(
    wb,
    sheet = "tests (total) n",
    x = tab
  )


  openxlsx::saveWorkbook(wb, filepath, overwrite = TRUE)


}
