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
  master[
    nordic_locations(),
    on="location_code",
    iso3:=iso3
  ]

  folder <- sc::path("output",argset$folder, create_dir = T)
  filename <- glue::glue(argset$filename)
  filepath <- fs::path(folder, filename)

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "cases pr100000")
  openxlsx::addWorksheet(wb, "tests (positive) pr100")
  openxlsx::addWorksheet(wb, "icu n")
  openxlsx::addWorksheet(wb, "cases n")
  openxlsx::addWorksheet(wb, "tests (total) n")

  #negStyle <- openxlsx::createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
  style_red <- openxlsx::createStyle(fgFill = "peachpuff")
  style_perc <- openxlsx::createStyle(numFmt="0.0%")
  style_1_decimal <- openxlsx::createStyle(numFmt="0.0")

  # cases_pr100000 ----
  tab <- master[tag_outcome=="cases" & yrwk >= "2020-20"]
  tab <- dcast.data.table(
    tab,
    iso3+location_code ~ yrwk,
    value.var = "pr100000"
  )
  tab[, location_name := get_nordic_location_name(location_code)]
  setcolorder(tab, c("iso3","location_code","location_name"))

  tab_alert <- master[tag_outcome=="cases" & yrwk >= "2020-20"]
  tab_alert <- dcast.data.table(
    tab_alert,
    iso3+location_code ~ yrwk,
    value.var = "n_status"
  )

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
  for(i in 1:ncol(tab_alert)){
    rows <- which(tab_alert[,i,with=F]=="high")
    if(length(rows)==0) next
    openxlsx::addStyle(
      wb,
      sheet = "cases pr100000",
      style = style_red,
      rows = rows+1,
      cols = i+1,
      gridExpand = F,
      stack = T
    )
  }


  # tests_pr100 ----
  tab <- master[tag_outcome=="tests" & yrwk >= "2020-20"]
  tab[, prop := pr100/100]
  tab <- dcast.data.table(
    tab,
    iso3+location_code ~ yrwk,
    value.var = "prop"
  )
  tab[, location_name := get_nordic_location_name(location_code)]
  setcolorder(tab, c("iso3","location_code","location_name"))

  tab_alert <- master[tag_outcome=="tests" & yrwk >= "2020-20"]
  tab_alert <- dcast.data.table(
    tab_alert,
    iso3+location_code ~ yrwk,
    value.var = "n_status"
  )

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
  for(i in 1:ncol(tab_alert)){
    rows <- which(tab_alert[,i,with=F]=="high")
    if(length(rows)==0) next
    openxlsx::addStyle(
      wb,
      sheet = "tests (positive) pr100",
      style = style_red,
      rows = rows+1,
      cols = i+1,
      gridExpand = F,
      stack = T
    )
  }

  # icu_n ----
  tab <- master[
    tag_outcome=="icu" &
      yrwk >= "2020-20" &
      location_code %in% c(
        "DK",
        "FI",
        "IS",
        "SE"
      )
  ]

  tab <- dcast.data.table(
    tab,
    iso3+location_code ~ yrwk,
    value.var = "n"
  )
  tab[, location_name := get_nordic_location_name(location_code)]
  setcolorder(tab, c("iso3","location_code","location_name"))

  openxlsx::freezePane(wb, "icu n", firstActiveRow=2, firstActiveCol=4)
  openxlsx::setColWidths(wb, "icu n", cols=c(1:3), width="auto", hidden=c(F,T,F))
  openxlsx::writeData(
    wb,
    sheet = "icu n",
    x = tab
  )

  # cases_n ----
  tab <- master[tag_outcome=="cases" & yrwk >= "2020-20"]

  tab <- dcast.data.table(
    tab,
    iso3+location_code ~ yrwk,
    value.var = "n"
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
  tab <- master[tag_outcome=="tests" & yrwk >= "2020-20"]

  tab <- dcast.data.table(
    tab,
    iso3+location_code ~ yrwk,
    value.var = "n"
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
