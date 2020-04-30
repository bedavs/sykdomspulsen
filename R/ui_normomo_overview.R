
ui_normomo_overview <- function(data, argset, schema) {
  if(FALSE){
    tm_run_task("ui_normomo_overview_by_location")
    tm_run_task("ui_normomo_overview_by_age")

    tm_update_plans("ui_normomo_overview_by_location")
    data <- tm_get_data("ui_normomo_overview_by_location", index_plan=1)
    argset <- tm_get_argset("ui_normomo_overview_by_location", index_plan=1, index_argset = 1)
    schema <- tm_get_schema("ui_normomo_overview_by_location")

    tm_update_plans("ui_normomo_overview_by_age")
    data <- tm_get_data("ui_normomo_overview_by_age", index_plan=1)
    argset <- tm_get_argset("ui_normomo_overview_by_age", index_plan=1, index_argset = 1)
    schema <- tm_get_schema("ui_normomo_overview_by_age")
  }

  d <- copy(data$data)

  # folder
  folder <- path("output",glue::glue(argset$folder))
  file <- glue::glue(argset$file)
  filepath <- fs::path(folder,file)

  fs::dir_create(folder)

  # caption
  caption <- glue::glue('Sist oppdatert: {strftime(argset$today, format = "%d/%m/%Y")}')

  x_yrwk <- rev(sort(as.character(unique(d$yrwk))))[1:52]
  plotData <- d[yrwk %in% x_yrwk]
  plotData[, status := "1veryhigh"]
  plotData[ncor_est < ncor_baseline_thresholdu1, status := "2high"]
  plotData[ncor_est < ncor_baseline_thresholdu0, status := "3expected"]

  plotData[norway_locations_long(), on = "location_code", location_name := location_name]
  plotData <- plotData[!is.na(location_name)]
  unique(plotData$location_code)
  unique(plotData$location_name)

  plotData[, age := factor(
    age,
    levels = c("0-4", "5-14", "15-64", "65+", "65-74", "75-84", "85+", "total"),
    labels = c("0-4", "5-14", "15-64", "65+", "65-74", "75-84", "85+", "Totalt")
  )]
  plotData[, location_name := factor(location_name, levels = norway_locations_long()[location_code %in% plotData$location_code]$location_name)]

  if(argset$by=="location"){
    pretty_labs <- unique(plotData[, c("location_name", "age")])
    setorder(pretty_labs, -location_name, age)
    pretty_labs[, pretty_cat := glue::glue(
      "{location_name}",
      location_name = location_name
    )]
    pretty_labs[, pretty_cat := factor(pretty_cat, levels = pretty_cat)]

    plotData[pretty_labs, on = c("location_name", "age"), pretty_cat := pretty_cat]
    title <- glue::glue("Antall d\u00F8de per uke siste \u00E5r (aldersgruppe: {stringr::str_to_title(argset$age)})")
  } else {
    plotData[, pretty_cat := age]
    title <- glue::glue("Antall d\u00F8de per uke siste \u00E5r i {get_location_name(argset$location_code)}")
  }

  plotColours <- plotData[1:4]
  # plotColours[1,status:="4lower"]
  plotColours[2, status := "3expected"]
  plotColours[3, status := "2high"]
  plotColours[4, status := "1veryhigh"]

  q <- ggplot(plotData, aes(x = yrwk, y = pretty_cat, fill = status))
  q <- q + geom_tile(colour = "black")
  q <- q + geom_tile(data = plotColours, alpha = 0)
  q <- q + scale_fill_manual("",
                             values = c("1veryhigh" = fhiplot::warning_color[["hig"]], "2high" = fhiplot::warning_color[["med"]], "3expected" = fhiplot::warning_color[["low"]]),
                             labels = c(
                               "Betydelig forh\u00F8yet",
                               "Forh\u00F8yet",
                               "Normalt"
                             )
  )
  q <- q + labs(title = title)
  q <- q + scale_x_discrete("\u00C5r-uke", expand = c(0, 0))
  q <- q + scale_y_discrete("", expand = c(0, 0))
  q <- q + labs(caption = caption)
  q <- q + fhiplot::theme_fhi_basic()
  q <- q + fhiplot::set_x_axis_vertical()
  # q
  fhiplot::save_a4(
    q,
    filepath,
    landscape = T
  )

}
