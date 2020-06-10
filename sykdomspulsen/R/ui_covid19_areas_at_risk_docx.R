#' ui_covid19_areas_at_risk_docx
#' @param data a
#' @param argset a
#' @param schema a
#' @export
ui_covid19_areas_at_risk_docx <- function(data, argset, schema) {
  # tm_run_task("ui_covid19_areas_at_risk_docx")

  if(plnr::is_run_directly()){
    sc::tm_update_plans("ui_covid19_areas_at_risk_docx")

    index_plan <- 1
    data <- sc::tm_get_data("ui_covid19_areas_at_risk_docx", index_plan=index_plan)
    argset <- sc::tm_get_argset("ui_covid19_areas_at_risk_docx", index_plan=index_plan, index_argset = 1)
    schema <- sc::tm_get_schema("ui_covid19_areas_at_risk_docx")
  }

  # create the table
  # figure out which location_codes have alerts in the last *2* weeks
  # pull ou tthe last 4 weeks of data for these location codes
  # and then make it look like the word document
  tab1 <- areas_at_risk_msis_norsyss(data)
  tab2 <- areas_at_risk_norsyss(data)
  # render it

  file <- glue::glue(argset$file)
  folder <- sc::path("output",argset$folder, create_dir = T)
  filepath <- fs::path(folder, file)
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

areas_at_risk_msis_norsyss <- function(data){
  d <- copy(data$data)
  if(nrow(d)==0) return(NULL)
  d <- d[age=="total"]

  yrwks <- rev(sort(unique(d$yrwk)))[1:4]
  yrwks_alert <- yrwks[1:2]

  # alerts, later 2 weeks
  alerts_msis <- unique(d[yrwk %in% yrwks_alert & n_msis_status=="high"]$location_code)
  alerts_norsyss <- unique(d[yrwk %in% yrwks_alert & n_norsyss_status=="high"]$location_code)
  location_codes <- unique(alerts_msis, alerts_norsyss)

  ## last 4 weeks

  retval <- d[yrwk %in% yrwks & location_code %in% location_codes]

  tab <- retval[,c("location_code",
            "yrwk",
            "n_msis",
            "n_msis_baseline_thresholdu0",
            "n_msis_status",
            "n_norsyss",
            "pr100_norsyss",
            "pr100_norsyss_baseline_thresholdu0",
            "n_norsyss_status")]


  tab[, pretty_msis_threshold:=fhiplot::format_nor(n_msis_baseline_thresholdu0)]
  tab[, pretty_n_msis:=fhiplot::format_nor(n_msis)]
  tab[, pretty_norsyss_n:=fhiplot::format_nor(n_norsyss)]
  tab[, pretty_norsyss_pr100:=fhiplot::format_nor_perc_1(pr100_norsyss)]
  tab[, pretty_norsyss_pr100_threshold:=fhiplot::format_nor_perc_1(pr100_norsyss_baseline_thresholdu0)]



  tab[,uke:=1:.N, by=c("location_code")]

  #tab[uke %in% 1:2, pretty_msis_threshold:=""]
  #tab[uke %in% 1:2, pretty_norsyss_pr100_threshold:=""]

  tab[,location_name := get_location_name(location_code)]
  tab[location_name=="Bergen"]

  tab[uke %in% 3:4,msis_difference := n_msis-n_msis_baseline_thresholdu0]
  tab[uke %in% 3:4,norsyss_difference := pr100_norsyss-pr100_norsyss_baseline_thresholdu0]

  #tab[, yrwk := uke]
  levels(tab$yrwk) <- yrwks

  # get the ordering of locations right
  ordering_msis <- na.omit(tab[,c("location_name","location_code","msis_difference","norsyss_difference")])
  setorder(ordering_msis, -msis_difference, -norsyss_difference)
  ordering_msis <- unique(ordering_msis$location_code)

  ordering_norsyss <- na.omit(tab[,c("location_name","location_code","msis_difference","norsyss_difference")])
  setorder(ordering_norsyss, -norsyss_difference, -msis_difference)
  ordering_norsyss <- unique(ordering_norsyss$location_code)

  location_codes_1 <- ordering_msis[ordering_msis %in% alerts_msis]
  location_codes_2 <- ordering_norsyss[!ordering_norsyss %in% alerts_msis]
  location_codes <- c(location_codes_1, location_codes_2)

  tab[,location_code:=factor(location_code, levels = location_codes)]
  setorder(tab,location_code,yrwk)



  return(tab)
}

areas_at_risk_norsyss <- function(data){
  d <- copy(data$data)
  if(nrow(d)==0) return(NULL)
  d <- d[age!="total"]

  yrwks <- rev(sort(unique(d$yrwk)))[1:4]
  yrwks_alert <- yrwks[1:2]

  # alerts, later 2 weeks
  location_codes <- unique(d[yrwk %in% yrwks_alert & n_norsyss_status=="high"]$location_code)

  ## last 4 weeks

  retval <- d[yrwk %in% yrwks & location_code %in% location_codes]

  tab <- retval[,c("location_code",
                   "yrwk",
                   "age",
                   "n_norsyss",
                   "pr100_norsyss",
                   "pr100_norsyss_baseline_thresholdu0",
                   "n_norsyss_status")]

  tab[, pretty_norsyss_n:=fhiplot::format_nor(n_norsyss)]
  tab[, pretty_norsyss_pr100:=fhiplot::format_nor_perc_1(pr100_norsyss)]
  tab[, pretty_norsyss_pr100_threshold:=fhiplot::format_nor_perc_1(pr100_norsyss_baseline_thresholdu0)]


  tab[,uke:=1:.N, by=c("location_code")]

  #tab[uke %in% 1:2, pretty_msis_threshold:=""]
  #tab[uke %in% 1:2, pretty_norsyss_pr100_threshold:=""]

  tab[,location_name := get_location_name(location_code)]

  tab[uke %in% 3:4,norsyss_difference := pr100_norsyss-pr100_norsyss_baseline_thresholdu0]

  #tab[, yrwk := uke]
  levels(tab$yrwk) <- yrwks

  # get the ordering of locations right
  ordering_norsyss <- na.omit(tab[,c("location_name","location_code","norsyss_difference")])
  setorder(ordering_norsyss, -norsyss_difference)
  location_codes <- unique(ordering_norsyss$location_code)

  tab[,location_code:=factor(location_code, levels = location_codes)]
  setorder(tab,location_code,yrwk, age)



  return(tab)
}

areas_at_risk_ft <- function(tab){
  msis_index_hig <-which(tab$n_msis_status=="high")
  norsyss_index_hig <- which(tab$n_norsyss_status=="high")

  ft <- flextable::flextable(
    data.frame(
      "Geo"=tab$location_name,
      "Uke"=tab$yrwk,
      "Tilfeller"=tab$pretty_n_msis,
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

areas_at_risk_norsyss_ft <- function(tab){
  norsyss_index_hig <- which(tab$n_norsyss_status=="high")

  ft <- flextable::flextable(
    data.frame(
      "Geo"=tab$location_name,
      "Uke"=tab$yrwk,
      "Alder"=tab$age,
      "Konsultasjoner"=tab$pretty_norsyss_n,
      "Andel"=tab$pretty_norsyss_pr100,
      "Terskel"=tab$pretty_norsyss_pr100_threshold
    )
  )
  labs <- as.list(c(
    "Geo",
    "Uke",
    "Alder",
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
      "NorSySS"
    ),
    colwidths = c(
      3,
      3
    )
  )
  ft <- flextable::theme_box(ft)
  ft <- flextable::align(ft, align = "center", part = "all")

  if (length(norsyss_index_hig) > 0) ft <- flextable::bg(
    ft,
    i = norsyss_index_hig,
    j = 5,
    bg=fhiplot::warning_color[["hig"]]
  )

  return(ft)
}

