ui_norsyss_pdf <- function(data, argset, schema) {
  if(plnr::is_run_directly()){
    # tm_run_task("ui_norsyss_pdf")
    # tm_run_task("datar_normomo_drop")
    # tm_run_task("analysis_normomo")

    sc::tm_update_plans("ui_norsyss_pdf")
    data <- sc::tm_get_data("ui_norsyss_pdf", index_plan=1)
    argset <- sc::tm_get_argset("ui_norsyss_pdf", index_plan=1, index_argset = 1)
    schema <- sc::tm_get_schema("ui_norsyss_pdf")
  }

  folder <- sc::path("output", "sykdomspulsen_norsyss_restricted_output", "norsyss_pdf", argset$today, create_dir = T)
  fs::dir_create(fs::path(folder, "svg"))
  fs::dir_create(fs::path(folder, "markdown"))
  fs::dir_create(fs::path(folder, "pdf"))

  locs <- unique(norway_locations()[, c("county_code", "county_name")])

  table <- schema$input$dplyr_tbl()

  for (tag in argset$tags) {
    message(glue::glue("sykdomspulspdf {tag}"))
    # setup
    files <- c("monthly_report.Rmd", "monthly_reportALL.Rmd")

    for (f in files) {
      file.copy(
        from = system.file("rmd", "norsyss_pdf",f, package = "sykdomspulsen"),
        to = fs::path(folder, "markdown", f),
        overwrite = !sc::config$is_production
      )
    }

    file_before <- glue::glue("child_{tag}.Rmd")
    files_after <- glue::glue("{locs$county_code}_child_{tag}.Rmd")
    for (i in seq_along(files_after)) {
      file.copy(
        from = system.file("rmd", "norsyss_pdf", file_before, package = "sykdomspulsen"),
        to = fs::path(folder, "markdown", files_after[i]),
        overwrite = !sc::config$is_production
      )
    }

    fhi::sykdompulspdf_resources_copy(fs::path(folder,"markdown"))

    # graphs
    q <- sykdomspulspdf_plot_total(table, location_code = "norge", x_tag = tag)
    ggsave(
      filename = fs::path(folder, "svg", glue::glue("{tag} Norge alle alder {argset$today}.svg")),
      plot = q,
      width = 7,
      height = 4,
      units = "in"
    )

    q <- sykdomspulspdf_plot_ages(table, location_code = "norge", x_tag = tag)
    ggsave(
      filename = fs::path(folder, "svg", glue::glue("{tag} Norge aldersfordelt {argset$today}.svg")),
      plot = q,
      width = 7,
      height = 4,
      units = "in"
    )

    # pdfs

    for (i in 1:nrow(locs)) {
      input <- fs::path(folder, "markdown", "monthly_report.Rmd")
      output_dir <- fs::path(folder, "pdf")
      output_file <- glue::glue("{tag}_{locs$county_code[i]}_monthly_report.pdf")
      output_file_renamed <- glue::glue("{locs$county_name[i]}_{tag}.pdf")
      rmarkdown::render(
        input = fs::path(folder, "markdown", "monthly_report.Rmd"),
        output_dir = output_dir,
        output_file = output_file,
        params = list(
          tag = tag,
          location_code = locs$county_code[i]
        ),
        envir = new.env(),
        quiet = TRUE
      )
      # rename
      fs::file_move(
        fs::path(output_dir, output_file),
        fs::path(output_dir, output_file_renamed)
      )
    }

    rmarkdown::render(
      input = fs::path(folder,"markdown", "monthly_reportALL.Rmd"),
      output_file = glue::glue("{tag}_ALL_monthly_report.pdf"),
      output_dir = fs::path(folder, "pdf", argset$today),
      params = list(
        tag = tag
      ),
      envir = new.env(),
      quiet = TRUE
    )
  }
}


sykdomspulspdf_plot_total <- function(table, location_code, x_tag) {
  data_long <- table %>%
    dplyr::filter(granularity_time == "week") %>%
    dplyr::filter(location_code == !!location_code) %>%
    dplyr::filter(tag_outcome == !!x_tag) %>%
    dplyr::filter(age == "total") %>%
    dplyr::collect() %>%
    sc::latin1_to_utf8()
  # remove last 3 weeks
  yrwks <- rev(sort(unique(data_long$yrwk)))[-c(1:3)]
  data_long <- data_long[yrwk %in% yrwks]
  data_long[, location_name:=get_location_name(location_code)]

  seasons <- rev(unique(data_long$season))[1:5]
  labs <- unique(data_long[, c("week","x")])
  labs <- labs[as.numeric(week) %in% seq(2, 52, 2)]

  data_long <- data_long[season %in% seasons]
  yrange <- max(data_long$n)

  title <- glue::glue(
    "{syndrome}, {location}, alle aldersgrupper",
    syndrome = config$def$long_names[x_tag],
    location = data_long$location_name[1]
  )

  q <- ggplot(data_long, aes(x = x, y = n, color = season))
  q <- q + geom_vline(xintercept = fhi::x(51), lty = 2)
  q <- q + geom_vline(xintercept = fhi::x(2), lty = 2)
  q <- q + geom_vline(xintercept = fhi::x(11), lty = 2)
  q <- q + geom_vline(xintercept = fhi::x(17), lty = 2)
  q <- q + annotate("label", x = (fhi::x(51) + fhi::x(2)) / 2, y = yrange * 0.03, label = "Jul/Nytt\u00E5r", size = 3)
  q <- q + annotate("label", x = fhi::x(14), y = yrange * 0.03, label = "P\u00E5ske", size = 3)
  q <- q + geom_line(lwd = 0.5)
  q <- q + geom_line(data = data_long[season == max(season)], lwd = 1.5)
  q <- q + fhiplot::theme_fhi_basic(10)
  q <- q + fhiplot::scale_color_fhi("", palette = "combination", direction = -1)
  q <- q + guides(color = guide_legend(reverse = FALSE))
  q <- q + expand_limits(y = 0)
  q <- q + scale_y_continuous("Antall konsultasjoner", expand = expand_scale(mult = c(0, 0.1)))
  q <- q + scale_x_continuous(
    "Ukenummer",
    expand = expand_scale(mult = c(0, 0)),
    breaks = labs$x,
    labels = labs$week
  )
  q <- q + labs(title = title)
  q
}


sykdomspulspdf_folder <- function(folder, date, further = NULL) {
  if (is.null(further)) {
    retval <- sc::path("output", "norsyss_pdfs", date, folder)
  } else {
    retval <- sc::path("output", "norsyss_pdfs", date, folder, further)
  }
  return(retval)
}

sykdomspulspdf_plot_ages <- function(table, location_code, x_tag) {
  data_long <- table %>%
    dplyr::filter(granularity_time == "week") %>%
    dplyr::filter(location_code == !!location_code) %>%
    dplyr::filter(tag_outcome == !!x_tag) %>%
    dplyr::filter(age != "total") %>%
    dplyr::collect() %>%
    sc::latin1_to_utf8()
  # remove last 3 weeks
  yrwks <- rev(unique(data_long$yrwk))[-c(1:3)]
  data_long <- data_long[yrwk %in% yrwks]
  data_long[, location_name:=get_location_name(location_code)]
  data_long[, age := car::recode(
    age,
    glue::glue(
      "c('5-14','15-19')='5-19';",
      "c('20-29','30-64')='20-64'"
    )
  )]
  data_long <- data_long[, .(
    n = sum(n)
  ), keyby = .(
    age,
    season,
    x,
    week,
    location_name
  )]
  data_long[, age := factor(
    age,
    levels = c("0-4", "5-19", "20-64", "65+"),
    labels = c(
      glue::glue("0-4 {fhi::nb$aa}r"),
      glue::glue("5-19 {fhi::nb$aa}r"),
      glue::glue("20-64 {fhi::nb$aa}r"),
      glue::glue("65+ {fhi::nb$aa}r")
    )
  )]

  seasons <- rev(unique(data_long$season))[1:5]
  labs <- unique(data_long[, c("week", "x")])
  labs <- labs[as.numeric(week) %in% seq(2, 52, 4)]

  data_long <- data_long[season %in% seasons]
  yrange <- max(data_long$n)

  title <- glue::glue(
    "{syndrome}, {location}, aldersfordelt",
    syndrome = config$def$long_names[x_tag],
    location = data_long$location_name[1]
  )

  q <- ggplot(data_long, aes(x = x, y = n, color = season))
  q <- q + geom_vline(xintercept = fhi::x(51), lty = 2)
  q <- q + geom_vline(xintercept = fhi::x(2), lty = 2)
  q <- q + geom_vline(xintercept = fhi::x(11), lty = 2)
  q <- q + geom_vline(xintercept = fhi::x(17), lty = 2)
  q <- q + geom_line(lwd = 0.5)
  q <- q + geom_line(data = data_long[season == max(season)], lwd = 1.5)
  q <- q + lemon::facet_rep_wrap(~age, repeat.tick.labels = "all", scales = "free")
  q <- q + fhiplot::theme_fhi_basic(10)
  q <- q + fhiplot::scale_color_fhi("", palette = "combination", direction = -1)
  q <- q + guides(color = FALSE)
  q <- q + expand_limits(y = 0)
  q <- q + scale_y_continuous("Antall konsultasjoner", expand = expand_scale(mult = c(0, 0.1)))
  q <- q + scale_x_continuous(
    "Ukenummer",
    expand = expand_scale(mult = c(0, 0)),
    breaks = labs$x,
    labels = labs$week
  )
  q <- q + labs(title = title)
  q
}
