covid19_ui <- function(id, config) {
  ns <- NS(id)
  dimensionId <- ns("dimension")

  tagList(
    tags$head(tags$script(sprintf("
      var dimensionId = '%s';
      var dimension = [0, 0];

      $(document).on('shiny:connected', function(e) {
        dimension[0] = window.innerWidth;
        dimension[1] = window.innerHeight;
        Shiny.onInputChange(dimensionId, dimension);
      });

      $(window).resize(function(e) {
        dimension[0] = window.innerWidth;
        dimension[1] = window.innerHeight;
        Shiny.onInputChange(dimensionId, dimension);
      });
    ", dimensionId))),

    fluidRow(
      column(
        width=2,
        p("")
      ),
      column(
        width=8, align="center",
        br(),br(),br(),
        p(glue::glue(
          "Det ble opprettet en egen covid-19 (mistenkt eller bekreftet) ICPC-2 diagnosekode (R991) 06.03.2020 ",
          "som legene kan bruke ved konsultasjoner der koronavirus er mistenkt eller bekreftet. ",
          "Diagnosene på legekontor og legevakt blir satt på bakgrunn av kliniske tegn hos pasienten ",
          "og sykehistorie, de er som regel ikke laboratorieverifisert. ",
          "<br>De kliniske tegnene på Covid-19 er akutt luftveisinfeksjon med symptomer som feber, ",
          "hoste og kortpustethet. Det er sesong for vanlig forkjølelse og influensa som også ",
          "kan gi slike symptomer og vi har testet mange med luftveisinfeksjoner den siste tiden, ",
          "og ser at < 5 % har fått påvist Covid-19. ",
          "<br>Det er derfor viktig å påpeke at Covid-19 ",
          "diagnosen i denne sammenheng ikke nødvendigvis er koronavirus, ",
          "men overvåkningen den girr en oversikt over hvor stort press det er på primærhelsetjenesten. ",
          "<br>Geografisk område basert på stedet for legekonsultasjon, ikke pasientens bosted."
        )),
        br()
      ),
      column(
        width=2,
        p("")
      )
    ),
    fluidRow(
      br(),br(),br(),br()
    ),
    fluidRow(
      column(
        width=12, align="center",
        plotOutput(ns("overview_plot_national_syndromes_proportion"), height = "700px")
      )
    ),
    fluidRow(
      br(),br(),br(),br(),
      br(),br(),br(),br()
    ),
    fluidRow(
      column(
        width=10, align="center",
        plotOutput(ns("overview_plot_national_age_proportion"), height = "700px")
      ),
      column(
        width=2,
        p("30-64 year olds, followed by 20-29 year olds have the highest proportion of consultations due to COVID-19 (R991). While these numbers will likely change in the future (as delayed data is received), they are still representative right now as the numerator and denominator have equal delays.")
      )
    ),
    fluidRow(
      br(),br(),br(),br(),
      br(),br(),br(),br()
    ),
    fluidRow(
      column(
        width=10, align="center",
        plotOutput(ns("overview_plot_county_proportion"), height = "900px")
      ),
      column(
        width=2,
        p("The proportion of consultations due to COVID-19 (R991) has different trends/patterns depending on the county. While these numbers will likely change in the future (as delayed data is received), they are still representative right now as the numerator and denominator have equal delays.")
      )
    ),
    fluidRow(
      br(),br(),br(),br(),
      br(),br(),br(),br()
    ),
    fluidRow(
      column(
        width=10, align="center",
        plotOutput(ns("overview_map_municip_proportion"), height = "900px")
      ),
      column(
        width=2,
        p("The proportion of consultations due to COVID-19 (R991) has different trends/patterns depending on the county. While these numbers will likely change in the future (as delayed data is received), they are still representative right now as the numerator and denominator have equal delays.")
      )
    ),
    fluidRow(
      br(),br(),br(),br(),
      br(),br(),br(),br()
    )
  )
}

covid19_server <- function(input, output, session, config) {
  #width <-  as.numeric(input$dimension[1])

  output$overview_plot_national_syndromes_proportion <- renderCachedPlot({
    pd <- pool %>% dplyr::tbl("data_norsyss") %>%
      dplyr::filter(tag_outcome %in% c(
        "covid19_lte",
        "engstelig_luftveissykdom_ika_lte",
        "influensa_lte",
        "rxx_for_covid19_lte",
        "akkut_ovre_luftveisinfeksjon_lte"
      )) %>%
      dplyr::filter(date >= !!config$start_date) %>%
      dplyr::filter(age >= "Totalt") %>%
      dplyr::filter(location_code >= "norge") %>%
      dplyr::collect()
    setDT(pd)

    pd[, andel := 100*n/consult_with_influenza]
    pd[, name_outcome := factor(
      tag_outcome,
      levels = c(
        "covid19_lte",
        "engstelig_luftveissykdom_ika_lte",
        "influensa_lte",
        "akkut_ovre_luftveisinfeksjon_lte",
        "rxx_for_covid19_lte"
      ),
      labels = c(
        "COVID-19 liknenede symptomer (R991)",
        "Engstelig luftveissykdom IKA (R27)",
        "Influensa (R80)",
        "Akutt øvre luftveisinfeksjon (R74)",
        "Luftvei diagnosekoder (samlet*)"
      )
    )]

    labels <- pd[date == max(date)]

    q <- ggplot(pd, aes(x=date, y=andel, color=name_outcome))
    q <- q + geom_line(size=12)
    q <- q + ggrepel::geom_label_repel(
      data = labels,
      mapping = aes(label = name_outcome),
      nudge_y = 0.0,
      nudge_x = 0.5,
      direction = "y",
      angle = 0,
      vjust = 0,
      hjust = 0,
      label.r=0,
      segment.size = 4,
      label.size = 4,
      label.padding = 2,
      box.padding = 2,
      size=30
    )
    q <- q + scale_y_continuous(
      "Andel",
      breaks = fhiplot::pretty_breaks(6),
      expand = expand_scale(mult = c(0, 0.1)),
      labels = format_nor_perc
    )
    q <- q + expand_limits(y = 0)
    q <- q + scale_x_date(
      "Dato",
      date_breaks = "2 days",
      date_labels = "%d.%m",
      expand = expand_scale(mult = c(0.02, 0.65))
    )
    q <- q + fhiplot::scale_color_fhi("Syndrome", guide = "none")
    q <- q + fhiplot::theme_fhi_lines(80, panel_on_top = F)
    q <- q + theme(legend.key.size = unit(2, "cm"))
    q <- q + labs(title="Andel konsultasjoner i Norge")
    q <- q + labs(caption=glue::glue(
      "Nevneren er totalt antall konsultasjoner\n",
      "*R26, R71, R73, R80, R84, R85, R86, R87, R88, R89, R89, R90, R92, R95 og R96"
    ))
    q
  }, cacheKeyExpr={list(
    lubridate::today(),
    dev_invalidate_cache
  )},
    res = 24
  )

  output$overview_plot_national_age_proportion <- renderCachedPlot({
    pd <- pool %>% dplyr::tbl("data_norsyss") %>%
      dplyr::filter(date >= !!config$start_date) %>%
      dplyr::filter(tag_outcome == "covid19_lte") %>%
      dplyr::filter(location_code=="norge") %>%
      dplyr::collect()
    setDT(pd)

    pd[,age:=factor(
      age,
      levels = c(
        "Totalt",
        "0-4",
        "5-14",
        "15-19",
        "20-29",
        "30-64",
        "65+"
      )
    )]
    pd <- pd[,c("date","age","n","consult_with_influenza"),with=F]
    pd_totalt <- pd[age=="Totalt",.(date,consult_with_influenza_totalt=consult_with_influenza)]
    pd[
      pd_totalt,
      on="date",
      consult_with_influenza_totalt := consult_with_influenza_totalt
      ]

    labels <- pd[date == max(date)]

    max_date_uncertain <- max(pd$date)
    min_date_uncertain <- max_date_uncertain-6
    q <- ggplot(pd, aes(x=date,y=100*n/consult_with_influenza_totalt, color=age, group=age))
    q <- q + geom_line(size=12)
    q <- q + ggrepel::geom_label_repel(
      data = labels,
      mapping = aes(label = age),
      nudge_y = 0.0,
      nudge_x = 0.5,
      direction = "y",
      angle = 0,
      vjust = 0,
      hjust = 0,
      label.r=0,
      segment.size = 4,
      label.size = 4,
      label.padding = 2,
      box.padding = 2,
      size=30
    )
    q <- q + scale_y_continuous(
      "Andel",
      breaks = fhiplot::pretty_breaks(6),
      expand = expand_scale(mult = c(0, 0.1)),
      labels = format_nor_perc
    )
    q <- q + expand_limits(y = 0)
    q <- q + scale_x_date(
      "Dato",
      date_breaks = "2 days",
      date_labels = "%d.%m",
      expand = expand_scale(mult = c(0.02, 0.15))
    )
    q <- q + fhiplot::scale_color_fhi("Aldersgruppe", guide = "none")
    q <- q + fhiplot::theme_fhi_lines(80, panel_on_top = F)
    q <- q + theme(legend.key.size = unit(2, "cm"))
    q <- q + labs(title="Andel konsultasjoner i Norge som tilhører COVID-19 (R991) etter aldersgrupper")
    q <- q + labs(caption="Konsultasjoner er legekontakt, telefon, ekonsultasjoner til fastleger og legevakter\nNevneren til alle aldersgrupper er totalt antall konsultasjoner (alle aldersgrupper summert)")
    q
  }, cacheKeyExpr={list(
    lubridate::today(),
    dev_invalidate_cache
  )},
    res = 24
  )

  output$overview_plot_county_proportion <- renderCachedPlot({
    pd <- pool %>% dplyr::tbl("data_norsyss") %>%
      dplyr::filter(tag_outcome %in% c(
        "covid19_lte",
        "engstelig_luftveissykdom_ika_lte"
      )) %>%
      dplyr::filter(date >= !!config$start_date) %>%
      dplyr::filter(age >= "Totalt") %>%
      dplyr::filter(granularity_geo=="county") %>%
      dplyr::collect()
    setDT(pd)

    pd[
      fhidata::norway_locations_b2020,
      on="location_code==county_code",
      location_name:=county_name
      ]

    pd[, andel := 100*n/consult_with_influenza]
    pd[, name_outcome := factor(
      tag_outcome,
      levels = c(
        "covid19_lte",
        "engstelig_luftveissykdom_ika_lte"
      ),
      labels = c(
        "COVID-19 liknenede symptomer (R991)",
        "Engstelig luftveissykdom IKA (R27)"
      )
    )]

    labels <- pd[date == max(date)]
    labels[tag_outcome=="covid19_lte",lab := paste0("R991: ",format_nor_perc(andel))]
    labels[tag_outcome=="engstelig_luftveissykdom_ika_lte",lab := paste0("R27: ",format_nor_perc(andel))]

    labels[tag_outcome=="covid19_lte",lab := paste0("R991: ",format_nor_perc(andel))]
    labels[tag_outcome=="engstelig_luftveissykdom_ika_lte",date := min(pd$date)+2]

    max_val <- max(pd$andel)

    q <- ggplot(pd, aes(x=date, y=andel, color=name_outcome))
    q <- q + geom_line(size=8)
    q <- q + ggrepel::geom_label_repel(
      data = labels,
      mapping = aes(label = lab, y=max_val),
      nudge_y = 0.0,
      nudge_x = 0.0,
      direction = "x",
      angle = 0,
      vjust = 0,
      hjust = 0,
      label.r=0,
      segment.size = 0,
      label.size = 4,
      label.padding = 2,
      box.padding = 2,
      size=20
    )
    q <- q + lemon::facet_rep_wrap(~location_name, repeat.tick.labels = "y", ncol=4)
    q <- q + scale_y_continuous(
      "Andel",
      breaks = fhiplot::pretty_breaks(6),
      expand = expand_scale(mult = c(0, 0.1)),
      labels = format_nor_perc
    )
    q <- q + expand_limits(y = 0)
    q <- q + scale_x_date(
      NULL,
      date_breaks = "4 days",
      date_labels = "%d.%m",
      expand = expand_scale(mult = c(0.02, 0.02))
    )
    q <- q + fhiplot::scale_color_fhi(NULL)
    q <- q + fhiplot::theme_fhi_lines(80, panel_on_top = F)
    q <- q + theme(legend.key.size = unit(2, "cm"))
    q <- q + theme(legend.position="bottom")
    q <- q + labs(title="Andel konsultasjoner i Norge som tilhører COVID-19 liknenede symptomer (R991)")
    q <- q + labs(caption=glue::glue(
      "Nevneren er totalt antall konsultasjoner"
    ))
    q
  }, cacheKeyExpr={list(
    lubridate::today(),
    dev_invalidate_cache
  )},
  res = 24
  )

  output$overview_map_municip_proportion <- renderCachedPlot({
    d <- pool %>% dplyr::tbl("data_norsyss") %>%
      dplyr::filter(tag_outcome %in% c(
        "covid19_lte",
        "engstelig_luftveissykdom_ika_lte"
      )) %>%
      dplyr::filter(date == !!config$max_date_uncertain) %>%
      dplyr::filter(granularity_geo == "municip") %>%
      dplyr::filter(age == "Totalt") %>%
      dplyr::collect()
    setDT(d)
    d[,andel := 100*n/consult_with_influenza]
    d[is.nan(andel), andel := NA]
    d[, category := fancycut::fancycut(
      andel,
      na.bucket = "0%/ingen konsultasjoner",
      `0%/ingen konsultasjoner` = 0,
      `1-10%`="(0,10]",
      `11-20%`="(10,20]",
      `21-30%`="(20,30]",
      `31-50%`="(30,50]",
      `51%+`="(50,100]",
    )]

    sum(d$consult_with_influenza==0)
    xtabs(~d$category, addNA=T)

    d[, name_outcome := factor(
      tag_outcome,
      levels = c(
        "covid19_lte",
        "engstelig_luftveissykdom_ika_lte"
      ),
      labels = c(
        "COVID-19 liknenede symptomer (R991)",
        "Engstelig luftveissykdom IKA (R27)"
      )
    )]

    pd <- merge(
      fhidata::norway_map_municips_with_insert_b2020,
      d,
      on="location_code",
      allow.cartesian = TRUE
    )

    q <- ggplot()
    q <- q + geom_polygon(
      data = pd,
      aes( x = long, y = lat, group = group, fill=category),
      color="black",
      size=0.2
    )
    q <- q + annotate(
      "text",
      x = fhidata::norway_map_insert_title_position_b2020$long,
      y = fhidata::norway_map_insert_title_position_b2020$lat,
      label = "Oslo",
      size = 20
    )
    q <- q + lemon::facet_rep_wrap(~name_outcome, repeat.tick.labels = "y", ncol=4)
    q <- q + theme_void(80)
    q <- q + theme(legend.key.size = unit(4, "cm"))
    q <- q + coord_quickmap()
    q <- q + fhiplot::scale_fill_fhi("Andel konsultasjoner",palette = "map_seq_missing", direction = -1, drop=F)
    q <- q + labs(title = glue::glue("Andel konsultasjoner på {format(config$max_date_uncertain,'%d.%m.%Y')}\n\n"))
    q
  }, cacheKeyExpr={list(
    lubridate::today(),
    dev_invalidate_cache
  )},
  res = 24
  )

}
