covid19_ui <- function(id, config) {
  ns <- NS(id)
  dimensionId <- ns("dimension")

  tagList(
    fluidRow(
      column(
        width=12, align="left",
        p(
          id="toptext",
          "Det ble opprettet en egen ", strong("COVID-19 (mistenkt eller bekreftet) ICPC-2 diagnosekode (R991)"), "06.03.2020", br(),
          "som legene kan bruke ved konsultasjoner der koronavirus er mistenkt eller bekreftet.", br(), br(),
          "Direktoratet for e-helse og legeforeningen har i tillegg anbefalt legene å bruke diagnosekoden", br(),
          strong("Engstelig for sykdoms i luftveien IKA (R27)"), "ved sykmelding/konsultasjon/kontakt vedrørende COVID-19,",br(),
          "med unntak av bekreftet/mistenkt koronavirus-sykdom."
        )
      )
    ),

    tabsetPanel(
      tabPanel(
        title="Oversikt",
        tagList(
          fluidRow(
            column(
              width=2,
              p("")
            ),
            column(
              width=8, align="center",

              p("text text text text text text text text text text text text "),
              p("text text text text text text text text text text text text "),
              p("text text text text text text text text text text text text "),
              p("text text text text text text text text text text text text "),
              p("text text text text text text text text text text text text ")
            ),
            column(
              width=2,
              p("")
            )
          ),

          fluidRow(
            column(
              width=12, align="center",

              selectizeInput(
                inputId = ns("covid_location_code"),
                label = "Geografisk område",
                choices = config$choices_location,
                selected = "norge",
                multiple = FALSE,
                options = NULL,
                width = "400px"
              )

            )
          ),

          fluidRow(
            column(
              width=2,
              p("")
            ),
            column(
              width=8, align="center",

              p("text text text text text text text text text text text text "),
              p("text text text text text text text text text text text text "),
              p("text text text text text text text text text text text text "),
              p("text text text text text text text text text text text text "),
              p("text text text text text text text text text text text text ")
            ),
            column(
              width=2,
              p("")
            )
          ),

          fluidRow(
            column(
              width=12, align="center",
              plotOutput(ns("overview_plot_national_syndromes_proportion"), height = "700px")
            )
          ),

          fluidRow(
            column(
              width=2,
              p("")
            ),
            column(
              width=8, align="center",

              p("text text text text text text text text text text text text "),
              p("text text text text text text text text text text text text "),
              p("text text text text text text text text text text text text "),
              p("text text text text text text text text text text text text "),
              p("text text text text text text text text text text text text ")
            ),
            column(
              width=2,
              p("")
            )
          ),

          fluidRow(
            column(
              width=12, align="center",
              plotOutput(ns("overview_plot_national_source_proportion"), height = "700px")
            )
          ),

          fluidRow(
            column(
              width=2,
              p("")
            ),
            column(
              width=8, align="center",

              p("text text text text text text text text text text text text "),
              p("text text text text text text text text text text text text "),
              p("text text text text text text text text text text text text "),
              p("text text text text text text text text text text text text "),
              p("text text text text text text text text text text text text ")
            ),
            column(
              width=2,
              p("")
            )
          ),

          fluidRow(
            column(
              width=12, align="center",
              plotOutput(ns("overview_plot_national_age_burden"), height = "700px")
            )
          ),

          fluidRow(
            column(
              width=2,
              p("")
            ),
            column(
              width=8, align="center",

              p("text text text text text text text text text text text text "),
              p("text text text text text text text text text text text text "),
              p("text text text text text text text text text text text text "),
              p("text text text text text text text text text text text text "),
              p("text text text text text text text text text text text text ")
            ),
            column(
              width=2,
              p("")
            )
          ),

          fluidRow(
            column(
              width=12, align="center",
              plotOutput(ns("overview_plot_national_age_trends"), height = "700px")
            )
          ),

          fluidRow(
            column(
              width=2,
              p("")
            ),
            column(
              width=8, align="center",

              p("text text text text text text text text text text text text "),
              p("text text text text text text text text text text text text "),
              p("text text text text text text text text text text text text "),
              p("text text text text text text text text text text text text "),
              p("text text text text text text text text text text text text ")
            ),
            column(
              width=2,
              p("")
            )
          ),

          fluidRow(
            column(
              width=12, align="center",
              uiOutput(ns("overview_ui_county_proportion"))
              #plotOutput(ns("overview_plot_county_proportion"), height = "900px")
            )
          ),

          fluidRow(
            column(
              width=2,
              p("")
            ),
            column(
              width=8, align="center",

              p("text text text text text text text text text text text text "),
              p("text text text text text text text text text text text text "),
              p("text text text text text text text text text text text text "),
              p("text text text text text text text text text text text text "),
              p("text text text text text text text text text text text text ")
            ),
            column(
              width=2,
              p("")
            )
          ),

          fluidRow(
            column(
              width=12, align="center",
              plotOutput(ns("overview_map_county_proportion"), height = "600px"),
              br(),br(),br()
            )
          )
        )
      ),
      tabPanel(
        title="Formålet",
        tagList(
          fluidRow(
            column(
              width=2,
              p("")
            ),
            column(
              width=8, align="center",

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
              ))
            ),
            column(
              width=2,
              p("")
            )
          )
        )
      )
    )
  )
}

covid19_server <- function(input, output, session, config) {
  #width <-  as.numeric(input$dimension[1])

  output$overview_plot_national_syndromes_proportion <- renderCachedPlot({
    req(input$covid_location_code)

    covid19_overview_plot_national_syndromes_proportion(
      location_code = input$covid_location_code,
      config = config
    )
  }, cacheKeyExpr={list(
    input$covid_location_code,
    dev_invalidate_cache
  )},
  res = 72
  )

  output$overview_plot_national_source_proportion <- renderCachedPlot({
    req(input$covid_location_code)

    covid19_overview_plot_national_source_proportion(
      location_code = input$covid_location_code,
      config = config
    )
  }, cacheKeyExpr={list(
    input$covid_location_code,
    dev_invalidate_cache
  )},
  res = 72
  )

  output$overview_plot_national_age_burden <- renderCachedPlot({
    req(input$covid_location_code)

    covid19_overview_plot_national_age_burden(
      location_code = input$covid_location_code,
      config = config
    )
  }, cacheKeyExpr={list(
    input$covid_location_code,
    dev_invalidate_cache
  )},
  res = 72
  )

  output$overview_plot_national_age_trends <- renderCachedPlot({
    req(input$covid_location_code)

    covid19_overview_plot_national_age_trends(
      location_code = input$covid_location_code,
      config = config
    )
  }, cacheKeyExpr={list(
    input$covid_location_code,
    dev_invalidate_cache
  )},
  res = 72
  )

  output$overview_plot_county_proportion <- renderCachedPlot({
    req(input$covid_location_code)

    covid19_overview_plot_county_proportion(
      location_code = input$covid_location_code,
      config = config
    )
  }, cacheKeyExpr={list(
    input$covid_location_code,
    dev_invalidate_cache
  )},
  res = 72
  )

  output$overview_ui_county_proportion <- renderUI({
    ns <- session$ns
    req(input$covid_location_code)

    location_codes <- get_dependent_location_codes(location_code = input$covid_location_code)
    height <- round(250*ceiling(length(location_codes)/4))
    height <- max(400, height)
    height <- paste0(height,"px")
    plotOutput(ns("overview_plot_county_proportion"), height = height)
  })

  output$overview_map_county_proportion <- renderCachedPlot({

    covid19_overview_map_county_proportion(
      location_code = input$covid_location_code,
      config = config
    )
  }, cacheKeyExpr={list(
    input$covid_location_code,
    dev_invalidate_cache
  )},
  res = 72
  )

  outputOptions(output, "overview_plot_national_syndromes_proportion", priority = 10)
  outputOptions(output, "overview_plot_national_age_burden", priority = 9)
  outputOptions(output, "overview_plot_county_proportion", priority = 8)
  outputOptions(output, "overview_map_county_proportion", priority = 7)

}


covid19_overview_plot_national_syndromes_proportion <- function(
  location_code,
  config
){

    pd <- pool %>% dplyr::tbl("data_norsyss") %>%
      dplyr::filter(tag_outcome %in% c(
        "covid19_lf_lte",
        "engstelig_luftveissykdom_ika_lf_lte",
        "influensa_lf_lte",
        "rxx_for_covid19_lf_lte",
        "akkut_ovre_luftveisinfeksjon_lf_lte"
      )) %>%
      dplyr::filter(date >= !!config$start_date) %>%
      dplyr::filter(age == "Totalt") %>%
      dplyr::filter(location_code == !!location_code) %>%
      dplyr::select(tag_outcome, date, n, consult_with_influenza) %>%
      dplyr::collect()
    setDT(pd)
    pd[, date:= as.Date(date)]

    pd[, andel := 100*n/consult_with_influenza]
    pd[, no_data := consult_with_influenza==0]
    pd[is.nan(andel), andel := 0]

    pd[, name_outcome := factor(
      tag_outcome,
      levels = c(
        "covid19_lf_lte",
        "engstelig_luftveissykdom_ika_lf_lte",
        "influensa_lf_lte",
        "akkut_ovre_luftveisinfeksjon_lf_lte",
        "rxx_for_covid19_lf_lte"
      ),
      labels = c(
        "COVID-19 (mistenkt\neller bekreftet) (R991)",
        "Engstelig luftveissykdom\nIKA (R27)",
        "Influensa (R80)",
        "Akutt øvre\nluftveisinfeksjon (R74)",
        "Luftvei diagnosekoder\n(samlet*)"
      )
    )]

    labels <- pd[date == max(date)]
    #labels[,]

    q <- ggplot(pd, aes(x=date, y=andel, color=name_outcome))
    q <- q + geom_line(size=4)
    if(sum(pd$no_data)>0){
      q <- q + geom_vline(data=pd[no_data==TRUE], mapping=aes(xintercept = date),color= "red", lty=3, lwd=3)
    }
    q <- q + ggrepel::geom_label_repel(
      data = labels,
      mapping = aes(label = name_outcome),
      nudge_y = 1,
      nudge_x = 0.5,
      direction = "y",
      angle = 0,
      vjust = 0,
      hjust = 0,
      label.r=0,
      segment.size = 1,
      label.size = 1,
      label.padding = 1,
      box.padding = 1,
      size=8
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
      expand = expand_scale(mult = c(0.02, 0.5))
    )
    q <- q + fhiplot::scale_color_fhi("Syndrome", guide = "none")
    q <- q + fhiplot::theme_fhi_lines(20, panel_on_top = F)
    q <- q + theme(legend.key.size = unit(1, "cm"))
    q <- q + labs(title = glue::glue(
      "{names(config$choices_location)[config$choices_location==location_code]}\n",
      "Andel konsultasjoner"
    ))
    q <- q + labs(caption=glue::glue(
      "Nevneren er totalt antall konsultasjoner\n",
      "*R26, R71, R73, R80, R84, R85, R86, R87, R88, R89, R89, R90, R92, R95 og R96\n",
      "Røde stiplede vertikale linjer på grafen betyr at ingen data ble rapportert på disse dagene"
    ))
    q
}

covid19_overview_plot_national_source_proportion <- function(
  location_code,
  config
){

    pd <- pool %>% dplyr::tbl("data_norsyss") %>%
      dplyr::filter(tag_outcome %in% c(
        "covid19_f_l",
        "covid19_f_t",
        "covid19_f_e",
        "covid19_l_l",
        "covid19_l_t",
        "covid19_l_e"
      )) %>%
      dplyr::filter(date >= !!config$start_date) %>%
      dplyr::filter(age >= "Totalt") %>%
      dplyr::filter(location_code == !!location_code) %>%
      dplyr::select(tag_outcome, date, n, consult_with_influenza) %>%
      dplyr::collect()
    setDT(pd)
    pd[, date:= as.Date(date)]

    pd[,
       contact_type := dplyr::case_when(
         stringr::str_detect(tag_outcome, "_e$") ~ "Ekonsultasjon",
         stringr::str_detect(tag_outcome, "_t$") ~ "Telefon",
         stringr::str_detect(tag_outcome, "_l$") ~ "Lege"
       )]

    pd[,
       practice_type := dplyr::case_when(
         stringr::str_detect(tag_outcome, "_f_") ~ "Fastlege",
         stringr::str_detect(tag_outcome, "_l_") ~ "Legevakt"
       )]
    pd[, cat:= paste0(contact_type,"/",practice_type)]
    pd[,cat := factor(
      cat,
      levels = c(
        "Ekonsultasjon/Fastlege",
        "Ekonsultasjon/Legevakt",
        "Telefon/Fastlege",
        "Telefon/Legevakt",
        "Lege/Fastlege",
        "Lege/Legevakt"
      )
    )]

    weekends <- unique(pd$date)
    weekends <- weekends[lubridate::wday(weekends, week_start = 1) %in% c(6,7)]
    weekends <- data.frame(date = weekends)

    max_y <- max(pd[,.(n=sum(n)),by=.(date)]$n, na.rm=T)
    min_y_start <- -0.085*max_y*1.01
    min_y_end <- -0.05*max_y*1.01

    pd_line <- pd[,.(
      n=sum(n),
      consult_with_influenza=sum(consult_with_influenza)
    ),
    by=.(date)
    ]
    pd_line[, andel := n/consult_with_influenza]
    max_andel <- max(pd_line$andel, na.rm=T)
    pd_line[, andel := max_y * andel / max_andel]

    q <- ggplot(pd, aes(x=date, y=n))
    q <- q + geom_col(mapping=aes(fill=cat))
    q <- q + geom_line(data=pd_line, mapping=aes(y=andel),color="red", size = 3)
    q <- q + geom_segment(
      data = weekends,
      mapping = aes(
        x = date, xend=date,
        y = min_y_start, yend = min_y_end
      ),
      color = "red",
      size = 1,
      arrow = arrow(length = unit(0.1, "inches"))
    )
    q <- q + fhiplot::scale_fill_fhi(NULL)
    q <- q + scale_y_continuous(
      "Antall",
      breaks = fhiplot::pretty_breaks(6),
      labels = fhiplot::format_nor,
      sec.axis = sec_axis(
        ~ . * 100 / max_y * max_andel,
        breaks = fhiplot::pretty_breaks(6),
        labels = format_nor_perc,
        name = "Andel"
      )
    )
    q <- q + scale_x_date(
      "Dato",
      date_breaks = "2 days",
      date_labels = "%d.%m"
    )
    q <- q + fhiplot::scale_color_fhi("Syndrome", guide = "none")
    q <- q + fhiplot::theme_fhi_lines(20, panel_on_top = T)
    q <- q + theme(legend.key.size = unit(1, "cm"))
    q <- q + coord_cartesian(ylim=c(0, max_y), clip="off", expand = F)
    q <- q + labs(title = glue::glue(
      "{names(config$choices_location)[config$choices_location==location_code]}\n",
      "COVID-19 (mistenkt eller bekreftet) (R991) konsultasjoner etter kilde"
    ))
    q <- q + labs(caption=glue::glue(
      "Røde piler viser helgen\n",
      "Kolonnene tilhører høyre-aksen, den røde linjen tilhører venstre-aksen\n",
      "Nevneren til andelen er totalt antall konsultasjoner"
    ))
    q
}

covid19_overview_plot_national_age_burden <- function(
  location_code,
  config
){

  pd <- pool %>% dplyr::tbl("data_norsyss") %>%
    dplyr::filter(date >= !!config$start_date) %>%
    dplyr::filter(tag_outcome == "covid19_lf_lte") %>%
    dplyr::filter(location_code== !!location_code) %>%
    dplyr::filter(age != "Totalt") %>%
    dplyr::select(date, age, n, consult_with_influenza) %>%
    dplyr::collect()
  setDT(pd)
  pd[, date:= as.Date(date)]

  pd[,age:=factor(
    age,
    levels = c(
      "0-4",
      "5-14",
      "15-19",
      "20-29",
      "30-64",
      "65+"
    )
  )]
  pd[, consult_with_influenza_totalt := sum(consult_with_influenza), by=.(date)]

  pd[, andel := 100*n/consult_with_influenza_totalt]
  pd[, no_data := consult_with_influenza_totalt==0]
  pd[is.nan(andel), andel := 0]

  labels <- pd[date == max(date)]

  max_date_uncertain <- max(pd$date)
  min_date_uncertain <- max_date_uncertain-6
  q <- ggplot(pd, aes(x=date,y=andel))
  q <- q + geom_col(mapping=aes(fill=age))
  if(sum(pd$no_data)>0){
    q <- q + geom_vline(data=pd[no_data==TRUE], mapping=aes(xintercept = date),color= "red", lty=3, lwd=3)
  }
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
    date_labels = "%d.%m"
  )
  q <- q + fhiplot::scale_fill_fhi("Aldersgruppe")
  q <- q + fhiplot::theme_fhi_lines(20, panel_on_top = T)
  q <- q + theme(legend.key.size = unit(1, "cm"))
  q <- q + labs(title = glue::glue(
    "{names(config$choices_location)[config$choices_location==location_code]}\n",
    "Andel konsultasjoner som tilhører COVID-19 (mistenkt eller bekreftet) (R991)"
  ))
  q <- q + labs(caption=glue::glue(
    "Konsultasjoner er legekontakt, telefon, ekonsultasjoner til fastleger og legevakter\n",
    "Nevneren til alle aldersgrupper er totalt antall konsultasjoner (alle aldersgrupper summert)\n",
    "Røde stiplede vertikale linjer på grafen betyr at ingen data ble rapportert på disse dagene"
  ))
  q
}

covid19_overview_plot_national_age_trends <- function(
  location_code,
  config
){

  pd <- pool %>% dplyr::tbl("data_norsyss") %>%
    dplyr::filter(date >= !!config$start_date) %>%
    dplyr::filter(tag_outcome == "covid19_lf_lte") %>%
    dplyr::filter(location_code== !!location_code) %>%
    dplyr::select(date, age, n, consult_with_influenza) %>%
    dplyr::collect()
  setDT(pd)
  pd[, date:= as.Date(date)]

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

  pd[, andel := 100*n/consult_with_influenza]

  pd[, consult_with_influenza_totalt := sum(consult_with_influenza), by=.(date)]
  pd[, no_data := consult_with_influenza_totalt==0]
  pd[is.nan(andel), andel := 0]

  labels <- pd[date == max(date)]

  max_date_uncertain <- max(pd$date)
  min_date_uncertain <- max_date_uncertain-6
  q <- ggplot(pd, aes(x=date,y=andel))
  q <- q + geom_col()
  if(sum(pd$no_data)>0){
    q <- q + geom_vline(data=pd[no_data==TRUE], mapping=aes(xintercept = date),color= "red", lty=3, lwd=3)
  }
  q <- q + scale_y_continuous(
    "Andel",
    breaks = fhiplot::pretty_breaks(5),
    expand = expand_scale(mult = c(0, 0.1)),
    labels = format_nor_perc
  )
  q <- q + expand_limits(y = 0)
  q <- q + scale_x_date(
    "Dato",
    date_breaks = "4 days",
    date_labels = "%d.%m"
  )
  q <- q + lemon::facet_rep_wrap(~age, repeat.tick.labels = "all", ncol=3)
  q <- q + fhiplot::theme_fhi_lines(20, panel_on_top = T)
  q <- q + theme(legend.key.size = unit(1, "cm"))
  q <- q + labs(title = glue::glue(
    "{names(config$choices_location)[config$choices_location==location_code]}\n",
    "Andel konsultasjoner som tilhører COVID-19 (mistenkt eller bekreftet) (R991)"
  ))
  q <- q + labs(caption=glue::glue(
    "Konsultasjoner er legekontakt, telefon, ekonsultasjoner til fastleger og legevakter\n",
    "Nevneren er beregnet innen hver aldersgruppe\n",
    "Røde stiplede vertikale linjer på grafen betyr at ingen data ble rapportert på disse dagene"
  ))
  q
}

covid19_overview_plot_county_proportion <- function(
  location_code,
  config
){

    location_codes <- get_dependent_location_codes(location_code = location_code)

    pd <- pool %>% dplyr::tbl("data_norsyss") %>%
      dplyr::filter(tag_outcome %in% c(
        "covid19_lf_lte",
        "engstelig_luftveissykdom_ika_lf_lte"
      )) %>%
      dplyr::filter(date >= !!config$start_date) %>%
      dplyr::filter(age >= "Totalt") %>%
      dplyr::filter(location_code %in% !!location_codes) %>%
      dplyr::select(tag_outcome, location_code, date, n, consult_with_influenza) %>%
      dplyr::collect()
    setDT(pd)
    pd[, date:= as.Date(date)]

    pd[
      fhidata::norway_locations_long_b2020,
      on="location_code",
      location_name:=location_name
      ]

    pd[, andel := 100*n/consult_with_influenza]
    pd[, name_outcome := factor(
      tag_outcome,
      levels = c(
        "covid19_lf_lte",
        "engstelig_luftveissykdom_ika_lf_lte"
      ),
      labels = c(
        "COVID-19 (mistenkt eller bekreftet) (R991)",
        "Engstelig luftveissykdom IKA (R27)"
      )
    )]

    pd[,location_code := factor(location_code, levels = location_codes)]
    setorder(pd,location_code)
    location_names <- unique(pd$location_name)
    pd[,location_name := factor(location_name, levels = location_names)]

    max_val <- max(pd$andel,na.rm=T)
    labels <- pd[date == max(date)]
    labels[tag_outcome=="covid19_lf_lte",lab := paste0("R991: ",format_nor_perc(andel))]
    labels[tag_outcome=="engstelig_luftveissykdom_ika_lf_lte",lab := paste0("R27: ",format_nor_perc(andel))]

    labels[tag_outcome=="covid19_lf_lte",lab := paste0("R991: ",format_nor_perc(andel))]


    labels[tag_outcome=="covid19_lf_lte",andel := max_val]
    labels[tag_outcome=="engstelig_luftveissykdom_ika_lf_lte",andel := max_val-5]

    q <- ggplot(pd, aes(x=date, y=andel))
    q <- q + geom_col(mapping = aes(fill=name_outcome), position = "dodge", width=1)
    q <- q + ggrepel::geom_label_repel(
      data = labels,
      mapping = aes(label = lab, y=andel, color=name_outcome),
      nudge_y = 0.0,
      nudge_x = 0.0,
      direction = "y",
      angle = 0,
      vjust = 0,
      hjust = 0,
      label.r=0,
      segment.size = 0,
      label.size = 0.5,
      label.padding = 0.5,
      box.padding = 0.25,
      size=4
    )
    q <- q + lemon::facet_rep_wrap(~location_name, repeat.tick.labels = "y", ncol=3)
    q <- q + scale_y_continuous(
      "Andel",
      breaks = fhiplot::pretty_breaks(5),
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
    q <- q + fhiplot::scale_fill_fhi(NULL)
    q <- q + fhiplot::scale_color_fhi(NULL, guide="none")
    q <- q + fhiplot::theme_fhi_lines(20, panel_on_top = F)
    q <- q + theme(legend.key.size = unit(1, "cm"))
    q <- q + theme(legend.position="bottom")
    q <- q + labs(title="Andel konsultasjoner etter geografiske område")
    q <- q + labs(caption=glue::glue(
      "Nevneren er totalt antall konsultasjoner"
    ))
    q
}

covid19_overview_map_county_proportion <- function(
  location_code,
  config
){

    granularity_geo <- get_granularity_geo(location_code = location_code)
    location_codes <- get_dependent_location_codes(location_code = location_code)

    if(granularity_geo == "nation"){
      d <- pool %>% dplyr::tbl("data_norsyss") %>%
        dplyr::filter(tag_outcome %in% c(
          "covid19_lf_lte",
          "engstelig_luftveissykdom_ika_lf_lte"
        )) %>%
        dplyr::filter(date >= !!config$start_date) %>%
        dplyr::filter(granularity_geo == "county") %>%
        dplyr::filter(age == "Totalt") %>%
        dplyr::collect()
    } else {
      d <- pool %>% dplyr::tbl("data_norsyss") %>%
        dplyr::filter(tag_outcome %in% c(
          "covid19_lf_lte",
          "engstelig_luftveissykdom_ika_lf_lte"
        )) %>%
        dplyr::filter(date >= !!config$start_date) %>%
        dplyr::filter(granularity_geo == "municip") %>%
        dplyr::filter(location_code %in% !!location_codes) %>%
        dplyr::filter(age == "Totalt") %>%
        dplyr::collect()
    }
    setDT(d)
    setorder(d,tag_outcome, location_code, date)
    d[,cum_n := cumsum(n), by=.(tag_outcome, location_code)]
    d <- d[date==max(date)]

    # summary(d$cum_n)
    max_cat <- paste0("2001-",max(5000,max(d$cum_n)))
    d[, category := fancycut::wafflecut(
      x = cum_n,
      intervals = c(
        "0",
        "(0,500]",
        "(500,1000]",
        "(1000,2000]",
        "(2000,10000000]"
      ),
      buckets = c(
        "0",
        "1-500",
        "501-1000",
        "1001-2000",
        max_cat
      )
    )]

    # xtabs(~d$category, addNA=T)

    d[, name_outcome := factor(
      tag_outcome,
      levels = c(
        "covid19_lf_lte",
        "engstelig_luftveissykdom_ika_lf_lte"
      ),
      labels = c(
        "COVID-19 (mistenkt eller bekreftet) (R991)",
        "Engstelig luftveissykdom IKA (R27)"
      )
    )]

    if(granularity_geo == "nation"){
      pd <- merge(
        fhidata::norway_map_counties_with_insert_b2020,
        d,
        on="location_code",
        allow.cartesian = TRUE
      )
    } else {
      pd <- merge(
        fhidata::norway_map_municips_b2020,
        d,
        on="location_code",
        allow.cartesian = TRUE
      )
    }

    q <- ggplot()
    q <- q + geom_polygon(
      data = pd,
      aes( x = long, y = lat, group = group, fill=category),
      color="black",
      size=0.2
    )
    if(granularity_geo == "nation"){
      q <- q + annotate(
        "text",
        x = fhidata::norway_map_insert_title_position_b2020$long,
        y = fhidata::norway_map_insert_title_position_b2020$lat,
        label = "Oslo",
        size = 8
      )
    }
    q <- q + lemon::facet_rep_wrap(~name_outcome, repeat.tick.labels = "y", ncol=4)
    q <- q + theme_void(20)
    q <- q + theme(legend.key.size = unit(1, "cm"))
    q <- q + coord_quickmap()
    q <- q + fhiplot::scale_fill_fhi("Kumulativt\nantall",palette = "map_seq_missing", direction = -1, drop=F)
    q <- q + labs(title = glue::glue("Kumulativt antall konsultasjoner f.o.m {format(config$start_date,'%d.%m.%Y')} t.o.m {format(config$max_date_uncertain,'%d.%m.%Y')}\n\n"))
    q
}
