covid19_ui <- function(id, config) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width=2,
        p("")
      ),
      column(
        width=8, align="center",
        br(),br(),br(),
        p("NorSySS (Norwegian Syndromic Surveillance System) er et overvåkningssystem basert på diagnosekoder (ICPC-2 koder) satt på legekontor og legevakter i hele Norge."),
        p("Formålet med NorSySS er å se trender og utbredelse av smittsomme sykdommer slik at utbrudd oppdages så tidlig som mulig. I tillegg kan overvåkningen brukes til å vurdere effekt av folkehelsetiltak."),
        p("Diagnosekoder som registreres hos lege eller legevakt sendes til Helsedirektoratet som en del av legenes refusjonskrav (KUHR-systemet). Folkehelseinstituttet mottar daglig oppdatert KUHR-data til Sykdomspulsen. Dataene er anonyme uten pasientidentifikasjon, men med informasjon om kjønn, aldersgruppe, konsultasjonsdato og sted for konsultasjon."),
        p("Geografisk område basert på stedet for legekonsultasjon, ikke pasientens bosted."),
        p("Fra 08.03.2020 begynte helsevesnet å bruke ICPC-2 koden R99.1 til COVID-19."),
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
        width=10, align="center",
        plotOutput(ns("overview_plot_national_syndromes_proportion"), height = "700px")
      ),
      column(
        width=2,
        p("On March 12th COVID-19 (R99.1) consultations were a larger proportion than Influenza (R80). While these numbers will likely change in the future (as delayed data is received), they are still representative right now as the numerator and denominator have equal delays.")
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
        p("30-64 year olds, followed by 20-29 year olds have the highest proportion of consultations due to COVID-19 (R99.1). While these numbers will likely change in the future (as delayed data is received), they are still representative right now as the numerator and denominator have equal delays.")
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
        p("The proportion of consultations due to COVID-19 (R99.1) has different trends/patterns depending on the county. While these numbers will likely change in the future (as delayed data is received), they are still representative right now as the numerator and denominator have equal delays.")
      )
    ),
    fluidRow(
      br(),
      br(),
      br()
    ),
    fluidRow(
      column(
        width=10, align="center",
        plotOutput(ns("overview_plot_national_age_numbers"), height = "700px")
      ),
      column(
        width=2,
        p("These numbers will change in the future (as delayed data is received) and should be interpreted with caution. These numbers are severely affected by reporting delays.")
      )
    ),
    fluidRow(
      br(),br(),br(),br(),
      br(),br(),br(),br()
    ),
    fluidRow(
      column(
        width=10, align="center",
        plotOutput(ns("overview_plot_county_numbers"), height = "700px")
      ),
      column(
        width=2,
        p("These numbers will change in the future (as delayed data is received) and should be interpreted with caution. These numbers are severely affected by reporting delays.")
      )
    ),
    fluidRow(
      br(),br(),br(),br(),
      br(),br(),br(),br()
    )
  )
}

covid19_server <- function(input, output, session, config) {

  output$overview_plot_national_syndromes_proportion <- renderCachedPlot({
    pd <- pool %>% dplyr::tbl("data_norsyss") %>%
      dplyr::filter(tag_outcome %in% c(
        "covid19_lte",
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
        "influensa_lte",
        "akkut_ovre_luftveisinfeksjon_lte",
        "rxx_for_covid19_lte"
      ),
      labels = c(
        "COVID-19 liknenede symptomer (R99.1)",
        "Influensa (R80)",
        "Akutt øvre luftveisinfeksjon (R74)",
        "Luftvei diagnosekoder (samlet*)"
      )
    )]

    q <- ggplot(pd, aes(x=date, y=andel, color=name_outcome))
    q <- q + geom_line(size=2)
    q <- q + scale_y_continuous(
      "Andel",
      breaks = fhiplot::pretty_breaks(6),
      expand = expand_scale(mult = c(0, 0.1)),
      labels = format_nor_perc
    )
    q <- q + expand_limits(y = 0)
    q <- q + scale_x_date("Dato", date_labels = "%d.%m")
    q <- q + fhiplot::scale_color_fhi("Syndrome")
    q <- q + fhiplot::theme_fhi_lines(20)
    q <- q + theme(legend.key.size = unit(1, "cm"))
    q <- q + labs(title="Andel konsultasjoner i Norge")
    q <- q + labs(caption=glue::glue(
      "Nevneren er totalt antall konsultasjoner\n",
      "*R26, R71, R73, R80, R84, R85, R86, R87, R88, R89, R89, R90, R92, R95 og R96"
    ))
    q
  }, cacheKeyExpr={list(
    lubridate::now(),
    lubridate::today(),
    input$overview_age
  )})

  output$overview_plot_county_proportion <- renderCachedPlot({
    pd <- pool %>% dplyr::tbl("data_norsyss") %>%
      dplyr::filter(tag_outcome %in% c(
        "covid19_lte",
        "influensa_lte",
        "rxx_for_covid19_lte",
        "akkut_ovre_luftveisinfeksjon_lte"
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
        "influensa_lte",
        "akkut_ovre_luftveisinfeksjon_lte",
        "rxx_for_covid19_lte"
      ),
      labels = c(
        "COVID-19 liknenede symptomer (R99.1)",
        "Influensa (R80)",
        "Akutt øvre luftveisinfeksjon (R74)",
        "Luftvei diagnosekoder (samlet*)"
      )
    )]

    labels <- pd[date == max(date) & tag_outcome=="covid19_lte"]
    labels[,lab := paste0("R991: ",format_nor_perc(andel))]

    max_val <- max(pd$andel)

    q <- ggplot(pd, aes(x=date, y=andel))
    q <- q + geom_line(mapping=aes(color=name_outcome),size=1)
    q <- q + geom_line(data=pd[tag_outcome %in% c("covid19_lte")],mapping=aes(color=name_outcome),size=3.5)
    q <- q + ggrepel::geom_label_repel(
      data = labels,
      mapping = aes(label = lab, y=max_val),
      nudge_y = 0.0,
      nudge_x = 0.0,
      direction = "y",
      angle = 0,
      vjust = 0,
      hjust = 0,
      label.r=0,
      segment.size = 0.2,
      size=6
    )
    q <- q + lemon::facet_rep_wrap(~location_name, repeat.tick.labels = "y", ncol=3)
    q <- q + scale_y_continuous(
      "Andel",
      breaks = fhiplot::pretty_breaks(6),
      expand = expand_scale(mult = c(0, 0.1)),
      labels = format_nor_perc
    )
    q <- q + expand_limits(y = 0)
    q <- q + scale_x_date("Dato", date_labels = "%d.%m")
    q <- q + fhiplot::scale_color_fhi("Syndrome")
    q <- q + fhiplot::theme_fhi_lines(20, panel_on_top = F)
    q <- q + theme(legend.key.size = unit(1, "cm"))
    q <- q + labs(title="Andel konsultasjoner i Norge")
    q <- q + labs(caption=glue::glue(
      "Nevneren er totalt antall konsultasjoner\n",
      "*R26, R71, R73, R80, R84, R85, R86, R87, R88, R89, R89, R90, R92, R95 og R96"
    ))
    q
  }, cacheKeyExpr={list(
    lubridate::now(),
    lubridate::today(),
    input$overview_age
  )})

  output$overview_plot_national_age_numbers <- renderCachedPlot({
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
    pd <- pd[,c("date","age","n"),with=F]

    max_date_uncertain <- max(pd$date)
    min_date_uncertain <- max_date_uncertain-6
    q <- ggplot(pd, aes(x=date,y=n, color=age, group=age))
    q <- q + annotate(
      "rect",
      ymin=-Inf,
      ymax=Inf,
      xmin=config$min_date_uncertain,
      xmax=config$max_date_uncertain,
      fill=fhiplot::base_color,
      alpha=0.2
    )
    q <- q + geom_line(size=2)
    q <- q + scale_y_continuous(
      "Antall konsultasjoner",
      breaks = fhiplot::pretty_breaks(6),
      expand = expand_scale(mult = c(0, 0.1))
    )
    q <- q + scale_x_date("Dato", date_labels = "%d.%m")
    q <- q + fhiplot::scale_color_fhi("Aldersgruppe")
    q <- q + fhiplot::theme_fhi_lines(20)
    q <- q + theme(legend.key.size = unit(1, "cm"))
    q <- q + labs(title="Antall COVID-19 (R99.1) konsultasjoner i Norge etter aldersgrupper")
    q <- q + labs(caption="Skyggelagt område må tolkes med varsom pga forsinkelse i rapportering\nKonsultasjoner er legekontakt, telefon, ekonsultasjoner til fastleger og legevakter")
    q
  }, cacheKeyExpr={list(
    lubridate::now(),
    lubridate::today(),
    input$overview_age
  )})

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

    max_date_uncertain <- max(pd$date)
    min_date_uncertain <- max_date_uncertain-6
    q <- ggplot(pd, aes(x=date,y=100*n/consult_with_influenza_totalt, color=age, group=age))
    q <- q + geom_line(size=2)
    q <- q + scale_y_continuous(
      "Andel",
      breaks = fhiplot::pretty_breaks(6),
      expand = expand_scale(mult = c(0, 0.1)),
      labels = format_nor_perc
    )
    q <- q + scale_x_date("Dato", date_labels = "%d.%m")
    q <- q + fhiplot::scale_color_fhi("Aldersgruppe")
    q <- q + fhiplot::theme_fhi_lines(20)
    q <- q + theme(legend.key.size = unit(1, "cm"))
    q <- q + labs(title="Andel konsultasjoner i Norge som tilhører COVID-19 (R99.1) etter aldersgrupper")
    q <- q + labs(caption="Konsultasjoner er legekontakt, telefon, ekonsultasjoner til fastleger og legevakter\nNevneren til alle aldersgrupper er totalt antall konsultasjoner (alle aldersgrupper summert)")
    q
  }, cacheKeyExpr={list(
    lubridate::now(),
    lubridate::today(),
    input$overview_age
  )})

  output$overview_plot_county_numbers <- renderCachedPlot({
    pd <- pool %>% dplyr::tbl("data_norsyss") %>%
      dplyr::filter(date >= !!config$start_date) %>%
      dplyr::filter(tag_outcome == "covid19_lte") %>%
      dplyr::filter(granularity_geo=="county") %>%
      dplyr::filter(age=="Totalt") %>%
      dplyr::collect()
    setDT(pd)

    pd[
      fhidata::norway_locations_b2020,
      on="location_code==county_code",
      location_name:=county_name
      ]

    pd <- pd[,c("location_name","location_code","date","n","pop"),with=F]
    setorder(pd, location_code, date)
    pd[,cum_n:=cumsum(n),by=location_code]
    pd[,cum_n_per_1000:=1000*cum_n/pop]

    labels <- pd[date == max(date)]
    formatter <- function(x) fhiplot::format_nor(x, digits=2)

    q <- ggplot(pd, aes(x=date,y=cum_n_per_1000, group=location_code))
    q <- q + annotate(
      "rect",
      ymin=-Inf,
      ymax=Inf,
      xmin=config$min_date_uncertain,
      xmax=config$max_date_uncertain,
      fill=fhiplot::base_color,
      alpha=0.2
    )
    q <- q + geom_line(mapping=aes(color=location_name),size=2)
    q <- q + ggrepel::geom_label_repel(
      data = labels,
      mapping = aes(label = location_name),
      nudge_y = 0.0,
      nudge_x = 1.0,
      direction = "y",
      angle = 0,
      vjust = 0,
      hjust = 0,
      label.r=0,
      segment.size = 0.2,
      size=6
    )
    q <- q + scale_y_continuous(
      "Kumulative antall konsultasjoner per 1.000 befolkning",
      breaks = fhiplot::pretty_breaks(6),
      expand = expand_scale(mult = c(0, 0.1)),
      labels = formatter
    )
    q <- q + scale_x_date(
      "Dato",
      lim = c(config$start_date, config$max_date_uncertain + 4),
      date_labels = "%d.%m"
    )
    q <- q + fhiplot::scale_color_fhi("Fylke", guide="none")
    q <- q + fhiplot::theme_fhi_lines(20, panel_on_top = FALSE)
    q <- q + theme(legend.key.size = unit(1, "cm"))
    q <- q + labs(title="Kumulative antall COVID-19 (R99.1) konsultasjoner per 1.000 befolkning")
    q <- q + labs(caption="Skyggelagt område må tolkes med varsom pga forsinkelse i rapportering\nKonsultasjoner er legekontakt, telefon, ekonsultasjoner til fastleger og legevakter")
    q
  }, cacheKeyExpr={list(
    lubridate::now(),
    lubridate::today(),
    input$overview_age
  )})

}
