

norsyss_ui <- function(id, config) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width=12, align="left",
        p(
          id="toptext",
          strong("NorSySS (Norwegian Syndromic Surveillance System)"),
          " er et overvåkningssystem basert på",
          br(),
          "diagnosekoder (ICPC-2 koder) satt på legekontor og legevakter i hele Norge."
          )
      )
    )
    ,
    tabsetPanel(
      tabPanel(title="Oversikt",
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
              plotOutput(ns("norsyss_plot_barometer_age"), height = "500px")
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
              plotOutput(ns("norsyss_plot_barometer_location"), height = "3000px")
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
              plotOutput(ns("norsyss_plot_trends"), height = "2000px"),
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
          )
        )
      )
    )
  )
}

norsyss_server <- function(input, output, session, config) {

  plot_barometer_age <- function(tag_outcome = "respiratoryexternal_lt"){
    min_date1 <- lubridate::today()-7*16
    pd1 <- pool %>% dplyr::tbl("results_norsyss_standard") %>%
      dplyr::filter(tag_outcome %in% !!tag_outcome) %>%
      dplyr::filter(date >= !!min_date1) %>%
      dplyr::filter(granularity_time == "weekly") %>%
      dplyr::filter(location_code %in% c("norge")) %>%
      dplyr::collect()

    min_date2 <- lubridate::today()-16
    pd2 <- pool %>% dplyr::tbl("results_norsyss_standard") %>%
      dplyr::filter(tag_outcome %in% !!tag_outcome) %>%
      dplyr::filter(date >= !!min_date2) %>%
      dplyr::filter(granularity_time == "daily") %>%
      dplyr::filter(location_code %in% c("norge")) %>%
      dplyr::collect()

    pd <- rbind(pd1,pd2)
    setDT(pd)

    pd[, age := factor(
      age,
      levels = c(
        "65+",
        "30-64",
        "20-29",
        "15-19",
        "5-14",
        "0-4",
        "Totalt"
      )
    )]

    pd[, n_status := factor(
      n_status,
      levels = c("Normal", "Medium", "High")
    )]

    q <- ggplot(pd[granularity_time=="weekly"], aes(x=yrwk,y=age,fill=n_status))
    q <- q + geom_tile(color="black")
    q <- q + scale_y_discrete(NULL)
    q <- q + scale_x_discrete(NULL)
    q <- q + scale_fill_manual(
      NULL,
      values = c(
        "Normal" = fhiplot::warning_color[["low"]],
        "Medium" = fhiplot::warning_color[["med"]],
        "High" = fhiplot::warning_color[["hig"]]
        )
    )
    q <- q + fhiplot::theme_fhi_basic(20)
    q <- q + theme(legend.key.size = unit(1, "cm"))
    q <- q + theme(legend.position="bottom")
    q <- q + fhiplot::set_x_axis_vertical()
    q1 <- q + labs(title="Ukentlig status etter aldersgrupper i Norge")

    q <- ggplot(pd[granularity_time=="daily"], aes(x=date,y=age,fill=n_status))
    q <- q + geom_tile(color="black")
    q <- q + scale_y_discrete(NULL)
    q <- q + scale_x_date(
      NULL,
      date_breaks = "1 day",
      expand = expand_scale(mult = c(0, 0))
    )
    q <- q + scale_fill_manual(
      NULL,
      values = c(
        "Normal" = fhiplot::warning_color[["low"]],
        "Medium" = fhiplot::warning_color[["med"]],
        "High" = fhiplot::warning_color[["hig"]]
      )
    )
    q <- q + fhiplot::theme_fhi_basic(20)
    q <- q + theme(legend.key.size = unit(1, "cm"))
    q <- q + theme(legend.position="bottom")
    q <- q + fhiplot::set_x_axis_vertical()
    q2 <- q + labs(title="Daglig status etter aldersgrupper i Norge")

    title <- cowplot::ggdraw() +
      cowplot::draw_label(
        tag_outcome,
        fontface = 'bold',
        x = 0,
        hjust = 0,
        size=30
      ) +
      theme(
        # add margin on the left of the drawing canvas,
        # so title is aligned with left edge of first plot
        plot.margin = margin(0, 0, 0, 7)
      )

    cowplot::plot_grid(
      title,
      NULL,
      q1,
      q2,
      ncol=2,
      rel_heights = c(0.2, 1)
    )
  }

  plot_barameter_location <- function(tag_outcome = "respiratoryexternal_lt"){
    min_date1 <- lubridate::today()-7*16
    pd_week <- pool %>% dplyr::tbl("results_norsyss_standard") %>%
      dplyr::filter(tag_outcome %in% !!tag_outcome) %>%
      dplyr::filter(date >= !!min_date1) %>%
      dplyr::filter(granularity_time == "weekly") %>%
      dplyr::filter(granularity_geo %in% c("national","county")) %>%
      dplyr::collect()

    min_date2 <- lubridate::today()-16
    pd_day <- pool %>% dplyr::tbl("results_norsyss_standard") %>%
      dplyr::filter(tag_outcome %in% !!tag_outcome) %>%
      dplyr::filter(date >= !!min_date2) %>%
      dplyr::filter(granularity_time == "daily") %>%
      dplyr::filter(granularity_geo %in% c("national","county")) %>%
      dplyr::collect()

    pd <- rbind(pd_week,pd_day)
    setDT(pd)

    pd[, age := factor(
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

    pd[
      fhidata::norway_locations_long_b2020,
      on="location_code",
      location_name := location_name
      ]
    setorder(pd,location_code)
    locs <- unique(pd$location_name)
    locs <- rev(unique(c("Norge",locs)))
    pd[,location_name := factor(location_name, levels=locs)]

    pd[, n_status := factor(
      n_status,
      levels = c(
        "Normal",
        "Medium",
        "High"
      )
    )]

    q <- ggplot(pd[granularity_time=="weekly"], aes(x=yrwk,y=location_name,fill=n_status))
    q <- q + geom_tile(color="black")
    q <- q + scale_y_discrete(NULL)
    q <- q + scale_x_discrete(NULL)
    q <- q + lemon::facet_rep_wrap(~age, repeat.tick.labels = "y", scales="free", ncol=1)
    q <- q + scale_fill_manual(
      NULL,
      values = c(
        "Normal" = fhiplot::warning_color[["low"]],
        "Medium" = fhiplot::warning_color[["med"]],
        "High" = fhiplot::warning_color[["hig"]]
      )
    )
    q <- q + fhiplot::theme_fhi_basic(20)
    q <- q + theme(legend.key.size = unit(1, "cm"))
    q <- q + theme(legend.position="bottom")
    q <- q + fhiplot::set_x_axis_vertical()
    q_week <- q + labs(title="Ukentlig status etter aldersgrupper")

    q <- ggplot(pd[granularity_time=="daily"], aes(x=date,y=location_name,fill=n_status))
    q <- q + geom_tile(color="black")
    q <- q + scale_y_discrete(NULL)
    q <- q + scale_x_date(
      NULL,
      date_breaks = "1 day",
      expand = expand_scale(mult = c(0, 0))
    )
    q <- q + lemon::facet_rep_wrap(~age, repeat.tick.labels = "y", scales="free", ncol=1)
    q <- q + scale_fill_manual(
      NULL,
      values = c(
        "Normal" = fhiplot::warning_color[["low"]],
        "Medium" = fhiplot::warning_color[["med"]],
        "High" = fhiplot::warning_color[["hig"]]
      )
    )
    q <- q + fhiplot::theme_fhi_basic(20)
    q <- q + theme(legend.key.size = unit(1, "cm"))
    q <- q + theme(legend.position="bottom")
    q <- q + fhiplot::set_x_axis_vertical()
    q_day <- q + labs(title="Daglig status etter aldersgrupper")

    title <- cowplot::ggdraw() +
      cowplot::draw_label(
        tag_outcome,
        fontface = 'bold',
        x = 0,
        hjust = 0,
        size=30
      ) +
      theme(
        # add margin on the left of the drawing canvas,
        # so title is aligned with left edge of first plot
        plot.margin = margin(0, 0, 0, 7)
      )

    cowplot::plot_grid(
      title,
      NULL,
      q_week,
      q_day,
      ncol=2,
      rel_heights = c(0.03, 1)
    )
    #
    # pd[, name_outcome := factor(
    #   tag_outcome,
    #   levels = c("gastro_lt", "respiratoryexternal_lt"),
    #   labels = c(
    #     "Magetarm (D11, D70, D73)",
    #     "Luftveisinfeksjoner (R05, R74, R78, R83)"
    #   )
    # )]
  }


  plot_trends_single <- function(pd,x_age){
    pd <- pd[age==x_age]

    pd_med <- pd[plot_n>plot_u1 & plot_n<=plot_u2]
    pd_hig <- pd[plot_n>plot_u2]

    q <- ggplot(pd, aes(x=date,y=plot_n))
    q <- q + geom_ribbon(mapping=aes(ymin=-Inf,ymax=plot_u1),fill=fhiplot::warning_color[["low"]])
    q <- q + geom_ribbon(mapping=aes(ymin=plot_u1,ymax=plot_u2),fill=fhiplot::warning_color[["med"]])
    q <- q + geom_ribbon(mapping=aes(ymin=plot_u2,ymax=Inf),fill=fhiplot::warning_color[["hig"]])
    q <- q + geom_line(color="black")
    if(nrow(pd_med)>0){
      q <- q + geom_point(data=pd_med,size=4)
      q <- q + geom_point(data=pd_med, color=fhiplot::warning_color[["med"]],size=3)
    }
    if(nrow(pd_hig)>0){
      q <- q + geom_point(data=pd_hig,size=4)
      q <- q + geom_point(data=pd_hig, color=fhiplot::warning_color[["hig"]],size=3)
    }
    q <- q + lemon::facet_rep_wrap(~type, repeat.tick.labels = "y", scales="free", nrow=1)
    q <- q + scale_y_continuous(NULL)
    q <- q + scale_x_date(
      NULL,
      date_labels = "%d.%m"
    )
    q <- q + fhiplot::scale_fill_fhi(NULL)
    q <- q + fhiplot::theme_fhi_lines(20)
    q <- q + theme(legend.key.size = unit(1, "cm"))
    #q <- q + fhiplot::set_x_axis_vertical()
    #q <- q + labs(title=x_age)
    q
  }

  plot_trends_multiple <- function(tag_outcome){
    min_date_daily <- lubridate::today()-28
    min_date_weekly <- lubridate::today()-365
    pdday <- pool %>% dplyr::tbl("results_norsyss_standard") %>%
      dplyr::filter(tag_outcome == !!tag_outcome) %>%
      dplyr::filter(date >= !!min_date_daily) %>%
      dplyr::filter(granularity_time == "daily") %>%
      dplyr::filter(location_code %in% c("norge")) %>%
      dplyr::collect()
    setDT(pdday)

    pd <- pool %>% dplyr::tbl("results_norsyss_standard") %>%
      dplyr::filter(tag_outcome == !!tag_outcome) %>%
      dplyr::filter(date >= !!min_date_weekly) %>%
      dplyr::filter(granularity_time == "weekly") %>%
      dplyr::filter(location_code %in% c("norge")) %>%
      dplyr::collect()
    setDT(pd)

    pd1 <- copy(pd)
    pd1[,type:="Ukentlig andel (%)"]
    pd1[, plot_n := 100 * n / n_denominator]
    pd1[, plot_u1 := 100 * n_baseline_thresholdu0 / n_denominator]
    pd1[, plot_u2 := 100 * n_baseline_thresholdu1 / n_denominator]

    pd2 <- copy(pd)
    pd2[,type:="Ukentlig konsultasjoner"]
    pd2[, plot_n := n]
    pd2[, plot_u1 := n_baseline_thresholdu0]
    pd2[, plot_u2 := n_baseline_thresholdu1]

    pd3 <- copy(pd)
    pd3[,type:="Ukentlig eksess"]
    pd3[,plot_n := pmax(0, n - n_baseline_thresholdu0)]
    pd3[, plot_u1 := 0]
    pd3[, plot_u2 := n_baseline_thresholdu1 - n_baseline_thresholdu0]

    pd4 <- copy(pdday)
    pd4[,type:="Daglig eksess"]
    pd4[,plot_n := pmax(0, n - n_baseline_thresholdu0)]
    pd4[, plot_u1 := 0]
    pd4[, plot_u2 := n_baseline_thresholdu1 - n_baseline_thresholdu0]

    pd <- rbind(pd1,pd2,pd3,pd4)
    pd[,type:=factor(
      type,
      levels=c(
        "Ukentlig andel (%)",
        "Ukentlig konsultasjoner",
        "Ukentlig eksess",
        "Daglig eksess"
      )
    )]

    title <- cowplot::ggdraw() +
      cowplot::draw_label(
        tag_outcome,
        fontface = 'bold',
        x = 0,
        hjust = 0,
        size=30
      ) +
      theme(
        # add margin on the left of the drawing canvas,
        # so title is aligned with left edge of first plot
        plot.margin = margin(0, 0, 0, 7)
      )

    cowplot::plot_grid(
      title,
      plot_trends_single(pd, "Totalt"),
      plot_trends_single(pd, "0-4"),
      plot_trends_single(pd, "5-14"),
      plot_trends_single(pd, "15-19"),
      plot_trends_single(pd, "20-29"),
      plot_trends_single(pd, "30-64"),
      plot_trends_single(pd, "65+"),
      ncol=1,
      rel_heights = c(0.2, rep(1,7)),
      labels=c(
        "",
        "Totalt",
        "0-4",
        "5-14",
        "15-19",
        "20-29",
        "30-64",
        "65+"
      ),
      label_x = 0,
      hjust = 0,
      label_size=26
    )
  }

  output$norsyss_plot_barometer_age <- renderCachedPlot({
    plot_barometer_age(tag_outcome = "respiratoryexternal_lt")
  }, cacheKeyExpr={list(
    lubridate::today(),
    dev_invalidate_cache
  )},
  res = 72
  )

  output$norsyss_plot_barometer_location <- renderCachedPlot({
    plot_barameter_location(tag_outcome = "respiratoryexternal_lt")
  }, cacheKeyExpr={list(
    lubridate::today(),
    dev_invalidate_cache
  )},
  res = 72
  )

  output$norsyss_plot_trends <- renderCachedPlot({
    plot_trends_multiple("respiratoryexternal_lt")
  }, cacheKeyExpr={list(
    lubridate::today(),
    dev_invalidate_cache
  )},
  res = 72
  )


  outputOptions(output, "norsyss_plot_barometer_age", priority = 10)
  outputOptions(output, "norsyss_plot_barometer_location", priority = 9)

}
