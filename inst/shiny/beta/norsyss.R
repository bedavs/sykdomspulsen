norsyss_ui <- function(id, config) {
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
        plotOutput(ns("norsyss_plot_barometer_location"), height = "700px")
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
        plotOutput(ns("norsyss_plot_barometer_age"), height = "700px")
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
        plotOutput(ns("norsyss_plot_trends_1"), height = "700px")
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
        plotOutput(ns("norsyss_plot_trends_2"), height = "2000px")
      ),
      column(
        width=2,
        p("On March 12th COVID-19 (R99.1) consultations were a larger proportion than Influenza (R80). While these numbers will likely change in the future (as delayed data is received), they are still representative right now as the numerator and denominator have equal delays.")
      )
    ),
    fluidRow(
      br(),br(),br(),br(),
      br(),br(),br(),br()
    )
  )
}

norsyss_server <- function(input, output, session, config) {

  output$norsyss_plot_barometer_location <- renderCachedPlot({
    min_date <- lubridate::today()-56
    pd <- pool %>% dplyr::tbl("results_norsyss_standard") %>%
      dplyr::filter(tag_outcome %in% c(
        "gastro_lt",
        "respiratoryexternal_lt"
      )) %>%
      dplyr::filter(date >= !!min_date) %>%
      dplyr::filter(age >= "Totalt") %>%
      dplyr::filter(granularity_time == "weekly") %>%
      dplyr::filter(granularity_geo %in% c("national","county")) %>%
      dplyr::collect()
    setDT(pd)

    q <- ggplot(pd, aes(x=yrwk,y=location_code,fill=n_status))
    q <- q + geom_tile(color="black")
    q <- q + lemon::facet_rep_wrap(~tag_outcome, repeat.tick.labels = "y", ncol=2)
    q <- q + fhiplot::scale_fill_fhi("Status")
    q <- q + fhiplot::theme_fhi_basic(20)
    q <- q + theme(legend.key.size = unit(1, "cm"))
    q <- q + fhiplot::set_x_axis_vertical()
    q
  }, cacheKeyExpr={list(
    lubridate::now(),
    lubridate::today(),
    input$overview_age
  )})


  output$norsyss_plot_barometer_age <- renderCachedPlot({
    min_date <- lubridate::today()-56
    pd <- pool %>% dplyr::tbl("results_norsyss_standard") %>%
      dplyr::filter(tag_outcome %in% c(
        "gastro_lt",
        "respiratoryexternal_lt"
      )) %>%
      dplyr::filter(date >= !!min_date) %>%
      dplyr::filter(granularity_time == "weekly") %>%
      dplyr::filter(location_code %in% c("norge")) %>%
      dplyr::collect()
    setDT(pd)

    q <- ggplot(pd, aes(x=yrwk,y=age,fill=n_status))
    q <- q + geom_tile(color="black")
    q <- q + lemon::facet_rep_wrap(~tag_outcome, repeat.tick.labels = "y", ncol=2)
    q <- q + fhiplot::scale_fill_fhi("Status")
    q <- q + fhiplot::theme_fhi_basic(20)
    q <- q + theme(legend.key.size = unit(1, "cm"))
    q <- q + fhiplot::set_x_axis_vertical()
    q
  }, cacheKeyExpr={list(
    lubridate::now(),
    lubridate::today(),
    input$overview_age
  )})



  output$norsyss_plot_trends_1 <- renderCachedPlot({
    min_date <- lubridate::today()-365
    pd <- pool %>% dplyr::tbl("results_norsyss_standard") %>%
      dplyr::filter(tag_outcome %in% c(
        "gastro_lt"
      )) %>%
      dplyr::filter(date >= !!min_date) %>%
      dplyr::filter(granularity_time == "weekly") %>%
      dplyr::filter(location_code %in% c("norge")) %>%
      dplyr::collect()
    setDT(pd)

    q <- ggplot(pd, aes(x=date,y=n))
    q <- q + geom_line(color="black")
    q <- q + lemon::facet_rep_wrap(~age, repeat.tick.labels = "y", scales="free_y")
    q <- q + scale_x_date(
      "Dato",
      date_labels = "%d.%m"
    )
    q <- q + fhiplot::scale_fill_fhi("Status")
    q <- q + fhiplot::theme_fhi_lines(20)
    q <- q + theme(legend.key.size = unit(1, "cm"))
    q <- q + fhiplot::set_x_axis_vertical()
    q
  }, cacheKeyExpr={list(
    lubridate::now(),
    lubridate::today(),
    input$overview_age
  )})


  plot_trends <- function(pd,x_age){
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
    q <- q + lemon::facet_rep_wrap(~type, repeat.tick.labels = "y", scales="free_y")
    q <- q + scale_y_continuous(NULL)
    q <- q + scale_x_date(
      NULL,
      date_labels = "%d.%m"
    )
    q <- q + fhiplot::scale_fill_fhi("Status")
    q <- q + fhiplot::theme_fhi_lines(20)
    q <- q + theme(legend.key.size = unit(1, "cm"))
    #q <- q + fhiplot::set_x_axis_vertical()
    #q <- q + labs(title=x_age)
    q
  }

  output$norsyss_plot_trends_2 <- renderCachedPlot({
    min_date <- lubridate::today()-365
    pd <- pool %>% dplyr::tbl("results_norsyss_standard") %>%
      dplyr::filter(tag_outcome %in% c(
        "respiratoryexternal_lt"
      )) %>%
      dplyr::filter(date >= !!min_date) %>%
      dplyr::filter(granularity_time == "weekly") %>%
      dplyr::filter(location_code %in% c("norge")) %>%
      dplyr::collect()
    setDT(pd)
    pd[,
      age:=factor(
        age,
        levels=c(
          "Totalt",
          "0-4",
          "5-14",
          "15-19",
          "20-29",
          "30-64",
          "65+"
        )
      )
    ]
    pd1 <- copy(pd)
    pd1[,type:="Konsultasjoner"]
    pd1[, plot_n := n]
    pd1[, plot_u1 := n_baseline_thresholdu0]
    pd1[, plot_u2 := n_baseline_thresholdu1]

    pd2 <- copy(pd)
    pd2[,type:="Andel (%)"]
    pd2[, plot_n := 100 * n / n_denominator]
    pd2[, plot_u1 := 100 * n_baseline_thresholdu0 / n_denominator]
    pd2[, plot_u2 := 100 * n_baseline_thresholdu1 / n_denominator]

    pd3 <- copy(pd)
    pd3[,type:="Eksess"]
    pd3[,plot_n := pmax(0, n - n_baseline_thresholdu0)]
    pd3[, plot_u1 := 0]
    pd3[, plot_u2 := n_baseline_thresholdu1 - n_baseline_thresholdu0]

    pd <- rbind(pd1,pd2,pd3)
    pd[,type:=factor(
      type,
      levels=c(
        "Konsultasjoner",
        "Eksess",
        "Andel (%)"
      )
    )]

    title <- cowplot::ggdraw() +
      cowplot::draw_label(
        "respiratoryexternal_lt",
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
      plot_trends(pd, "Totalt"),
      plot_trends(pd, "0-4"),
      plot_trends(pd, "5-14"),
      plot_trends(pd, "15-19"),
      plot_trends(pd, "20-29"),
      plot_trends(pd, "30-64"),
      plot_trends(pd, "65+"),
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
      label_size=26
    )

  }, cacheKeyExpr={list(
    lubridate::now(),
    lubridate::today(),
    input$overview_age
  )})

}
