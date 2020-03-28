dagsrapport_ui <- function(id, config) {
  ns <- NS(id)
  dimensionId <- ns("dimension")

  tagList(
    fluidRow(
      column(
        width=12, align="left",
        p(
          id="toptext",
          "Nedenfor finner du diagrammer med tabeller fra ",strong("koronavirusovervåkingen ved Folkehelseinstituttet."),
          br(),
          br(),
          "Følgende diagrammer er presentert:", br(),
          "- Antall meldte tilfeller, fordelt på fylker, kjønn, alder og smittested. Dataene er medlt inn fra leger og laboratorier.", br(),
          "- Antall tester som er gjort. Dataene er meldt inn fra mikrobiologiske laboratorier til Folkehelseinstituttet.", br(),
          "- Grafene er generert 26.03.2020. Tallene er midlertidige og kan bli endret."
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
              width=8, align="left",
              p(
                strong("Hovedpunkter"), br(),
                "- Det er totalt meldt 3156 personer med påvist covid -19 til Meldingssystem for ",
                "smittsomme sykdommer (MSIS). Oppdatert kl 24.00 den 25.03.2020.",br(),
                "- Totalt 73 892 personer er rapportert testet for koronavirus (SARS-CoV2) ",
                "(per 25.03.2020 kl. 15.00).", br(),
                "- I gjennomsnitt har 4 prosent testet positivt for covid-19 gjennom utbruddet så langt.", br(),
                "Blant de 255 tilfellene som ble meldt til MSIS 25.03, hadde 21 av tilfellene prøvetakingsdato 25.03. ",
                "Figuren under viser antall tilfeller etter prøvetakingsdato siden epidemiens start."
              )
            ),
            column(
              width=2,
              p("")
            )
          ),

          fluidRow(
            column(
              width=12, align="left",
              p(strong("Utvikling over tid, alder, kjønn og fylke")),
              br(),
              p("OBS TEST. Figuren er generert på data fra før 25. mars."),
              plotOutput(ns("daggrapport_fig1"), height = "700px")
            )
          ),

          fluidRow(
            column(
              width=12, align="left",
              br(),
              br(),
              p("Gjennomsnittsalder for tilfellene er 47 år, 52 % er menn."),
              plotOutput(ns("daggrapport_fig2"), height = "700px")
            )
          ),

          fluidRow(
            column(
              width=12, align="left",
              br(),
              br(),
              p("Personer med påvist covid-19 meldt til MSIS siste døgn, fordelt på fylke."),
              plotOutput(ns("daggrapport_fig3"), height = "600px")
            )
          ),

          fluidRow(
            column(
              width=12, align="left",
              br(),
              br(),
              p("Personer med påvist covid-19 meldt til MSIS siste døgn, fordelt på fylke."),
              plotOutput(ns("daggrapport_fig4"), height = "600px")
            )
          ),

          fluidRow(
            column(
              width=12, align="left",
              br(),
              br(),
              p("Personer med påvist covid-19 meldt til MSIS fordelt på fylke, per 100.000 befolkning."),
              plotOutput(ns("daggrapport_fig5"), height = "700px")
            )
          ),

          fluidRow(
            column(
              width=12, align="left",
              br(),
              br(),
              p("Personer med påvist covid-19 meldt til MSIS siste døgn, fordelt på fylke."),
              plotOutput(ns("daggrapport_fig6"), height = "700px")
            )
          ),

          fluidRow(
            column(
              width=12, align="left",
              br(),
              br(),
              br(),
              p(strong("Laboratorietester")),
              br(),
              p("Andel positive av de som er testet. I alt inngår 64 581 tester i figuren. I tillegg kommer tester hvor prøvetakingsdato ikke er spesifisert. Inkludert disse er det totalt gjort 73 892 tester."),
              plotOutput(ns("daggrapport_fig7"), height = "700px")
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

dagsrapport_server <- function(input, output, session, config) {
  #width <-  as.numeric(input$dimension[1])

  output$daggrapport_fig1 <- renderCachedPlot({
    plot_daggrapport_fig1()
  }, cacheKeyExpr={list(
    dev_invalidate_cache
  )},
  res = 72
  )

  output$daggrapport_fig2 <- renderCachedPlot({
    plot_daggrapport_fig2()
  }, cacheKeyExpr={list(
    dev_invalidate_cache
  )},
  res = 72
  )

  output$daggrapport_fig3 <- renderCachedPlot({
    plot_daggrapport_fig3()
  }, cacheKeyExpr={list(
    dev_invalidate_cache
  )},
  res = 72
  )

  output$daggrapport_fig4 <- renderCachedPlot({
    plot_daggrapport_fig4()
  }, cacheKeyExpr={list(
    dev_invalidate_cache
  )},
  res = 72
  )

  output$daggrapport_fig5 <- renderCachedPlot({
    plot_daggrapport_fig5()
  }, cacheKeyExpr={list(
    dev_invalidate_cache
  )},
  res = 72
  )

  output$daggrapport_fig6 <- renderCachedPlot({
    plot_daggrapport_fig6()
  }, cacheKeyExpr={list(
    dev_invalidate_cache
  )},
  res = 72
  )

  output$daggrapport_fig7 <- renderCachedPlot({
    plot_daggrapport_fig7()
  }, cacheKeyExpr={list(
    dev_invalidate_cache
  )},
  res = 72
  )
}

plot_daggrapport_fig1 <- function(){
  pd <- fread("data/personer-med-pvist-covid.csv")
  pd[, date:= as.Date(DateTime, format="%d.%m.%Y")]
  setnames(pd, "Total antall tilfeller", "n")
  pd

  q <- ggplot(pd, aes(x = date, y = n))
  q <- q + geom_col(fill = fhiplot::base_color, width = 0.8)
  q <- q + geom_text(mapping=aes(label = n), nudge_y = 10, size=6, angle=90, hjust=0, label.size=0)
  q <- q + scale_x_date(
    NULL,
    date_breaks = "2 days",
    date_labels ="%d.%m",
    expand = expand_scale(mult = c(0, 0.0))
  )
  q <- q + scale_y_continuous("Antall",
                              breaks = fhiplot::pretty_breaks(5),
                              expand = expand_scale(mult = c(0, 0.1))
  )
  q <- q + labs(title = "Personer med påvist covid-19")
  q <- q + labs(caption = glue::glue(
    "Kilde: Folkehelseinstituttet\n",
    "Personer med påvist covid -19 meldt til MSIS, etter prøvetakingsdato"
  ))
  q <- q + fhiplot::theme_fhi_lines(18)
  q <- q + theme( panel.grid.major.x=element_blank())
  q <- q + theme( panel.grid.minor.x=element_blank())
  q
}


plot_daggrapport_fig2 <- function(){
  pd <- fread("data/personer-med-pvist-covid-alder-kjonn.csv")
  pd[,Totalt:=NULL]

  m <- max(c(pd$Kvinner, pd$Menn))
  pd[,Menn := -1*Menn]
  pd[,Alder := factor(Alder, levels=Alder)]
  pd <- melt.data.table(pd, id.vars="Alder")

  q <- ggplot(pd, aes(x = Alder, y=value, fill = variable))
  q <- q + geom_bar(data = pd[variable=="Kvinner"], stat = "identity")
  q <- q + geom_bar(data = pd[variable=="Menn"], stat = "identity")
  q <- q + coord_flip()
  q <- q + ylab("Antall tilfeller")
  q <- q + ylim(-m, m)
  q <- q + fhiplot::scale_fill_fhi("Kjønn", palette="primary")
  q <- q + fhiplot::theme_fhi_lines(18)
  q <- q + labs(title="Alders- og kjønnsfordeling")
  q <- q + xlab("Aldersgrupper")
  q <- q + scale_y_continuous(
    labels=c(seq(500,0,-50),seq(50,500,50)),
    breaks=c(-seq(500,0,-50),seq(50,500,50))
  )
  q <- q + labs(title="Personer med påvist covid -19 meldt til MSIS, etter aldersgrupper og kjønn.")
  q <- q + labs(caption = glue::glue(
    "Kilde: Folkehelseinstituttet\n",
    "Personer med påvist covid -19 meldt til MSIS, etter aldersgrupper og kjønn."
  ))
  q <- q + fhiplot::theme_fhi_lines(18)
  q
}


plot_daggrapport_fig3 <- function(){
  pd <- fread("data/personer-med-pvist-covid-fylke.csv")
  setnames(pd,c("cat","x_24_03_20","x_25_03_20","totalt"))
  pd[,cat_24_03_20 := fancycut::fancycut(
    x_24_03_20,
    "0" = "[0,0]",
    "1-5"="[1,5]",
    "6-10"="[6,10]",
    "11-20"="[11,20]",
    "21+"="[21,19999]"
  )]
  pd[,cat_25_03_20 := fancycut::fancycut(
    x_25_03_20,
    "0" = "[0,0]",
    "1-5"="[1,5]",
    "6-10"="[6,10]",
    "11-20"="[11,20]",
    "21+"="[21,19999]"
  )]
  pd[,cat_totalt := fancycut::fancycut(
    totalt,
    "0" = "[0,0]",
    "1-50"="[1,50]",
    "51-100"="[51,100]",
    "101-200"="[101,200]",
    "201-500"="[201,500]",
    "501+"="[501,99999]"
  )]
  pd[,county_name := c(
    "Agder",
    "Innlandet",
    "Møre og Romsdal",
    "Nordland",
    "Oslo",
    "Rogaland",
    "Troms og Finnmark",
    "Trøndelag",
    "Vestfold og Telemark",
    "Vestland",
    "Viken",
    "Ukjent fylke"
  )]
  pd[,county_code := c(
    "county42",
    "county34",
    "county15",
    "county18",
    "county03",
    "county11",
    "county54",
    "county50",
    "county38",
    "county46",
    "county30",
    "xx"
  )]
  pd[,Totalt:=NULL]

  library(ggplot2)
  library(data.table)

  pd <- merge(
    fhidata::norway_map_counties_with_insert_b2020,
    pd,
    by.x="location_code",
    by.y="county_code"
  )

  q <- ggplot()
  q <- q + geom_polygon(data = pd, aes( x = long, y = lat, group = group, fill=cat_totalt), color="black", size=0.1)
  q <- q + annotate(
    "text",
    x = fhidata::norway_map_insert_title_position_b2020$long,
    y = fhidata::norway_map_insert_title_position_b2020$lat,
    label = "Oslo",
    size=8
  )
  q <- q + fhiplot::scale_fill_fhi("Tilfeller", palette="map_seq_missing", direction = -1, drop=F)
  q <- q + theme_void(18)
  q <- q + coord_quickmap()
  q <- q + labs(title = "Meldte tilfeller totalt")
  q1 <- q

  q <- ggplot()
  q <- q + geom_polygon(data = pd, aes( x = long, y = lat, group = group, fill=cat_25_03_20), color="black", size=0.1)
  q <- q + annotate(
    "text",
    x = fhidata::norway_map_insert_title_position_b2020$long,
    y = fhidata::norway_map_insert_title_position_b2020$lat,
    label = "Oslo",
    size=8
  )
  q <- q + fhiplot::scale_fill_fhi("Tilfeller", palette="map_seq_missing", direction = -1, drop=F)
  q <- q + theme_void(18)
  q <- q + coord_quickmap()
  q <- q + labs(title = "Meldte tilfeller 25.03.2020")
  q2 <- q

  cowplot::plot_grid(q1, q2, align = "none")
}



plot_daggrapport_fig4 <- function(){
  pd <- fread("data/personer-med-pvist-covid-per-10000.csv")
  setnames(pd,c("cat","per100000"))
  pd[,per100000 := as.numeric(stringr::str_replace_all(per100000,",","."))]
  pd[,county_name := c(
    "Agder",
    "Innlandet",
    "Møre og Romsdal",
    "Nordland",
    "Oslo",
    "Rogaland",
    "Troms og Finnmark",
    "Trøndelag",
    "Vestfold og Telemark",
    "Vestland",
    "Viken"
  )]
  pd[,county_code := c(
    "county42",
    "county34",
    "county15",
    "county18",
    "county03",
    "county11",
    "county54",
    "county50",
    "county38",
    "county46",
    "county30"
  )]
  pd[,county_name := factor(county_name, levels=rev(county_name))]

  q <- ggplot(pd, aes(x = county_name, y = per100000))
  q <- q + geom_col(fill = fhiplot::base_color, width = 0.8)
  q <- q + geom_label(mapping=aes(label = per100000), nudge_y = 1, size=6, angle=0, hjust=0, label.size=0)

  q <- q + coord_flip()
  q <- q + scale_x_discrete(NULL)
  q <- q + scale_y_continuous("Antall",
                              breaks = fhiplot::pretty_breaks(5),
                              expand = expand_scale(mult = c(0, 0.1))
  )
  q <- q + labs(title = "Personer med påvist covid-19 per 100.000 befolkning")
  q <- q + labs(caption = glue::glue(
    "Kilde: Folkehelseinstituttet\n",
    "Personer med påvist covid -19 meldt til MSIS"
  ))
  q <- q + fhiplot::theme_fhi_lines(18, panel_on_top = F)

  q <- q + theme( panel.grid.major.y=element_blank())
  q
}


plot_daggrapport_fig5 <- function(){
  pd <- fread("data/smittested-norge-utland.csv")
  pd[,date := as.Date(DateTime)]
  pd[,DateTime:=NULL]
  pd <- melt.data.table(pd, id.vars = "date")

  q <- ggplot(pd, aes(x = date, y = value, fill = variable))
  q <- q + geom_col(width = 0.8)
  q <- q + fhiplot::scale_fill_fhi("Smittested")
  q <- q + scale_x_date(
    NULL,
    date_labels ="%d.%m"
  )
  q <- q + scale_y_continuous("Antall",
                              breaks = fhiplot::pretty_breaks(5),
                              expand = expand_scale(mult = c(0, 0.1))
  )
  q <- q + labs(title = "Personer med påvist covid-19 etter smittested")
  q <- q + labs(caption = glue::glue(
    "Kilde: Folkehelseinstituttet\n",
    "Personer med påvist covid -19 meldt til MSIS"
  ))
  q <- q + fhiplot::theme_fhi_lines(18, panel_on_top = F)
  q
}



plot_daggrapport_fig6 <- function(){
  pd <- fread("data/smittested-land-prosent.csv")
  pd[, land := c(
    "Norge",
    "Østerrike",
    "Italia",
    "Spania",
    "Storbritannia",
    "Sveits",
    "Frankrike",
    "Tyskland",
    "USA",
    "Portugal",
    "Iran",
    "Andre land",
    "Ukjent"
  )]
  pd <- pd[!land %in% c("Iran","Ukjent")]
  pd[, land := factor(land, levels=land)]
  pd[,n:=`Antall meldte`]
  pd[,cum_n:=rev(cumsum(rev(n)))-n/2]
  pd[,cum_n:=100*cum_n/sum(n)]
  pd[,n:=100*n/sum(n)]
  pd[,lab := paste0("N = ",`Antall meldte`)]

  q <- ggplot(pd, aes(x=land,y=n, fill=land))
  q <- q + geom_col()
  q <- q + geom_label(mapping=aes(label = lab), nudge_y = 1, size=6, angle=0, hjust=0, label.size=0, fill="white")
  q <- q + coord_flip()
  q <- q + scale_x_discrete(NULL,
                              expand = expand_scale(mult = c(0, 0.0)))
  q <- q + scale_y_continuous(
    "Prosent",
    lim=c(0,100),expand = expand_scale(mult = c(0, 0.0)),
    labels = format_nor_perc)
  q <- q + fhiplot::scale_fill_fhi("Smitteland", guide="none")
  q <- q + fhiplot::scale_color_fhi("Smitteland", guide="none")
  q <- q + labs(title = "Personer med påvist covid-19 etter smitteland")
  q <- q + labs(caption = glue::glue(
    "Kilde: Folkehelseinstituttet\n",
    "Personer med påvist covid -19 meldt til MSIS"
  ))
  q <- q + fhiplot::theme_fhi_lines(18, panel_on_top = F)
  q <- q + theme( panel.grid.major.y=element_blank())
  q
}



plot_daggrapport_fig7 <- function(){
  pd <- fread("data/antall-prver-testet-for.csv")
  pd[,date:=as.Date(Dato)]
  pd[,Dato:=NULL]

  pd_bars <- melt.data.table(pd,id.vars = "date",measure.vars = c("Positive","Negative"))
  pd_bars[,n:=sum(value),by=.(date)]

  weekends <- unique(pd$date)
  weekends <- weekends[lubridate::wday(weekends, week_start = 1) %in% c(6,7)]
  weekends <- data.frame(date = weekends)

  max_y <- max(pd_bars[,.(n=sum(n)),by=.(date)]$n, na.rm=T)
  min_y_start <- -0.085*max_y*1.01
  min_y_end <- -0.05*max_y*1.01

  pd_line <- pd[,.(
    andel = as.numeric(stringr::str_replace(Prosent,",","."))
  ),
  by=.(date)
  ]
  max_andel <- max(pd_line$andel, na.rm=T)
  pd_line[, andel := max_y * andel / max_andel]

  q <- ggplot(pd_bars, aes(x=date, y=value))
  q <- q + geom_col(mapping=aes(fill=variable))
  q <- q + geom_line(data=pd_line, mapping=aes(y=andel),color="red", size = 2)
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
      ~ . /  max_y * max_andel,
      breaks = fhiplot::pretty_breaks(6),
      labels = format_nor_perc,
      name = "Prosent"
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
  q <- q + labs(title = "Antall prøver testet for koronavirus og andel positive prøver")
  q <- q + labs(caption=glue::glue(
    "Kilde: Folkehelseinstituttet\n",
    "Røde piler viser helgen\n",
    "Kolonnene tilhører høyre-aksen, den røde linjen tilhører venstre-aksen"
  ))
  q <- q + theme( panel.grid.major.x=element_blank())
  q <- q + theme( panel.grid.minor.x=element_blank())
  q
}



