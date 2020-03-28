

norsyss_weekly_ui <- function(id, config) {
  ns <- NS(id)
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

        radioButtons(
          inputId = ns("norsyss_weekly_tag"),
          label = "Syndrome",
          choices = config$choices_norsyss_tag,
          selected = config$choices_norsyss_tag[[1]],
          width = "400px"
        ),

        br(),

        selectizeInput(
          inputId = ns("norsyss_weekly_location_code"),
          label = "Geografisk område",
          choices = config$choices_location,
          selected = "norge",
          multiple = FALSE,
          options = NULL,
          width = "400px"
        ),

        br(),

        sliderInput(
          inputId = ns("norsyss_weekly_date_range"),
          label = "Datoer",
          min = config$start_date_norsyss_standard_weekly,
          max = config$max_date_uncertain,
          value = c(config$max_date_uncertain-365, config$max_date_uncertain),
          width = "400px"
        )

      )
    ),

   fluidRow(
     column(
       width=12, align="center",
       plotOutput(ns("norsyss_weekly_trend"), height = "600px"),
       br(),
       br(),
       br(),
       br()
     )
   )
 )
}

norsyss_weekly_server <- function(input, output, session, config) {
  data_norsyss_weekly_trend <- reactive({
    req(input$norsyss_weekly_tag)
    req(input$norsyss_weekly_location_code)

    pd <- pool %>% dplyr::tbl("results_norsyss_standard") %>%
      dplyr::filter(tag_outcome %in% !!input$norsyss_weekly_tag) %>%
      dplyr::filter(date >= !!config$start_date_norsyss_standard_weekly) %>%
      dplyr::filter(granularity_time == "weekly") %>%
      dplyr::filter(age == "Totalt") %>%
      dplyr::filter(location_code %in% !!input$norsyss_weekly_location_code) %>%
      dplyr::select(date, yrwk, age, location_code, n, n_baseline_thresholdu0, n_baseline_thresholdu1) %>%
      dplyr::collect()

    setDT(pd)
    pd[, date:=as.Date(date)]
    return(pd)
  })

  output$norsyss_weekly_trend <- renderCachedPlot({
    req(input$norsyss_weekly_tag)
    req(input$norsyss_weekly_location_code)

    plot_norsyss_weekly_trend(
      tag_outcome = input$norsyss_weekly_tag,
      location_code = input$norsyss_weekly_location_code,
      date_range = input$norsyss_weekly_date_range,
      data = data_norsyss_weekly_trend(),
      config = config
    )
  }, cacheKeyExpr={list(
    input$norsyss_weekly_tag,
    input$norsyss_weekly_location_code,
    input$norsyss_weekly_date_range,
    dev_invalidate_cache
  )},
  res = 72
  )
}


plot_norsyss_weekly_trend <- function(
  tag_outcome = "gastro_lf_tl",
  location_code = "norge",
  date_range = as.Date(c("2019-01-01", "2020-01-01")),
  data,
  config = config
){
  pd <- data[date >= date_range[1] & date <= date_range[2]]

  pd_med <- pd[n>n_baseline_thresholdu0 & n<=n_baseline_thresholdu1]
  pd_hig <- pd[n>n_baseline_thresholdu1]

  title_text <- glue::glue(
    "{names(config$choices_location)[config$choices_location==location_code]}\n",
    "{names(config$choices_norsyss_tag)[config$choices_norsyss_tag==tag_outcome]}\n",
  )

  q <- ggplot(pd, aes(x=date,y=n))
  q <- q + geom_ribbon(mapping=aes(ymin=-Inf,ymax=n_baseline_thresholdu0),fill=fhiplot::warning_color[["low"]])
  q <- q + geom_ribbon(mapping=aes(ymin=n_baseline_thresholdu0,ymax=n_baseline_thresholdu1),fill=fhiplot::warning_color[["med"]])
  q <- q + geom_ribbon(mapping=aes(ymin=n_baseline_thresholdu1,ymax=Inf),fill=fhiplot::warning_color[["hig"]])
  q <- q + geom_line(color="black", size=2)
  if(nrow(pd_med)>0){
    q <- q + geom_point(data=pd_med,size=7)
    q <- q + geom_point(data=pd_med, color=fhiplot::warning_color[["med"]],size=5)
  }
  if(nrow(pd_hig)>0){
    q <- q + geom_point(data=pd_hig,size=7)
    q <- q + geom_point(data=pd_hig, color=fhiplot::warning_color[["hig"]],size=5)
  }
  #q <- q + lemon::facet_rep_wrap(~type, repeat.tick.labels = "y", scales="free", nrow=1)
  q <- q + scale_y_continuous(
    "Antall konsultasjoner",
    expand = expand_scale(mult = c(0, 0))
  )
  q <- q + expand_limits(y = 0)
  q <- q + scale_x_date(
    NULL,
    date_labels = "%d.%m.%Y",
    expand = expand_scale(mult = c(0.01, 0.01))
  )
  q <- q + fhiplot::scale_fill_fhi(NULL)
  q <- q + fhiplot::theme_fhi_lines(16)
  q <- q + theme(legend.key.size = unit(1, "cm"))
  #q <- q + fhiplot::set_x_axis_vertical()
  q <- q + labs(title=title_text)
  q <- q + labs(caption = glue::glue(
    "Folkehelseinstituttet, {format(lubridate::today(),'%d.%m.%Y')}"
  ))
  q
}
