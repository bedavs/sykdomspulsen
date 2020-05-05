ui_norsyss_kht_email <- function(data, argset, schema) {
  if(FALSE){
    tm_run_task("ui_norsyss_kht_email")
  }

  if(plnr::is_run_directly()){
    tm_update_plans("ui_norsyss_kht_email")

    data <- tm_get_data("ui_norsyss_kht_email", index_plan=1)
    argset <- tm_get_argset("ui_norsyss_kht_email", index_plan=1, index_argset = 1)
    schema <- tm_get_schema("ui_norsyss_kht_email")
  }

  email_subject <- glue::glue("OBS varsel fra Sykdomspulsen {lubridate::today()}")

  email_text_top <- glue::glue(
    "<b>Dette er et OBS varsel fra Sykdomspulsen for kommunehelsetjenesten</b><br><br>",

    "Nytt fra Sykdomspulsen:<br>",
    "- Vi har endret websiden som kommer etter p{fhi::nb$aa}loggingen p{fhi::nb$aa} ",
    "<a href='https://spuls.fhi.no'>https://spuls.fhi.no</a> s{fhi::nb$aa} det skal v{fhi::nb$ae}re ",
    "lettere {fhi::nb$aa} se hvor man skal trykke for {fhi::nb$aa} komme seg videre.<br>",
    "- 04.05.2020 ble det opprettet en ny ICPC-2 kode for covid-19 ",
    "(<a href='https://fastlegen.no/artikkel/diagnosekoder-ved-covid-19'>https://fastlegen.no/artikkel/diagnosekoder-ved-covid-19</a>). ",
    "Denne uken vil vi fortsatt kun ha den gamle koden (R991: covid-19 (mistenkt eller bekreftet), ",
    "men fra neste uke vil vi inkludere den nye koden i oversikten. <br>",
    "-  Vi {fhi::nb$oe}nsker s{fhi::nb$aa}rt deltakere til et brukerpanel som kan gi innspill om websiden og OBS varselet, send mail til sykdomspulsen@fhi.no<br><br>",

    "Mer informasjon om Sykdomspulsen og OBS varselet finner du under tabellene og grafer finnes p{fhi::nb$aa} websiden <a href='https://spuls.fhi.no'>https://spuls.fhi.no</a><br><br>",

    "Dersom dere har problemer med p{fhi::nb$aa}loggingen eller andre sp\u00F8rsm\u00E5l, vennligst send en mail til sykdomspulsen@fhi.no<br><br><br>"
  )

  email_text_bottom <- glue::glue(
    "Dette er et OBS varsel fra Sykdomspulsen.<br><br>",

    "Dette OBS varslet er for det geografiske omr{fhi::nb$aa}det ",
    "du valgte i websiden Sykdomspulsen for kommunehelsetjenesten.<br><br>",

    "Tabellen med covid-19 viser antall konsultasjoner og andel konsultasjoner ",
    "hos lege og legevakt (NorSySS) og antall bekreftede tilfeller registrert i MSIS.<br><br>",

    "Tabellene med mage-tarminfeksjoner og luftveisinfeksjoner har kun NorSySS data og viser disse verdiene:<br>",
    "Antall konsultasjoner: Dette er ikke antall personer da en person kan telles flere ganger om den ",
    "g{fhi::nb$aa}r til legen flere ganger.<br>",
    "Flere enn normalt: Differansen mellom antall registrerte og {fhi::nb$oe}vre grense for normalt antall (95% prediksjonsintervall)<br>",
    "Z-verdi: antall ganger standardavvik ut fra forventet antall konsultasjoner.<br>",
    "Bl{fhi::nb$aa}tt felt: Antall konsultasjoner er som forventet (Z-verdi < 2)<br>",
    "Gult felt: Antall konsultasjoner er h{fhi::nb$oe}yere enn forventet (Z-verdi mellom 2 og 4 og minst 3 konsultasjoner)<br>",
    "R{fhi::nb$oe}dt felt: Antall konsultasjoner er betydelig h{fhi::nb$oe}yere enn forventet (Z-verdi >= 4 og minst 4 konsultasjoner)<br><br>",

    "Varselet er en informasjon om at det kan v{fhi::nb$ae}re noe som b{fhi::nb$oe}r f{fhi::nb$oe}lges opp i din kommune eller i et fylke. ",
    "Det anbefales {fhi::nb$ae} g{fhi::nb$ae} inn i Sykdomspulsen websiden og sjekke det ut. Varselet beh{fhi::nb$oe}ver ikke {fhi::nb$aa} bety noe alvorlig.<br><br>",

    "Sykdomspulsen kan i noen tilfeller generere et OBS varsel selv om det bare er en eller to konsultasjoner for et symptom/sykdom. ",
    "Dette sees som oftest i sm{fhi::nb$aa} kommuner der det vanligvis ikke er mange konsultasjoner. For ikke {fhi::nb$aa} bli forstyrret ",
    "av slike signaler har vi nå lagt inn en nedre grense for gult signal p{fhi::nb$aa} p{fhi::nb$aa} minst tre konsultasjoner og en nedre grense for ",
    "r{fhi::nb$oe}dt signal p{fhi::nb$aa} minst fire konsultasjoner.<br><br>",

    "Ta kontakt med oss om du har sp{fhi::nb$oe}rsm{fhi::nb$aa}l eller om det er noe som er uklart p{fhi::nb$aa} sykdomspulsen@fhi.no.<br><br>",

    "Vi {fhi::nb$oe}nsker ogs{fhi::nb$aa} tilbakemelding p{fhi::nb$aa} om dette varselet er nyttig for dere eller ikke.<br><br>",

    "Hilsen:<br><br>",

    "Sykdomspulsen ved Folkehelseinstituttet<br>",
    "v/Gry M Gr{fhi::nb$oe}neng (prosjektleder), Richard White (statistiker og webansvarlig) og Gunnar R{fhi::nb$oe} (statistiker og webansvarlig)<br><br>"
  )

  email_text <- email_text_top

  email_text <- paste0(email_text, "<hr width='60%' size='5px' noshade><br>\n")

  email_text <- paste0(
    email_text,
    "<h2>NorSySS+MSIS: covid-19 oversikt</h2>",
    norsyss_kht_covid19_table(data = data)
    )

  email_text <- paste0(email_text, "<hr width='60%' size='5px' noshade><br>\n")

  # include outbreaks
  for(tag_outcome in argset$tag_outcome){
    email_text <- paste0(
      email_text,
      "<h2>NorSySS: ",config$def$norsyss$long_names[[tag_outcome]]," varsler</h2>",
      norsyss_kht_obs_table(
        results = data$alert[[tag_outcome]],
        tag_outcome = tag_outcome
      )
    )
  }

  email_text <- paste0(email_text, "<hr width='60%' size='5px' noshade><br>")

  # add in bottom text
  email_text <- paste0(email_text, email_text_bottom)

  bcc <- NULL
  if(config$is_production) bcc <- "sykdomspulsen@fhi.no"
  mailr(
    subject = e_subject(
      email_subject,
      is_final = config$permissions$ui_norsyss_kht_email$is_final()
      ),
    html = email_text,
    to = argset$email,
    bcc = bcc,
    is_final = config$permissions$ui_norsyss_kht_email$is_final()
  )
}

norsyss_kht_obs_table <- function(results, tag_outcome) {
  r_long <- copy(results)

  tag_pretty <- config$def$norsyss$long_names[[tag_outcome]]

  if (nrow(r_long) == 0) {
    return(sprintf("<b>%s:</b> <span style='color:red;text-decoration:underline;'>Ingen varsler registrert</span><br><br><br>", tag_pretty))
  }

  setorder(r_long, tag_outcome, yrwk)
  r_long[age=="total",age:="Totalt"]
  r_long[, week_id := 1:.N,by=.(location_code,age,sex)]
  r_long[, tag_pretty := tag_pretty]
  r_long[, excessp := fhiplot::format_nor(ceiling(pmax(0, n - n_baseline_thresholdu0)),0)]
  r_long[, zscorep := fhiplot::format_nor(n_zscore, 1)]
  r_long[, n := fhiplot::format_nor(n, 0)]

  r_wide <- dcast.data.table(
    r_long,
    tag_pretty + location_code + age ~ week_id,
    value.var = c("n", "excessp", "n_baseline_thresholdu0", "n_zscore", "zscorep", "n_status")
  )
  r_wide <- r_wide[!(n_status_1=="normal" & n_status_2=="normal" & n_status_3=="normal" & n_status_4=="normal")]
  setorder(r_wide, -n_zscore_4)
  r_wide[,location_name := get_location_name(location_code)]

  yrwks <- unique(r_long[, c("week_id", "yrwk")])
  setorder(yrwks, week_id)

  tab <- huxtable::huxtable(
    geo = r_wide$location_name,
    Alder = r_wide$age,
    `n_1` = r_wide$n_1,
    `n_2` = r_wide$n_2,
    `n_3` = r_wide$n_3,
    `n_4` = r_wide$n_4,
    `excess_1` = r_wide$excessp_1,
    `excess_2` = r_wide$excessp_2,
    `excess_3` = r_wide$excessp_3,
    `excess_4` = r_wide$excessp_4,
    `zscore_1` = r_wide$zscorep_1,
    `zscore_2` = r_wide$zscorep_2,
    `zscore_3` = r_wide$zscorep_3,
    `zscore_4` = r_wide$zscorep_4
  ) %>%
    huxtable::add_colnames() %>%
    fhiplot::huxtable_theme_fhi_basic()

  # coloring in
  for (i in 1:4) {
    z <- glue::glue("n_status_{i}")
    column_to_color <- c(2, 6, 10) + i
    index_low <- which(r_wide[[z]] == "normal") + 1
    index_med <- which(r_wide[[z]] == "medium") + 1
    index_hig <- which(r_wide[[z]] == "high") + 1

    if (length(index_low) > 0) huxtable::background_color(tab)[index_low, column_to_color] <- fhiplot::warning_color[["low"]]
    if (length(index_med) > 0) huxtable::background_color(tab)[index_med, column_to_color] <- fhiplot::warning_color[["med"]]
    if (length(index_hig) > 0) huxtable::background_color(tab)[index_hig, column_to_color] <- fhiplot::warning_color[["hig"]]
  }

  tab[1, ] <- c(
    "Geografisk omr\u00E5de",
    "Alder",
    yrwks$yrwk,
    yrwks$yrwk,
    yrwks$yrwk
  )

  tab <- huxtable::add_rows(tab, tab[1, ], after = 0)

  huxtable::escape_contents(tab)[, 1] <- FALSE

  tab <- huxtable::merge_cells(tab, 1:2, 1)
  tab <- huxtable::merge_cells(tab, 1:2, 2)

  tab <- huxtable::merge_cells(tab, 1, 3:6)
  tab[1, 3] <- "Antall konsultasjoner"

  tab <- huxtable::merge_cells(tab, 1, 7:10)
  tab[1, 7] <- "Flere enn normalt<sup>1</sup>"

  tab <- huxtable::merge_cells(tab, 1, 11:14)
  tab[1, 11] <- "Z-verdi<sup>2</sup>"

  huxtable::left_border(tab)[, c(3, 7, 11)] <- 5
  huxtable::left_border_style(tab)[, c(3, 7, 11)] <- "double"

  huxtable::align(tab) <- "center"

  nr0 <- nrow(tab) + 1
  tab <- huxtable::add_footnote(tab, glue::glue(
    "<sup>1</sup>Differansen mellom antall registrete og {fhi::nb$oe}vre grense for normalt antall (95% prediksjonsintervall)<br>",
    "<sup>2</sup>Z-verdi: antall ganger standardavvik ut fra forventet antall konsultasjoner<br>",
    "Bl{fhi::nb$aa}tt felt: Antall konsultasjoner er som forventet (Z-verdi < 2)<br>",
    "Gult felt: Antall konsultasjoner er h{fhi::nb$oe}yere enn forventet (Z-verdi mellom 2 og 4 og minst 3 konsultasjoner)<br>",
    "R{fhi::nb$oe}dt felt: Antall konsultasjoner er betydelig h{fhi::nb$oe}yere enn forventet (Z-verdi >= 4 og minst 4 konsultasjoner)<br>",
  ), border = 0)
  nr1 <- nrow(tab)

  huxtable::escape_contents(tab)[1, c(7, 11)] <- F
  huxtable::escape_contents(tab)[nr0:nr1, ] <- F

  huxtable::left_padding(tab) <-  5
  huxtable::right_padding(tab) <-  5

  # return(tab)
  return(huxtable::to_html(tab))
}

norsyss_kht_covid19_table <- function(data){
  tab <- copy(data$covid19$norsyss)
  setnames(tab, "n", "n_norsyss")
  tab[
    data$covid19$msis,
    on=c("location_code","yrwk"),
    n_msis := fhiplot::format_nor(n)
  ]

  tab[,pr100_norsyss := fhiplot::format_nor_perc_1(100*n_norsyss/consult_with_influenza)]
  tab[consult_with_influenza==0, pr100_norsyss := "0,0%"]
  tab[,n_norsyss:=fhiplot::format_nor(n_norsyss)]

  tab[,week_id := 1:.N,by=.(location_code)]

  tab_wide <- dcast.data.table(
    tab,
    location_code ~ week_id,
    value.var = c("pr100_norsyss","n_norsyss","n_msis")
  )
  tab_wide <- rbind(tab_wide[location_code=="norge"],tab_wide[location_code!="norge"])
  tab_wide[,location_name := get_location_name(location_code)]
  tab_wide[, location_code := NULL]
  setcolorder(tab_wide, "location_name")

  yrwks <- unique(tab[, c("week_id", "yrwk")])
  setorder(yrwks, week_id)

  ht <- huxtable::as_hux(tab_wide) %>%
    huxtable::add_colnames() %>%
    fhiplot::huxtable_theme_fhi_basic()

  ht[1, ] <- c(
    "Geografisk omr\u00E5de",
    yrwks$yrwk,
    yrwks$yrwk,
    yrwks$yrwk
  )

  ht <- huxtable::add_rows(ht, ht[1, ], after = 0)

  huxtable::escape_contents(ht)[, 2] <- FALSE

  ht <- huxtable::merge_cells(ht, 1:2, 1)

  ht <- huxtable::merge_cells(ht, 1, 2:5)
  ht[1, 2] <- "NorSySS<sup>12</sup> andel"

  ht <- huxtable::merge_cells(ht, 1, 6:9)
  ht[1, 6] <- "NorSySS<sup>2</sup> antall"

  ht <- huxtable::merge_cells(ht, 1, 10:13)
  ht[1, 10] <- "MSIS antall"

  huxtable::left_border(ht)[, c(2, 6, 10)] <- 5
  huxtable::left_border_style(ht)[, c(2, 6, 10)] <- "double"

  huxtable::align(ht) <- "center"

  nr0 <- nrow(ht) + 1
  ht <- huxtable::add_footnote(ht, glue::glue(
    "<sup>1</sup>Nevneren til andelen er totalt antall konsultasjoner i det samme geografiske omr{fhi::nb$aa}det.<br>",
    "<sup>2</sup>NorSySS er forkortelsen for Norwegian Syndromic Surveillance System og her refererer til ICPC-2 koden R991: covid-19 (mistenkt eller bekreftet)<br>",
  ), border = 0)
  nr1 <- nrow(ht)

  huxtable::escape_contents(ht)[1, ] <- F
  huxtable::escape_contents(ht)[nr0:nr1, ] <- F

  huxtable::left_padding(ht) <-  5
  huxtable::right_padding(ht) <-  5

  return(huxtable::to_html(ht))
}

ui_norsyss_kht_email_alert_function_factory <- function(location_codes, x_tags, yrwk, n_status = c("medium", "high")){
  force(location_codes)
  force(x_tags)
  force(yrwk)
  force(n_status)
  function(){
    retval <- list()
    for(tag in x_tags){
      x_location_codes <- tbl("results_norsyss_standard") %>%
        dplyr::filter(granularity_time == "week") %>%
        dplyr::filter(location_code %in% !!location_codes) %>%
        dplyr::filter(tag_outcome %in% !!tag) %>%
        dplyr::filter(yrwk %in% !!yrwk) %>%
        dplyr::filter(n_status %in% !!n_status) %>%
        dplyr::distinct(location_code) %>%
        dplyr::collect() %>%
        latin1_to_utf8()

      x_location_codes <- x_location_codes$location_code

      if(length(x_location_codes)==0){
        retval[[tag]] <- data.table()
      } else {
        retval[[tag]] <- tbl("results_norsyss_standard") %>%
          dplyr::filter(granularity_time == "week") %>%
          dplyr::filter(location_code %in% !!x_location_codes) %>%
          dplyr::filter(tag_outcome %in% !!tag) %>%
          dplyr::filter(yrwk %in% !!yrwk) %>%
          dplyr::collect() %>%
          latin1_to_utf8()
      }
    }

    retval
  }
}

ui_norsyss_kht_email_covid19_function_factory <- function(location_codes, yrwk){
  force(location_codes)
  force(yrwk)
  function(){
    retval <- list()

    retval$msis <- tbl("data_covid19_msis_by_time_location") %>%
      dplyr::filter(granularity_time == "week") %>%
      dplyr::filter(location_code %in% !!location_codes) %>%
      dplyr::filter(yrwk %in% !!yrwk) %>%
      dplyr::collect() %>%
      latin1_to_utf8()

    retval$norsyss <- tbl("data_norsyss") %>%
      dplyr::filter(granularity_time=="day") %>%
      dplyr::filter(location_code %in% !!location_codes) %>%
      dplyr::filter(age=="total") %>%
      dplyr::filter(yrwk %in% !!yrwk) %>%
      dplyr::filter(tag_outcome %in% "covid19_vk_ote") %>%
      dplyr::select(location_code, yrwk, n, consult_with_influenza) %>%
      dplyr::group_by(location_code,yrwk) %>%
      dplyr::summarize(n=sum(n), consult_with_influenza=sum(consult_with_influenza)) %>%
      dplyr::collect() %>%
      latin1_to_utf8()

    retval
  }
}

ui_norsyss_kht_email_plans <- function(){
  x_tags <- c("respiratoryexternal_vk_ot", "gastro_vk_ot")
  yrwk <- fhi::isoyearweek(lubridate::today()-seq(0,21,7)-1)

  #yrwk <- fhi::isoyearweek(lubridate::today()-seq(48,70,7))

  val <- tbl("datar_norsyss_kht_email") %>%
    dplyr::collect()
  setDT(val)

  # if it's not final, then restrict the email addresses
  if(!config$permissions$ui_norsyss_kht_email$is_final()){
    val <- val[
      email %in% c(
        "richardaubrey.white@fhi.no",
        "sykdomspulsen@fhi.no"
      )]
  }

  list_plan <- list()
  for(em in unique(val$email)){
    n_status <- c("medium", "high")
    if(em %in% c(
      "utbrudd@fhi.no"
    )) n_status <- c("high")

    list_plan[[length(list_plan)+1]] <- plnr::Plan$new()

    list_plan[[length(list_plan)]]$add_data(
      name = "alert",
      fn=ui_norsyss_kht_email_alert_function_factory(
        location_codes = val[email == em]$location_code,
        x_tags = x_tags,
        yrwk = yrwk,
        n_status = n_status
      )
    )

    list_plan[[length(list_plan)]]$add_data(
      name = "covid19",
      fn=ui_norsyss_kht_email_covid19_function_factory(
        location_codes = val[email == em]$location_code,
        yrwk = yrwk
      )
    )

    list_plan[[length(list_plan)]]$add_analysis(
      fn = ui_norsyss_kht_email,
      email = em,
      tag_outcome = x_tags,
      yrwk = yrwk,
      n_status = n_status
    )
  }

  return(list_plan)
}
