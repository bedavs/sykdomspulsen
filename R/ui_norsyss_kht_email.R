ui_norsyss_kht_email <- function(data, argset, schema) {
  if(FALSE){
    tm_run_task("ui_norsyss_kht_email")

    tm_update_plans("ui_norsyss_kht_email")

    data <- tm_get_data("ui_norsyss_kht_email", index_plan=1)
    argset <- tm_get_argset("ui_norsyss_kht_email", index_plan=1, index_argset = 1)
    schema <- tm_get_schema("ui_norsyss_kht_email")
  }

  email_subject <- glue::glue("OBS varsel fra Sykdomspulsen {lubridate::today()}")

  email_text_top <- glue::glue(
    "<b>Dette er et OBS varsel fra Sykdomspulsen for kommunehelsetjenesten, logg dere p\u00E5 her! https://spuls.fhi.no</b><br><br>",
    "Dersom dere har problemer med p{fhi::nb$aa}loggingen eller andre sp\u00F8rsm\u00E5l, vennligst send en mail til sykdomspulsen@fhi.no<br><br><br>"
  )

  email_text_bottom <- glue::glue(
    "<b>Sykdomspulsen til kommunehelsetjenesten</b> f\u00E5r et varsel p\u00E5",
    "e-post dersom deres kommune eller et fylke har flere konsultasjoner enn ",
    "forventet av henholdsvis mage-tarminfeksjoner eller luftveisinfeksjoner en av de siste 4 ukene.<br><br>",

    "Tabellen under viser informasjon om stedet der det er mer enn forventet ",
    "antall meldte tilfeller og aldersgruppe, antallet tilfeller flere enn normalt ",
    "og en verdi som viser hvor ekstremt signalet er (z-score). ",
    "Hvis z-scoret er mellom 2 og 4 er antallet konsultasjoner h{fhi::nb$oe}yere enn ",
    "forventet og man vil se at det ligger i gul sone p\u00E5 Sykdomspulsen websiden. ",
    "Dersom z-scoret er over 4 er antallet konsultasjoner betydelig h\u00F8yere ",
    "enn forventet og man vil se at det ligger i r\u00F8d sone p\u00E5 Sykdomspulsen websiden.<br><br>",

    "I tabellen over er det en link til stedet der du kan se OBS varselet i Sykdomspulsen. ",
    "Denne virker ikke dersom den \u00E5pnes i Internet explorer. Dersom du har problemer ",
    "med linken kan du h\u00F8yreklikke p\u00E5 koblingen og kopiere den for deretter ",
    "\u00E5 lime den inn i for eksempel Google chrome eller en annen nettleser. ",
    "Du kan ogs\u00E5 logge deg inn p\u00E5 Sykdomspulsen p\u00E5 vanlig m\u00E5te ",
    "(<a href='http://sykdomspulsen.fhi.no/lege123/'>http://sykdomspulsen.fhi.no/lege123/</a>) ",
    "og selv finne aktuell kommune eller fylke.<br><br>",

    "Varselet er en informasjon om at det kan v\u00E6re noe som b\u00F8r f\u00F8lges ",
    "opp i din kommune eller i et fylke. Det anbefales \u00E5 g\u00E5 inn i Sykdomspulsen ",
    "websiden og sjekke det ut. Varselet beh\u00F8ver ikke \u00E5 bety noe alvorlig.<br><br>",

    "Nederst i denne mailen viser vi hvilke(n) kommune(r) du f\u00E5r varsel for. ",
    "Alle f\u00E5r varsel for alle fylker og hele Norge. Dersom det ikke st\u00E5r noen ",
    "kommune i tabellen mangler vi det for deg og vi ber deg kontakte oss for \u00E5 ",
    "f\u00E5 satt opp riktig kommune(r).<br><br>",

    "Sykdomspulsen kan i noen tilfeller generere et OBS varsel selv om det bare er en ",
    "eller to konsultasjoner for et symptom/sykdom. Dette sees som oftest i sm\u00E5 ",
    "kommuner der det vanligvis ikke er mange konsultasjoner. For ikke \u00E5 bli forstyrret ",
    "av slike signaler har vi n\u00E5 lagt inn en nedre grense for gult signal p\u00E5 to ",
    "konsultasjoner og en nedre grense for r\u00F8dt signal p\u00E5 tre konsultasjoner.<br><br>",

    "Ta kontakt med oss om du har sp\u00F8rsm\u00E5l eller om det er noe som er uklart ",
    "p\u00E5 sykdomspulsen@fhi.no.<br><br>",

    "Send oss ogs\u00E5 en tilbakemelding dersom du \u00F8nsker varsel for andre kommuner ",
    "eller fylker.<br><br>",

    "Vi \u00F8nsker ogs\u00E5 tilbakemelding p\u00E5 om dette varselet er nyttig for ",
    "dere eller ikke.<br><br>",

    "<b> NB! Oppdatering av Sykdomspulsen vil n\u00E5 skje p\u00E5 onsdager istedenfor ",
    "tirsdager. H\u00E5per dette ikke vil for\u00E5rsake noen ulemper for dere.</b> <br><br>",

    "Hilsen:<br><br>",
    "Sykdomspulsen ved Folkehelseinstituttet<br>",
    "v/Gry M Gr\u00F8neng (prosjektleder) og Richard White (statistiker og webansvarlig)<br><br>"
  )

  email_text <- email_text_top

  email_text <- paste0(email_text, "<hr width='60%' size='5px' noshade><br>\n")

  # include outbreaks
  for(tag_outcome in argset$tag_outcome){
    email_text <- paste0(
      email_text,
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
    return(sprintf("<b>%s:</b> <span style='color:red;text-decoration:underline;'>Ingen utbrudd registrert</span><br><br><br>", tag_pretty))
  }

  setorder(r_long, tag_outcome, yrwk)
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
  setorder(r_wide, -n_zscore_4)
  r_wide[,location_name := get_location_name(location_code)]

  yrwks <- unique(r_long[, c("week_id", "yrwk")])
  setorder(yrwks, week_id)

  tab <- huxtable::huxtable(
    Syndrom = r_wide$tag_pretty,
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
    column_to_color <- c(3, 7, 11) + i
    index_low <- which(r_wide[[z]] == "normal") + 1
    index_med <- which(r_wide[[z]] == "medium") + 1
    index_hig <- which(r_wide[[z]] == "high") + 1

    if (length(index_low) > 0) huxtable::background_color(tab)[index_low, column_to_color] <- fhiplot::warning_color[["low"]]
    if (length(index_med) > 0) huxtable::background_color(tab)[index_med, column_to_color] <- fhiplot::warning_color[["med"]]
    if (length(index_hig) > 0) huxtable::background_color(tab)[index_hig, column_to_color] <- fhiplot::warning_color[["hig"]]
  }

  tab[1, ] <- c(
    "Syndrom",
    "Geografisk omr\u00E5de",
    "Alder",
    yrwks$yrwk,
    yrwks$yrwk,
    yrwks$yrwk
  )

  tab <- huxtable::add_rows(tab, tab[1, ], after = 0)

  huxtable::escape_contents(tab)[, 2] <- FALSE

  tab <- huxtable::merge_cells(tab, 1:2, 1)
  tab <- huxtable::merge_cells(tab, 1:2, 2)
  tab <- huxtable::merge_cells(tab, 1:2, 3)

  tab <- huxtable::merge_cells(tab, 1, 4:7)
  tab[1, 4] <- "Meldte tilfeller"

  tab <- huxtable::merge_cells(tab, 1, 8:11)
  tab[1, 8] <- "Flere enn normalt<sup>1</sup>"

  tab <- huxtable::merge_cells(tab, 1, 12:15)
  tab[1, 12] <- "Z-verdi<sup>3</sup>"

  huxtable::left_border(tab)[, c(4, 8, 12)] <- 5
  huxtable::left_border_style(tab)[, c(4, 8, 12)] <- "double"

  huxtable::align(tab) <- "center"

  nr0 <- nrow(tab) + 1
  tab <- huxtable::add_footnote(tab, glue::glue(
    "<sup>1</sup>Differansen mellom antall registrete og {fhi::nb$oe}vre grense for normalt antall<sup>2</sup><br>",
    "<sup>2</sup>95% prediksjonsintervall<br>",
    "<sup>3</sup>Z-verdi: antall ganger standardaviket verdien er fra forventet antall konsultasjoner<br>",
    "<sup>3</sup>Z-verdi mellom 2 og 4 og flere enn 2,5 meldte tilfeller indikerer at det er et h{fhi::nb$oe}yere antall meldte tilfeller enn normalt (vist som gul)<br>",
    "<sup>3</sup>Z-verdi >= 4 og flere enn 3 meldte tilfeller indikerer at det er et betydlig h{fhi::nb$oe}yere antall meldte tilfeller enn normalt (vist som r{fhi::nb$oe}d)<br>",
  ), border = 0)
  nr1 <- nrow(tab)

  huxtable::escape_contents(tab)[1, c(8, 12)] <- F
  huxtable::escape_contents(tab)[nr0:nr1, ] <- F

  huxtable::left_padding(tab) <-  5
  huxtable::right_padding(tab) <-  5

  # return(tab)
  return(huxtable::to_html(tab))
}

ui_norsyss_kht_email_function_factory <- function(location_codes, x_tags, yrwk, n_status = c("medium", "high")){
  force(location_codes)
  force(x_tags)
  force(yrwk)
  force(n_status)
  function(){
    retval <- list()
    for(tag in x_tags){
      x_location_codes <- tbl("results_norsyss_standard") %>%
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

ui_norsyss_kht_email_plans <- function(){
  x_tags <- c("respiratoryexternal_vk_ot", "gastro_vk_ot")
  yrwk <- fhi::isoyearweek(lubridate::today()-seq(0,21,7))

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
      fn=ui_norsyss_kht_email_function_factory(
        location_codes = val[email == em]$location_code,
        x_tags = x_tags,
        yrwk = yrwk,
        n_status = n_status
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
