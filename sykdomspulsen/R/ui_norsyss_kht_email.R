#' ui_norsyss_kht_email
#' @param data a
#' @param argset a
#' @param schema a
#' @export
ui_norsyss_kht_email <- function(data, argset, schema) {
  if(FALSE){
    # GRY RUN THIS
    tm_run_task("datar_norsyss_kht_email")
    # tm_run_task("ui_norsyss_kht_email")
  }

  if(plnr::is_run_directly()){
    # GRY, START RUNNING HERE
    sc::tm_update_plans("ui_norsyss_kht_email")
    length(sc::config$tasks$list_task$ui_norsyss_kht_email$plans)

    index_plan <- 1
    data <- sc::tm_get_data("ui_norsyss_kht_email", index_plan=index_plan)
    argset <- sc::tm_get_argset("ui_norsyss_kht_email", index_plan=index_plan, index_argset = 1)
    schema <- sc::tm_get_schema("ui_norsyss_kht_email")

    argset$email <- "grmg@fhi.no"
  } else {
    # need this so that the email server doesn't die
    Sys.sleep(30)
  }

  email_subject <- glue::glue("Ukentlig oversikt, FHI data {lubridate::today()}")

  email_text_top <- glue::glue(
    "<b>Dette er en ukentlig oversikt over FHI data fra Sykdomspulsen for kommunehelsetjenesten</b><br>",
    "Under ser du tabeller med de geografiske omr{fhi::nb$aa}dene du har valgt med informasjon ",
    "om de siste tre ukene og denne uken ({fhi::nb$aa}r-ukenummer).<br>",
    "Du kan endre det geografiske omr{fhi::nb$aa}de ved {fhi::nb$aa} g{fhi::nb$aa} til 'Geografisk omr{fhi::nb$aa}de' i websiden.<br>",
    "Den siste uken som vises i tabellen er den n{fhi::nb$aa}v{fhi::nb$ae}rende uken og har derfor kun data fra mandag og tirsdag.<br><br>",

    "<u>Nytt fra Sykdomspulsen:</u><br>",
    "- Det er nå mulig {fhi::nb$aa} laste ned tallmaterialet bak den første grafen og modelleringsresultatene p{fhi::nb$aa} websiden.",
    " Det er også mulig {fhi::nb$aa} kopiere figurene p{fhi::nb$aa} ved {fhi::nb$aa} h{fhi::nb$oe}yreklikke p{fhi::nb$aa} bildet og velge 'kopier'.<br>",
    "- Du vil innen et par uker få en spørreundersøkelse fra Sykdomspulsen for kommunehelsetjenesten om websiden og de ukentlige mailene.",
    "Vi setter stor pris på om du kan svare på denne.<br><br>",

    "Mer informasjon og nyheter fra Sykdomspulsen finner du under tabellene. Mer data og grafer finnes p{fhi::nb$aa} websiden <a href='https://spuls.fhi.no'>https://spuls.fhi.no</a><br><br>",

    "Dersom dere har problemer med p{fhi::nb$aa}loggingen eller andre sp\u00F8rsm\u00E5l, vennligst send en mail til sykdomspulsen@fhi.no<br>"

  )

  email_text_bottom <- glue::glue(

    "<u>Tabellen med covid-19 viser</u> antall konsultasjoner hos lege og legevakt (NorSySS) ",
    "og antall bekreftede tilfeller registrert i MSIS.<br>",
    "For NorSySS data ble diagnosekoden R991: covid-19 (mistenkt eller bekreftet) ",
    "brukt mellom 06.03.2020 til 03.05.2020. Det ble en endring i covid-19 ICPC-2 diagnosekodene ",
    "04.05.2020 til R991: covid-19 (mistenkt/sannsynlig) og R992: covid-19 (bekreftet). ",
    "For uke 18 viser R991 data for covid-19 (mistenkt eller bekreftet),",
    "mens R992 ikke var opprettet enda s{fhi::nb$aa} det er ingen data for denne uken.",
    "For uke 19, 20 og 21 brukes R991 for covid-19 (mistenkt/sannsynlig) og R992",
    "for covid-19 (bekreftet).  Sykdomspulsen har data for konsultasjoner,",
    "ikke personer s{fhi::nb$aa} for eksempel en person med bekreftet diagnose kan telles",
    "flere ganger hvis personen kontakter legen flere ganger.<br>",

    "For covid-19 signaler har vi valgt {fhi::nb$aa} holde R991 og R992 samlet for {fhi::nb$aa} f{fhi::nb$aa} mest",
    "mulig enhetlig data for hele tidsperioden. Vi vurderer {fhi::nb$aa} endre dette etterhvert.<br>",
    "Signalsystemet bruker gjennomsnittet med 95% konfidensintervall av uke ",
    "{fhi::isoyearweek(lubridate::today()-21-1)} og {fhi::isoyearweek(lubridate::today()-14-1)} ",
    "som en basis for {fhi::nb$aa} beregne terskelverdi for uke ",
    "{fhi::isoyearweek(lubridate::today()-7-1)} og {fhi::isoyearweek(lubridate::today()-0-1)}.<br>",
    "Dette er kun et signal og trenger ikke {fhi::nb$aa} v{fhi::nb$ae}re noe man m{fhi::nb$aa} gj{fhi::nb$oe}re noe med,",
    " men det kan v{fhi::nb$ae}re en fordel {fhi::nb$aa} sjekke websiden og f{fhi::nb$oe}lge med.<br><br>",

    "<u>Tabellene med mage-tarminfeksjoner og luftveisinfeksjoner har kun NorSySS data og viser disse verdiene:</u><br>",
    "Antall konsultasjoner: Dette er ikke antall personer da en person kan telles flere ganger om den ",
    "g{fhi::nb$aa}r til legen flere ganger.<br>",
    "Flere enn normalt: Differansen mellom antall registrerte og {fhi::nb$oe}vre grense for normalt antall (95% prediksjonsintervall)<br>",
    "Z-verdi: antall ganger standardavvik ut fra forventet antall konsultasjoner.<br>",
    "Bl{fhi::nb$aa}tt felt: Antall konsultasjoner er som forventet (Z-verdi < 2)<br>",
    "Gult felt: Antall konsultasjoner er h{fhi::nb$oe}yere enn forventet (Z-verdi mellom 2 og 4 og minst 3 konsultasjoner)<br>",
    "R{fhi::nb$oe}dt felt: Antall konsultasjoner er betydelig h{fhi::nb$oe}yere enn forventet (Z-verdi >= 4 og minst 4 konsultasjoner)<br>",
    "Varselet er en informasjon om at det kan v{fhi::nb$ae}re noe som b{fhi::nb$oe}r f{fhi::nb$oe}lges opp i din kommune eller i et fylke. ",
    "Det anbefales {fhi::nb$aa} g{fhi::nb$aa} inn i Sykdomspulsen websiden og sjekke det ut. Varselet beh{fhi::nb$oe}ver ikke {fhi::nb$aa} bety noe alvorlig.<br><br>",

    "Sykdomspulsen kan i noen tilfeller generere et signal selv om det bare er en eller to konsultasjoner for et symptom/sykdom. ",
    "Dette sees som oftest i sm{fhi::nb$aa} kommuner der det vanligvis ikke er mange konsultasjoner. For ikke {fhi::nb$aa} bli forstyrret ",
    "av slike signaler har vi n{fhi::nb$aa} lagt inn en nedre grense for gult signal p{fhi::nb$aa} p{fhi::nb$aa} minst tre konsultasjoner og en nedre grense for ",
    "r{fhi::nb$oe}dt signal p{fhi::nb$aa} minst fire konsultasjoner.<br><br>",

    "Vi vet at det er mange som ikke liker v{fhi::nb$aa}r p{fhi::nb$aa}loggingsl{fhi::nb$oe}sning med egen ",
    "kommune(over)lege(n) e-postadresse. Vi synes dette er synd, men ser ingen annen utvei s{fhi::nb$aa} lenge ",
    "det ikke finnes et nasjonalt autorisert register over kommuneleger/kommuneoverleger som fortl{fhi::nb$oe}pende ",
    "blir oppdatert med endringer i kommunene. Folkehelseinstituttet har ikke kapasitet til {fhi::nb$aa} forvalte ",
    "et slikt register p{fhi::nb$aa} vegne av kommunene, men samarbeider gjerne med kommunene og KS for {fhi::nb$aa} ",
    "f{fhi::nb$aa} til et slikt register som vil v{fhi::nb$ae}re viktig i fremtidig beredskapsarbeid med ",
    "{fhi::nb$oe}kt digitalisering. Dette er bakgrunnen for at vi er blitt n{fhi::nb$oe}dt til {fhi::nb$aa} ",
    "foresl{fhi::nb$aa} denne l{fhi::nb$oe}sningen med rollebaserte mailkontoer i den enkelte kommune. Dermed er ",
    "det kommunene selv som m{fhi::nb$aa} p{fhi::nb$aa}se at den som enhver tid innehar rollen ogs{fhi::nb$aa} har ",
    "tilgang  til denne e-postadressen.<br><br>",


    "Ta kontakt med oss om du har sp{fhi::nb$oe}rsm{fhi::nb$aa}l eller om det er noe som er uklart p{fhi::nb$aa} sykdomspulsen@fhi.no.<br><br>",

    "Vi {fhi::nb$oe}nsker ogs{fhi::nb$aa} tilbakemelding p{fhi::nb$aa} om dette varselet er nyttig for dere eller ikke.<br><br>",

    "Hilsen:<br><br>",

    "Sykdomspulsen ved Folkehelseinstituttet<br>"

  )

  # here we start to make the email
  email_text <- email_text_top

  # covid19 overview
  email_text <- paste0(email_text, "<br><hr width='60%' size='5px' noshade><br>\n")

  email_text <- paste0(
    email_text,
    "<h2>NorSySS + MSIS: Covid-19 oversikt</h2>",
    norsyss_kht_covid19_overview_table(data = data)
  )

  # covid19 alerts
  email_text <- paste0(email_text, "<br><hr width='60%' size='5px' noshade><br>\n")

  email_text <- paste0(
    email_text,
    glue::glue(
      "<h2>NorSySS + MSIS: Signaler for covid-19</h2>",

      "Dersom du ser en tabell under, betyr det at det i det geografiske omr{fhi::nb$aa}det du har valgt, har ",
      "v{fhi::nb$ae}rt en eller flere uker med en {fhi::nb$oe}kning som er statistisk signifikant.<br>",
      "Vi bruker her gjennomsnittet med 95% konfidensintervall av uke ",
      "{fhi::isoyearweek(lubridate::today()-21-1)} og {fhi::isoyearweek(lubridate::today()-14-1)} ",
      "som en basis for {fhi::nb$aa} beregne terskelverdi for uke ",
      "{fhi::isoyearweek(lubridate::today()-7-1)} og {fhi::isoyearweek(lubridate::today()-0-1)}.<br><br>",
    ),
    norsyss_kht_covid19_obs_table(data = data)
  )

  # general norsyss alerts
  email_text <- paste0(email_text, "<br><hr width='60%' size='5px' noshade><br>\n")

  for(tag_outcome in argset$tag_outcome){
    email_text <- paste0(
      email_text,
      "<h2>NorSySS: Varsler om ",stringr::str_to_lower(config$def$norsyss$long_names[[tag_outcome]]),"</h2>",
      norsyss_kht_obs_table(
        results = data$alert[[tag_outcome]],
        tag_outcome = tag_outcome
      )
    )
  }

  email_text <- paste0(email_text, "<br><hr width='60%' size='5px' noshade><br>")

  # add in bottom text
  email_text <- paste0(email_text, email_text_bottom)

  argset$email

  bcc <- NULL
  if(sc::config$is_production) bcc <- "sykdomspulsen@fhi.no"
  mailr(
    subject = e_subject(
      email_subject,
      is_final = sc::config$permissions$ui_norsyss_kht_email$is_final()
    ),
    html = email_text,
    to = argset$email,
    bcc = bcc,
    is_final = sc::config$permissions$ui_norsyss_kht_email$is_final()
  )
  # END OF FUNCTION
  # GRY DONT RUN BEYOND HERE
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

norsyss_kht_covid19_overview_table <- function(data){
  tab <- copy(data$covid19$norsyss_separate)
  setnames(tab, "n", "n_norsyss")

  tab[,pr100_norsyss := fhiplot::format_nor_perc_1(100*n_norsyss/consult_with_influenza)]
  tab[consult_with_influenza==0, pr100_norsyss := "0,0%"]
  tab[,n_norsyss:=fhiplot::format_nor(n_norsyss)]

  tab <- dcast.data.table(
    tab,
    location_code + yrwk ~ tag_outcome,
    value.var = c("pr100_norsyss", "n_norsyss")
  )
  tab

  tab[
    data$covid19$msis,
    on=c("location_code","yrwk"),
    n_msis := fhiplot::format_nor(n)
    ]

  setorder(tab,location_code,yrwk)
  tab[,week_id := 1:.N,by=.(location_code)]

  tab_wide <- dcast.data.table(
    tab,
    location_code ~ week_id,
    value.var = c(
      "n_norsyss_covid19_r991_vk_ote",
      "pr100_norsyss_covid19_r991_vk_ote",
      "n_norsyss_covid19_r992_vk_ote",
      "pr100_norsyss_covid19_r992_vk_ote",
      "n_msis"
    )
  )
  tab_wide <- rbind(tab_wide[location_code=="norge"],tab_wide[location_code!="norge"])
  tab_wide[,location_name := get_location_name(location_code)]
  tab_wide[, location_code := NULL]
  tab_wide <- unique(tab_wide)
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
    yrwks$yrwk,
    yrwks$yrwk,
    yrwks$yrwk
  )

  ht <- huxtable::add_rows(ht, ht[1, ], after = 0)
  ht <- huxtable::add_rows(ht, ht[1, ], after = 0)

  huxtable::escape_contents(ht)[1:2, ] <- FALSE

  ht <- huxtable::merge_cells(ht, 1:3, 1)

  # top row
  ht <- huxtable::merge_cells(ht, 1, 2:9)
  ht[1, 2] <- "NorSySS Konsultasjoner R991: Covid-19 (mistenkt/sannsynlig)<sup>1</sup>"

  ht <- huxtable::merge_cells(ht, 1, 10:17)
  ht[1, 10] <- "NorSySS Konsultasjoner R992: Covid-19 (bekreftet) <sup>1</sup>"

  ht <- huxtable::merge_cells(ht, 1, 18:21)
  ht[1, 18] <- "MSIS Tilfeller"

  # second row
  ht <- huxtable::merge_cells(ht, 2, 2:5)
  ht[2, 2] <- "Antall"
  ht <- huxtable::merge_cells(ht, 2, 6:9)
  ht[2, 6] <- "Andel<sup>2</sup>"

  ht <- huxtable::merge_cells(ht, 2, 10:13)
  ht[2, 10] <- "Antall"
  ht <- huxtable::merge_cells(ht, 2, 14:17)
  ht[2, 14] <- "Andel<sup>2</sup>"

  ht <- huxtable::merge_cells(ht, 2, 18:21)
  ht[2, 18] <- "Antall"

  # border styles
  huxtable::left_border(ht)[, seq(2,21,4)] <- 5
  huxtable::left_border_style(ht)[, seq(2,21,4)] <- "double"

  huxtable::align(ht) <- "center"

  nr0 <- nrow(ht) + 1
  ht <- huxtable::add_footnote(ht, glue::glue(
    "<sup>1</sup>NorSySS er forkortelsen for Norwegian Syndromic Surveillance System og refererer her til konsultasjoner hos lege og legevakt med ICPC-2 kodene R991 og R992.",
    "<sup>2</sup>Nevneren til andelen er totalt antall konsultasjoner i det samme geografiske omr{fhi::nb$aa}det.<br>",
  ), border = 0)
  nr1 <- nrow(ht)

  huxtable::escape_contents(ht)[1, ] <- F
  huxtable::escape_contents(ht)[nr0:nr1, ] <- F

  huxtable::left_padding(ht) <-  5
  huxtable::right_padding(ht) <-  5

  return(huxtable::to_html(ht))
}

norsyss_kht_covid19_obs_table <- function(data){
  tab <- areas_at_risk(data)

  if (is.null(tab)) {
    return(sprintf("<b>%s:</b> <span style='color:red;text-decoration:underline;'>Ingen varsler registrert</span><br><br><br>"))
  }

  ht <- areas_at_risk_ht(tab)

  return(huxtable::to_html(ht))
}

areas_at_risk_ht <- function(tab){
  msis_index_hig <- which(tab$msis_n > tab$msis_threshold & tab$variable %in% 3:4)
  norsyss_index_hig <- which(tab$norsyss_pr100 > tab$norsyss_pr100_threshold & tab$variable %in% 3:4)

  ht <- huxtable::hux(
    "Omr\u00E5de"=tab$location_name,
    "Uke"=tab$yrwk,
    "Tilfeller"=tab$pretty_msis_n,
    "Terskel"=tab$pretty_msis_threshold,
    "Konsultasjoner"=tab$pretty_norsyss_n,
    "Andel<sup>3</sup>"=tab$pretty_norsyss_pr100,
    "Terskel"=tab$pretty_norsyss_pr100_threshold
  )%>%
    huxtable::add_colnames() %>%
    fhiplot::huxtable_theme_fhi_basic()
  ht <- huxtable::set_background_color(ht, huxtable::evens, huxtable::everywhere, "#FFFFFF")

  if (length(msis_index_hig) > 0) huxtable::background_color(ht)[msis_index_hig+1, 3] <- fhiplot::warning_color[["hig"]]
  if (length(norsyss_index_hig) > 0) huxtable::background_color(ht)[norsyss_index_hig+1, 6] <- fhiplot::warning_color[["hig"]]


  ht <- huxtable::add_rows(ht, ht[1, ], after = 0)

  ht <- huxtable::merge_cells(ht, 1:2, 1)
  ht <- huxtable::merge_cells(ht, 1:2, 2)

  ht <- huxtable::merge_cells(ht, 1, 3:4)
  ht[1, 3] <- "MSIS"

  ht <- huxtable::merge_cells(ht, 1, 5:7)
  ht[1, 5] <- "NorSySS<sup>1</sup> (R991+R992)<sup>1</sup>"

  huxtable::left_border(ht)[, c(3, 5)] <- 2
  #huxtable::left_border_style(ht)[, c(3, 5)] <- "double"

  ht <- huxtable::merge_repeated_rows(ht, huxtable::everywhere, 1)

  nr0 <- nrow(ht) + 1
  ht <- huxtable::add_footnote(ht, glue::glue(
    "<sup>1</sup>NorSySS er forkortelsen for Norwegian Syndromic Surveillance System og refererer her til konsultasjoner hos lege og legevakt. <br>",
    "<sup>2</sup>ICPC-2 kodene R991 og R992 er i denne tabellen samlet (covid-19, mistenkt. sannsynlig og bekreftet).<br>",
    "<sup>3</sup>Nevneren til andelen er totalt antall konsultasjoner i det samme geografiske omr{fhi::nb$aa}det.<br>",
  ), border = 0)
  nr1 <- nrow(ht)

  huxtable::escape_contents(ht)[1:2, ] <- F
  huxtable::escape_contents(ht)[nr0:nr1, ] <- F
  huxtable::width(ht) <- 1

  ht
}

ui_norsyss_kht_email_alert_function_factory <- function(location_codes, x_tags, yrwk, n_status = c("medium", "high")){
  force(location_codes)
  force(x_tags)
  force(yrwk)
  force(n_status)
  function(){
    retval <- list()
    for(tag in x_tags){
      x_location_codes <- sc::tbl("results_norsyss_standard") %>%
        dplyr::filter(granularity_time == "week") %>%
        dplyr::filter(location_code %in% !!location_codes) %>%
        dplyr::filter(tag_outcome %in% !!tag) %>%
        dplyr::filter(yrwk %in% !!yrwk) %>%
        dplyr::filter(n_status %in% !!n_status) %>%
        dplyr::distinct(location_code) %>%
        dplyr::collect() %>%
        sc::latin1_to_utf8()

      x_location_codes <- x_location_codes$location_code

      if(length(x_location_codes)==0){
        retval[[tag]] <- data.table()
      } else {
        retval[[tag]] <- sc::tbl("results_norsyss_standard") %>%
          dplyr::filter(granularity_time == "week") %>%
          dplyr::filter(location_code %in% !!x_location_codes) %>%
          dplyr::filter(tag_outcome %in% !!tag) %>%
          dplyr::filter(yrwk %in% !!yrwk) %>%
          dplyr::collect() %>%
          sc::latin1_to_utf8()
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

    retval$msis <- sc::tbl("data_covid19_msis_by_time_location") %>%
      dplyr::filter(granularity_time == "week") %>%
      dplyr::filter(location_code %in% !!location_codes) %>%
      dplyr::filter(yrwk %in% !!yrwk) %>%
      dplyr::collect() %>%
      sc::latin1_to_utf8()

    retval$norsyss_combined <- sc::tbl("data_norsyss") %>%
      dplyr::filter(granularity_time=="day") %>%
      dplyr::filter(location_code %in% !!location_codes) %>%
      dplyr::filter(age=="total") %>%
      dplyr::filter(yrwk %in% !!yrwk) %>%
      dplyr::filter(
        tag_outcome %in% c(
          "covid19_vk_ote"
        )
      )%>%
      dplyr::select(tag_outcome, location_code, yrwk, n, consult_with_influenza) %>%
      dplyr::group_by(tag_outcome, location_code,yrwk) %>%
      dplyr::summarize(n=sum(n), consult_with_influenza=sum(consult_with_influenza)) %>%
      dplyr::collect() %>%
      sc::latin1_to_utf8()

    retval$norsyss_separate <- sc::tbl("data_norsyss") %>%
      dplyr::filter(granularity_time=="day") %>%
      dplyr::filter(location_code %in% !!location_codes) %>%
      dplyr::filter(age=="total") %>%
      dplyr::filter(yrwk %in% !!yrwk) %>%
      dplyr::filter(
        tag_outcome %in% c(
          "covid19_r991_vk_ote",
          "covid19_r992_vk_ote"
        )
      )%>%
      dplyr::select(tag_outcome, location_code, yrwk, n, consult_with_influenza) %>%
      dplyr::group_by(tag_outcome, location_code,yrwk) %>%
      dplyr::summarize(n=sum(n), consult_with_influenza=sum(consult_with_influenza)) %>%
      dplyr::collect() %>%
      sc::latin1_to_utf8()

    retval
  }
}

ui_norsyss_kht_email_plans <- function(){
  x_tags <- c("respiratoryexternal_vk_ot", "gastro_vk_ot")
  yrwk <- fhi::isoyearweek(lubridate::today()-seq(0,21,7)-2)

  #yrwk <- fhi::isoyearweek(lubridate::today()-seq(48,70,7))

  val <- sc::tbl("datar_norsyss_kht_email") %>%
    dplyr::collect()
  setDT(val)

  # if it's not final, then restrict the email addresses
  if(!sc::config$permissions$ui_norsyss_kht_email$is_final()){
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
