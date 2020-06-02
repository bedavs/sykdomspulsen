#' data_pre_norsyss
#' @param data a
#' @param argset a
#' @param schema a
#' @export
data_pre_norsyss <- function(data, argset, schema){
  # tm_run_task("data_pre_norsyss")

  if(plnr::is_run_directly()){
    data <- sc::tm_get_data("data_pre_norsyss")
    argset <- sc::tm_get_argset("data_pre_norsyss")
    schema <- sc::tm_get_schema("data_pre_norsyss")
  }

  argset_extra <- sc::tm_get_argset("data_norsyss")

  if(!identical(
    sort(argset_extra$syndromes$tag_input),
    sort(argset_extra$syndromes$tag_input)
  )){
    argset$date_from <- "2006-01-02"
  } else if(!"data_norsyss" %in% sc::list_tables()){
    argset$date_from <- "2006-01-02"
  } else {
    date_min <- sc::tbl("data_norsyss") %>%
      dplyr::summarize(date_min = min(date)) %>%
      dplyr::collect()
    date_min <- date_min$date_min
    if(date_min != "2006-01-02"){
      date_min <- "2006-01-02"
    } else {
      date_max <- sc::tbl("data_norsyss") %>%
        dplyr::summarize(date_max = max(date)) %>%
        dplyr::collect()
      date_max <- date_max$date_max
      date_min <-  date_max-365*2
      year_min <- fhi::isoyear_n(date_min)
      date_min <- fhidata::days[year==year_min][1]$mon
    }
    argset$date_from <- date_min
  }

  message(glue::glue("Downloading from {argset$date_from}"))
  norsyss_fetch_raw_data_and_aggregate(
    date_from = argset$date_from,
    date_to = lubridate::today()-1,
    folder = sc::path("input", "sykdomspulsen_norsyss_input", create_dir = TRUE),
    overwrite_file=FALSE,
    diags = argset$diags
  )
  get_n_doctors(sc::path("input", "sykdomspulsen_norsyss_input"))
  return(TRUE)
}



takstkoder <- list(
  "11ad" = "oppmote",
  "11ak" = "oppmote",
  "1ad" = "telefonkontakt",
  "1ak" = "telefonkontakt",
  "1bd" = "telefonkontakt",
  "1be" = "ekonsultasjon", #
  "1bk" = "telefonkontakt",
  "1g" = "telefonkontakt",
  "1h" = "telefonkontakt",
  "2ad" = "oppmote",
  "2ae" = "ekonsultasjon", # this used to be telefonkontakt -- changed on 2020-04-05
  "2ak" = "oppmote",
  "2fk" = "oppmote"
)


# NAV Kommune nummer til FREG
# Some municip numbers received by KUHR do not match the expected
# numbers from folkeregistret. This table translates between them

# Any other municip numbers not in config for sykdomspulsen will be set to 9999
#Bydels number also exist for these codes (see docoumentation)
nav_to_freg <- list(
  "312" = 301,
  "313" = 301,
  "314" = 301,
  "315" = 301,
  "316" = 301,
  "318" = 301,
  "319" = 301,
  "321" = 301,
  "326" = 301,
  "327" = 301,
  "328" = 301,
  "330" = 301,
  "331" = 301,
  "334" = 301,
  "335" = 301,
  "1161" = 1103,
  "1162" = 1103,
  "1164" = 1103,
  "1165" = 1103,
  "1202" = 1201,
  "1203" = 1201,
  "1204" = 1201,
  "1205" = 1201,
  "1206" = 1201,
  "1208" = 1201,
  "1209" = 1201,
  "1210" = 1201,
  "1603" = 301,
  "1604" = 1601,
  "1605" = 1601,
  "1607" = 1601
)

nav_to_freg_bydel <- c(
  "312" = 30105,
  "313"	= 30104,
  "314"	= 30103,
  "315"	= 30102,
  "316"	= 30101,
  "318"	= 30114,
  "319"	= 30115,
  "321"	= 30113,
  "326"	= 30112,
  "327"	= 30111,
  "328"	= 30110,
  "330"	= 30109,
  "331"	= 30108,
  "334"	= 30107,
  "335"	= 30106,
  "1161" = 110303,
  "1162" = 110301,
  "1164" = 110306,
  "1165" = 110304,
  "1202" = 120103,
  "1203" = 120108,
  "1204" = 120101,
  "1205" = 120104,
  "1206" = 120105,
  "1208" = 120107,
  "1209" = 120102,
  "1210" = 120106
)
nav_to_freg_bydel <- data.table(
  nav= as.numeric(names(nav_to_freg_bydel)),
  freg = nav_to_freg_bydel
)


# norsyss_fetch_raw_data_and_aggregate
norsyss_fetch_raw_data_and_aggregate <- function(
  date_from = "2018-01-01",
  date_to = lubridate::today(),
  folder,
  overwrite_file = FALSE,
  diags,
  ...) {
  file_name <- glue::glue("norsyss_{lubridate::today()}.txt")
  file_temp <- fs::path(fhi::temp_dir(), file_name)
  file_permanent <- fs::path(folder, file_name)

  if (overwrite_file == FALSE) {
    if (file.exists(file_permanent)) {
      x <- fread(file_permanent)
      max_date <- as.Date(max(x$date, na.rm = T))
      # as long as last date in the file is within 2 days of the requested date
      if (abs(as.numeric(difftime(date_to, max_date, units = "days"))) <= 2) {
        message("file already exists! exiting...")
        return()
      }
    }
  }

  db <- RODBC::odbcDriverConnect("driver={ODBC Driver 17 for SQL Server};server=dm-prod;database=SykdomspulsenAnalyse; trusted_connection=yes")

  # calculate dates
  datesToExtract <- data.table(from = seq(as.Date(date_from), by = "month", length.out = 300), to = seq(as.Date(date_from), by = "month", length.out = 301)[-1] - 1)
  # Remove future dates
  datesToExtract <- datesToExtract[from <= date_to]

  for (i in 1:nrow(datesToExtract)) {
    cat(i, "/", nrow(datesToExtract), "\n")

    command <- paste0(
      "select Id,Diagnose,PasientAlder,PasientKommune,BehandlerKommune,Konsultasjonsdato,Takst,Praksis from Konsultasjon join KonsultasjonDiagnose on Id=KonsultasjonId join KonsultasjonTakst on Id=KonsultasjonTakst.KonsultasjonId where Konsultasjonsdato >='",
      datesToExtract[i]$from,
      "' AND Konsultasjonsdato <= '",
      datesToExtract[i]$to,
      "'"
    )
    d <- RODBC::sqlQuery(db, command)
    d <- data.table(d)
    # taskt 1be should only apply to R991
    d <- d[!(Diagnose!="R991" & Takst=="1be")]
    d <- norsyss_aggregate_raw_data(d, diags = diags)
    if (i == 1) {
      utils::write.table(d, file_temp, sep = "\t", row.names = FALSE, col.names = TRUE, append = FALSE)
    } else {
      utils::write.table(d, file_temp, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE)
    }
  }
  #pb$terminate()
  system(glue::glue("mv {file_temp} {file_permanent}"))
}

norsyss_aggregate_raw_data <- function(d, diags) {
  . <- BehandlerKommune <- Diagnose <- Id <-
    Konsultasjonsdato <- Kontaktype <- PasientAlder <-
    Praksis <- Takst <- age <- consult <- from <-
    municip <- n_diff <- pb <- NULL

  for (i in seq_along(diags)) {
    d[, (names(diags)[i]) := 0]
    d[Diagnose %in% diags[[i]], (names(diags)[i]) := 1]
  }

  ### Praksis

  d[
    Praksis %in% c(
      "Fastl\u00F8nnet",
      "Fastlege"
    ),
    Praksis := "legekontor"
    ]
  d[
    Praksis %in% c(
      "kommunal legevakt",
      "Legevakt"
    ),
    Praksis := "legevakt"
    ]
  d[
    Praksis %in% c(
      "Annet"
    ),
    Praksis := "annet"
    ]

  d[, Kontaktype := "ukjent"]
  ### Kontaktkode
  for (takstkode in names(takstkoder)) {
    d[Takst == takstkode, Kontaktype := takstkoder[takstkode]]
  }

  # select "best"??
  dups <- d[, .(n_diff = length(unique(Kontaktype))), by = .(Id)]
  d <- d[!(Id %in% dups[n_diff >= 2, Id] & Kontaktype == "telefonkontakt")]

  d[, age := "ukjent"]
  d[PasientAlder == "0-4", age := "0-4"]
  d[PasientAlder == "5-9", age := "5-14"]
  d[PasientAlder == "0-9", age := "5-14"]
  d[PasientAlder == "10-14", age := "5-14"]
  d[PasientAlder == "10-19", age := "15-19"]
  d[PasientAlder == "15-19", age := "15-19"]
  d[PasientAlder == "20-29", age := "20-29"]
  d[PasientAlder == "30-39", age := "30-64"]
  d[PasientAlder == "40-49", age := "30-64"]
  d[PasientAlder == "50-59", age := "30-64"]
  d[PasientAlder == "60-64", age := "30-64"]
  d[PasientAlder == "65-69", age := "65+"]
  d[PasientAlder == "60-69", age := "65+"]
  d[PasientAlder == "70-79", age := "65+"]
  d[PasientAlder == "80+", age := "65+"]

  # Fixing behandler kommune nummer
  for (old in names(nav_to_freg)) {
    d[as.character(BehandlerKommune) == old, BehandlerKommune := nav_to_freg[old]]
  }

  # extract out the bydel and append
  unique(d$PasientKommune)
  d[
    nav_to_freg_bydel,
    on="PasientKommune==nav",
    bydel := freg
  ]
  d[, is_bydel := FALSE]
  b <- d[!is.na(bydel)]
  b[, BehandlerKommune := bydel]
  b[, is_bydel := TRUE]
  d <- rbind(d,b)

  # Collapsing it down to 1 row per consultation
  d <- d[,
         lapply(.SD, sum),
         by = .(
           Id,
           is_bydel,
           BehandlerKommune,
           age,
           Konsultasjonsdato,
           Praksis,
           Kontaktype
         ),
         .SDcols = names(diags)
         ]
  d[, consult := 1]

  # Collapsing it down to 1 row per kommune/age/day
  d <- d[, lapply(.SD, sum), ,
         by = .(
           is_bydel,
           BehandlerKommune,
           age,
           Konsultasjonsdato,
           Praksis,
           Kontaktype
         ),
         .SDcols = c(names(diags), "consult")
         ]

  d[, location_code := paste0("municip", formatC(BehandlerKommune, width = 4, flag = 0))]
  d[is_bydel==T, location_code := paste0("ward", formatC(BehandlerKommune, width = 6, flag = 0, format="fg"))]
  d[, BehandlerKommune := NULL]
  setnames(d, "Konsultasjonsdato", "date")

  return(d)
}

old_delete_norsyss_aggregate_format_raw_data <- function(d, configs) {
  d[, influensa := 0]
  d[Diagnose %in% "R80", influensa := 1]

  d[, gastro := 0]
  d[Diagnose %in% c("D11", "D70", "D73"), gastro := 1]

  d[, respiratoryexternal := 0]
  d[Diagnose %in% c("R05", "R74", "R78", "R83"), respiratoryexternal := 1]

  d[, respiratoryinternal := 0]
  d[Diagnose %in% c("R05", "R74", "R83"), respiratoryinternal := 1]

  d[, lungebetennelse := 0]
  d[Diagnose %in% "R81", lungebetennelse := 1]

  d[, bronkitt := 0]
  d[Diagnose %in% "R78", bronkitt := 1]

  d[, skabb := 0]
  d[Diagnose %in% "S72", skabb := 1]

  # included because of covid19
  d[, hoste := 0]
  d[Diagnose %in% "R05", hoste := 1]

  d[, akkut_ovre_luftveisinfeksjon := 0]
  d[Diagnose %in% "R74", akkut_ovre_luftveisinfeksjon := 1]

  d[, luftveisinfeksjon_ika := 0]
  d[Diagnose %in% "R83", luftveisinfeksjon_ika := 1]

  d[, luftveissykdom_ika := 0]
  d[Diagnose %in% "R99", luftveissykdom_ika := 1]

  d[, virusinfeksjon_ika := 0]
  d[Diagnose %in% "A77", virusinfeksjon_ika := 1]

  # all R codes:
  # R01 R02 R03 R04 R05 R06 R07 R08 R09
  # R21 R24 R25 R27 R270000 R29 R71 R72 R74 R75 R76
  # R77 R78 R79 R80 R81 R82 R83 R95 R96 R99 R991 R9910000
  d[, rxx_for_covid19 := 0]
  d[stringr::str_detect(Diagnose, "^R"), rxx_for_covid19 := 1]
  d[Diagnose %in% c(
    "R26", # Engstelig for kreft luftveier
    "R71", # Kikhoste
    "R73", # Nesebyll
    "R80", # Influensa
    "R84", # Ondartet svulst bronkie/lunge
    "R85", # Ondartet svulst luftveier

    "R86", # Godartet svulst luftveier
    "R87", # Fremmedlegme i nese/larynx/brinkie
    "R88", # Skade luftveier IKA
    "R89", # Medfødt feil luftveier
    "R89", # Medfødt feil luftveier
    "R90", # Hypertrofi tonsiller/adenoid vev
    "R92", # Uspesifisert svulst luftveier
    "R95", # Kronisk obstruktiv lungesykdom
    "R96" # Astma
  ), rxx_for_covid19 := 0]

  d[, covid19 := 0]
  d[Diagnose %in% "R991", covid19 := 1]

  d[, engstelig_luftveissykdom_ika := 0]
  d[Diagnose %in% "R27", engstelig_luftveissykdom_ika := 1]


  ### Praksis

  d[
    Praksis %in% c(
      "Fastl\u00F8nnet",
      "Fastlege"
    ),
    Praksis := "legekontor"
  ]
  d[
    Praksis %in% c(
      "kommunal legevakt",
      "Legevakt"
    ),
    Praksis := "legevakt"
  ]
  d[
    Praksis %in% c(
      "Annet"
    ),
    Praksis := "annet"
  ]


  d[, Kontaktype := "ukjent"]
  ### Kontaktkode
  for (takstkode in names(takstkoder)) {
    d[ Takst == takstkode, Kontaktype := takstkoder[takstkode]]
  }

  dups <- d[, .(n_diff = length(unique(Kontaktype))), by = .(Id)]
  d <- d[ !(Id %in% dups[n_diff >= 2, Id] & Kontaktype == "telefonkontakt")]

  d[, age := "ukjent"]
  d[PasientAlder == "0-4", age := "0-4"]
  d[PasientAlder == "5-9", age := "5-14"]
  d[PasientAlder == "0-9", age := "5-14"]
  d[PasientAlder == "10-14", age := "5-14"]
  d[PasientAlder == "10-19", age := "15-19"]
  d[PasientAlder == "15-19", age := "15-19"]
  d[PasientAlder == "20-29", age := "20-29"]
  d[PasientAlder == "30-39", age := "30-64"]
  d[PasientAlder == "40-49", age := "30-64"]
  d[PasientAlder == "50-59", age := "30-64"]
  d[PasientAlder == "60-64", age := "30-64"]
  d[PasientAlder == "65-69", age := "65+"]
  d[PasientAlder == "60-69", age := "65+"]
  d[PasientAlder == "70-79", age := "65+"]
  d[PasientAlder == "80+", age := "65+"]



  #Fixing behandler kommune nummer
  for(old in names(nav_to_freg)){
    d[as.character(BehandlerKommune) == old, BehandlerKommune:=nav_to_freg[old]]
  }



  # Collapsing it down to 1 row per consultation
  d <- d[, .(
    influensa = sum(influensa),
    gastro = sum(gastro),
    respiratoryexternal = sum(respiratoryexternal),
    respiratoryinternal = sum(respiratoryinternal),
    lungebetennelse = sum(lungebetennelse),
    bronkitt = sum(bronkitt),
    skabb = sum(skabb),

    hoste = sum(hoste),
    akkut_ovre_luftveisinfeksjon = sum(akkut_ovre_luftveisinfeksjon),
    luftveisinfeksjon_ika = sum(luftveisinfeksjon_ika),
    luftveissykdom_ika = sum(luftveissykdom_ika),
    virusinfeksjon_ika = sum(virusinfeksjon_ika),
    rxx_for_covid19 = sum(rxx_for_covid19),

    covid19 = sum(covid19),
    engstelig_luftveissykdom_ika = sum(engstelig_luftveissykdom_ika)
  ),
  by = .(
    Id,
    BehandlerKommune,
    age,
    Konsultasjonsdato,
    Praksis,
    Kontaktype
  )
  ]

  # Collapsing it down to 1 row per kommune/age/day
  d <- d[, .(
    influensa = sum(influensa),
    gastro = sum(gastro),
    respiratoryexternal = sum(respiratoryexternal),
    respiratoryinternal = sum(respiratoryinternal),
    lungebetennelse = sum(lungebetennelse),
    bronkitt = sum(bronkitt),
    skabb = sum(skabb),

    hoste = sum(hoste),
    akkut_ovre_luftveisinfeksjon = sum(akkut_ovre_luftveisinfeksjon),
    luftveisinfeksjon_ika = sum(luftveisinfeksjon_ika),
    luftveissykdom_ika = sum(luftveissykdom_ika),
    virusinfeksjon_ika = sum(virusinfeksjon_ika),
    rxx_for_covid19 = sum(rxx_for_covid19),

    covid19 = sum(covid19),
    engstelig_luftveissykdom_ika = sum(engstelig_luftveissykdom_ika),

    consult = .N
  ),
  by = .(
    BehandlerKommune,
    age,
    Konsultasjonsdato,
    Praksis,
    Kontaktype
  )
  ]

  d[, municip := paste0("municip", formatC(BehandlerKommune, width = 4, flag = 0))]
  d[, BehandlerKommune := NULL]
  setnames(d, "Konsultasjonsdato", "date")

  return(d)
}

# get_n_doctors
#
# A function to extract the number of doctors per week
get_n_doctors <- function(folder = "/input/sykdomspulsen_norsyss_input") {
  db <- RODBC::odbcDriverConnect("driver={ODBC Driver 17 for SQL Server};server=dm-prod;database=SykdomspulsenAnalyse; trusted_connection=yes")
  res <- RODBC::sqlQuery(db, 'select count(distinct(Behandler_Id)) as behandlere, DATEPART("ISO_WEEK", Konsultasjonsdato) as week ,DATEPART("YEAR", Konsultasjonsdato) as year from Konsultasjon group by DATEPART("ISO_WEEK", Konsultasjonsdato) ,DATEPART("YEAR", Konsultasjonsdato)')
  setDT(res)

  file_permanent <- fs::path(folder, "behandlere.txt")

  fwrite(res[order(year, week)], file_permanent)
  close(db)
}
