#' datar_pre_norsyss
#' @param data a
#' @param argset a
#' @param schema a
#' @export
datar_pre_norsyss <- function(data, argset, schema){
  # tm_run_task("datar_pre_norsyss")

  if(plnr::is_run_directly()){
    data <- sc::tm_get_data("datar_pre_norsyss")
    argset <- sc::tm_get_argset("datar_pre_norsyss")
    schema <- sc::tm_get_schema("datar_pre_norsyss")
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
  date_from <- argset$date_from
  date_to <- lubridate::today()-1
  folder <- sc::path("input", "sykdomspulsen_norsyss_input", create_dir = TRUE)
  overwrite_file <- FALSE
  diags = argset$diags

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
    d <- d[!(Diagnose!="R991" & Takst=="1be")] ##??
    d <- norsyss_aggregate_raw_data_to_takst(d, diags = diags)
    if (i == 1) {
      utils::write.table(d, file_temp, sep = "\t", row.names = FALSE, col.names = TRUE, append = FALSE)
    } else {
      utils::write.table(d, file_temp, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE)
    }
  }
  #pb$terminate()
  system(glue::glue("mv {file_temp} {file_permanent}"))

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

norsyss_aggregate_raw_data_to_takst <- function(d, diags) {
  . <- BehandlerKommune <- Diagnose <- Id <-
    Konsultasjonsdato <- Kontaktype <- PasientAlder <-
    Praksis <- Takst <- age <- consult <- from <-
    municip <- n_diff <- pb <- NULL

  for (i in seq_along(diags)) {
    d[, (names(diags)[i]) := 0]
    d[Diagnose %in% diags[[i]], (names(diags)[i]) := 1]
  }

  # BEA, PLEASE UNAGGREAGATE "PRAKSIS"
  # GET RID OF NORWEGIAN LETTERS; MAKE ALL LOWERCASE
  ### Praksis


  d[
    Praksis == "Fastl\u00F8nnet",
    Praksis := "fastl\u00F8nnet"
  ]

  d[
      Praksis == "Fastlege",
      Praksis := "fastlege"
    ]

  d[
    Praksis == "Legevakt",
    Praksis := "legevakt"
  ]

  d[
    Praksis == "Annet",
    Praksis := "annet"
    ]


  ### BEA, WE NEED YOU TO DELETE "KONTAKTTYPE" AND AGGREAGATE ON TAKSTKODE INSTEAD
  ### Kontaktkode <- BEA, KEEP ALL TAKSTKODER
  for (tk in names(takstkoder)) {
    d[Takst == tk, takstkode := tk]
  }


  ### Kontaktkode <- BEA, KEEP ALL AGE GROUPS, EXCEPT CONVERT TO 5-14
  d[, age := "ukjent"]
  d[PasientAlder == "0-4", age := "0-4"]

  # these are the annoying age groups we want to "clean"
  d[PasientAlder == "5-9", age := "5-14"] # -> 5-14
  d[PasientAlder == "0-9", age := "5-14"] # -> 5-14 # for kommune with <500 people
  d[PasientAlder == "10-14", age := "5-14"] # -> 5-14
  d[PasientAlder == "10-19", age := "15-19"] # -> 15-19 # for kommune with <500 people
  d[PasientAlder == "15-19", age := "15-19"] # -> 15-19

  # keep these age groups as they were originally
  d[PasientAlder == "20-29", age := "20-29"] # 20-29
  d[PasientAlder == "30-39", age := "30-39"] # 30-39
  d[PasientAlder == "40-49", age := "40-49"] # 40-49
  d[PasientAlder == "50-59", age := "50-59"] # 50-59
  d[PasientAlder == "60-64", age := "60-64"] # 60-64
  d[PasientAlder == "65-69", age := "65-69"]
  d[PasientAlder == "60-69", age := "60-69"]
  d[PasientAlder == "70-79", age := "70-79"]
  d[PasientAlder == "80+", age := "80+"]

  ages <- unique(d$age)
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
           age, # this will be unaggregated
           Konsultasjonsdato,
           Praksis, # this will be unaggregated
           takstkode # this will be takst
         ),
         .SDcols = names(diags)
         ]
  d[, consult := 1]

  # Collapsing it down to 1 row per kommune/age/day/takstkode <- BEA
  d <- d[, lapply(.SD, sum), ,
         by = .(
           is_bydel,
           BehandlerKommune,
           age,
           Konsultasjonsdato,
           Praksis,
           takstkode
         ),
         .SDcols = c(names(diags), "consult")
         ]

  d[, location_code := paste0("municip", formatC(BehandlerKommune, width = 4, flag = 0))]
  d[is_bydel==T, location_code := paste0("ward", formatC(BehandlerKommune, width = 6, flag = 0, format="fg"))]
  d[, BehandlerKommune := NULL]
  setnames(d, "Konsultasjonsdato", "date")


  # step 1. create "norway level" (with skeleton)

  d_norway <- d[is_bydel==F, lapply(.SD, sum), ,
         by = .(
           age,
           date,
           Praksis,
           takstkode
         ),
         .SDcols = c(names(diags), "consult")
  ]

  d_norway[, location_code := "norge"]
  d_norway[,granularity_geo:="county"]

  skeleton <- expand.grid(
    location_code = "norge",
    date = seq.Date(
      date_min,
      date_max,
      by = 1
    ),
    age=ages,
    stringsAsFactors = FALSE
  )
  setDT(skeleton)

  d_norway <-
    merge(skeleton,
          d_norway,
          by = c("location_code", "age", "date"),
          all.x = TRUE
    )

  # step 2. CONVERT current kommune-data TO THE CURRENT KOMMUNESAMMENSLAAING (with skeleton)

  d_municip <- d[is_bydel==F, lapply(.SD, sum), ,
                by = .(
                  age,
                  date,
                  location_code,
                  Praksis,
                  takstkode
                ),
                .SDcols = c(names(diags), "consult")
  ]
  d_municip[,year:=year(date)]
  d_municip[,granularity_geo:="municip"]

  d_municip <-
    merge(d_municip,
          norway_municip_merging()[, c("municip_code_original", "year", "municip_code_current", "weighting")],
          by.x = c("location_code", "year"),
          by.y = c("municip_code_original", "year"),
          all.x = T,
          allow.cartesian = T
    )

  d_municip <- d_municip[!is.na(municip_code_current)]

  skeleton <-
      data.table(expand.grid(
        location_code = unique(norway_municip_merging()[
            municip_code_current %in% unique(d_municip$location_code) |
              municip_code_original %in% unique(d_municip$location_code)
          ]$municip_code_original),
          date = seq.Date(
            date_min,
            date_max,
            by = 1
          ),
          age=ages,
          stringsAsFactors = FALSE
        ))
  setDT(skeleton)

  d_municip <-
    merge(skeleton,
        d_municip,
        by = c("location_code", "age", "date"),
        all.x = TRUE
  )

  d_municip[,year:=NULL]
  d_municip[,weighting:=NULL]

  d_municip <-d_municip[!is.na(municip_code_current)]
  d_municip[,location_code:=municip_code_current]
  d_municip[,municip_code_current:=NULL]

  # step 3. aggregate up "new kommunedata" to "new fylkedata" (shouldnt need skeleton)

  d_county <-
    merge(d_municip,
          norway_locations()[, c("municip_code", "county_code")],
          by.x = "location_code",
          by.y = "municip_code",
          all.x = T
    )


  d_county <- d_county[, lapply(.SD, sum), ,
                 by = .(
                   age,
                   date,
                   county_code,
                   Praksis,
                   takstkode
                 ),
                 .SDcols = c(names(diags), "consult")
  ]
  d_county[,granularity_geo:="county"]
  d_county[,location_code:=county_code]
  d_county[,county_code:=NULL]


  setcolorder(d_county, names(d_norway))
  # step 4. rbind kommune, fylke, norge data
  # final result: 99% clean dataset

  d <- rbind(d_norway, d_county, d_municip)


  # after this, do datar_norsyss which will load this dataset into the database table datar_norsyss

  # then we will have data_norsyss which will aggregate datar_norsyss into kontakttype (oppmote/telefon/ekons)


  # old system:
  # data_pre_norsyss creates fairly clean text file
  # data_norsyss reads this in

  # new system:
  # datar_pre_norsyss creates raw raw text file
  # datar_norsyss reads this into database
  # data_norsyss aggregates datar_norsyss

  return(d)
}

