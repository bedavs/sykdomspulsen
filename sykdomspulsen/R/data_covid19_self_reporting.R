#' data_covid19_self_reporting
#' @param data a
#' @param argset a
#' @param schema a
#' @export
data_covid19_self_reporting <- function(data, argset, schema){
  # tm_run_task("data_covid19_self_reporting")

  if(plnr::is_run_directly()){
    data <- sc::tm_get_data("data_covid19_self_reporting")
    argset <- sc::tm_get_argset("data_covid19_self_reporting")
    schema <- sc::tm_get_schema("data_covid19_self_reporting")
  }

  folder <- sc::path("input","sykdomspulsen_covid19_selvrapportering_input", create_dir = T)

  file <- fs::dir_ls(folder, regexp="selvrapportering_kommune_fylke_faar_kjonn_[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9].csv")
  file <- max(file)

  master <- fread(file, encoding="Latin-1")

  setnames(master, epitrix::clean_labels(names(master)))
  master[,sendtinndato := stringr::str_sub(sendtinndato, 1, 9)]
  master[,hvilken_dag_fikk_du_symptomer := stringr::str_sub(hvilken_dag_fikk_du_symptomer, 1, 9)]

  # date cleaning 1
  xtabs(~master$sendtinndato, addNA=T)
  master[, sendtinndato := as.Date(sendtinndato, format="%d%b%Y")]
  setorder(master,sendtinndato)
  xtabs(~master$sendtinndato, addNA=T)

  # date cleaning 2
  sum(is.na(master$hvilken_dag_fikk_du_symptomer))
  master[, hvilken_dag_fikk_du_symptomer := as.Date(paste0(hvilken_dag_fikk_du_symptomer,"0"), format="%d.%m.%Y")]
  sum(is.na(master$hvilken_dag_fikk_du_symptomer))

  # drop some people
  # only keep the ones who responded after 2020-03-22
  nrow(master)
  master <- master[sendtinndato>="2020-03-22"]
  # symptoms must happen before or on same day as reporting
  nrow(master)
  master <- master[hvilken_dag_fikk_du_symptomer<=sendtinndato]

  # cant have symptoms after day of data
  nrow(master)
  master <- master[hvilken_dag_fikk_du_symptomer<=date_max]
  nrow(master)
  master <- master[hvilken_dag_fikk_du_symptomer>="2020-03-15"]
  nrow(master)

  # dates
  date_max <- as.Date(stringr::str_extract(file,"[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]"))-1
  date_min <- min(master$hvilken_dag_fikk_du_symptomer)

  # gen variables
  master[,age := lubridate::year(sendtinndato)-fodselsaar]

  # symptoms
  master[, is_symp_cough := !is.na(hoste)]
  master[, is_symp_fever := !is.na(feber)]
  master[, is_symp_throat := !is.na(sar_hals)]
  master[, is_symp_headache := !is.na(hodepine)]
  master[, is_symp_nose := !is.na(tett_eller_rennende_nese)]
  master[, is_symp_muscle := !is.na(muskelsmerter)]
  master[, is_symp_breath := !is.na(tungpust)]
  master[, is_symp_gastro := !is.na(magesmerter_kvalme_diare)]
  master[, is_symp_taste_smell := !is.na(tap_av_smaks_luktesans)]
  master[, is_symp_other := !is.na(andre)]

  master[,
    n_symp :=
    is_symp_cough +
    is_symp_fever +
    is_symp_throat +
    is_symp_headache +
    is_symp_nose +
    is_symp_muscle +
    is_symp_breath +
    is_symp_gastro +
    is_symp_taste_smell +
    is_symp_other
  ]

  master[, is_symps_yesfever_cough_or_breath := is_symp_fever & (is_symp_cough | is_symp_breath)]
  master[, is_symps_nofever_cough_or_breath := !is_symp_fever & (is_symp_cough | is_symp_breath)]

  master[, is_symps_yesfever_throat_or_nose := is_symp_fever & (is_symp_throat | is_symp_nose)]
  master[, is_symps_nofever_throat_or_nose := !is_symp_fever & (is_symp_throat | is_symp_nose)]

  master[, is_symps_yesfever_muscle_or_headache := is_symp_fever & (is_symp_muscle | is_symp_headache)]
  master[, is_symps_nofever_muscle_or_headache := !is_symp_fever & (is_symp_muscle | is_symp_headache)]

  master[, is_symps_yesfever_gastro_or_taste_smell_or_other := is_symp_fever & (is_symp_breath | is_symp_taste_smell | is_symp_other)]
  master[, is_symps_nofever_gastro_or_taste_smell_or_other := !is_symp_fever & (is_symp_breath | is_symp_taste_smell | is_symp_other)]

  # other variables
  master[, is_today_0normal := hvordan_er_formen_din == 0]
  master[, is_today_1tired := hvordan_er_formen_din == 1]
  master[, is_today_2need_rest := hvordan_er_formen_din == 2]
  master[, is_today_3bedridden_some_help := hvordan_er_formen_din == 3]
  master[, is_today_4bedridden_lots_help := hvordan_er_formen_din == 4]

  # tested
  master[, is_tested := har_du_blitt_testet_for_koronavirus_i_lopet_av_denne_sykdomsperioden == 1]
  master[, is_tested_has_any_result := is_tested==T & hva_var_svaret_pa_proven %in% c(1:2)]
  master[, is_tested_has_pos_result := is_tested==T & hva_var_svaret_pa_proven %in% c(1)]

  # contact with doctor
  master[, is_contact_doctor_yes := har_du_vaert_i_kontakt_med_lege_eller_legevakt_pa_grunn_av_symptomene_dine==1]
  master[, is_contact_doctor_no := har_du_vaert_i_kontakt_med_lege_eller_legevakt_pa_grunn_av_symptomene_dine==2]

  # location_code
  master[ , kommunenummer := paste0("municip",formatC(kommunenummer,width=4,flag="0"))]
  master[ , fylkenummer := paste0("county",formatC(fylkenummer,width=2,flag="0"))]

  # duplicate for sex = total
  master[, sex := dplyr::case_when(
    kjonn == "M" ~ "male",
    kjonn == "K" ~ "female"
  )]
  d1 <- copy(master)
  d1[, sex := "total"]
  master <- rbind(d1,master)

  # duplicate for age = total
  master[, age:=as.character(age)]
  d1 <- copy(master)
  d1[, age := "total"]
  master <- rbind(d1,master)

  # duplicate for location_code = norge
  master[, location_code := fylkenummer]
  d1 <- copy(master)
  d1[, location_code := "norge"]
  master <- rbind(d1,master)

  # aggregate
  agg <- master[,.(
    n = .N,
    n_symps_0 = sum(n_symp==0),
    n_symps_1 = sum(n_symp==1),
    n_symps_2 = sum(n_symp==2),
    n_symps_3 = sum(n_symp==3),
    n_symps_4 = sum(n_symp==4),
    n_symps_5 = sum(n_symp==5),
    n_symps_6 = sum(n_symp==6),
    n_symps_7 = sum(n_symp==7),
    n_symps_8 = sum(n_symp==8),
    n_symps_9 = sum(n_symp==9),
    n_symps_10 = sum(n_symp==10),
    n_symp_cough = sum(is_symp_cough),
    n_symp_fever = sum(is_symp_fever),
    n_symp_throat = sum(is_symp_throat),
    n_symp_headache = sum(is_symp_headache),
    n_symp_muscle = sum(is_symp_muscle),
    n_symp_breath = sum(is_symp_breath),
    n_symp_taste_smell = sum(is_symp_taste_smell),
    n_symp_other = sum(is_symp_other),

    n_symps_yesfever_cough_or_breath = sum(is_symps_yesfever_cough_or_breath),
    n_symps_nofever_cough_or_breath = sum(is_symps_nofever_cough_or_breath),
    n_symps_yesfever_throat_or_nose = sum(is_symps_yesfever_throat_or_nose),
    n_symps_nofever_throat_or_nose = sum(is_symps_nofever_throat_or_nose),
    n_symps_yesfever_muscle_or_headache = sum(is_symps_yesfever_muscle_or_headache),
    n_symps_nofever_muscle_or_headache = sum(is_symps_nofever_muscle_or_headache),
    n_symps_yesfever_gastro_or_taste_smell_or_other = sum(is_symps_yesfever_gastro_or_taste_smell_or_other),
    n_symps_nofever_gastro_or_taste_smell_or_other = sum(is_symps_nofever_gastro_or_taste_smell_or_other),

    n_today_0normal = sum(is_today_0normal),
    n_today_1tired = sum(is_today_1tired),
    n_today_2need_rest = sum(is_today_2need_rest),
    n_today_3bedridden_some_help = sum(is_today_3bedridden_some_help),
    n_today_4bedridden_lots_help = sum(is_today_4bedridden_lots_help),
    n_tested = sum(is_tested),
    n_tested_has_any_result = sum(is_tested_has_any_result),
    n_tested_has_pos_result = sum(is_tested_has_pos_result),
    n_contact_doctor_yes = sum(is_contact_doctor_yes),
    n_contact_doctor_no = sum(is_contact_doctor_no)
  ),keyby=.(
    date = hvilken_dag_fikk_du_symptomer,
    location_code,
    sex,
    age
  )]

  skeleton <- expand.grid(
    date = seq.Date(date_min, date_max, by=1),
    location_code = c("norge",unique(fhidata::norway_locations_b2020$county_code)),
    sex = c("total", "male", "female"),
    age = c(0:100, "total")
  )
  setDT(skeleton)

  nrow(skeleton)
  retval <- merge(
    skeleton,
    agg,
    by=c("date","location_code","sex","age"),
    all.x=T
  )
  nrow(skeleton)

  for(i in names(retval)) retval[is.na(get(i)), (i):=0]
  retval[, granularity_time:="day"]
  fill_in_missing(retval)

  schema$output$db_drop_table()
  schema$output$db_connect()
  schema$output$db_drop_constraint()
  schema$output$db_load_data_infile(retval)

}




