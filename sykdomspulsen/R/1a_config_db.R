set_db <- function(){

  # rundate ----
  sc::add_schema(
    name = "rundate",
    schema = sc::Schema$new(
      db_config = sc::config$db_config,
      db_table = "rundate",
      db_field_types = c(
        "task" = "TEXT",
        "date" = "DATE",
        "datetime" = "DATETIME"
      ),
      db_load_folder = tempdir(),
      keys = c(
        "task"
      )
    )
  )

  # covid19 ----
  # data_covid19_nordic ----
  sc::add_schema(
    name = "data_covid19_nordic",
    schema = sc::Schema$new(
      db_table = "data_covid19_nordic",
      db_config = sc::config$db_config,
      db_field_types =  c(
        "granularity_time" = "TEXT",
        "granularity_geo" = "TEXT",
        "location_code" = "TEXT",
        "border" = "INTEGER",
        "age" = "TEXT",
        "sex" = "TEXT",
        "year" = "INTEGER",
        "week" = "INTEGER",
        "yrwk" = "TEXT",
        "season" = "TEXT",
        "x" = "DOUBLE",
        "date" = "DATE",

        "pop" = "INTEGER",

        "n_cases" = "INTEGER",
        "pr100000_cases" = "DOUBLE",
        "n_tests" = "INTEGER",
        "pr100_tests" = "DOUBLE",
        "n_icu" = "INTEGER",
        "pr100000_icu" = "DOUBLE"
      ),
      db_load_folder = tempdir(),
      keys =  c(
        "granularity_time",
        "location_code",
        "date"
      ),
      validator_field_types = sc::validator_field_types_sykdomspulsen,
      validator_field_contents = sc::validator_field_contents_sykdomspulsen
    )
  )

  # results_covid19_model ----
  sc::add_schema(
    name = "results_covid19_model",
    schema = sc::Schema$new(
      db_table = "results_covid19_model",
      db_config = sc::config$db_config,
      db_field_types =  c(
        "granularity_time" = "TEXT",
        "granularity_geo" = "TEXT",
        "location_code" = "TEXT",
        "border" = "INTEGER",
        "age" = "TEXT",
        "sex" = "TEXT",
        "year" = "INTEGER",
        "week" = "INTEGER",
        "yrwk" = "TEXT",
        "season" = "TEXT",
        "x" = "DOUBLE",
        "date" = "DATE",

        "infectious_prev_est" = "DOUBLE",
        "infectious_prev_thresholdl0" = "DOUBLE",
        "infectious_prev_thresholdu0" = "DOUBLE",

        "incidence_est" = "DOUBLE",
        "incidence_thresholdl0" = "DOUBLE",
        "incidence_thresholdu0" = "DOUBLE",

        "hosp_prev_est" = "DOUBLE",
        "hosp_prev_thresholdl0" = "DOUBLE",
        "hosp_prev_thresholdu0" = "DOUBLE",

        "icu_prev_est" = "DOUBLE",
        "icu_prev_thresholdl0" = "DOUBLE",
        "icu_prev_thresholdu0" = "DOUBLE"
      ),
      db_load_folder = tempdir(),
      keys =  c(
        "location_code",
        "date"
      ),
      validator_field_types = sc::validator_field_types_sykdomspulsen,
      validator_field_contents = sc::validator_field_contents_sykdomspulsen
    )
  )

  # data_covid19_msis_by_time_location ----
  sc::add_schema(
    name = "data_covid19_msis_by_time_location",
    schema = sc::Schema$new(
      db_table = "data_covid19_msis_by_time_location",
      db_config = sc::config$db_config,
      db_field_types =  c(
        "granularity_time" = "TEXT",
        "granularity_geo" = "TEXT",
        "location_code" = "TEXT",
        "border" = "INTEGER",
        "age" = "TEXT",
        "sex" = "TEXT",
        "year" = "INTEGER",
        "week" = "INTEGER",
        "yrwk" = "TEXT",
        "season" = "TEXT",
        "x" = "DOUBLE",
        "date" = "DATE",

        "n" = "INTEGER"
      ),
      db_load_folder = tempdir(),
      keys =  c(
        "granularity_time",
        "location_code",
        "date"
      ),
      validator_field_types = sc::validator_field_types_sykdomspulsen,
      validator_field_contents = sc::validator_field_contents_sykdomspulsen
    )
  )
  # prelim_data_covid19_msis_by_time_location ----
  sc::add_schema(
    name = "prelim_data_covid19_msis_by_time_location",
    schema = sc::Schema$new(
      db_table = "prelim_data_covid19_msis_by_time_location",
      db_config = sc::config$db_config,
      db_field_types =  c(
        "granularity_time" = "TEXT",
        "granularity_geo" = "TEXT",
        "location_code" = "TEXT",
        "border" = "INTEGER",
        "age" = "TEXT",
        "sex" = "TEXT",
        "year" = "INTEGER",
        "week" = "INTEGER",
        "yrwk" = "TEXT",
        "season" = "TEXT",
        "x" = "DOUBLE",
        "date" = "DATE",

        "n" = "INTEGER"
      ),
      db_load_folder = tempdir(),
      keys =  c(
        "granularity_time",
        "location_code",
        "date"
      ),
      validator_field_types = sc::validator_field_types_sykdomspulsen,
      validator_field_contents = sc::validator_field_contents_sykdomspulsen
    )
  )

  # data_covid19_msis_by_time_infected_abroad ----
  sc::add_schema(
    name = "data_covid19_msis_by_time_infected_abroad",
    schema = sc::Schema$new(
      db_table = "data_covid19_msis_by_time_infected_abroad",
      db_config = sc::config$db_config,
      db_field_types =  c(
        "granularity_time" = "TEXT",
        "granularity_geo" = "TEXT",
        "location_code" = "TEXT",
        "border" = "INTEGER",
        "age" = "TEXT",
        "sex" = "TEXT",
        "year" = "INTEGER",
        "week" = "INTEGER",
        "yrwk" = "TEXT",
        "season" = "TEXT",
        "x" = "DOUBLE",
        "date" = "DATE",

        "tag_location_infected" = "TEXT",
        "n" = "INTEGER"
      ),
      db_load_folder = tempdir(),
      keys =  c(
        "granularity_time",
        "location_code",
        "date",
        "tag_location_infected"
      ),
      validator_field_types = sc::validator_field_types_sykdomspulsen,
      validator_field_contents = sc::validator_field_contents_sykdomspulsen
    )
  )

  # prelim_data_covid19_msis_by_time_infected_abroad ----
  sc::add_schema(
    name = "prelim_data_covid19_msis_by_time_infected_abroad",
    schema = sc::Schema$new(
      db_table = "prelim_data_covid19_msis_by_time_infected_abroad",
      db_config = sc::config$db_config,
      db_field_types =  c(
        "granularity_time" = "TEXT",
        "granularity_geo" = "TEXT",
        "location_code" = "TEXT",
        "border" = "INTEGER",
        "age" = "TEXT",
        "sex" = "TEXT",
        "year" = "INTEGER",
        "week" = "INTEGER",
        "yrwk" = "TEXT",
        "season" = "TEXT",
        "x" = "DOUBLE",
        "date" = "DATE",

        "tag_location_infected" = "TEXT",
        "n" = "INTEGER"
      ),
      db_load_folder = tempdir(),
      keys =  c(
        "granularity_time",
        "location_code",
        "date",
        "tag_location_infected"
      ),
      validator_field_types = sc::validator_field_types_sykdomspulsen,
      validator_field_contents = sc::validator_field_contents_sykdomspulsen
    )
  )

  # data_covid19_msis_by_time_sex_age ----
  sc::add_schema(
    name = "data_covid19_msis_by_time_sex_age",
    schema = sc::Schema$new(
      db_table = "data_covid19_msis_by_time_sex_age",
      db_config = sc::config$db_config,
      db_field_types =  c(
        "granularity_time" = "TEXT",
        "granularity_geo" = "TEXT",
        "location_code" = "TEXT",
        "border" = "INTEGER",
        "age" = "TEXT",
        "sex" = "TEXT",
        "year" = "INTEGER",
        "week" = "INTEGER",
        "yrwk" = "TEXT",
        "season" = "TEXT",
        "x" = "DOUBLE",
        "date" = "DATE",

        "n" = "INTEGER"
      ),
      db_load_folder = tempdir(),
      keys =  c(
        "granularity_time",
        "location_code",
        "age",
        "sex",
        "date"
      ),
      validator_field_types = sc::validator_field_types_sykdomspulsen,
      validator_field_contents = sc::validator_field_contents_sykdomspulsen
    )
  )

  # prelim_data_covid19_msis_by_time_sex_age ----
  sc::add_schema(
    name = "prelim_data_covid19_msis_by_time_sex_age",
    schema = sc::Schema$new(
      db_table = "prelim_data_covid19_msis_by_time_sex_age",
      db_config = sc::config$db_config,
      db_field_types =  c(
        "granularity_time" = "TEXT",
        "granularity_geo" = "TEXT",
        "location_code" = "TEXT",
        "border" = "INTEGER",
        "age" = "TEXT",
        "sex" = "TEXT",
        "year" = "INTEGER",
        "week" = "INTEGER",
        "yrwk" = "TEXT",
        "season" = "TEXT",
        "x" = "DOUBLE",
        "date" = "DATE",

        "n" = "INTEGER"
      ),
      db_load_folder = tempdir(),
      keys =  c(
        "granularity_time",
        "location_code",
        "age",
        "sex",
        "date"
      ),
      validator_field_types = sc::validator_field_types_sykdomspulsen,
      validator_field_contents = sc::validator_field_contents_sykdomspulsen
    )
  )

  # data_covid19_lab_by_time ----
  sc::add_schema(
    name = "data_covid19_lab_by_time",
    schema = sc::Schema$new(
      db_table = "data_covid19_lab_by_time",
      db_config = sc::config$db_config,
      db_field_types =  c(
        "granularity_time" = "TEXT",
        "granularity_geo" = "TEXT",
        "location_code" = "TEXT",
        "border" = "INTEGER",
        "age" = "TEXT",
        "sex" = "TEXT",
        "year" = "INTEGER",
        "week" = "INTEGER",
        "yrwk" = "TEXT",
        "season" = "TEXT",
        "x" = "DOUBLE",
        "date" = "DATE",

        "n_neg" = "INTEGER",
        "n_pos" = "INTEGER",
        "pr100_pos" = "DOUBLE",
        "cum_n_tested" = "INTEGER"
      ),
      db_load_folder = tempdir(),
      keys =  c(
        "granularity_time",
        "location_code",
        "date"
      ),
      validator_field_types = sc::validator_field_types_sykdomspulsen,
      validator_field_contents = sc::validator_field_contents_sykdomspulsen
    )
  )

  # prelim_data_covid19_lab_by_time ----
  sc::add_schema(
    name = "prelim_data_covid19_lab_by_time",
    schema = sc::Schema$new(
      db_table = "prelim_data_covid19_lab_by_time",
      db_config = sc::config$db_config,
      db_field_types =  c(
        "granularity_time" = "TEXT",
        "granularity_geo" = "TEXT",
        "location_code" = "TEXT",
        "border" = "INTEGER",
        "age" = "TEXT",
        "sex" = "TEXT",
        "year" = "INTEGER",
        "week" = "INTEGER",
        "yrwk" = "TEXT",
        "season" = "TEXT",
        "x" = "DOUBLE",
        "date" = "DATE",

        "n_neg" = "INTEGER",
        "n_pos" = "INTEGER",
        "pr100_pos" = "DOUBLE",
        "cum_n_tested" = "INTEGER"
      ),
      db_load_folder = tempdir(),
      keys =  c(
        "granularity_time",
        "location_code",
        "date"
      ),
      validator_field_types = sc::validator_field_types_sykdomspulsen,
      validator_field_contents = sc::validator_field_contents_sykdomspulsen
    )
  )

  # data_covid19_hospital_by_time ----
  sc::add_schema(
    name = "data_covid19_hospital_by_time",
    schema = sc::Schema$new(
      db_table = "data_covid19_hospital_by_time",
      db_config = sc::config$db_config,
      db_field_types =  c(
        "granularity_time" = "TEXT",
        "granularity_geo" = "TEXT",
        "location_code" = "TEXT",
        "border" = "INTEGER",
        "age" = "TEXT",
        "sex" = "TEXT",
        "year" = "INTEGER",
        "week" = "INTEGER",
        "yrwk" = "TEXT",
        "season" = "TEXT",
        "x" = "DOUBLE",
        "date" = "DATE",

        "n_icu" = "INTEGER",
        "n_hospital_main_cause" = "INTEGER"
      ),
      db_load_folder = tempdir(),
      keys =  c(
        "granularity_time",
        "location_code",
        "date"
      ),
      validator_field_types = sc::validator_field_types_sykdomspulsen,
      validator_field_contents = sc::validator_field_contents_sykdomspulsen
    )
  )

  # prelim_data_covid19_hospital_by_time ----
  sc::add_schema(
    name = "prelim_data_covid19_hospital_by_time",
    schema = sc::Schema$new(
      db_table = "prelim_data_covid19_hospital_by_time",
      db_config = sc::config$db_config,
      db_field_types =  c(
        "granularity_time" = "TEXT",
        "granularity_geo" = "TEXT",
        "location_code" = "TEXT",
        "border" = "INTEGER",
        "age" = "TEXT",
        "sex" = "TEXT",
        "year" = "INTEGER",
        "week" = "INTEGER",
        "yrwk" = "TEXT",
        "season" = "TEXT",
        "x" = "DOUBLE",
        "date" = "DATE",

        "n_icu" = "INTEGER",
        "n_hospital_main_cause" = "INTEGER"
      ),
      db_load_folder = tempdir(),
      keys =  c(
        "granularity_time",
        "location_code",
        "date"
      ),
      validator_field_types = sc::validator_field_types_sykdomspulsen,
      validator_field_contents = sc::validator_field_contents_sykdomspulsen
    )
  )

  # data_covid19_deaths ----
  sc::add_schema(
    name = "data_covid19_deaths",
    schema = sc::Schema$new(
      db_table = "data_covid19_deaths",
      db_config = sc::config$db_config,
      db_field_types =  c(
        "granularity_time" = "TEXT",
        "granularity_geo" = "TEXT",
        "location_code" = "TEXT",
        "border" = "INTEGER",
        "age" = "TEXT",
        "sex" = "TEXT",
        "year" = "INTEGER",
        "week" = "INTEGER",
        "yrwk" = "TEXT",
        "season" = "TEXT",
        "x" = "DOUBLE",
        "date" = "DATE",

        "cum_n" = "INTEGER"
      ),
      db_load_folder = tempdir(),
      keys =  c(
        "granularity_time",
        "location_code",
        "date"
      ),
      validator_field_types = sc::validator_field_types_sykdomspulsen,
      validator_field_contents = sc::validator_field_contents_sykdomspulsen
    )
  )

  # prelim_data_covid19_deaths ----
  sc::add_schema(
    name = "prelim_data_covid19_deaths",
    schema = sc::Schema$new(
      db_table = "prelim_data_covid19_deaths",
      db_config = sc::config$db_config,
      db_field_types =  c(
        "granularity_time" = "TEXT",
        "granularity_geo" = "TEXT",
        "location_code" = "TEXT",
        "border" = "INTEGER",
        "age" = "TEXT",
        "sex" = "TEXT",
        "year" = "INTEGER",
        "week" = "INTEGER",
        "yrwk" = "TEXT",
        "season" = "TEXT",
        "x" = "DOUBLE",
        "date" = "DATE",

        "cum_n" = "INTEGER"
      ),
      db_load_folder = tempdir(),
      keys =  c(
        "granularity_time",
        "location_code",
        "date"
      ),
      validator_field_types = sc::validator_field_types_sykdomspulsen,
      validator_field_contents = sc::validator_field_contents_sykdomspulsen
    )
  )

  # data_covid19_demographics ----
  sc::add_schema(
    name = "data_covid19_demographics",
    schema = sc::Schema$new(
      db_table = "data_covid19_demographics",
      db_config = sc::config$db_config,
      db_field_types =  c(
        "granularity_time" = "TEXT",
        "granularity_geo" = "TEXT",
        "location_code" = "TEXT",
        "border" = "INTEGER",
        "age" = "TEXT",
        "sex" = "TEXT",
        "year" = "INTEGER",
        "week" = "INTEGER",
        "yrwk" = "TEXT",
        "season" = "TEXT",
        "x" = "DOUBLE",
        "date" = "DATE",

        "tag_outcome" = "TEXT",
        "n" = "INTEGER"
      ),
      db_load_folder = tempdir(),
      keys =  c(
        "granularity_time",
        "location_code",
        "date",
        "age",
        "sex",
        "tag_outcome"
      ),
      validator_field_types = sc::validator_field_types_sykdomspulsen,
      validator_field_contents = sc::validator_field_contents_sykdomspulsen
    )
  )

  # prelim_data_covid19_demographics ----
  sc::add_schema(
    name = "prelim_data_covid19_demographics",
    schema = sc::Schema$new(
      db_table = "prelim_data_covid19_demographics",
      db_config = sc::config$db_config,
      db_field_types =  c(
        "granularity_time" = "TEXT",
        "granularity_geo" = "TEXT",
        "location_code" = "TEXT",
        "border" = "INTEGER",
        "age" = "TEXT",
        "sex" = "TEXT",
        "year" = "INTEGER",
        "week" = "INTEGER",
        "yrwk" = "TEXT",
        "season" = "TEXT",
        "x" = "DOUBLE",
        "date" = "DATE",

        "tag_outcome" = "TEXT",
        "n" = "INTEGER"
      ),
      db_load_folder = tempdir(),
      keys =  c(
        "granularity_time",
        "location_code",
        "date",
        "age",
        "sex",
        "tag_outcome"
      ),
      validator_field_types = sc::validator_field_types_sykdomspulsen,
      validator_field_contents = sc::validator_field_contents_sykdomspulsen
    )
  )

  # data_covid19_self_reporting ----
  sc::add_schema(
    name = "data_covid19_self_reporting",
    schema = sc::Schema$new(
      db_table = "data_covid19_self_reporting",
      db_config = sc::config$db_config,
      db_field_types =  c(
        "granularity_time" = "TEXT",
        "granularity_geo" = "TEXT",
        "location_code" = "TEXT",
        "border" = "INTEGER",
        "age" = "TEXT",
        "sex" = "TEXT",
        "year" = "INTEGER",
        "week" = "INTEGER",
        "yrwk" = "TEXT",
        "season" = "TEXT",
        "x" = "DOUBLE",
        "date" = "DATE",

        "n" = "INTEGER",
        "n_symps_0" = "INTEGER",
        "n_symps_1" = "INTEGER",
        "n_symps_2" = "INTEGER",
        "n_symps_3" = "INTEGER",
        "n_symps_4" = "INTEGER",
        "n_symps_5" = "INTEGER",
        "n_symps_6" = "INTEGER",
        "n_symps_7" = "INTEGER",
        "n_symps_8" = "INTEGER",
        "n_symps_9" = "INTEGER",
        "n_symps_10" = "INTEGER",
        "n_symp_cough" = "INTEGER",
        "n_symp_fever" = "INTEGER",
        "n_symp_throat" = "INTEGER",
        "n_symp_headache" = "INTEGER",
        "n_symp_muscle" = "INTEGER",
        "n_symp_breath" = "INTEGER",
        "n_symp_taste_smell" = "INTEGER",
        "n_symp_other" = "INTEGER",
        "n_symps_yesfever_cough_or_breath" = "INTEGER",
        "n_symps_nofever_cough_or_breath" = "INTEGER",
        "n_symps_yesfever_throat_or_nose" = "INTEGER",
        "n_symps_nofever_throat_or_nose" = "INTEGER",
        "n_symps_yesfever_muscle_or_headache" = "INTEGER",
        "n_symps_nofever_muscle_or_headache" = "INTEGER",
        "n_symps_yesfever_gastro_or_taste_smell_or_other" = "INTEGER",
        "n_symps_nofever_gastro_or_taste_smell_or_other" = "INTEGER",
        "n_today_0normal" = "INTEGER",
        "n_today_1tired" = "INTEGER",
        "n_today_2need_rest" = "INTEGER",
        "n_today_3bedridden_some_help" = "INTEGER",
        "n_today_4bedridden_lots_help" = "INTEGER",
        "n_tested" = "INTEGER",
        "n_tested_has_any_result" = "INTEGER",
        "n_tested_has_pos_result" = "INTEGER",
        "n_contact_doctor_yes" = "INTEGER",
        "n_contact_doctor_no" = "INTEGER"
      ),
      db_load_folder = tempdir(),
      keys =  c(
        "granularity_time",
        "location_code",
        "date",
        "age",
        "sex"
      ),
      validator_field_types = sc::validator_field_types_sykdomspulsen,
      validator_field_contents = sc::validator_field_contents_sykdomspulsen
    )
  )

  # datar ----
  # datar_weather ----
  sc::add_schema(
    name = "datar_weather",
    schema = sc::Schema$new(
      db_config = sc::config$db_config,
      db_table = "datar_weather",
      db_field_types =  c(
        "granularity_time" = "TEXT",
        "granularity_geo" = "TEXT",
        "location_code" = "TEXT",
        "border" = "INTEGER",
        "age" = "TEXT",
        "sex" = "TEXT",
        "year" = "INTEGER",
        "week" = "INTEGER",
        "yrwk" = "TEXT",
        "season" = "TEXT",
        "x" = "DOUBLE",
        "date" = "DATE",

        "tg" = "DOUBLE",
        "tx" = "DOUBLE",
        "tn" = "DOUBLE",
        "rr" = "DOUBLE",
        "forecast" = "BOOLEAN"
      ),
      db_load_folder = tempdir(),
      keys =  c(
        "location_code",
        "date"
      ),
      validator_field_types = sc::validator_field_types_sykdomspulsen,
      validator_field_contents = sc::validator_field_contents_sykdomspulsen
    )
  )

  # datar_normomo ----
  sc::add_schema(
    name = "datar_normomo",
    schema = sc::Schema$new(
      db_config = sc::config$db_config,
      db_table = "datar_normomo",
      db_field_types =  c(
        "uuid" = "TEXT",
        "DoD" = "DATE",
        "DoR" = "DATE",
        "DoB" = "DATE",
        "age" = "INTEGER",
        "sex" = "TEXT",
        "location_code" = "TEXT",
        "date_extracted" = "DATE"
      ),
      db_load_folder = tempdir(),
      keys =  c(
        "uuid"
      )
    )
  )

  # datar_norsyss_kht_email ----
  sc::add_schema(
    name = "datar_norsyss_kht_email",
    schema = sc::Schema$new(
      db_config = sc::config$db_config,
      db_table = "datar_norsyss_kht_email",
      db_field_types =  c(
        "location_code" = "TEXT",
        "email" = "TEXT"
      ),
      db_load_folder = tempdir(),
      keys =  c(
        "location_code",
        "email"
      )
    )
  )

  # data ----
  # data_weather ----
  sc::add_schema(
    name = "data_weather",
    schema = sc::Schema$new(
      db_config = sc::config$db_config,
      db_table = "data_weather",
      db_field_types =  c(
        "granularity_time" = "TEXT",
        "granularity_geo" = "TEXT",
        "location_code" = "TEXT",
        "border" = "INTEGER",
        "age" = "TEXT",
        "sex" = "TEXT",
        "year" = "INTEGER",
        "week" = "INTEGER",
        "yrwk" = "TEXT",
        "season" = "TEXT",
        "x" = "DOUBLE",
        "date" = "DATE",

        "tg" = "DOUBLE",
        "tx" = "DOUBLE",
        "tn" = "DOUBLE",
        "rr" = "DOUBLE",
        "forecast" = "BOOLEAN"
      ),
      db_load_folder = tempdir(),
      keys =  c(
        "location_code",
        "date"
      ),
      validator_field_types = sc::validator_field_types_sykdomspulsen,
      validator_field_contents = sc::validator_field_contents_sykdomspulsen
    )
  )

  # data_normomo ----
  sc::add_schema(
    name = "data_normomo",
    schema = sc::Schema$new(
      db_config = sc::config$db_config,
      db_table = "data_normomo",
      db_field_types =  c(
        "granularity_time" = "TEXT",
        "granularity_geo" = "TEXT",
        "location_code" = "TEXT",
        "border" = "INTEGER",
        "age" = "TEXT",
        "sex" = "TEXT",
        "year" = "INTEGER",
        "week" = "INTEGER",
        "yrwk" = "TEXT",
        "season" = "TEXT",
        "x" = "DOUBLE",
        "date" = "DATE",

        "n_obs" = "INTEGER",
        "ncor_est" = "DOUBLE"
      ),
      db_load_folder = tempdir(),
      keys =  c(
        "granularity_time",
        "location_code",
        "age",
        "sex",
        "date"
      ),
      validator_field_types = sc::validator_field_types_sykdomspulsen,
      validator_field_contents = sc::validator_field_contents_sykdomspulsen
    )
  )

  # data_norsyss ----
  sc::add_schema(
    name = "data_norsyss",
    schema = sc::Schema$new(
      db_table = "data_norsyss",
      db_config = sc::config$db_config,
      db_field_types =  c(
        "granularity_time" = "TEXT",
        "granularity_geo" = "TEXT",
        "location_code" = "TEXT",
        "border" = "INTEGER",
        "age" = "TEXT",
        "sex" = "TEXT",
        "year" = "INTEGER",
        "week" = "INTEGER",
        "yrwk" = "TEXT",
        "season" = "TEXT",
        "x" = "DOUBLE",
        "date" = "DATE",

        "tag_outcome" = "TEXT",
        "holiday" = "DOUBLE",
        "n" = "INTEGER",
        "pop" = "INTEGER",
        "consult_with_influenza" = "INTEGER",
        "consult_without_influenza" = "INTEGER"
      ),
      db_load_folder = tempdir(),
      keys =  c(
        "tag_outcome",
        "location_code",
        "year",
        "date",
        "age"
      ),
      indexes = list(
        "ind1" = c("year"),
        "ind2" = c("year", "tag_outcome")
      ),
      validator_field_types = sc::validator_field_types_sykdomspulsen,
      validator_field_contents = sc::validator_field_contents_sykdomspulsen
    )
  )

  # data_msis ----
  sc::add_schema(
    name = "data_msis",
    schema = sc::Schema$new(
      db_config = sc::config$db_config,
      db_table = "data_msis",
      db_field_types =  c(
        "tag_outcome" = "TEXT",
        "location_code" = "TEXT",
        "granularity_time" = "TEXT",
        "granularity_geo" = "TEXT",
        "border" = "INTEGER",
        "age" = "TEXT",
        "sex" = "TEXT",
        "season" = "TEXT",
        "yrwk" = "TEXT",
        "year" = "INTEGER",
        "week" = "INTEGER",
        "n" = "INTEGER",
        "date" = "DATE",

        "month" = "TEXT"
      ),
      db_load_folder = tempdir(),
      keys =  c(
        "tag_outcome",
        "location_code",
        "year",
        "date"
      )
    )
  )

  # results ----
  # results_normomo_standard ----
  sc::add_schema(
    name = "results_normomo_standard",
    schema = sc::Schema$new(
      db_config = sc::config$db_config,
      db_table = "results_normomo_standard",
      db_field_types =  c(
        "granularity_time" = "TEXT",
        "granularity_geo" = "TEXT",
        "location_code" = "TEXT",
        "border" = "INTEGER",
        "age" = "TEXT",
        "sex" = "TEXT",
        "year" = "INTEGER",
        "week" = "INTEGER",
        "yrwk" = "TEXT",
        "season" = "TEXT",
        "x" = "DOUBLE",
        "date" = "DATE",

        "n_obs" = "INTEGER",

        "ncor_est" = "DOUBLE",
        "ncor_thresholdl0" = "DOUBLE",
        "ncor_thresholdu0" = "DOUBLE",
        "ncor_zscore" = "DOUBLE",
        "ncor_status" = "TEXT",
        "ncor_excess" = "DOUBLE",

        "ncor_baseline_expected" = "DOUBLE",
        "ncor_baseline_thresholdl0" = "DOUBLE",
        "ncor_baseline_thresholdu0" = "DOUBLE",
        "ncor_baseline_thresholdu1" = "DOUBLE",

        "forecast" = "BOOLEAN"
      ),
      db_load_folder = tempdir(),
      keys = c(
        "granularity_time",
        "granularity_geo",
        "location_code",
        "age",
        "sex",
        "year",
        "week",
        "date"
      ),
      validator_field_types = sc::validator_field_types_sykdomspulsen,
      validator_field_contents = sc::validator_field_contents_sykdomspulsen
    )
  )

  # results_norsyss_standard ----
  sc::add_schema(
    name = "results_norsyss_standard",
    schema = sc::Schema$new(
      db_table = "results_norsyss_standard",
      db_config = sc::config$db_config,
      db_field_types =  c(
        "granularity_time" = "TEXT",
        "granularity_geo" = "TEXT",
        "location_code" = "TEXT",
        "border" = "INTEGER",
        "age" = "TEXT",
        "sex" = "TEXT",
        "year" = "INTEGER",
        "week" = "INTEGER",
        "yrwk" = "TEXT",
        "season" = "TEXT",
        "x" = "DOUBLE",
        "date" = "DATE",

        "tag_outcome" = "TEXT",
        "n" = "INTEGER",
        "n_denominator" = "INTEGER",
        "n_baseline_expected" = "DOUBLE",
        "n_baseline_thresholdu0" = "DOUBLE",
        "n_baseline_thresholdu1" = "DOUBLE",
        "n_baseline_thresholdu2" = "DOUBLE",
        "n_zscore" = "DOUBLE",
        "n_status" = "TEXT",
        "failed" = "TINYINT",
        "source" = "TEXT"
      ),
      db_load_folder = tempdir(),
      keys =  c(
        "granularity_time",
        "granularity_geo",
        "tag_outcome",
        "location_code",
        "age",
        "year",
        "week",
        "date"
      ),
      validator_field_types = sc::validator_field_types_sykdomspulsen,
      validator_field_contents = sc::validator_field_contents_sykdomspulsen
    )
  )

  # results_norsyss_mem ----
  sc::add_schema(
    name = "results_norsyss_mem",
    schema = sc::Schema$new(
      db_table = "results_norsyss_mem",
      db_config = sc::config$db_config,
      db_field_types =  c(
        "granularity_time" = "TEXT",
        "granularity_geo" = "TEXT",
        "location_code" = "TEXT",
        "border" = "INTEGER",
        "age" = "TEXT",
        "sex" = "TEXT",
        "year" = "INTEGER",
        "week" = "INTEGER",
        "yrwk" = "TEXT",
        "season" = "TEXT",
        "x" = "DOUBLE",
        "date" = "DATE",

        "tag_outcome" = "TEXT",
        "n" = "INTEGER",
        "n_denominator" = "INTEGER",
        "rp100" = "DOUBLE",
        "rp100_baseline_thresholdu0" = "DOUBLE",
        "rp100_baseline_thresholdu1" = "DOUBLE",
        "rp100_baseline_thresholdu2" = "DOUBLE",
        "rp100_baseline_thresholdu3" = "DOUBLE",
        "rp100_status"= "TEXT"
      ),
      db_load_folder = tempdir(),
      keys =  c(
        "tag_outcome",
        "location_code",
        "year",
        "date",
        "age"
      ),
      validator_field_types = sc::validator_field_types_sykdomspulsen,
      validator_field_contents = sc::validator_field_contents_sykdomspulsen
    )
  )

  # results_norsyss_mem_limits ----
  sc::add_schema(
    name = "results_norsyss_mem_limits",
    schema = sc::Schema$new(
      db_table = "results_norsyss_mem_limits",
      db_config = sc::config$db_config,
      db_field_types = c(
        "granularity_time" = "TEXT",
        "granularity_geo" = "TEXT",
        "location_code" = "TEXT",
        "border" = "INTEGER",
        "age" = "TEXT",
        "sex" = "TEXT",
        "year" = "INTEGER",
        "week" = "INTEGER",
        "yrwk" = "TEXT",
        "season" = "TEXT",
        "x" = "DOUBLE",
        "date" = "DATE",

        "tag_outcome" = "TEXT",
        "rp100_baseline_thresholdu0" = "DOUBLE",
        "rp100_baseline_thresholdu1" = "DOUBLE",
        "rp100_baseline_thresholdu2" = "DOUBLE",
        "rp100_baseline_thresholdu3" = "DOUBLE"
      ),
      db_load_folder = tempdir(),
      keys = c("season", "tag_outcome", "age", "location_code"),
      validator_field_types = sc::validator_field_types_sykdomspulsen,
      validator_field_contents = sc::validator_field_contents_sykdomspulsen
    )
  )

  # results_covid19_areas_at_risk ----
  sc::add_schema(
    name = "results_covid19_areas_at_risk",
    schema = sc::Schema$new(
      db_table = "results_covid19_areas_at_risk",
      db_config = sc::config$db_config,
      db_field_types =  c(
        "granularity_time" = "TEXT",
        "granularity_geo" = "TEXT",
        "location_code" = "TEXT",
        "border" = "INTEGER",
        "age" = "TEXT",
        "sex" = "TEXT",
        "year" = "INTEGER",
        "week" = "INTEGER",
        "yrwk" = "TEXT",
        "season" = "TEXT",
        "x" = "DOUBLE",
        "date" = "DATE",

        "tag_outcome"= "TEXT",
        "n" =  "INTEGER",
        "n_denominator" = "INTEGER",
        "n_baseline_expected" = "INTEGER",
        "n_baseline_thresholdu0" = "INTEGER",
        "pr100" = "DOUBLE",
        "pr100_baseline_expected" = "DOUBLE",
        "pr100_baseline_thresholdu0" = "DOUBLE",
        "n_status" = "TEXT"

      ),
      db_load_folder = tempdir(),
      keys =  c(
        "granularity_time",
        "granularity_geo",
        "tag_outcome",
        "location_code",
        "age",
        "year",
        "week",
        "date"
      ),
      validator_field_types = sc::validator_field_types_sykdomspulsen,
      validator_field_contents = sc::validator_field_contents_sykdomspulsen
    )
  )

  # results_covid19_metrics ----
  sc::add_schema(
    name = "results_covid19_metrics",
    schema = sc::Schema$new(
      db_table = "results_covid19_metrics",
      db_config = sc::config$db_config,
      db_field_types =  c(
        "granularity_time" = "TEXT",
        "granularity_geo" = "TEXT",
        "location_code" = "TEXT",
        "border" = "INTEGER",
        "age" = "TEXT",
        "sex" = "TEXT",
        "year" = "INTEGER",
        "week" = "INTEGER",
        "yrwk" = "TEXT",
        "season" = "TEXT",
        "x" = "DOUBLE",
        "date" = "DATE",

        "tag_outcome" = "TEXT",
        "value" = "DOUBLE",
        "formatted" = "TEXT",
        "censor" = "BOOLEAN"
      ),
      db_load_folder = tempdir(),
      keys =  c(
        "granularity_time",
        "granularity_geo",
        "location_code",
        "age",
        "year",
        "week",
        "date",

        "tag_outcome"
      ),
      validator_field_types = sc::validator_field_types_sykdomspulsen,
      validator_field_contents = sc::validator_field_contents_sykdomspulsen
    )
  )

  sc::add_schema(
    name = "results_simple",
    schema = sc::Schema$new(
      db_table = "results_simple",
      db_config = sc::config$db_config,
      db_field_types =  c(
        "tag_outcome" = "TEXT",
        "source" = "TEXT",
        "location_code" = "TEXT",
        "granularity_time" = "TEXT",
        "granularity_geo" = "TEXT",
        "border" = "INTEGER",
        "age" = "TEXT",
        "sex" = "TEXT",
        "date" = "DATE",
        "season" = "TEXT",
        "yrwk" = "TEXT",
        "year" = "INTEGER",
        "week" = "INTEGER",
        "month" = "TEXT",
        "n" = "TEXT",
        "n_expected" = "DOUBLE",
        "n_threshold_0" = "DOUBLE",
        "n_status"= "TEXT"
      ),
      db_load_folder = tempdir(),
      keys =  c(
        "tag_outcome",
        "location_code",
        "year",
        "date"
      )
    )
  )


}

