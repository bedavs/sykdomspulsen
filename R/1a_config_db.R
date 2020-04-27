set_db <- function(){
  config$db_config <- list(
    driver = Sys.getenv("DB_DRIVER", "MySQL"),
    server = Sys.getenv("DB_SERVER", "db"),
    port = as.integer(Sys.getenv("DB_PORT", 1433)),
    user = Sys.getenv("DB_USER", "root"),
    password = Sys.getenv("DB_PASSWORD", "example"),
    db = Sys.getenv("DB_DB", "sykdomspuls"),
    trusted_connection = Sys.getenv("DB_TRUSTED_CONNECTION")
  )

  # set schema ----
  config$schema <- list(
    # rundate ----
    rundate = Schema$new(
      db_config = config$db_config,
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
    ),

    # covid19 ----
    # results_covid19_model ----
    results_covid19_model = Schema$new(
      db_table = "results_covid19_model",
      db_config = config$db_config,
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
      )
    ),

    # data_covid19_msis_by_time_location ----
    data_covid19_msis_by_time_location = Schema$new(
      db_table = "data_covid19_msis_by_time_location",
      db_config = config$db_config,
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
      )
    ),

    # data_covid19_msis_by_time_infected_abroad ----
    data_covid19_msis_by_time_infected_abroad = Schema$new(
      db_table = "data_covid19_msis_by_time_infected_abroad",
      db_config = config$db_config,
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
      )
    ),

    # data_covid19_msis_by_sex_age ----
    data_covid19_msis_by_sex_age = Schema$new(
      db_table = "data_covid19_msis_by_sex_age",
      db_config = config$db_config,
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
      )
    ),

    # data_covid19_lab_by_time ----
    data_covid19_lab_by_time = Schema$new(
      db_table = "data_covid19_lab_by_time",
      db_config = config$db_config,
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
        "pr100_pos" = "DOUBLE"
      ),
      db_load_folder = tempdir(),
      keys =  c(
        "granularity_time",
        "location_code",
        "date"
      )
    ),

    # data_covid19_nir_by_time ----
    data_covid19_nir_by_time = Schema$new(
      db_table = "data_covid19_nir_by_time",
      db_config = config$db_config,
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

        "n_icu" = "INTEGER"
      ),
      db_load_folder = tempdir(),
      keys =  c(
        "granularity_time",
        "location_code",
        "date"
      )
    ),

    # delete me soon data_covid19_msis ----
    data_covid19_msis = Schema$new(
      db_table = "data_covid19_msis",
      db_config = config$db_config,
      db_field_types =  c(
        "tag_outcome" = "TEXT",
        "location_code" = "TEXT",
        "granularity_time" = "TEXT",
        "granularity_geo" = "TEXT",
        "border" = "INTEGER",
        "age" = "TEXT",
        "sex" = "TEXT",
        "date" = "DATE",
        "yrwk" = "TEXT",
        "year" = "INTEGER",
        "week" = "INTEGER",
        "season" = "TEXT",
        "x" = "DOUBLE",

        "n" = "INTEGER"
      ),
      db_load_folder = tempdir(),
      keys =  c(
        "granularity_time",
        "tag_outcome",
        "location_code",
        "date"
      )
    ),

    # datar ----
    # datar_weather ----
    datar_weather = Schema$new(
      db_config = config$db_config,
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
      )
    ),
    # datar_normomo ----
    datar_normomo = Schema$new(
      db_config = config$db_config,
      db_table = "datar_normomo",
      db_field_types =  c(
        "uuid" = "TEXT",
        "DoD" = "DATE",
        "DoR" = "DATE",
        "DoB" = "DATE",
        "age" = "INTEGER",
        "location_code" = "TEXT",
        "date_extracted" = "DATE"
      ),
      db_load_folder = tempdir(),
      keys =  c(
        "uuid"
      )
    ),

    # data ----
    # data_weather ----
    data_weather = Schema$new(
      db_config = config$db_config,
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
      )
    ),

    # data_norsyss ----
    data_norsyss = Schema$new(
      db_table = "data_norsyss",
      db_config = config$db_config,
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
      )
    ),

    # data_msis ----
    data_msis = Schema$new(
      db_config = config$db_config,
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
    ),

    # results ----
    # results_normomo_standard ----
    results_normomo_standard = Schema$new(
      db_config = config$db_config,
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
      db_load_folder = tempdir()
    ),
    # results_norsyss_standard ----
    results_norsyss_standard = Schema$new(
      db_table = "results_norsyss_standard",
      db_config = config$db_config,
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
      )
    ),

    results_simple = Schema$new(
      db_table = "results_simple",
      db_config = config$db_config,
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
    ),
    results_mem = Schema$new(
      db_table = "results_mem",
      db_config = config$db_config,
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
      )
    ),
    results_mem_limits = Schema$new(
      db_table = "results_mem_limits",
      db_config = config$db_config,
      db_field_types = list(
        "season" = "TEXT",
        "tag_outcome" = "TEXT",
        "age" = "TEXT",
        "location_code" = "TEXT",
        "rp100_baseline_thresholdu0" = "DOUBLE",
        "rp100_baseline_thresholdu1" = "DOUBLE",
        "rp100_baseline_thresholdu2" = "DOUBLE",
        "rp100_baseline_thresholdu3" = "DOUBLE"
      ),
      db_load_folder = tempdir(),
      keys = c("season", "tag_outcome", "age", "location_code")
    )
  )
}

