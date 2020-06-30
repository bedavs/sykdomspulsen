set_tasks <- function() {
  # **** covid19 **** ----

  # data_pre_covid19_nordic ----
  sc::add_task(
    sc::task_from_config(
      name = "data_pre_covid19_nordic",
      type = "data",
      action = "sykdomspulsen::data_pre_covid19_nordic",
      schema = list(output = sc::config$schemas$data_covid19_nordic)
    )
  )

  # data_covid19_nordic ----
  sc::add_task(
    sc::task_from_config(
      name = "data_covid19_nordic",
      type = "data",
      action = "sykdomspulsen::data_covid19_nordic",
      schema = list(output = sc::config$schemas$data_covid19_nordic)
    )
  )

  # data_covid19_model ----
  sc::add_task(
    sc::task_from_config(
      name = "data_covid19_model",
      type = "data",
      action = "sykdomspulsen::data_covid19_model",
      schema = list(output = sc::config$schemas$results_covid19_model)
    )
  )

  # data_covid19_daily_report ----
  sc::add_task(
    sc::task_from_config(
      name = "data_covid19_daily_report",
      type = "data",
      action = "sykdomspulsen::data_covid19_daily_report",
      schema = list(
        data_covid19_msis_by_time_location = sc::config$schemas$data_covid19_msis_by_time_location,
        data_covid19_msis_by_time_infected_abroad = sc::config$schemas$data_covid19_msis_by_time_infected_abroad,
        data_covid19_msis_by_time_sex_age = sc::config$schemas$data_covid19_msis_by_time_sex_age,
        data_covid19_lab_by_time = sc::config$schemas$data_covid19_lab_by_time,
        data_covid19_lab_by_time_location = sc::config$schemas$data_covid19_lab_by_time_location,
        data_covid19_hospital_by_time = sc::config$schemas$data_covid19_hospital_by_time,
        data_covid19_deaths = sc::config$schemas$data_covid19_deaths,
        data_covid19_demographics = sc::config$schemas$data_covid19_demographics,
        data_covid19_dynamic_text = sc::config$schemas$data_covid19_dynamic_text

      )
    )
  )

  # prelim_data_covid19_daily_report ----
  sc::add_task(
    sc::task_from_config(
      name = "prelim_data_covid19_daily_report",
      type = "data",
      action = "sykdomspulsen::data_covid19_daily_report",
      schema = list(
        data_covid19_msis_by_time_location = sc::config$schemas$prelim_data_covid19_msis_by_time_location,
        data_covid19_msis_by_time_infected_abroad = sc::config$schemas$prelim_data_covid19_msis_by_time_infected_abroad,
        data_covid19_msis_by_time_sex_age = sc::config$schemas$prelim_data_covid19_msis_by_time_sex_age,
        data_covid19_lab_by_time = sc::config$schemas$prelim_data_covid19_lab_by_time,
        data_covid19_lab_by_time_location = sc::config$schemas$prelim_data_covid19_lab_by_time_location,
        data_covid19_hospital_by_time = sc::config$schemas$prelim_data_covid19_hospital_by_time,
        data_covid19_deaths = sc::config$schemas$prelim_data_covid19_deaths,
        data_covid19_demographics = sc::config$schemas$prelim_data_covid19_demographics
      )
    )
  )

  # data_covid19_self_reporting ----
  sc::add_task(
    sc::task_from_config(
      name = "data_covid19_self_reporting",
      type = "data",
      action = "sykdomspulsen::data_covid19_self_reporting",
      schema = list(
        output = sc::config$schemas$data_covid19_self_reporting
      )
    )
  )

  # **** data **** ----
  # data_pre_normomo ----
  sc::add_task(
    sc::task_from_config(
      name = "data_pre_normomo",
      type = "data",
      action = "sykdomspulsen::data_pre_normomo"
    )
  )
  # datar_normomo ----
  sc::add_task(
    sc::task_from_config(
      name = "datar_normomo",
      type = "data",
      action = "sykdomspulsen::datar_normomo",
      schema = list(output = sc::config$schemas$datar_normomo)
    )
  )
  # datar_normomo_drop ----
  sc::add_task(
    sc::task_from_config(
      name = "datar_normomo_drop",
      type = "data",
      action = "sykdomspulsen::datar_normomo_drop",
      schema = list(output = sc::config$schemas$datar_normomo)
    )
  )

  # datar_norsyss_registration ----
  sc::add_task(
    sc::task_from_config(
      name = "datar_norsyss_kht_email",
      type = "data",
      action = "sykdomspulsen::datar_norsyss_kht_email",
      schema = list(output = sc::config$schemas$datar_norsyss_kht_email)
    )
  )
  # datar_norsyss_kht_email_drop ----
  sc::add_task(
    sc::task_from_config(
      name = "datar_norsyss_kht_email_drop",
      type = "data",
      action = "sykdomspulsen::datar_norsyss_kht_email_drop",
      schema = list(output = sc::config$schemas$datar_norsyss_kht_email)
    )
  )

  # data_pre_norsyss ----
  sc::add_task(
    sc::task_from_config(
      name = "data_pre_norsyss",
      type = "data",
      action = "sykdomspulsen::data_pre_norsyss",
      schema = list(),
      args = list(
        date_from = "2006-01-01",
        diags = list(
          "influensa" = c("R80"),
          "gastro" = c("D11", "D70", "D73"),
          "respiratory" = c("R05", "R74", "R78", "R83"),
          "respiratoryexternal" = c("R05", "R74", "R78", "R83"),
          "respiratoryinternal" = c("R05", "R74", "R83"),
          "lungebetennelse" = c("R81"),
          "bronkitt" = c("R78"),
          "skabb" = c("S72"),

          "hoste" = c("R05"),
          "akkut_ovre_luftveisinfeksjon" = c("R74"),
          "luftveisinfeksjon_ika" = c("R83"),
          "luftveissykdom_ika" = c("R99"),
          "virusinfeksjon_ika" = c("A77"),
          "rxx_for_covid19" = c(
            "R01",
            "R02",
            "R03",
            "R04",
            "R05",
            "R06",
            "R07",
            "R08",
            "R09",
            "R21",
            "R24",
            "R25",
            "R27",
            #"R270000",
            "R29",
            #"R71",
            "R72",
            "R74",
            "R75",
            "R76",
            "R77",
            "R78",
            "R79",
            "R80",
            "R81",
            "R82",
            "R83",
            #"R95",
            #"R96",
            "R99",
            "R991"
            #"R9910000"
          ),

          "covid19" = c("R991", "R992"),
          "covid19_r991" = c("R991"),
          "covid19_r992" = c("R992"),
          "engstelig_luftveissykdom_ika" = c("R27")
        )
      )
    )
  )

  # data_norsyss ----
  sc::add_task(
    sc::task_from_config(
      name = "data_norsyss",
      type = "data",
      action = "sykdomspulsen::data_norsyss",
      schema = list(output = sc::config$schemas$data_norsyss),
      args = list(
        # v = (lege)vakt
        # k = (lege)kontor
        # o = oppmote
        # t = telefon
        # e = ekonsultasjon
        syndromes = rbind(
          data.table(
            tag_input = "influensa",
            tag_output = "influensa_vk_o",
            practice_type = list(c("legevakt", "legekontor")),
            contactType = list("oppmote")
          ),
          data.table(
            tag_input = "influensa",
            tag_output = "influensa_vk_ot",
            practice_type = list(c("legevakt", "legekontor")),
            contactType = list(c("oppmote", "telefonkontakt"))
          ),
          data.table(
            tag_input = "gastro",
            tag_output = "gastro_vk_ot",
            practice_type = list(c("legevakt", "legekontor")),
            contactType = list(c("oppmote", "telefonkontakt"))
          ),
          data.table(
            tag_input = "respiratoryinternal",
            tag_output = "respiratoryinternal_vk_ot",
            practice_type = list(c("legevakt", "legekontor")),
            contactType = list(c("oppmote", "telefonkontakt"))
          ),
          data.table(
            tag_input = "respiratoryexternal",
            tag_output = "respiratoryexternal_vk_ot",
            practice_type = list(c("legevakt", "legekontor")),
            contactType = list(c("oppmote", "telefonkontakt"))
          ),

          # ote
          data.table(
            tag_input = "covid19",
            tag_output = "covid19_vk_ote",
            practice_type = list(c("legevakt", "legekontor")),
            contactType = list(c("oppmote", "telefonkontakt", "ekonsultasjon"))
          ),
          data.table(
            tag_input = "covid19_r991",
            tag_output = "covid19_r991_vk_ote",
            practice_type = list(c("legevakt", "legekontor")),
            contactType = list(c("oppmote", "telefonkontakt", "ekonsultasjon"))
          ),
          data.table(
            tag_input = "covid19_r992",
            tag_output = "covid19_r992_vk_ote",
            practice_type = list(c("legevakt", "legekontor")),
            contactType = list(c("oppmote", "telefonkontakt", "ekonsultasjon"))
          ),
          data.table(
            tag_input = "influensa",
            tag_output = "influensa_vk_ote",
            practice_type = list(c("legevakt", "legekontor")),
            contactType = list(c("oppmote", "telefonkontakt", "ekonsultasjon"))
          ),
          data.table(
            tag_input = "rxx_for_covid19",
            tag_output = "rxx_for_covid19_vk_ote",
            practice_type = list(c("legevakt", "legekontor")),
            contactType = list(c("oppmote", "telefonkontakt", "ekonsultasjon"))
          ),
          data.table(
            tag_input = "akkut_ovre_luftveisinfeksjon",
            tag_output = "akkut_ovre_luftveisinfeksjon_vk_ote",
            practice_type = list(c("legevakt", "legekontor")),
            contactType = list(c("oppmote", "telefonkontakt", "ekonsultasjon"))
          ),
          data.table(
            tag_input = "engstelig_luftveissykdom_ika",
            tag_output = "engstelig_luftveissykdom_ika_vk_ote",
            practice_type = list(c("legevakt", "legekontor")),
            contactType = list(c("oppmote", "telefonkontakt", "ekonsultasjon"))
          ),

          # strata
          data.table(
            tag_input = "covid19",
            tag_output = "covid19_v_o",
            practice_type = list(c("legevakt")),
            contactType = list(c("oppmote"))
          ),
          data.table(
            tag_input = "covid19",
            tag_output = "covid19_v_t",
            practice_type = list(c("legevakt")),
            contactType = list(c("telefonkontakt"))
          ),
          data.table(
            tag_input = "covid19",
            tag_output = "covid19_v_e",
            practice_type = list(c("legevakt")),
            contactType = list(c("ekonsultasjon"))
          ),
          data.table(
            tag_input = "covid19",
            tag_output = "covid19_k_o",
            practice_type = list(c("legekontor")),
            contactType = list(c("oppmote"))
          ),
          data.table(
            tag_input = "covid19",
            tag_output = "covid19_k_t",
            practice_type = list(c("legekontor")),
            contactType = list(c("telefonkontakt"))
          ),
          data.table(
            tag_input = "covid19",
            tag_output = "covid19_k_e",
            practice_type = list(c("legekontor")),
            contactType = list(c("ekonsultasjon"))
          )
        )
      )
    )
  )

  # datar_weather ----
  sc::add_task(
    sc::task_from_config(
      name = "datar_weather",
      type = "data",
      action = "sykdomspulsen::datar_weather",
      schema = list(output = sc::config$schemas$datar_weather)
    )
  )
  # data_weather ----
  sc::add_task(
    sc::task_from_config(
      name = "data_weather",
      type = "data",
      action = "sykdomspulsen::data_weather",
      schema = list(
        input = sc::config$schemas$datar_weather,
        output = sc::config$schemas$data_weather
      )
    )
  )
  # data_msis ----
  sc::add_task(
    sc::task_from_config(
      name = "data_msis",
      type = "data",
      action = "sykdomspulsen::data_msis",
      schema = list(output = sc::config$schemas$data_msis),
      args = list(
        start_year = 2008,
        end_year = 2019,
        tags = c("Kikoste", "Campylobacteriose")
      )
    )
  )

  # **** analysis **** ----
  # analysis_normomo ----
  sc::add_task(
    sc::Task$new(
      name = "analysis_normomo",
      type = "analysis",
      update_plans_fn = analysis_normomo_plans,
      schema = c(
        "output" = sc::config$schemas$results_normomo_standard,
        "data_normomo" = sc::config$schemas$data_normomo
        ),
      cores = min(6, parallel::detectCores())
    )
  )

  # analysis_norsyss_qp_weekly ----
  sc::add_task(
    sc::task_from_config(
      name = "analysis_norsyss_qp_weekly",
      db_table = "data_norsyss",
      type = "analysis",
      cores = min(7, parallel::detectCores()),
      action = "sykdomspulsen::analysis_qp",
      for_each_plan = list(
        "age" = "all",
        "sex" = "total",
        "tag_outcome" = c(
          "gastro_vk_ot",
          "respiratoryexternal_vk_ot"
        )
      ),
      for_each_argset = list("location_code" = "all"),
      schema = list(
        output = sc::config$schemas$results_norsyss_standard
      ),
      upsert_at_end_of_each_plan = TRUE,
      args = list(
        train_length = 5,
        years = c(2015:fhi::isoyear_n()),
        weeklyDenominatorFunction = sum,
        denominator = "consult_without_influenza",
        granularity_time = "week"
      )
    )
  )

  # analysis_norsyss_qp_daily ----
  sc::add_task(
    sc::task_from_config(
      name = "analysis_norsyss_qp_daily",
      db_table = "data_norsyss",
      type = "analysis",
      cores = min(7, parallel::detectCores()),
      action = "sykdomspulsen::analysis_qp",
      filter = "(granularity_geo=='county' | granularity_geo=='nation')",
      for_each_plan = list(
        "age" = "all",
        "sex" = "total",
        "tag_outcome" = c(
          "gastro_vk_ot",
          "respiratoryexternal_vk_ot"
        )
      ),
      for_each_argset = list("location_code" = "all"),
      schema = list(
        output = sc::config$schemas$results_norsyss_standard
      ),
      upsert_at_end_of_each_plan = TRUE,
      args = list(
        train_length = 5,
        years = c(2018, 2019, 2020),
        weeklyDenominatorFunction = sum,
        denominator = "consult_without_influenza",
        granularity_time = "day"
      )
    )
  )

  # analysis_norsyss_mem_influensa_vk_o ----
  sc::add_task(
    sc::task_from_config(
      name = "analysis_norsyss_mem_influensa_vk_o",
      db_table = "data_norsyss",
      type = "analysis",
      action = "sykdomspulsen::analysis_mem",
      filter = "(granularity_geo=='county' | granularity_geo=='nation') & tag_outcome=='influensa_vk_o'",
      for_each_plan = list("location_code" = "all"),
      schema = list(
        output = sc::config$schemas$results_norsyss_mem,
        output_limits = sc::config$schemas$results_norsyss_mem_limits
      ),
      args = list(
        age = jsonlite::toJSON(list("total" = c("total"))),
        tag = "influensa_vk_o",
        weeklyDenominatorFunction = "sum",
        multiplicative_factor = 100,
        denominator = "consult_with_influenza"
      )
    )
  )

  # analysis_norsyss_mem_influensa_vk_ot ----
  sc::add_task(
    sc::task_from_config(
      name = "analysis_norsyss_mem_influensa_vk_ot",
      db_table = "data_norsyss",
      type = "analysis",
      action = "sykdomspulsen::analysis_mem",
      filter = "(granularity_geo=='county' | granularity_geo=='nation') & tag_outcome=='influensa_vk_ot'",
      for_each_plan = list("location_code" = "all"),
      schema = list(
        output = sc::config$schemas$results_norsyss_mem,
        output_limits = sc::config$schemas$results_norsyss_mem_limits
      ),
      args = list(
        age = jsonlite::toJSON(list(
          "0-4" = c("0-4"), "5-14" = c("5-14"),
          "15-64" = c("15-19", "20-29", "30-64"), "65+" = c("65+")
        )),
        tag = "influensa_vk_ot",
        weeklyDenominatorFunction = "sum",
        multiplicative_factor = 100,
        denominator = "consult_with_influenza"
      )
    )
  )

  # analysis_covid19_areas_at_risk ----
  sc::add_task(
    sc::Task$new(
      name = "analysis_covid19_areas_at_risk",
      type = "analysis",
      cores = min(7, parallel::detectCores()),
      update_plans_fn = analysis_covid19_areas_at_risk_plans,
      upsert_at_end_of_each_plan = TRUE,
      schema = c(
        "output" = sc::config$schemas$results_covid19_areas_at_risk
      )
    )
  )

  # analysis_covid19_metrics ----
  sc::add_task(
    sc::Task$new(
      name = "analysis_covid19_metrics",
      type = "analysis",
      update_plans_fn = analysis_covid19_metrics_plans,
      schema = c(
        "output" = sc::config$schemas$results_covid19_metrics
      )
    )
  )

  # analysis_covid19_nordic ----
  sc::add_task(
    sc::task_from_config(
      name = "analysis_covid19_nordic",
      type = "analysis",
      db_table = "data_covid19_nordic",
      action = "sykdomspulsen::analysis_covid19_nordic",
      schema = list(output = sc::config$schemas$results_covid19_nordic),
      for_each_plan = list("border" = "all")
    )
  )

  sc::add_task(
    sc::task_from_config(
      name = "analysis_simple_msis",
      type = "analysis",
      db_table = "data_msis",
      action = "sykdomspulsen::analysis_simple",
      schema = list(output = sc::config$schemas$results_simple),
      for_each_plan = list("location_code" = "all", "tag_outcome" = c("Kikoste", "Campylobacteriose")),
      args = list(
        group_by = "month",
        past_years = 5
      )
    )
  )

  # **** ui ****----
  # ui_surveillance_data ----
  sc::add_task(
    sc::task_from_config(
      name = "ui_surveillance_data",
      type = "single",
      action = "sykdomspulsen::ui_surveillance_data",
      schema = list(
        data_covid19_msis_by_time_location=sc::config$schemas$data_covid19_msis_by_time_location,
        data_covid19_msis_by_time_sex_age = sc::config$schemas$data_covid19_msis_by_time_sex_age,
        data_covid19_lab_by_time = sc::config$schemas$data_covid19_lab_by_time,
        data_covid19_hospital_by_time = sc::config$schemas$data_covid19_hospital_by_time,
        data_covid19_demographics = sc::config$schemas$data_covid19_demographics
      )
    )
  )

  # ui_normomo_ssi ----
  sc::add_task(
    sc::task_from_config(
      name = "ui_normomo_ssi",
      type = "single",
      action = "sykdomspulsen::ui_normomo_ssi",
      schema = list(input=sc::config$schemas$results_normomo_standard),
    )
  )

  # ui_normomo_thresholds_1yr_5yr ----
  sc::add_task(
    sc::task_from_config(
      name = "ui_normomo_thresholds_1yr_5yr",
      type = "ui",
      action = "sykdomspulsen::ui_normomo_thresholds_1yr_5yr",
      db_table = "results_normomo_standard",
      schema = list(input=sc::config$schemas$results_normomo_standard),
      for_each_plan = list("location_code" = "all", "age" = "all"),
      args = list(
        folder = "sykdomspulsen_normomo_restricted_output/{argset$today}/graphs_thresholds",
        filename = "{tag}_{argset$location_code}_{argset$age}_{argset$today}.png"
      )
    )
  )

  # ui_normomo_overview_by_location ----
  sc::add_task(
    sc::task_from_config(
      name = "ui_normomo_overview_by_location",
      type = "ui",
      action = "sykdomspulsen::ui_normomo_overview",
      db_table = "results_normomo_standard",
      schema = list(input=sc::config$schemas$results_normomo_standard),
      for_each_plan = list("age" = "all"),
      args = list(
        folder = "sykdomspulsen_normomo_restricted_output/{argset$today}/overview",
        filename = "by_{argset$by}_{argset$age}_{argset$today}.png",
        by="location"
      )
    )
  )

  # ui_normomo_overview_by_age ----
  sc::add_task(
    sc::task_from_config(
      name = "ui_normomo_overview_by_age",
      type = "ui",
      action = "sykdomspulsen::ui_normomo_overview",
      db_table = "results_normomo_standard",
      schema = list(input=sc::config$schemas$results_normomo_standard),
      for_each_plan = list("location_code" = "all"),
      args = list(
        folder = "sykdomspulsen_normomo_restricted_output/{argset$today}/overview",
        filename = "by_{argset$by}_{argset$location_code}_{argset$today}.png",
        by="age"
      )
    )
  )

  # ui_normomo_table_overview ----
  sc::add_task(
    sc::task_from_config(
      name = "ui_normomo_table_overview",
      type = "ui",
      action = "sykdomspulsen::ui_normomo_table_overview",
      db_table = "results_normomo_standard",
      schema = list(input=sc::config$schemas$results_normomo_standard),
      for_each_plan = list("location_code" = c("all")),
      #filter = "age=='total'",
      args = list(
        folder = "sykdomspulsen_normomo_restricted_output/{argset$today}/overview",
        filename = "overview_{argset$location_code}_{argset$today}.png"
      )
    )
  )

  # ui_normomo_table_excess_only_sort_location ----
  sc::add_task(
    sc::task_from_config(
      name = "ui_normomo_table_excess_only_sort_location",
      type = "ui",
      action = "sykdomspulsen::ui_normomo_table_excess_only",
      db_table = "results_normomo_standard",
      schema = list(input=sc::config$schemas$results_normomo_standard),
      for_each_plan = list("border" = 2020),
      #filter = "age=='total'",
      args = list(
        folder = "sykdomspulsen_normomo_restricted_output/{argset$today}/overview",
        filename = "overview_excess_only_sort_{argset$sort}_{argset$today}.png",
        sort = "location"
      )
    )
  )

  # ui_normomo_table_excess_only_sort_age ----
  sc::add_task(
    sc::task_from_config(
      name = "ui_normomo_table_excess_only_sort_age",
      type = "ui",
      action = "sykdomspulsen::ui_normomo_table_excess_only",
      db_table = "results_normomo_standard",
      schema = list(input=sc::config$schemas$results_normomo_standard),
      for_each_plan = list("border" = 2020),
      #filter = "age=='total'",
      args = list(
        folder = "sykdomspulsen_normomo_restricted_output/{argset$today}/overview",
        filename = "overview_excess_only_sort_{argset$sort}_{argset$today}.png",
        sort = "age"
      )
    )
  )

  # ui_normomo_data_files ----
  sc::add_task(
    sc::task_from_config(
      name = "ui_normomo_data_files",
      type = "ui",
      action = "sykdomspulsen::ui_normomo_data_files",
      db_table = "results_normomo_standard",
      schema = list(input=sc::config$schemas$results_normomo_standard),
      for_each_plan = list("border" = config$border),
      #filter = "age=='total'",
      args = list(
        folder = "sykdomspulsen_normomo_restricted_output/{argset$today}/data",
        filename = "data_{argset$today}.xlsx",
        filename_public = "offentliggjort_data_{tag_location}_{argset$today}.xlsx"
      )
    )
  )

  # ui_normomo_email_internal ----
  sc::add_task(
    sc::task_from_config(
      name = "ui_normomo_email_internal",
      type = "single",
      action = "sykdomspulsen::ui_normomo_email_internal",
      schema = list(input=sc::config$schemas$results_normomo_standard),
      args = list(
        tab1 = "overview_norge_{argset$today}.png",
        tab1_filepath = "sykdomspulsen_normomo_restricted_output/{argset$today}/overview/overview_norge_{argset$today}.png",

        tab2 = "overview_excess_only_sort_location_{argset$today}.png",
        tab2_filepath = "sykdomspulsen_normomo_restricted_output/{argset$today}/overview/overview_excess_only_sort_location_{argset$today}.png",

        fig1 = "incl_reported_norge_total_{argset$today}.png",
        fig1_filepath = "sykdomspulsen_normomo_restricted_output/{argset$today}/graphs_thresholds/incl_reported_norge_total_{argset$today}.png",

        fig2 = "by_location_total_{argset$today}.png",
        fig2_filepath = "sykdomspulsen_normomo_restricted_output/{argset$today}/overview/by_location_total_{argset$today}.png",

        fig3 = "by_age_norge_{argset$today}.png",
        fig3_filepath = "sykdomspulsen_normomo_restricted_output/{argset$today}/overview/by_age_norge_{argset$today}.png"
      )
    )
  )

  # ui_norsyss_kht_email ----
  sc::add_task(
    sc::Task$new(
      name = "ui_norsyss_kht_email",
      type = "ui",
      permission = sc::config$permissions$ui_norsyss_kht_email,
      update_plans_fn = ui_norsyss_kht_email_plans,
      schema = c("input" = sc::config$schemas$results_norsyss_standard)
    )
  )

  # ui_norsyss_mem_influensa_vk_o ----
  sc::add_task(
    sc::task_from_config(
      name = "ui_norsyss_mem_influensa_vk_o",
      type = "ui",
      action = "sykdomspulsen::ui_mem_plots",
      db_table = "results_norsyss_mem",
      schema = NULL,
      for_each_plan = list(tag_outcome = c("influensa_vk_o")),
      args = list(
        tag = "influensa_vk_o",
        icpc2 = "R60",
        excludeSeason = c("2009/2010"),
        contactType = "oppmote, telefonkontakt",
        folder_name = "mem_influensa",
        outputs = c("charts", "county_sheet", "region_sheet", "norway_sheet")
      )
    )
  )

  # ui_norsyss_mem_influensa_vk_ot ----
  sc::add_task(
    sc::task_from_config(
      name = "ui_norsyss_mem_influensa_vk_ot",
      type = "ui",
      action = "sykdomspulsen::ui_mem_plots",
      db_table = "results_norsyss_mem",
      schema = NULL,
      for_each_plan = list(tag_outcome = c("influensa_vk_ot")),
      args = list(
        tag = "influensa_vk_ot",
        icpc2 = "R80",
        excludeSeason = c("2009/2010"),
        contactType = "oppmote",
        folder_name = "mem_influensa",
        outputs = c("n_doctors_sheet")
      )
    )
  )

  # ui_norsyss_pdf ----
  sc::add_task(
    sc::task_from_config(
      name = "ui_norsyss_pdf",
      type = "data",
      schema=list(input=sc::config$schemas$results_norsyss_standard),
      action="sykdomspulsen::ui_norsyss_pdf",
      args = list(
        tags = c("gastro_vk_ot","respiratoryexternal_vk_ot"),
        name_short = config$def$norsyss$short_names,
        name_long = config$def$norsyss$long_names
      )
    )
  )

  # ui_covid19_areas_at_risk_docx ----
  sc::add_task(
    sc::task_from_config(
      name = "ui_covid19_areas_at_risk_docx",
      type = "ui",
      action = "sykdomspulsen::ui_covid19_areas_at_risk_docx",
      db_table = "results_covid19_areas_at_risk",
      schema = list(input=sc::config$schemas$results_covid19_areas_at_risk),
      for_each_plan = list("border" = config$border),
      filter = "location_code != 'municip0301'",
      args = list(
        folder = "sykdomspulsen_norsyss_restricted_output/covid19_at_risk/{argset$today}",
        filename = "covid19_areas_at_risk_{argset$today}.docx"
      )
    )
  )

  # ui_covid19_areas_at_risk_utbrudd_email ----
  sc::add_task(
    sc::task_from_config(
      name = "ui_covid19_areas_at_risk_utbrudd_email",
      type = "single",
      action = "sykdomspulsen::ui_covid19_areas_at_risk_utbrudd_email",
      schema = list(input=sc::config$schemas$results_normomo_standard),
      args = list(
        folder = "sykdomspulsen_norsyss_restricted_output/covid19_at_risk/{argset$today}",
        filename = "covid19_areas_at_risk_{argset$today}.docx"
      )
    )
  )

  # ui_covid19_nordic ----
  sc::add_task(
    sc::task_from_config(
      name = "ui_covid19_nordic",
      type = "ui",
      action = "sykdomspulsen::ui_covid19_nordic",
      schema = list(input=sc::config$schemas$results_covid19_nordic),
      db_table = "results_covid19_nordic",
      for_each_plan = list("border" = config$border),
      args = list(
        folder = "sykdomspulsen_covid19_nordic_output/{argset$today}",
        filename = "covid19_nordic_{argset$today}.xlsx"
      )
    )
  )



  sc::add_task(
    sc::task_from_config(
      name = "ui_threshold_plot_msis",
      type = "ui",
      action = "ui_create_threshold_plot",
      db_table = "results_simple",
      schema = NULL,
      for_each_plan = list("location_code" = "all", "tag_outcome" = c("Kikoste", "Campylobacteriose")),
      args = list(
        filename = "{location_code}.png",
        folder = " {tag_outcome}/{today}"
      ),
      filter = "year > 2010 & source == 'data_msis'"
    )
  )




  sc::add_task(
    sc::task_from_config(
      name = "ui_archive_results_norsyss_standard",
      type = "data",
      schema=list(input=sc::config$schemas$results_norsyss_standard),
      action="ui_archive_results",
      args = list(
        folder = "norsyss_qp",
        years = 2
      )
    )
  )




 p <- plnr::Plan$new(use_foreach=T)
 for(i in 1:30){
   p$add_analysis(fn = function(data, argset, schema){Sys.sleep(1)})
 }
 sc::add_task(
   sc::Task$new(
     name = "test_parallel_1",
     type = "analysis",
     plans = list(p),
     schema = c("output" = sc::config$schemas$results_normomo_standard),
     cores = min(6, parallel::detectCores())
   )
 )

 sc::add_task(
   sc::Task$new(
     name = "test_parallel_2",
     type = "analysis",
     plans = list(p,p),
     schema = c("output" = sc::config$schemas$results_normomo_standard),
     cores = min(6, parallel::detectCores())
   )
 )

}

test_parallel <- function(data, argset, schema){

}
