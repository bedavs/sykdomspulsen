#' Permission
#' @import R6
#' @export
Permission <- R6::R6Class(
  "Permission",
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    key = NULL,
    value = NULL,
    dev_always_performs = FALSE,
    production_days = c(1:7),
    db_schema = NULL,
    initialize = function(
      key,
      value,
      production_days = c(1:7)
      ) {
      value <<- value
      production_days <<- production_days

      if (is_final()) {
        key <<- glue::glue("FINAL_{key}")
      } else {
        key <<- glue::glue("PRELIM_{key}")
      }

      field_types <- c(
        "xkey" = "TEXT",
        "value" = "TEXT"
      )

      keys <- c(
        "xkey"
      )

      db_schema <<- schema$new(
        db_config = config$db_config,
        db_table = "permission",
        db_field_types = field_types,
        db_load_folder = "/xtmp/",
        keys = keys,
        check_fields_match = TRUE
      )
    },
    has_permission = function() {
      permission <- TRUE

      old_value <- current_value()
      if (length(old_value) > 0) {
        if (value == old_value) {
          permission <- FALSE
        }
      }

      if (!config$is_production) {
        permission <- TRUE
      }

      if (config$is_production) {
        permission <- FALSE
      }

      return(permission)
    },
    revoke_permission = function() {
      db_schema$db_connect()

      to_upload <- data.table(
        xkey = key,
        value = as.character(value)
      )

      db_schema$db_upsert_load_data_infile(to_upload)
      db_schema$db_disconnect()
    },
    is_final = function() {
      today <- lubridate::wday(lubridate::today(), week_start = 1)
      return(today %in% production_days)
    },
    current_value = function() {
      temp <- tbl("permission") %>%
        dplyr::collect() %>%
        fd::latin1_to_utf8()

      return(temp[xkey == key, value])
    }
  )
)
