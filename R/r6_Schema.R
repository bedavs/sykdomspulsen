#' schema class description
#'
#' @import data.table
#' @import R6
#' @export Schema
Schema <- R6Class("Schema",
                  public = list(
                    dt = NULL,
                    conn = NULL,
                    db_config = NULL,
                    db_table = NULL,
                    db_field_types = NULL,
                    db_load_folder = NULL,
                    keys = NULL,
                    keys_with_length = NULL,
                    initialize = function(dt = NULL, conn = NULL, db_config = NULL, db_table, db_field_types, db_load_folder, keys) {
                      self$dt <- dt
                      self$conn <- conn
                      self$db_config <- db_config
                      self$db_table <- db_table
                      self$db_field_types <- db_field_types
                      self$db_load_folder <- db_load_folder
                      self$keys <- keys
                      self$keys_with_length <- keys

                      ind <- self$db_field_types[self$keys] == "TEXT"
                      if (sum(ind) > 0) {
                        self$keys_with_length[ind] <- paste0(self$keys_with_length[ind], " (40)")
                      }
                      if (!is.null(self$conn)) self$db_create_table()
                    },
                    db_connect = function(db_config = self$db_config) {
                      self$conn <- get_db_connection(db_config=db_config)
                      use_db(self$conn, db_config$db)
                      self$db_create_table()
                    },
                    db_disconnect = function() {
                      if (!is.null(self$conn)) DBI::dbDisconnect(self$conn)
                    },
                    db_add_constraint = function(){
                      add_constraint(
                        conn = self$conn,
                        table = self$db_table,
                        keys = self$keys
                      )
                    },
                    db_drop_constraint = function(){
                      drop_constraint(
                        conn = self$conn,
                        table = self$db_table
                      )
                    },
                    db_create_table = function() {
                      create_tab <- TRUE
                      if (DBI::dbExistsTable(self$conn, self$db_table)) {
                        if (!self$db_check_fields_match()) {
                          message(glue::glue("Dropping table {self$db_table} because fields dont match"))
                          self$db_drop_table()
                        } else {
                          create_tab <- FALSE
                        }
                      }

                      if (create_tab) {
                        message(glue::glue("Creating table {self$db_table}"))
                        create_table(self$conn, self$db_table, self$db_field_types, self$keys)
                        self$db_add_constraint()
                      }
                    },
                    db_drop_table = function() {
                      if (DBI::dbExistsTable(self$conn, self$db_table)) {
                        DBI::dbRemoveTable(self$conn, self$db_table)
                      }
                    },
                    db_check_fields_match = function() {
                      fields <- DBI::dbListFields(self$conn, self$db_table)
                      retval <- identical(fields, names(self$db_field_types))
                      if(retval == FALSE){
                        message(glue::glue(
                          "given fields: {paste0(names(self$db_field_types),collapse=', ')}\n",
                          "db fields: {paste0(fields,collapse=', ')}"
                        ))
                      }
                      return(retval)
                    },
                    db_load_data_infile = function(newdata, verbose = TRUE) {
                      infile <- random_file(self$db_load_folder)
                      load_data_infile(
                        conn = self$conn,
                        db_config = self$db_config,
                        table = self$db_table,
                        dt = newdata,
                        file = infile
                      )
                    },
                    db_upsert_load_data_infile = function(newdata, drop_indexes = NULL, verbose = TRUE) {
                      infile <- random_file(self$db_load_folder)

                      upsert_load_data_infile(
                        # conn = self$conn,
                        db_config = self$db_config,
                        table = self$db_table,
                        dt = newdata[, names(self$db_field_types), with = F],
                        file = infile,
                        fields = names(self$db_field_types),
                        keys = self$keys,
                        drop_indexes = drop_indexes
                      )
                    },
                    db_drop_all_rows = function() {
                      drop_all_rows(self$conn, self$db_table)
                    },
                    db_drop_rows_where = function(condition){
                      drop_rows_where(self$conn, self$db_table, condition)
                    },
                    get_data = function(...) {
                      dots <- dplyr::quos(...)
                      params <- c()

                      for (i in seq_along(dots)) {
                        temp <- rlang::quo_text(dots[[i]])
                        temp <- stringr::str_extract(temp, "^[a-zA-Z0-9]+")
                        params <- c(params, temp)
                      }

                      if (length(params) > length(keys)) {
                        stop("Too many requests")
                      }
                      if (sum(!params %in% keys)) {
                        stop("names(...) not in keys")
                      }
                      if (nrow(self$dt) > 0 | ncol(self$dt) > 0) {
                        x <- self$get_data_dt(...)
                      } else {
                        x <- self$get_data_db(...)
                      }
                      return(x)
                    },
                    get_data_dt = function(...) {
                      dots <- dplyr::quos(...)
                      txt <- c()
                      for (i in seq_along(dots)) {
                        txt <- c(txt, rlang::quo_text(dots[[i]]))
                      }
                      if (length(txt) == 0) {
                        return(self$dt)
                      } else {
                        txt <- paste0(txt, collapse = "&")
                        return(self$dt[eval(parse(text = txt))])
                      }
                    },
                    get_data_db = function(...) {
                      dots <- dplyr::quos(...)
                      retval <- self$conn %>%
                        dplyr::tbl(self$db_table) %>%
                        dplyr::filter(!!!dots) %>%
                        dplyr::collect()
                      setDT(retval)
                      return(retval)
                    },
                    dplyr_tbl = function() {
                      retval <- self$conn %>%
                        dplyr::tbl(self$db_table)
                      return(retval)
                    },

                    add_index_db = function() {
                      txt <- glue::glue_collapse(glue::glue("`{self$keys}`(20)"), sep = ",")
                      DBI::dbExecute(
                        self$conn,
                        glue::glue("ALTER TABLE `{self$db_table}` ADD INDEX `ind1` ({txt})")
                      )
                    },

                    identify_dt_that_exists_in_db = function() {
                      setkeyv(self$dt, self$keys)
                      from_db <- self$get_data_db()
                      setkeyv(from_db, self$keys)
                      self$dt[, exists_in_db := FALSE]
                      self$dt[from_db, exists_in_db := TRUE]
                    }
                  ),
                  private = list(
                    finalize = function() {
                      # self$db_disconnect()
                    }
                  )
)
