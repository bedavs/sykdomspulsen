#' ui_normomo_data_files
#' @param data a
#' @param argset a
#' @param schema a
#' @export
ui_normomo_data_files <- function(data, argset, schema) {
  if(plnr::is_run_directly()){
    sc::tm_update_plans("ui_normomo_data_files")
    data <- sc::tm_get_data("ui_normomo_data_files", index_plan=1)
    argset <- sc::tm_get_argset("ui_normomo_data_files", index_plan=1, index_argset = 1)
    schema <- sc::tm_get_schema("ui_normomo_data_files")
  }

  d <- copy(data$data)

  # folder
  folder <- sc::path("output",argset$folder)
  fs::dir_create(folder)
  file <- glue::glue(argset$file)
  filepath <- fs::path(folder,file)

  writexl::write_xlsx(
    d,
    filepath
  )

}
