
ui_normomo_data_files <- function(data, argset, schema) {
  if(FALSE){
    tm_update_plans("ui_normomo_data_files")
    data <- tm_get_data("ui_normomo_data_files", index_plan=1)
    argset <- tm_get_argset("ui_normomo_data_files", index_plan=1, index_argset = 1)
    schema <- tm_get_schema("ui_normomo_data_files")
  }

  d <- copy(data$data)

  # folder
  folder <- path("output",glue::glue(argset$folder))
  fs::dir_create(folder)
  file <- glue::glue(argset$file)
  filepath <- fs::path(folder,file)

  writexl::write_xlsx(
    d,
    filepath
  )

}
