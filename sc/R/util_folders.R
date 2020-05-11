#' This function gets the right folder for results
#' @param type input, output
#' @param ... Second level and beyond
#' @export
path <- function(type="output", ...) {
  stopifnot(type %in% c("input","output"))

  start_location <- dplyr::case_when(
    type == "input" ~ config$path_input,
    type == "output" ~ config$path_output
  )

  paste0(start_location,"/",glue::glue(fs::path(...)))
}


#' Create latest folder
#'
#' This function copies results_folder/date til results_folder/latest
#' @param results_folder_name name of the results folder
#' @param date the date of extraction
#' @export
create_latest_folder <- function(results_folder_name, date) {
  from_folder <- path("output", results_folder_name, date)
  to_folder <- path("output", results_folder_name, "latest")
  processx::run("cp", c("-rT", from_folder, to_folder))
}
