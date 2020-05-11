

#' Create latest folder
#'
#' This function copies results_folder/date til results_folder/latest
#' @param results_folder_name name of the results folder
#' @param date the date of extraction
#' @export
create_latest_folder <- function(results_folder_name, date) {
  from_folder <- sc::path("output", results_folder_name, date)
  to_folder <- sc::path("output", results_folder_name, "latest")
  processx::run("cp", c("-rT", from_folder, to_folder))
}
