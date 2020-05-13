#' shortcut to get available task names
#' @export
tm_get_task_names <- function(){
  sc::tm_get_task_names()
}

#' Shortcut to run task
#' @param task_name Name of the task
#' @param index_plan Not used
#' @param index_argset Not used
#' @export
tm_run_task <- function(task_name, index_plan = NULL, index_argset = NULL) {
  message(glue::glue("sykdomspulsen {utils::packageVersion('sykdomspulsen')}"))

  sc::tm_run_task(
    task_name = task_name,
    index_plan = index_plan,
    index_argset = index_argset
  )
}
