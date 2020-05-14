.onAttach <- function(libname, pkgname) {
  packageStartupMessage(glue::glue("sykdomspulsen {utils::packageVersion('sykdomspulsen')}"))

  packageStartupMessage("Available tasks:")
  for(i in tm_get_task_names()){
    packageStartupMessage(glue::glue("- sykdomspulsen::tm_run_task('{i}')"))
  }
}
