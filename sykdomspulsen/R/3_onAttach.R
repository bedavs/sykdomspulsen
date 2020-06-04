.onAttach <- function(libname, pkgname) {
  packageStartupMessage("\nAvailable tasks:")
  for(i in tm_get_task_names()){
    packageStartupMessage(glue::glue("Rscript -e \"sykdomspulsen::tm_run_task('{i}')\""))
  }
  packageStartupMessage("\ndevtools::install_local('/git/sykdomspulsen/sykdomspulsen/', force=T, upgrade='never')")
}
