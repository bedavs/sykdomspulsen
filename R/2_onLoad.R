.onLoad <- function(libname, pkgname) {
  try(system2("/bin/authenticate.sh", stdout = NULL),TRUE)

  set_config()

  invisible()
}
