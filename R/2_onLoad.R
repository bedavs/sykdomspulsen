.onLoad <- function(libname, pkgname) {
  system2("/bin/authenticate.sh", stdout = NULL)

  set_config()

  invisible()
}
