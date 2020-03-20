.onLoad <- function(libname, pkgname) {
  system("/bin/authenticate.sh")

  set_config()

  invisible()
}
