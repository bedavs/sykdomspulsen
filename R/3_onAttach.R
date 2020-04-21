.onAttach <- function(libname, pkgname) {
  packageStartupMessage(glue::glue("sykdomspulsen {packageVersion('sykdomspulsen')}"))
}
