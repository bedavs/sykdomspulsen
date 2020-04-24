.onAttach <- function(libname, pkgname) {
  packageStartupMessage(glue::glue("sykdomspulsen {utils::packageVersion('sykdomspulsen')}"))
}
