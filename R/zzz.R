.onLoad <- function(libname, pkgname) {
  .cid_locale <- Sys.getlocale("LC_CTYPE")
  Sys.setlocale("LC_CTYPE", "Japanese")
  invisible(NULL)
}

.onUnload <- function(libname, libpath) {
  Sys.setlocale("LC_CTYPE", .cid_locale)
}
