#' @export
.cid_locale <- Sys.getlocale("LC_CTYPE")

.onLoad <- function(libname, pkgname) {
  Sys.setlocale("LC_CTYPE", "Japanese")
}

.onUnload <- function(libname, libpath) {
  Sys.setlocale("LC_CTYPE", .cid_locale)
}
