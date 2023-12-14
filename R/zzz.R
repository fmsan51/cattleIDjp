#' @export
.cid_locale <- Sys.getlocale("LC_CTYPE")

.onLoad <- function(libname, pkgname) {
  Sys.setlocale("LC_CTYPE", "ja_JP.UTF-8")
}

.onUnload <- function(libname, libpath) {
  Sys.setlocale("LC_CTYPE", .cid_locale)
}
