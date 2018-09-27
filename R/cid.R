# cid
#' Search cattle information by ID
#'
#' Searches cattle information from https://www.id.nlbc.go.jp/ by cattle identification numbers and output a csv file.
#'
#' Searches and downloads cattle information from database of National Livestock Breeding Center, Japan (https://www.id.nlbc.go.jp/) by cattle identification numbers and outputs a csv file contains cattle information.
#'
#' \code{cid_dialog} opens dialog to choose an input file and an output file and set options. This one is most recommended.
#'
#' \code{cid_csv} uses a csv file as input.
#'
#' \code{cid_vector} uses a vector as input.
#'
#' \code{cid_clipboard} uses the clipboard as input.
#'
#' Search speed is about 10-12 cattles/min.
#'
#' @name cid
#' @rdname cid
#'
#' @param input Path to a input csv file (\code{cid_csv}) or a vector (integer/numeric/character/factor) (\code{cid_vector}). A factor vector is interpretted 'literally'.
#' @param output Path to a output csv file.
#'   If \code{NULL}, no csv file will be made.
#' @param append If an output file is already exist, append output file (T)
#'   or create a new file (F). When \code{append = T}, ERROR rows in previous
#'   output file will be removed.
#' @param col The column number of the input file contains cattle ID numbers.
#' @param skip,nrows,... Be passed to \code{\link{read.csv}}
#' @param encoding Encoding of file names
#' @param fileEncoding Encoding of the input/output file. See \code{\link{file}}.
#'
#' @return A data.frame contains cattle information
#'
#' @examples
#' \dontrun{
#' cid_dialog()
#' }
#'
#' id <- c(1083079037, 0123456789, 0396006198, 1389725782)
#' cid_vector(input = id, output = NULL)
NULL



# cid_dialog
#' @rdname cid
#' @export
cid_dialog <- function(encoding = getOption("encoding"),
                       fileEncoding = getOption("encoding")) {
  loc <- Sys.getlocale("LC_CTYPE")
  Sys.setlocale("LC_CTYPE", "Japanese")
  on.exit(Sys.setlocale("LC_CTYPE", loc))

  res_dialog <- make_dialog(encoding)
  ids <- validate_ids(res_dialog$ids)
  info <- scrape_nlbc(ids, res_dialog$output, append = res_dialog$append,
                      fileEncoding = fileEncoding, gui_pb = T)
  dialog_finished()

  invisible(info)
}

# cid_csv
#' @rdname cid
#' @export
cid_csv <- function(input, output = "cattle_info.csv", append = T, col = 1,
                    skip = 0, nrows = -1, fileEncoding = getOption("encoding"),
                    ...) {
  loc <- Sys.getlocale("LC_CTYPE")
  Sys.setlocale("LC_CTYPE", "Japanese")
  on.exit(Sys.setlocale("LC_CTYPE", loc))

  ids <- load_ids(input = input, col = col, skip = skip, nrows = nrows,
                  fileEncoding = fileEncoding, ...)
  ids <- validate_ids(ids)
  info <- scrape_nlbc(ids, output, append = append,
                      fileEncoding = fileEncoding, gui_pb = F)

  if (is.null(output)) {
    return(info)
  } else {
    invisible(info)
  }
}


#' @rdname cid
#' @export
cid_vector <- function(input, output = "cattle_info.csv",
                       append = T, fileEncoding = getOption("encoding")) {
  loc <- Sys.getlocale("LC_CTYPE")
  Sys.setlocale("LC_CTYPE", "Japanese")
  on.exit(Sys.setlocale("LC_CTYPE", loc))

  ids <- validate_ids(input)
  info <- scrape_nlbc(ids, output, append = append,
                      fileEncoding = fileEncoding, gui_pb = F)

  if (is.null(output)) {
    return(info)
  } else {
    invisible(info)
  }
}


# cid_clipboard
#' @rdname cid
#' @export
cid_clipboard <- function(output = "cattle_info.csv", append = T,
                          fileEncoding = getOption("encoding")) {
  loc <- Sys.getlocale("LC_CTYPE")
  Sys.setlocale("LC_CTYPE", "Japanese")
  on.exit(Sys.setlocale("LC_CTYPE", loc))

  ids <- load_ids(use_clipboard = T, fileEncoding = fileEncoding)
  ids <- validate_ids(ids)
  info <- scrape_nlbc(ids, output, append = append,
                      fileEncoding = fileEncoding, gui_pb = F)

  if (is.null(output)) {
    return(info)
  } else {
    invisible(info)
  }
}
