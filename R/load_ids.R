# load_ids
#' Loads cattle ID numbers from a csv file or clipboard
#'
#' @param input Path to a input csv file
#' @param use_clipboard If TRUE, clipboard is used instead of a input file
#' @param col The column number contains cattle ID numbers.
#'   When use_clipboard = T, this parameter is ignored.
#' @param skip,nrows,... Be passed to \code{\link{read.csv}}
#' @return Loaderd cattle id numbers
#'
#' @importFrom utils askYesNo read.csv read.table
load_ids <- function(input, use_clipboard = F, col = 1, skip = 0, nrows = -1, ...) {
  if (use_clipboard) {
    # ids <-
    #   tryCatch(read.table(file = "clipboard", colClasses = "character", ...),
    #            warning = function(e) {
    #              if (grep("incomplete final line", e$message) == 0) {
    #                warning(e)
    #              } else {
    #                rm(e)
    #                read.table(file = "clipboard", colClasses = "character",
    #                           skip = skip, nrows = nrows, ...)
    #              }
    #            })
    ids <- withCallingHandlers(
      read.table(file = "clipboard", colClasses = "character",
                 skip = skip, nrows = nrows, ...),
      warning = function(w) {
        if (any(grepl("incomplete final line", w))) {
          invokeRestart("muffleWarning")
        }
      })
    ids <- as.vector(as.matrix(ids))
  } else {
    ids <- read.csv(file = input,
                    colClasses = "character", skip = skip, nrows = nrows, ...)
    if (!is.vector(ids)) {
      ids <- try(ids[, col], silent = T)
      if (class(ids) == "try-error") {
        stop("Cannot find the specified column")
      }
    }
  }

  return(ids)
}


# validate_ids
#' Validates cattle ID numbers
#'
#' @param ids A vector contains cattle ID numbers.
#'   If an factor vector is supplied, it's interpretted as literally.
#' @return Cattle ID numbers as character
#'
#' @importFrom utils askYesNo read.csv read.table
validate_ids <- function(ids) {
  ids <- as.character(ids)  # To interpret a factor vector literally

  if (sum(is.na(ids)) != 0) {
    message("Input contains NAs.\n",
            "If continue, NAs will be ignored.")
    ans <- askYesNo("Continue?", prompts = c("Y/n/c"))
    if (!is.na(ans) | ans) {
      ids <- ids[!is.na(ids)]
    } else {
      stop()
    }
  }

  tryCatch(as.numeric(ids),
           warning = function(e) {
             stop("Input must be integers")
           })
  if (sum(as.numeric(ids) %% 1) != 0) {
    stop("Input must be integers")
  }
  ids <- as.character(ids)

  digit <- nchar(ids)
  if (sum(digit == 10 | digit == 9) != length(digit)) {
    stop("Input must be 9 or 10 digit integers")
  }

  ids[which(digit == 9)] <- paste(0, ids[which(digit == 9)], sep = "")

  return(ids)
}

