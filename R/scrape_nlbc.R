# scrape_nlbc
#' Scrape NLBC websites
#'
#' @param ids Cattle ID numbers as a character vector
#' @param output Path to output file.
#'   If \code{NULL}, no csv file will be made.
#' @param append If an output file is already exist, append output file (T)
#'   or create a new file (F).
#' @param fileEncoding Encoding of the input/output file. See \code{\link{file}}.
#' @param gui_pb Show progress bar in a window (T) or in console (F)
#'
#' @importFrom utils write.table
scrape_nlbc <- function(ids, output = "cattle_info.csv", append = T,
                        fileEncoding = getOption("encoding"), gui_pb = F) {
  lng_ids <- length(ids)
  err_catch <- NULL
  on.exit(
    write_log(err_file, flag_error, ids, now_scraping, lng_ids, err_catch)
  )
  on.exit(close(pb), add = T)
  on.exit(return(info), add = T)

  cat(paste(msg_scrape$estimate,
            round(Sys.time() + (lng_ids * 5.5) + (lng_ids %/% 50 * 30)),
            "\n", sep = " "))

  # Make output file
  if (is.null(output)) {
    err_file_name <- "cid_error.log"
    err_file <- file.path(getwd(), err_file_name)
  } else {
    err_file_name <- paste0("cid_error_",
                            gsub("\\..+$", "", basename(output)),
                            ".log")
    err_file <- file.path(dirname(output), err_file_name)
    if (file.exists(output) & append == T) {
      prev_out <- read.csv(output, header = T)
      prev_out <- prev_out[(!is.na(prev_out[, 2])), ]
      write.table(prev_out, file = output, sep = ",",
                  row.names = F, col.names = T, fileEncoding = fileEncoding)
    } else {
      table_title <- matrix(nrow = 0,
                            ncol = length(c(msg_info$cattle, msg_info$farm)))
      colnames(table_title) <- c(msg_info$cattle, msg_info$farm)
      write.table(table_title, file = output, sep = ",",
                  row.names = F, col.names = T, fileEncoding = fileEncoding)
    }
  }
  file.create(err_file)

  # Output table
  info <- data.frame(
    matrix(NA_character_,
           nrow = 0, ncol = length(c(msg_info$cattle, msg_info$farm)))
  )
  colnames(info) <- c(msg_info$cattle, msg_info$farm)

  now_scraping <- 0
  scrape_start <- 1
  flag_end <- F
  flag_error <- F
  flag_unknown_error <- F
  flag_nocattle <- F
  env_nlbc <- environment()

  pb_max <- ifelse(lng_ids == 1, 2, lng_ids)
  pb <- ProgressBar(gui_pb,
                    title = msg_scrape$searching, min = 1, max = pb_max)

  repeat {

    if (scrape_start + 49 < lng_ids) {
      scrape_end <- scrape_start + 49
    } else {
      scrape_end <- lng_ids
      flag_end <- T
    }

    err_catch <-
      try(scrape_50(scrape_start, scrape_end, ids, output, lng_ids,
                    fileEncoding, gui_pb, pb, env_nlbc),
          silent = T)
    if (class(err_catch) == "try-error") {
      flag_error <- T
      attributes(err_catch)$condition$message <-
        paste0("scrape_50(): ", attributes(err_catch)$condition$message)
      # Continue scraping if a error is caused by
      # that there is no cattle correspoinding to a ID,
      # otherwise terminate scraping.
      if (!is.null(attributes(err_catch)$condition$err_nocattle) &&
          attributes(err_catch)$condition$err_nocattle) {
        flag_nocattle <- T
        flag_end <- (now_scraping == lng_ids)
      } else {
        flag_unknown_error <- T
        flag_end <- T
      }
    }

    scrape_start <- now_scraping + 1

    if (flag_end) {
      if (!flag_error) {
        cat("\n", msg_scrape$finished, "\n")
        file.remove(err_file)
      } else {
        if (flag_nocattle) {
          warning("Some cattle were not found on the database.", call. = F)
        }
        if (flag_unknown_error) {
          warning("Encontered unknown error.", call. = F)
        }
        warning("See ", err_file_name, ".", call. = F)
      }
      break
    }

    Sys.sleep(30)

  }
}


# scrape_info_cattle
#' Scrape information of cattle from a NLBC information page
#'
#' @param page html page
#' @importFrom rvest html_elements
#' @importFrom rvest html_table
scrape_info_cattle <- function(page) {
  nodes <- html_elements(page, xpath = "//span/table[1]")
  if (length(nodes) == 0) {
    stop("no cattle info")
  } else {
    table <- html_table(nodes, convert = F)[[1]]
    return(table)
  }
}

# scrape_info_farm
#' Scrape information of farmer from a NLBC information page
#'
#' @param page html page
#' @importFrom rvest html_elements
#' @importFrom rvest html_table
scrape_info_farm <- function(page) {
  nodes <- html_elements(page, xpath = "//span/table[2]")
  table <- html_table(nodes, convert = F)[[1]]
  if (ncol(table) == 2) {
    return(NULL)
  } else {
    return(table[2:nrow(table), 2:ncol(table)])
  }
}


# scrape_info
#' Scrape information from a NLBC information page
#'
#' @param page html page
#'
#' @importFrom methods cbind2
scrape_info <- function(page) {
  info_cattle <- tryCatch(scrape_info_cattle(page),
    error = function(e) {
      e$err_nocattle <- (e$message == "no cattle info")
      e$message <- paste0("scrape_info_cattle(): ", e$message)
      stop(e)
    })

  info_farm <- tryCatch(scrape_info_farm(page),
    error = function(e) {
      e$message <- paste0("scrape_info_farm(): ", e$message)
      stop(e)
    })
  if (is.null(info_farm)) {
    info_farm <- as.data.frame(matrix(ncol = length(msg_info$farm)))
  }
  colnames(info_farm) <- msg_info$farm
  rownames(info_farm) <- NULL

  info_cattle <- info_cattle[rep(1, nrow(info_farm)), , drop = F]
  rownames(info_cattle) <- NULL

  return(cbind2(info_cattle, info_farm))
}


# save2csv
#' Save scraped information to a csv file
#'
#' @param info_50 A table contains scraped information
#' @param output Path to the output file
#' @param fileEncoding Encoding of the output file. See \code{\link{file}}.
#' @param env Envirionment which has a variable "info". (Don't set it manually.)
#'
#' @importFrom dplyr bind_rows
#' @importFrom utils write.table
save2csv <- function(info_50, output, fileEncoding, env) {
  info <- get("info", envir = env)
  info <- dplyr::bind_rows(info, info_50)
  assign("info", info, envir = env)
  if (!is.null(output)) {
    write.table(info_50, file = output, sep = ",",
                row.names = F, col.names = F, append = T,
                fileEncoding = fileEncoding)
  }
}


#' Scrape 50 cattle
#'
#' @param scrape_start,scrape_end Define lines to scrape
#' @param ids A character vector of cattle ID number
#' @param output Path to the output file
#' @param lng_ids Length of ids
#' @param fileEncoding Encoding of the output file. See \code{\link{file}}.
#' @param gui_pb Show progress bar in a window (T) or in console (F)
#'   (Don't set it manually)
#' @param pb Progress bar (Don't set it manually)
#' @param env_nlbc Parent frame (Don't set it manually)
#'
#' @importFrom dplyr bind_rows
#' @importFrom rvest html_form html_form_set session session_submit
scrape_50 <- function(scrape_start, scrape_end, ids, output, lng_ids,
                      fileEncoding, gui_pb, pb, env_nlbc) {
  # Table to output
  info_50 <- as.data.frame(
    matrix(NA_character_,
           nrow = 0, ncol = length(c(msg_info$cattle, msg_info$farm)))
  )
  colnames(info_50) <- c(msg_info$cattle, msg_info$farm)
  on.exit(save2csv(info_50, output, fileEncoding, env_nlbc))

  assign("now_scraping", scrape_start, envir = env_nlbc)

  # Make submit button & data to POST
  gosearch_x <- list(name = "method:goSearch.x",
                     type = "image",
                     value = 0,
                     disabled = NULL,
                     readonly = NULL,
                     required = FALSE)
  attr(gosearch_x, "class") <- "input"
  gosearch_y <- gosearch_x
  gosearch_y$name <- "method:goSearch.y"
  dosearch_x <- gosearch_x
  dosearch_x$name <- "method:doSearch.x"
  dosearch_y <- gosearch_x
  dosearch_y$name <- "method:doSearch.y"
  # fake_submit_button <- list(name = NULL,
  #                            type = "submit",
  #                            value = NULL,
  #                            disabled = NULL,
  #                            readonly = NULL,
  #                            required = FALSE)
  # attr(fake_submit_button, "class") <- "input"

  agree_page <- tryCatch(
    session(
      "https://www.id.nlbc.go.jp/CattleSearch/search/agreement"),
    error = function(e) {
      e$message <- paste0(e$message, "\n",
                          "\tPossibly proxy error.")
      stop(e)
    }
  )
  agree_form <- html_form(agree_page)[[1]]
  agree_form[["fields"]][["method:goSearch"]] <- NULL
  agree_form[["fields"]][["method:goSearch.x"]] <- gosearch_x
  agree_form[["fields"]][["method:goSearch.y"]] <- gosearch_y
  # agree_form[["fields"]][["submit"]] <- fake_submit_button
  info_page <- session_submit(agree_page, agree_form, "method:goSearch.x")

  # scrape_info(info_page, ids, scrape_start)
  setProgressBar(gui_pb, pb, scrape_start,
                 title = sprintf("%d%%",
                                 round((scrape_start - 1) / lng_ids * 100)),
                 label = paste0(scrape_start, "/", lng_ids))

  for (i in scrape_start:scrape_end) {

    Sys.sleep(5)

    search_form <- html_form(info_page)[[1]]
    search_form <- html_form_set(search_form, 'txtIDNO' = ids[i])
    search_form[["fields"]][["method:doSearch"]] <- NULL
    search_form[["fields"]][["method:doSearch.x"]] <- dosearch_x
    search_form[["fields"]][["method:doSearch.y"]] <- dosearch_y
    # search_form[["fields"]][["submit"]] <- fake_submit_button
    info_page <- session_submit(info_page, search_form,
                                       "method:doSearch.x")

    assign("now_scraping", i, envir = env_nlbc)
    info_50 <- dplyr::bind_rows(info_50,
      tryCatch(scrape_info(info_page),
               error = function(e) {
                 e$message = paste0("scrape_info(): ID ", ids[i], ": ", e$message)
                 stop(e)
               })
    )

    setProgressBar(gui_pb, pb, i,
                   title = sprintf("%d%%", round((i - 1) / lng_ids * 100)),
                   label = paste0(i, "/", lng_ids))
  }
}


#' Output error-log
#'
#' @param err_file Path to the error log file
#' @param flag_error Flag whether an error had occurred
#' @param ids IDs to search
#' @param now_scrapoing Index of ID of current scraping cattle
#' @param lng_ids Length of IDs
#' @param err_catch An object of class `try-error`
#'
#' @importFrom glue glue
write_log <- function(err_file, flag_error,
                      ids, now_scraping, lng_ids, err_catch) {
  if (flag_error) {
    cat(glue::glue("
      [{Sys.time()}] Encountered error and \\
      information for following cattle were not obtained: \\
      {ids[now_scraping]}\\
      {ifelse(lng_ids == now_scraping, \"\", msg_scrape$after)}\\
      {ifelse(is.null(err_catch), \"\",
       paste0(\"  \", attributes(err_catch)$condition$message))}"
    ), file = err_file)
  }
  invisible(NULL)
}
