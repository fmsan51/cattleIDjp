# scrape_nlbc
#' Scrape NLBC websites
#'
#' @param ids Cattle ID numbers as a character vector
#' @param output Path to output file.
#'   If \code{NULL}, no csv file will be made.
#' @param append If an output file is already exist, append output file (T)
#'   or create a new file (F). When append = T, ERROR rows in previous output
#'   file will be removed.
#' @param fileEncoding Encoding of the input/output file. See \code{\link{file}}.
#' @param gui_pb Show progress bar in a window (T) or in console (F)
#'
#' @importFrom utils write.table
#' @importFrom glue glue
scrape_nlbc <- function(ids, output = "cattle_info.csv", append = T,
                        fileEncoding = getOption("encoding"), gui_pb = F) {
  lng_ids <- length(ids)
  on.exit(write_log(err_file, flag_nocattle, now_scraping, lng_ids))
  on.exit(close(pb), add = T)
  on.exit(return(info), add = T)

  cat(paste(msg_scrape$estimate,
            Sys.time() + (lng_ids * 5.5) + (lng_ids %/% 50 * 30),
            "\n", sep = " "))

  err_file <- file.path(dirname(output), "cid_error.log")
  file.create(err_file)

  # Make output file
  if (!is.null(output) & (file.exists(output) | append == F)) {
    table_title <- matrix(nrow = 0, ncol = 10)
    colnames(table_title) <- c(msg_info$cattle, msg_info$farm)
    write.table(table_title, file = output, sep = ",",
                row.names = F, col.names = T, fileEncoding = fileEncoding)
  }

  # Output table
  info <- data.frame(matrix(nrow = 0, ncol = 10))
  colnames(info) <- c(msg_info$cattle, msg_info$farm)

  now_scraping <- 0
  scrape_start <- 1
  flag_end <- 0
  flag_error <- 0
  flag_nocattle <- 0
  errmsg_start <- NULL
  errmsg_end <- NULL
  env_nlbc <- environment()

  pb_max <- ifelse(lng_ids == 1, 2, lng_ids)
  pb <- ProgressBar(gui_pb,
                    title = msg_scrape$searching, min = 1, max = pb_max)

  repeat {

    if (scrape_start + 49 < lng_ids) {
      scrape_end <- scrape_start + 49
    } else {
      scrape_end <- lng_ids
      flag_end <- 1
    }

    err_catch <-
      try(scrape_50(scrape_start, scrape_end, ids, output, lng_ids,
                    fileEncoding, gui_pb, pb, env_nlbc),
          silent = T)
    if (class(err_catch) == "try-error") {
      flag_error <- 1
      # Continue scraping if a error is caused by
      # that there is no cattle correspoinding to a ID,
      # otherwise terminate scraping.
      if (attributes(err_catch)$condition$message == "err_nocattle") {
        if (flag_nocattle == 0) {
          cat("Following cattle were not in the database: ", file = err_file)
          flag_nocattle <- 1
        }
        cat(now_scraping, "", file = err_file, append = T)
        flag_end <- ifelse(now_scraping == lng_ids, 1, 0)
      } else {
        write_log(err_file, flag_nocattle, now_scraping, lng_ids)
        flag_unknown_error <- 1
        flag_end <- 1
      }
    }

    scrape_start <- now_scraping + 1

    if (flag_end == 1) {
      if (flag_error == 0) {
        cat("\n", msg_scrape$finished, "\n")
        file.remove(err_file)
      } else {
        if (flag_nocattle == 1) {
          warning("Some cattle were not found on database.", call. = F)
        }
        if (flag_unknown_error == 1) {
          warning("Encontered unknown error.", call. = F)
        }
        warning("See cid_err.log.", call. = F)
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
#' @importFrom rvest html_nodes
#' @importFrom rvest html_table
scrape_info_cattle <- function(page) {
  nodes <- html_nodes(page, xpath = "//span/table[1]")
  table <- html_table(nodes, fill = T)[[1]]
  return(table)
}

# scrape_info_farm
#' Scrape information of farmer from a NLBC information page
#'
#' @param page html page
#' @importFrom rvest html_nodes
#' @importFrom rvest html_table
scrape_info_farm <- function(page) {
  nodes <- html_nodes(page, xpath = "//span/table[2]")
  table <- html_table(nodes, fill = T)[[1]]
  return(table[2:nrow(table), 2:ncol(table)])
}


# scrape_info
#' Scrape information from a NLBC information page
#'
#' @param page html page
#'
#' @importFrom methods cbind2
scrape_info <- function(page) {

  info_cattle <- tryCatch(scrape_info_cattle(page),
    error = function(e) {stop("err_nocattle")})
  colnames(info_cattle) <- msg_info$cattle

  info_farm <- scrape_info_farm(page)
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
#' @importFrom utils write.table
save2csv <- function(info_50, output, fileEncoding, env) {
  info <- get("info", envir = env)
  info <- rbind(info, info_50)
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
#' @import rvest
scrape_50 <- function(scrape_start, scrape_end, ids, output, lng_ids,
                      fileEncoding, gui_pb, pb, env_nlbc) {
  # Table to output
  info_50 <- matrix(nrow = 0, ncol = 10)
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
    rvest::html_session(
      "https://www.id.nlbc.go.jp/CattleSearch/search/agreement"),
    error = function(e) {
      e$message <- paste0(e$message, "\n",
                          "\tPossibly proxy error.")
      stop(e)
    }
  )
  agree_form <- rvest::html_form(agree_page)[[1]]
  agree_form[["fields"]][["method:goSearch"]] <- NULL
  agree_form[["fields"]][["method:goSearch.x"]] <- gosearch_x
  agree_form[["fields"]][["method:goSearch.y"]] <- gosearch_y
  # agree_form[["fields"]][["submit"]] <- fake_submit_button
  info_page <- rvest::submit_form(agree_page, agree_form, "method:goSearch.x")

  # scrape_info(info_page, ids, scrape_start)
  setProgressBar(gui_pb, pb, scrape_start,
                 title = sprintf("%d%%",
                                 round((scrape_start - 1) / lng_ids * 100)),
                 label = paste0(scrape_start, "/", lng_ids))

  for (i in scrape_start:scrape_end) {

    Sys.sleep(5)

    search_form <- rvest::html_form(info_page)[[1]]
    search_form <- rvest::set_values(search_form, 'txtIDNO' = ids[i])
    search_form[["fields"]][["method:doSearch"]] <- NULL
    search_form[["fields"]][["method:doSearch.x"]] <- dosearch_x
    search_form[["fields"]][["method:doSearch.y"]] <- dosearch_y
    # search_form[["fields"]][["submit"]] <- fake_submit_button
    info_page <- rvest::submit_form(info_page, search_form,
                                    "method:doSearch.x")

    assign("now_scraping", i, envir = env_nlbc)
    info_50 <- rbind(info_50, scrape_info(info_page))

    setProgressBar(gui_pb, pb, i,
                   title = sprintf("%d%%", round((i - 1) / lng_ids * 100)),
                   label = paste0(i, "/", lng_ids))
  }
}


#' Output error-log
#'
#' @param err_file Path to the error log file
#' @param flag_nocattle Flag whether an error had occured due to no cattle in the database.
#' @param now_scrapoing Index of ID of current scraping cattle
#' @param lng_ids Length of IDs
write_log <- function(err_file, flag_nocattle, now_scraping, lng_ids) {
  if (flag_nocattle == 1) {
    cat("\n", file = err_file)
  }
  cat(glue::glue(
    "[{Sys.time()}] Encountered unknown error and \\",
    "information for following cattle were not obtained: \\",
    "{now_scraping}{ifelse(lng_ids == now_scraping, \"\", msg_scrape$after)}"
  ), file = err_file, append = T)
  invisible(NULL)
}
