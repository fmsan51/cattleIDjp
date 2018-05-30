context("sysdata")

# library(readr)
# library(stringr)

msgs <- list("msg_make_dialog",
             "msg_dialog_finished",
             "msg_info",
             "msg_scrape")
vars_in_msgs <- character(0)
for (msg in msgs) {
  vars_in_msgs <- c(vars_in_msgs,
                    paste0(msg, "$", names(
                      eval(parse(text = msg))
                    )))
}

files <- list.files("../../R", pattern = "\\.R$")

code <- ""
for (file in files) {
  code <- paste0(code,
                 readr::read_file(paste0("../..//R/", file)),
                 collapse = "\r\n")
}
code <- stringr::str_remove_all(code, "(?m)^ *#.+$\r?\n")
vars_in_files <- stringr::str_extract_all(code,
                                          "(?<=[\\s(,])msg_\\S+\\$[^\\s),]+"
                                          )[[1]]

test_that("vars_in_msgs",
          expect_equal(sum(duplicated(vars_in_msgs)), 0))
test_that("msg_make_dialog",
          expect_setequal(vars_in_files, vars_in_msgs))
