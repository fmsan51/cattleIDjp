context("make_dialog")
library(glue)

test_that("make_dialog", {
  cat("\nClick Cancel\n")
  expect_silent(try(make_dialog()))
  cat(glue::glue("
                 Input follows:
                 Input data: tests/testthat/data/test_id.csv
                   Line: 2
                   Row: from 2 to 4
                 "))
  expect_equal(make_dialog(),
               list(ids = c("1083079037", "396006198", "1389725782"),
                    output = paste0(getwd(), "/cattle_info.csv"),
                    append = T))
  cat("\nUncheck 'to append' option\n")
  expect_equal(make_dialog(),
               list(ids = c("test", letters[1:4]),
                    output = paste0(getwd(), "/cattle_info.csv"),
                    append = F))
  cat("\nClick OK\n")
  expect_null(dialog_finished())
})


