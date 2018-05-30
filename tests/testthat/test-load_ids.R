context("load_ids")

cb_old <- readClipboard(raw = T)

writeClipboard(c(charToRaw("1083079037\r\n0396006198\r\n1389725782"),
                 as.raw(0x00)))
# Raw is added to invoke "incomplete final line error".

test_that("incomplete final line", {
  expect_silent(load_ids(NULL, use_clipboard = T)) %>%
    expect_equal(c("1083079037", "0396006198", "1389725782"))
  expect_error(load_ids("data/test_id.csv", col = 3), "Cannot find")
  expect_equal(load_ids("data/test_id.csv", col = 2, nrows = 4, skip = 1),
               c("1083079037", "396006198", "1389725782", "123456789"))
})

writeClipboard(cb_old)

test_that("validate ids", {
  cat("\nPUSH ENTER\n")
  expect_message(validate_ids(c("1083079037", "0396006198", "1389725782", NA)),
                 "Input contains NAs")
  expect_error(validate_ids("A"))
  expect_error(validate_ids("1.1"))
  expect_error(validate_ids(12345678))
  expect_equal(validate_ids(as.factor("1083079037")), "1083079037")
  expect_equal(validate_ids(396006198), "0396006198")
})

