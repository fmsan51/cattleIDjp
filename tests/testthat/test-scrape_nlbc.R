context("scrape_nlbc")

expected1 <- read.csv("data/expected_output1.csv",
                      stringsAsFactors = F)
expected1[, 1] <- as.numeric(expected1[, 1])

test_that("scrape_nlbc", {
  cat("\nWait about 10 sec.\n")
  expect_warning(scrape_nlbc(c("1389725782", "0123456789"),
                             output = "data/actual_output.csv",
                             append = F),
                 msg_scrape$cannot_find) %>%
    expect_identical(expected1)
  cat("\nWait about 10 sec.\n")
  Sys.sleep(5)
  scrape_nlbc(c("1381820713"), output = "data/actual_output.csv")
  expect_identical(read.csv("data/actual_output.csv",
                            stringsAsFactors = F),
                   read.csv("data/expected_output2.csv",
                            stringsAsFactors = F))
})


